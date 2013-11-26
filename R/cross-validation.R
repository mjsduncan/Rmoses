## run moses on data set partitions and validate results on their compliments

#########################################
####    cross validation

f_K_fold <- function(Nobs,K=10){
    rs <- runif(Nobs)
    id <- seq(Nobs)[order(rs)]
    k <- as.integer(Nobs * seq(1, K-1) / K)
    k <- matrix(c(0, rep(k, each=2), Nobs), ncol = 2, byrow = TRUE)
    k[,1] <- k[,1]+1
    l <- lapply(seq.int(K), function(x, k, d)
        list(train=d[!(seq(d) %in% seq(k[x, 1],k[x, 2]))],
             test=d[seq(k[x,1],k[x,2])]),
        k=k,d=id)
    return(l)
}

## catools

# Generate some test data
x <- runif(100)*10 #Random values between 0 and 10
y <- x+rnorm(100)*.1 #y~x+error
dataset <- data.frame(x,y) #Create data frame
plot(dataset$x,dataset$y) #Plot the data

#install.packages("cvTools")
library(cvTools) #run the above line if you don't have this library

k <- 10 #the number of folds

folds <- cvFolds(NROW(dataset), K=k)
dataset$holdoutpred <- rep(0,nrow(dataset))

for(i in 1:k){
  train <- dataset[folds$subsets[folds$which != i], ] #Set the training set
  validation <- dataset[folds$subsets[folds$which == i], ] #Set the validation set

  newlm <- lm(y~x,data=train) #Get your new linear model (just fit on the train data)
  newpred <- predict(newlm,newdata=validation) #Get the predicitons for the validation set (from the model just fit on the train data)

  dataset[folds$subsets[folds$which == i], ]$holdoutpred <- newpred #Put the hold out prediction in the data set for later use
}

dataset$holdoutpred #do whatever you want with these predictions

##  caret example

require(caret)
flds <- createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)
names(flds)[1] <- "train"
# Then each element of flds is a list of indexes for each dataset. If your dataset is called dat, then 
dat[flds$train,] # gets you the training set, 
dat[ flds[[2]], ] # gets you the second fold set, etc.


##  balanced folds functions

permute.rows.Rsup <-function(x)

{
        dd <- dim(x)
        n <- dd[1]
        p <- dd[2]
        mm <- runif(length(x)) + rep(seq(n) * 10, rep(p, n))
        matrix(t(x)[order(mm)], n, p, byrow = TRUE)
}

balanced.folds.Rsup <- function(y, nfolds = min(min(table(y)), 10)) 

{

   totals <- table(y)
   fmax <- max(totals)
   nfolds <- min(nfolds, fmax)     
                                    # makes no sense to have more folds than the max class size
   folds <- as.list(seq(nfolds))
   yids <- split(seq(y), y)        
                                    # nice to get the ids in a list, split by class

   ##Make a big matrix, with enough rows to get in all the folds per class
   bigmat <- matrix(NA, ceiling(fmax/nfolds) * nfolds, length(totals))
   for(i in seq(totals)) {
     bigmat[seq(totals[i]), i] <- sample(yids[[i]])
   }
   smallmat <- matrix(bigmat, nrow = nfolds)       # reshape the matrix

   ## Now do a clever sort to mix up the NAs
   smallmat <- permute.rows.Rsup(t(smallmat))   ## Now a clever unlisting
                                                ## the "clever" unlist doesn't work when there are no NAs
                                                ##       apply(smallmat, 2, function(x)
                                                ##       x[!is.na(x)])
   res <-vector("list", nfolds)
   for(j in 1:nfolds) {
     jj <- !is.na(smallmat[, j])
     res[[j]] <- smallmat[jj, j]
   }
   return(res)
 }

#########################################################################################################
##
## do a bunch of supervised algorithms
## (default parameters, but some flexibility?)
##
##
## cross-validate
## boxplot results
## write table
## output significant genes/metagenes
##
## include:
## pelora (supclust)
## svm (e1071)
##
## pls (pls.pcr)
## lda (??)
## ann (??)
## pam (pamr)
##
##
## single wraps, and overall wrapper for comparison plots/tables?
##

 
########################################################################################################
##
## pelora.Rsup
##
##
##

require (supclust)

pelora.Rsup <- function(dat, pheno, genes,
                        noc=2, ## from pelora
                        k=10)  ## k fold cross validation (k=number of samples-1 gives complete leave-one-out

{ 

    nrow(dat)->numgenes

    dat<-t(dat)

    heaps = balanced.folds.Rsup(pheno,k) # create k balanced folds

    acc = numeric() # initialize list of accuracies
    
    pelgenes <- c(NULL) # list of predictor genes
    
    
    # run through all k folds
    for (i in 1:k)
    
    { 

    test= heaps[[i]] # indices of test data

    PEL = pelora(dat[-test,], pheno[-test],noc=noc) # train PEL on train set

    p = predict.pelora(PEL,newdata=dat[test,],type="cla") # predict labels of TEST set

    a = 100*sum(p == pheno[test])/length(test) # compute accuracy

    acc = c(acc,a) # collect accuracies
    
    ## get genes
    for (j in 1:noc)
    
        {
        pelgenes <- c(pelgenes,PEL$genes[[j]])
        } 

    }
    
    
    ## print results
    cat ("\ntotal accuracy=",mean(acc))
    cat ("\nsingle accuracies=",round(acc),"\n\n")
    
    ## ouput this as well?
    return(acc)
    
    
    ## output gene list
    for (l in 1:length(pelgenes))
    
        {
        if (pelgenes[l]>numgenes) pelgenes[l]<-pelgenes[l]-numgenes
        else pelgenes[l]<-pelgenes[l]
        }
        
    pgcount<-as.factor(pelgenes)
    summary(pgcount)->pgsum
    pgsum<-as.data.frame(pgsum)
    colnames(pgsum)<-c("pelora.clus")
            
    merge(genes,pgsum,by.x=0,by.y=0)->pgout
        
    ordp<-order(pgout$pelora.clus,decreasing=T)
    pgout2<-pgout[ordp,]
    
    rownames(pgout2)<-pgout2$Row.names
    pgout2<-pgout2[,2:ncol(pgout2)]
    
    #return (pgout2)

    

}


##########################################################################################################
##
## svm.Rsup
##
##
## support vector machines from e1071
##
##

require (e1071)

svm.Rsup <- function(dat, pheno, genes,
                        k=10)  ## k fold cross validation (k=number of samples-1 gives complete leave-one-out

{ 

    nrow(dat)->numgenes

    dat<-t(dat)
    
    pheno<-as.factor(pheno)

    heaps = balanced.folds.Rsup(pheno,k) # create k balanced folds

    acc = numeric() # initialize list of accuracies
    
    #svmgenes <- c(NULL) # list of predictor genes
    
    
    # run through all k folds
    for (i in 1:k)
    
    { 

    test= heaps[[i]] # indices of test data

    SVM = svm(dat[-test,], pheno[-test]) # train SVM on train set

    p = predict(SVM,dat[test,]) # predict labels of TEST set

    a = 100*sum(p == pheno[test])/length(test) # compute accuracy

    acc = c(acc,a) # collect accuracies
    
    }
    
    
    
    ## print results
    cat ("\ntotal accuracy=",mean(acc))
    cat ("\nsingle accuracies=",round(acc),"\n\n")
    ## ouput this as well?
    
    return(acc)

}

##########################################################################################################
##
## randomForest.Rsup
##
##
## from library randomForests!
##
##

require (randomForest)

randomForest.Rsup <- function(dat, pheno, #genes,
                        k=10)  ## k fold cross validation (k=number of samples-1 gives complete leave-one-out

{ 

    nrow(dat)->numgenes

    dat<-t(dat)
    
    pheno<-as.factor(pheno)

    heaps = balanced.folds.Rsup(pheno,k) # create k balanced folds

    acc = numeric() # initialize list of accuracies
    
    #svmgenes <- c(NULL) # list of predictor genes
    
    
    # run through all k folds
    for (i in 1:k)
    
    { 

    test= heaps[[i]] # indices of test data

    RF = randomForest(dat[-test,], pheno[-test]) # train SVM on train set

    p = predict(RF,dat[test,]) # predict labels of TEST set

    a = 100*sum(p == pheno[test])/length(test) # compute accuracy

    acc = c(acc,a) # collect accuracies
    
    }
    
    
    
    ## print results
    cat ("\ntotal accuracy=",mean(acc))
    cat ("\nsingle accuracies=",round(acc),"\n\n")
    ## ouput this as well?
    
    return(acc)

}



##########################################################################################################
##
## pls.Rsup
##
##
## from library pls.pcr
##
##

require (pls.pcr)

pls.Rsup <- function(dat, pheno, #genes,
                        k=10)  ## k fold cross validation (k=number of samples-1 gives complete leave-one-out

{ 

    nrow(dat)->numgenes

    dat<-t(dat)
    
    #pheno<-as.factor(pheno)

    heaps = balanced.folds.Rsup(pheno,k) # create k balanced folds

    acc = numeric() # initialize list of accuracies
    
    #svmgenes <- c(NULL) # list of predictor genes
    
    
    # run through all k folds
    for (i in 1:k)
    
    { 

    test= heaps[[i]] # indices of test data

    PLS = pls(dat[-test,], pheno[-test],validation="CV") # train SVM on train set

    p = predict(PLS,dat[test,],nlv=1) # predict labels of TEST set

    a = 100*sum(p == pheno[test])/length(test) # compute accuracy

    acc = c(acc,a) # collect accuracies
    
    }
    
    
    
    ## print results
    cat ("\ntotal accuracy=",mean(acc))
    cat ("\nsingle accuracies=",round(acc),"\n\n")
    ## ouput this as well?
    
    return(acc)

}


##########################################################################################################
##
## nnet.Rsup
##
##
## from nnet!
##
##

require (nnet)

nnet.Rsup <- function(dat, pheno, #genes,
                        k=10)  ## k fold cross validation (k=number of samples-1 gives complete leave-one-out

{ 

    nrow(dat)->numgenes

    dat<-t(dat)
    
    pheno<-as.factor(pheno)

    heaps = balanced.folds.Rsup(pheno,k) # create k balanced folds

    acc = numeric() # initialize list of accuracies
    
    #svmgenes <- c(NULL) # list of predictor genes
    
    
    # run through all k folds
    for (i in 1:k)
    
    { 

    test= heaps[[i]] # indices of test data

    NNET = nnet(dat[-test,], pheno[-test],size=1) # train SVM on train set

    p = predict(NNET,dat[test,]) # predict labels of TEST set

    a = 100*sum(p == pheno[test])/length(test) # compute accuracy

    acc = c(acc,a) # collect accuracies
    
    }
    
    
    
    ## print results
    cat ("\ntotal accuracy=",mean(acc))
    cat ("\nsingle accuracies=",round(acc),"\n\n")
    ## ouput this as well?
    
    return(acc)

}


##########################################################################################################
##
## knn.Rsup
##
##
## from library class!
##
##

require (class)

knn.Rsup <- function(dat, pheno, #genes,
                        k=10)  ## k fold cross validation (k=number of samples-1 gives complete leave-one-out

{ 

    nrow(dat)->numgenes

    dat<-t(dat)
    
    pheno<-as.factor(pheno)

    heaps = balanced.folds.Rsup(pheno,k) # create k balanced folds

    acc = numeric() # initialize list of accuracies
    
    #svmgenes <- c(NULL) # list of predictor genes
    
    
    # run through all k folds
    for (i in 1:k)
    
    { 

    test= heaps[[i]] # indices of test data

   #KNN 
   p= knn.cv(dat[test,], pheno[test]) # train SVM on train set

    #p = knn(KNN,dat[test,],pheno[-test]) # predict labels of TEST set

    a = 100*sum(p == pheno[test])/length(test) # compute accuracy

    acc = c(acc,a) # collect accuracies
    
    }
    
    
    
    ## print results
    cat ("\ntotal accuracy=",mean(acc))
    cat ("\nsingle accuracies=",round(acc),"\n\n")
    ## ouput this as well?
    
    return(acc)

}


##########################################################################################################
##
## pamr.Rsup
##
##
## from library pamr
##
##

require (pamr)

pamr.Rsup <- function(dat, pheno, #genes,
                        k=10)  ## k fold cross validation (k=number of samples-1 gives complete leave-one-out

{ 

    

    pam.dat <- list(x=(as.matrix(dat)),y=pheno)

    #nrow(dat)->numgenes

    #dat<-t(dat)
    
    pheno<-as.factor(pheno)

    heaps = balanced.folds.Rsup(pheno,k) # create k balanced folds

    acc = numeric() # initialize list of accuracies
    
    #svmgenes <- c(NULL) # list of predictor genes
    
    
    # run through all k folds
    for (i in 1:k)
    
    { 

    test= heaps[[i]] # indices of test data

    pam.dat.train <- list(x=(as.matrix(dat[,-test])),y=pheno[-test])
     pam.dat.test <- list(x=(as.matrix(dat[,test])),y=pheno[test])
   
   PAMR = pamr.train(pam.dat.train)#, pheno[test]) # train SVM on train set

    PAMR$threshold[which.min(PAMR$errors)]->pamthres

    p = pamr.predict(PAMR,as.matrix(dat[,test]),threshold=pamthres)#,pheno[-test]) # predict labels of TEST set

    a = 100*sum(p == pheno[test])/length(test) # compute accuracy

    acc = c(acc,a) # collect accuracies
    
    }
    
    
    
    ## print results
    cat ("\ntotal accuracy=",mean(acc))
    cat ("\nsingle accuracies=",round(acc),"\n\n")
    ## ouput this as well?
    
    return(acc)

}


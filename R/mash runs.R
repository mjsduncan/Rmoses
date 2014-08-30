 ### load bed stuy moses results
library("stringr")
setwd("~/GitHub/test_run2")

# attach output .rdata file from each run
for(i in 1:10) attach(paste("bestOfRun_", i, ".rdata"))

# get highest scoring combos
bests <- c("gpl1261_1_hx5.best", "gpl341_1_hx5.best", "gpl81_1_hx5.best", "gpl85_1_hx5.best", "gpl96_1_hx5.bestN5", "gpl97_1_hx5.best") 
bests.cum <- str_replace(bests, fixed("_1_"), "_cum_")


# get the best of the runs for each chip
getBlist <- function(chip, run) get(chip, pos = paste("file:bestOfRun_", run, ".rdata"))
for(i in 1:10){
  for(j in 1:6){
    rlist <- getBlist(bests[j], i)
    assign(str_replace(bests[j], fixed("_1_"), paste("_", i, "_", sep = "")), rlist, inherits = TRUE)
  }
}

# bind together results by chip
lbind <- function(list1, list2) {
  if(sum(sapply(list1, class) != sapply(list2, class)) > 0) return("lists element classes do not match")
  out <- vector("list", length(list1))
  names(out) <- names(list1)
  for(i in seq_along(out)) {
    ifelse(is.atomic(list1[[i]]), out[[i]] <- c(list1[[i]], list2[[i]]), out[[i]] <- rbind(list1[[i]], list2[[i]]))
  }
    return(out)
}

# bind together results by chip using combo counting arguement
lbind2 <- function(list1, list2) {
  if(sum(sapply(list1, class) != sapply(list2, class)) > 0) return("lists element classes do not match")
  out <- vector("list", length(list1))
  names(out) <- names(list1)
  for(i in seq_along(out)) {
    if(is.atomic(list1[[i]])) {out[[i]] <- c(list1[[i]], list2[[i]])}
    if(!is.atomic(list1[[i]]))  {
      fdf <- rbind(list1[[i]], list2[[i]])
      out[[i]] <- aggregate(. ~ feature + upregulated, data = fdf, sum)
      out[[i]] <- out[[i]][order(out[[i]][, 1]),]
    }
  }
  return(out)
}

# go through list of lists of results and mash them together
for(i in 1:6) assign(bests.cum[i], Reduce(lbind, lapply(ls(pos = 1, pattern = unlist(str_split(bests.cum[i], "_", 2))[1]), get)))

# only one of the best combo programs (for gpl1261) was duplicated by 
sapply(lapply(ls(pattern = "_cum_"), get), function(x) c(length(x$combo), dim(x$features)[1]))
#      [,1] [,2] [,3] [,4] [,5] [,6]
# [1,]   55   40   47   30  785   43
# [2,]  113   99  102  119  357   90

sapply(lapply(ls(pattern = "_cum_"), get), function(x) c(length(unique(x$combo)), dim(unique(x$features))[1]))
#      [,1] [,2] [,3] [,4] [,5] [,6]
# [1,]   54   40   47   30  616   43
# [2,]   86   80   90  102  333   68

# look at ranks & accuracies of best combos
unlist(sapply(lapply(ls(pattern = "_cum_"), get), function(x) summary(as.factor(x$rank))))
#            gpl1261   gpl341    gpl81             gpl85       gpl96   gpl97 
# rank        -1   0   -1   0   -2  -1     0  -2  -1   0  -2  -1   0  -1   0 
# features    21  34    2  38    1  10    36   1  10  19  61 410 314   9  34 

unlist(sapply(lapply(ls(pattern = "_cum_"), get), function(x) summary(as.factor(signif(x$score$Accuracy, 3)))))
#       gpl1261                gpl341      gpl81                     gpl85                 
# 0.882 0.941 1   0.939 0.959 0.98  1   0.938  1   0.861 0.889 0.917 0.944 
#    17    35 3       7     5   16 12      26 21       2    17     7     4   
#
#                       gpl96               gpl97 
# 0.667 0.733 0.8 0.867 0.933     1 0.867 0.933 1 
#    30   140 271   210   107    27     6    33 4 

unlist(sapply(lapply(ls(pattern = "_cum_"), get), function(x) summary(as.factor(signif(x$score$`Balanced Accuracy`, 3)))))
#                   gpl1261                            gpl341            gpl81                       
# 0.879   0.9 0.929  0.95 1   0.94 0.944 0.952 0.972 0.984  1   0.929 0.944  1    
#    14     3     9    26 3      6     5     1    10     6 12      14    12 21    
#
#                                     gpl85                            gpl97
# 0.812 0.833 0.854 0.875 0.896 0.917 0.958  0.857 0.866 0.875 0.929 0.938 1  
#     1    11     1     5     8     3     1      2     3     1    22    11 4
#
# gpl96  
# 0.643 0.652 0.661  0.67 0.679 0.688 0.714 0.723 0.732 0.741  0.75 0.786 0.795 0.804 0.812 0.857 0.866    
#     1     5    13     5     5     1    23    35    30    38    14   43    86    94    48    67   110    
# 0.875 0.929 0.938 1
# 33    48    59   27 

save(list = ls(pattern = "_cum_"), file = "~/GitHub/stevia/data/gplCumX6.rdata")

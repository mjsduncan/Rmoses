###  wrapper/interface functions for moses binary

## generate 2-fold cross validation run
#make k 2-fold partitions of moses input dfs, save training sets, and return list of testing dfs and df of column indices
# TODO:  add checks & warnings when writing over files
makeMpartitions <- function(data, k = 10, control = 1, dir = "./", namestr = deparse(substitute(data)), ...){
  require(caret)
  flds <- createDataPartition(as.factor(data[, control]), times = k, ...)
  TrainOut <- vector("list", k)
  TestOut <- vector("list", k)
  names(TrainOut) <- paste(namestr, "_f", 1:k, sep = "")
  names(TestOut) <- paste(namestr, "_f", 1:k, sep = "")
  names(flds) <- names(TrainOut)
  for(i in 1:k){
  	TrainOut[[i]] <- data[flds[[i]],]
    write.csv(TrainOut[[i]], paste(dir, names(TrainOut)[i], ".csv", sep = ""), row.names = FALSE)
    TestOut[[i]] <- data[-flds[[i]],]
  }
  TestOut[["fold_index"]] <- as.data.frame(flds)
  out <- list(train = TrainOut, test = TestOut)
  save(out, file = paste0(namestr, "_data.rdata"))
  return(out)
}

# run moses on a file.  log file is named as input file name to first "." plus ".log".  output can be a file name.
moses <- function( flags = "", DSVfile = "", output = TRUE, ...) {
  if(flags == "help") flags <- "--help"          # moses(help) -> moses --help
  if(flags == "version")  flags <- "--version"    # moses() -> moses --version
  if(DSVfile != "") flags <- paste("--input-file", DSVfile, "--log-file", paste0(word(DSVfile, sep = fixed(".")), ".log"), flags)
  system2("moses", args=flags, stdout = output, ...)
}

# run moses on all csv files in a directory.  "output" argument can be a file name.
runMfolder <- function(flags = "", dir = getwd(),  output = TRUE) {
  require(stringr)
  files <- list.files(path = dir)[str_detect(list.files(path = dir), "csv$")]
  out <- vector("list", length(files))
  names(out) <- word(files, sep = fixed("."))
  for(i in seq_along(files)) out[[i]] <- moses(flags, files[i], output)
  save(out, file = paste0(str_split_fixed(files[1], "_", 1), ".rdata"))
  return(out)
}

# extract combo strings and make feature dfs from moses output.  this works for --output-cscore 0 or 1
# this won't work if moses is called with -S [ --output-score ] = 0
mscore2vector <- function(str) eval(parse(text = paste0("c(", gsub("\\[| |.$", "", str), ")")))

# convert list of vectors of moses output strings to list(combo = vector of combo strings, score = list of combo score component matrices)
moses2combo <- function(mout) {
  require(stringr)
	# check if complexity score is included in output string by checking for bracket
	if(length(grep("\\[", mout[[1]][1])) == 0) {
  	mout <- lapply(mout, function(x) str_split_fixed(x, fixed(" "), 2))
  	out <- list(combo = lapply(mout, function(x) str_trim(x[, 2])), score = lapply(mout, function(x) x[, 1]))
  	return(out)
  }

  # generate complexity score matrix
  mout <- lapply(mout, function(x) str_split_fixed(x, fixed("["), 2))
  score <- lapply(mout, function(x) x[, 2])
  score <- lapply(score, function(x) t(vapply(x, mscore2vector, vector("numeric", 5), USE.NAMES = FALSE)))
  combo <- lapply(mout, function(x) x[, 1])
  combo <- lapply(combo, function(x) str_split_fixed(x, fixed(" "), 2))
  combo <- lapply(combo, function(x) str_trim(x[, 2]))
  for(i in 1:length(score)) dimnames(score[[i]]) <- list(combo[[i]], c("score", "penalized score", "complexity", "complexity penalty", "diversity penalty"))
  return(list(combo = combo, score = score))
}

# make combo strings and feature dfs using moses2combo and combo2fcount
combo2fcount <- function(combo, stripX = FALSE, split = FALSE) {
	out <- vector("list", 2)
	names(out) <- c("up", "down")
	combo <- gsub("and|or|[`()$]", "", unlist(combo))
	combo <- unlist(strsplit(combo, split = " ", fixed = TRUE))
	out$up <- grep("!", combo, value = TRUE, fixed = TRUE, invert = TRUE)
	out$down <- substr(grep("!", combo, value = TRUE, fixed = TRUE), 2, 100)
	if(stripX) out <- lapply(out, substr, 2, 100)
	out <- lapply(out, function (feature) as.data.frame(table(feature), stringsAsFactors = FALSE))
	if(split) return(lapply(out, function(x) x[order(x$Freq, decreasing = TRUE),]))
	out$up$level <- "up"
	out$down$level <- "down"
	out <- rbind(out$up, out$down)[order(rbind(out$up, out$down)$Freq, decreasing = TRUE),]
	out[order(out$Freq, out$feature, decreasing = TRUE),]
  rownames(out) <- NULL
  return(out)
}

# makes list of combos with moses output scores df and feature count df ?
parseMout <- function(mout, strip = FALSE) {
  require(stringr)
  m2c <- moses2combo(mout)
  out <- vector("list", 2)
  names(out) <- c("combos", "features")
  out[[1]] <- data.frame(combo = unlist(m2c[["combo"]]), score = as.numeric(unlist(lapply(m2c[["score"]], function(x) x[1,]))), stringsAsFactors = FALSE)
  rownames(out[[1]]) <- NULL
  out[[2]] <- combo2fcount(out[[1]], stripX = strip, split = FALSE)
  return(out)
}

##### evaluate combo string vectors on testing sets and generate scores & confusion matrices
# The formulas used for the confusion matrix are:
#
# Sensitivity = A/(A+C)
#
# Specificity = D/(B+D)
#
# Prevalence = (A+C)/(A+B+C+D)
#
# PPV = (sensitivity * Prevalence)/((sensitivity*Prevalence) + ((1-specificity)*(1-Prevalence))) = A/(A+D)
#
# NPV = (specificity * (1-Prevalence))/(((1-sensitivity)*Prevalence) + ((specificity)*(1-Prevalence)))
#
# Detection Rate = A/(A+B+C+D)
#
# Detection Prevalence = (A+B)/(A+B+C+D)
#
# Balanced Accuracy = (Sensitivity+Specificity)/2

# define n >=2 argument boolean operators
and <- function(x) Reduce("&", x)
or <- function(x) Reduce("|", x)
true <- TRUE
false <- FALSE

# turn boolean combo string vector into list of R function combinations
combo.edit <- function(str) {
	require(stringr)
  mc_ops <- list(c("true", "TRUE"), c("(", "(list("), c(")", "))"), c(" ", ", "),c("$", ""))
  for(i in seq_along(mc_ops)) str <- str_trim(str_replace_all(str, fixed(mc_ops[[i]][1]), mc_ops[[i]][2]))
  return(str)
}

# evaluate translated expression
evalstring <- function(str){
  eval(parse(text=str))
}

# put confusionMatrix outputs in table

cmv <- function(cm) {
  c('[['(cm, 3)[c(1, 3:6)], '[['(cm, 4))
}

cml2df <- function(cmlist) {
  out <- t(vapply(cmlist, cmv, numeric(13)))
  return(as.data.frame(out))
}

# evaluate combos from output of  moses2combo() on their corresponding test df and return result lists using caret::confusionMatrix
# return.cm is flag for return of confusion matrix output
testCstring <- function(combos, testdf, casecol = 1, caserat, return.cm = TRUE) {
	if(class(testdf) != "data.frame") testdf <- as.data.frame(testdf)
	case <- testdf[[casecol]]
	n <- length(case)
	attach(testdf, warn = FALSE)
	m <- length(combos)
	results <- matrix(nrow = m,ncol = n, dimnames = list(combos, row.names(testdf)))
	if(return.cm) {
		metrics <- vector("list", m)
		for(i in 1:m){
			results[i,] <- as.numeric(evalstring(combo.edit(combos[i])))
			fresult <- as.factor(results[i,])
			levels(fresult) <- c(0, 1)
			metrics[[i]] <- caret::confusionMatrix(fresult, as.factor(case), prev = caserat)
		}
		detach(testdf)
		metrics <- cml2df(metrics)
		rownames(metrics) <- rownames(results)
		results <- as.data.frame(rbind(case, results))
		return(list(result = results, score = metrics))
	}

	# if not returning confusion matrix do
	for(i in 1:m){
		results[i,] <- as.numeric(evalstring(combo.edit(combos[i])))
	}
	detach(testdf)
	results <- as.data.frame(rbind(case, results))
	return(results)
}

# evaluate list of combo strings & compute catagorization metrics.  "cc_ratio" is cases/1's over cases + controls/0's
# returns list of lists of pairs of score & confusion matrices for training sets and test sets
# list of cross validation runs for combos and data sets are ordered to insure correct match
# TODO: make sure validation run names are identical?
testClist <- function(clist, tdatlist, caseCol = 1) {
  require(gtools)
	# compute case prevalence
	case <- tdatlist$test[[1]][, caseCol]
	caseprev <- sum(case) / length(case)
	if(is.nan(caseprev) | caseprev == 0) stop("malformed case column")

	clist <- clist[mixedorder(names(clist))]
	tdatlist$test[["fold_index"]] <- NULL
	trainOut <- Map(testCstring, clist, tdatlist$train[mixedorder(names(tdatlist$train))], caserat = caseprev, casecol = caseCol)
	testOut <- Map(testCstring, clist, tdatlist$test[mixedorder(names(tdatlist$test))], caserat = caseprev, casecol = caseCol)
	return(list(train = trainOut, test = testOut))
}

## get combos with best test set scores, with optional pvalue alpha for null hypothesis guess value 50-50?
# if no alpha is given then only combos with accuracy > than AccuracyNull are returned
# output ordered by decreasing accuracy
bestTestCombos <- function(testClist_out, alpha = NULL) {
  out <- lapply(testClist_out$test, function(x) x[[2]])
  out <- Reduce(rbind, out)
  out <- aggregate(. ~ row.names(out), data = out, mean)
  if(is.null(alpha)) out <- out[out$Accuracy > out$AccuracyNull,] else out <- out[out$AccuracyPValue < alpha,]
  names(out)[1] <- "combo"
  return(out[order(out[[2]], decreasing = TRUE),])
}

# TODO:  fix bug where duplicate combos are not removed but have a number appended to end of combo string!!
# function to add before reduce:  dropDuplicateCombos(out)
# make list of list of rownames
# unlist and find duplicates
# use names of names to delete duplicate rowname rows

## get combos with best training set scores, with optional pvalue alpha for null hypothesis guess value 50-50?
# if no alpha is given then only combos with accuracy > than AccuracyNull are returned
bestTrainCombos <- function(testClist_out, alpha = NULL) {
  out <- lapply(testClist_out$train, function(x) x[[2]])
  out <- Reduce(rbind, out)
  out <- aggregate(. ~ row.names(out), data = out, mean)
  if(is.null(alpha)) out <- out[out$Accuracy > out$AccuracyNull,] else out <- out[out$AccuracyPValue < alpha,]
  names(out)[1] <- "combo"
  return(out[order(out[[2]], decreasing = TRUE),])
}

## generate plots comparing scores from testing & training sets
# make list of 3d arrays of selected metrics for each cross validation run (combo x score x test/train)
testVtrain <- function(testClist_out, scores = c("Accuracy", "AccuracyLower", "AccuracyUpper", "AccuracyNull", "AccuracyPValue", "Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Prevalence", "Detection Rate", "Detection Prevalence", "Balanced Accuracy")) {
  require(abind)
  out <- vector("list", length(testClist_out[[1]]))
  names(out) <- names(testClist_out[[1]])
  for(i in 1:length(out)) {
    out[[i]] <- list(test = testClist_out$test[[i]][[2]][, scores], train = testClist_out$train[[i]][[2]][, scores])
    out[[i]] <- abind(out[[i]]$test, out[[i]]$train, along = 3)
    dimnames(out[[i]])[[3]] <- c("test", "train")
  }
  # out <- lapply(out, function(x))
  return(out)
}

# plot train vs test with different color for each cross-validation run
tvtScatter <- function(testVtrain) {
  # combine c-v runs into one df
  len <- dim(testVtrain[[1]])[2]
  data <- vector("list", len)
  names(data) <- dimnames(testVtrain[[1]])[[2]]
  out <- data
  for(i in 1:len) {
    for(j in names(testVtrain)) data[[i]] <- rbind(data[[i]], data.frame(run = j, testVtrain[[j]][, i,]))
  }
  # generate plot for each metric
  # require(ggplot2)
  # qplot(data = mr5scores$Accuracy, x = train, y = test, color = run, ) + stat_smooth(method="lm")
  # qplot(data = mr5scores$Accuracy, x = train, y = test, color = run, geom = "jitter") + stat_smooth(method="lm")
  # save & return data
  return(data)
}


### combine runs & extract best models
# extract, combine & recast results matrices from testClist()
name2col <- function(df, name = "name") {
	df <- cbind(rownames(df), df, stringsAsFactors = FALSE)
	rownames(df) <- NULL
	names(df)[1] <- name
	return(df)
}

col2name <- function(df, col = 1) {
	names <- df[[col]]
	names <- gsub("`", "", names, fixed = TRUE)
	rownames(df) <- names
	df[[col]] <- NULL
	return(df)
}

# get the combo results on training & testing sets for each cross validation fold
getResults <- function(testOut) {
	TrainOut <- lapply(testOut$train, function(x) x[[1]])
	TrainOut <- lapply(TrainOut, name2col)
	TestOut <- lapply(testOut$test, function(x) x[[1]])
	TestOut <- lapply(TestOut, name2col)
	out <- c(train = TrainOut, test = TestOut)
	return(out)
}

# vector distance defined as sum of number of differences/false results.  equals negative of moses binary raw score.
vdist <- function(x, y) {
  if(length(x) != length(y)) return("vectors have different lengths")
  sum(x != y, na.rm = TRUE)
}

# sort results by increasing distance combo score vector from case vector of samples
sortResults <- function(aggOut) {
	out <- aggOut[c(grep("^case$", row.names(aggOut)), grep("^case$", row.names(aggOut), invert = TRUE)), gtools::mixedorder(names(aggOut))]
 	out <- cbind(out, score = apply(as.matrix(out), 1, function(x) vdist(out[1,], x)))
	return(out[order(out$score, rowSums(is.na(out))),])
}

aggScore <- function(aggOut) {

	# drop "score" column from kth cross-validation run
	results <- aggOut[, -dim(aggOut)[2]]
	case <- as.factor(unlist(results[1,]))
	print(summary(case)[2] / length(case))
	results <- results[-1,]
	n <- dim(results)[1]
	metrics <- vector("list", n)
	for(i in 1:n){
		fresult <- as.factor(unlist(results[i,]))
    levels(fresult) <- c(0, 1)
		metrics[[i]] <- caret::confusionMatrix(fresult, case, prev = summary(case)[2] / length(case))
	}
	names(metrics) <- rownames(results)
	return(cml2df(metrics))
}

aggResults <- function(testOut) {
	out <- reshape2::melt(getResults(testOut), "name", variable.name = "sample", value.name = "case")
	out <- unique(out[-4])
	out <- reshape2::dcast(out, name ~ sample)
	out <- col2name(out)
	out <- sortResults(out)
	return(list(results = out, confusionMatrix = aggScore(out)))
}

# function to merge resulting list of aggregated moses runs into 1000 row results matrix & confusion matrix
# (top 10 combos x 10 cross-validation runs x 10 meta-runs)
# aggScores <- lapply(scoresNconfusionMatrix, aggResults)
# mergedRuns <- mergeAggList(aggScores)
mergeAggList <- function(aglst) {
  results <- lapply(aglst, function(x) x$results[, order(names(x$results))])
  results <- Reduce(rbind, lapply(results, name2col))
  results <- results[!grepl("^case", results),]
  confusionMatrix <- lapply(aglst, function(x) x$confusionMatrix)
  confusionMatrix <- Reduce(rbind, lapply(confusionMatrix, name2col))
  out <- list(results = results[order(results[,length(results[1,])], decreasing = TRUE),],
              confusionMatrix = confusionMatrix[order(confusionMatrix[, 14], decreasing = TRUE),])
  out <- lapply(out, unique)
  out <- lapply(out, col2name)
  return(out)
}

# function to apply vector of combos to data set matrix and return ensemble scores.  rows for results of ensemble applied to each sample are added to output$results and ensemble accuracy is computed and printed.  weighting of ensemble components by accuracy and ppd are implemented but usefulness/correctness isn't validated (~ 0.1% cumulative accuracy improvement in one example)
# TODO:  add complete score row to individual scores in output$score
testCensemble <- function(cvec, tdatframe, caseCol = 1) {

  # compute case prevalence
  case <- tdatframe[, caseCol]
  caseprev <- sum(case) / length(case)
  if(is.nan(caseprev) | caseprev == 0) stop("malformed case column")
  out <- testCstring(cvec, tdatframe, caserat = caseprev, casecol = caseCol)

  # compute ensemble scores
  eScore <- colMeans(out$result[-1,])
  aWeScore <- out$score$Accuracy %*% as.matrix(out$result[-1,]) / sum(out$score$Accuracy)
  ppvWeScore <- out$score$`Pos Pred Value` %*% as.matrix(out$result[-1,]) / sum(out$score$`Pos Pred Value`)
  senWeScore <- out$score$Sensitivity %*% as.matrix(out$result[-1,]) / sum(out$score$Sensitivity)
  out$result <- rbind(eScore, aWeScore, ppvWeScore, senWeScore, out$result)
  rownames(out$result)[1:4] <- c("mean", "acc_wt_mean", "ppd_wt_mean", "sen_wt_mean")

  # compute ensemble score accuracies
  nSamples <- dim(tdatframe)[1]
  print(paste0("ensemble mean accuracy = ", 1 - (sum(abs(round(out$result["mean",]) - out$result["case",])) / nSamples)))
  print(paste0("accuracy weighted mean = ", 1 - (sum(abs(round(out$result["acc_wt_mean",]) - out$result["case",])) / nSamples)))
  print(paste0("ppd weighted mean = ", 1 - (sum(abs(round(out$result["ppd_wt_mean",]) - out$result["case",])) / nSamples)))
  print(paste0("sensitivity weighted mean = ", 1 - (sum(abs(round(out$result["sen_wt_mean",]) - out$result["case",])) / nSamples)))
  return(out)
}

testCensXrun <- function(testClist_out, alpha = 0.05, top = 1) {
  scores <- lapply(testVtrain(testClist_out, c("Accuracy", "AccuracyPValue", "Pos Pred Value", "Sensitivity")), function(x) x[,, "train"])
  scores <- lapply(scores, function(x) x[x[,"AccuracyPValue"] < alpha, -2])
  scores <- lapply(scores, function(x) x[order(x[,"Accuracy"])[1:round(top * length(x[,1]))],])
  out <- lapply(testClist_out$test, function(x) x$result)
  out <- mapply(function(x, y) x[c("case", rownames(y)),], out, scores, SIMPLIFY = FALSE)
  # compute ensemble scores by run
  eScore <- lapply(out, function(x) colMeans(x[-1,]))
  aWeScore <- mapply(function(x, y) x[, "Accuracy"] %*% as.matrix(y[-1,]) / sum(x[, "Accuracy"]), scores, out, SIMPLIFY = FALSE)
  ppvWeScore <- mapply(function(x, y) x[, "Pos Pred Value"] %*% as.matrix(y[-1,]) / sum(x[, "Pos Pred Value"]), scores, out, SIMPLIFY = FALSE)
  senWeScore <- mapply(function(x, y) x[, "Sensitivity"] %*% as.matrix(y[-1,]) / sum(x[, "Sensitivity"]), scores, out, SIMPLIFY = FALSE)
  out <- mapply(rbind, eScore, aWeScore, ppvWeScore, senWeScore, out, SIMPLIFY = FALSE)
  for(i in 1:length(out)) rownames(out[[i]])[1:4] <- c("mean", "acc_wt_mean", "ppd_wt_mean", "sen_wt_mean")

  # compute ensemble score accuracies
  nSamples <- sapply(out, function(x) dim(x)[2] - 4)
  `ensemble mean accuracy` = mapply(function(x, y) 1 - sum(abs(round(x["mean",]) - x["case",])) / y, out, nSamples)
  `accuracy weighted mean` = mapply(function(x, y) 1 - sum(abs(round(x["acc_wt_mean",]) - x["case",])) / y, out, nSamples)
  `ppd weighted mean` = mapply(function(x, y) 1 - sum(abs(round(x["ppd_wt_mean",]) - x["case",])) / y, out, nSamples)
  `sensitivity weighted mean` = mapply(function(x, y) 1 - sum(abs(round(x["sen_wt_mean",]) - x["case",])) / y, out, nSamples)
  print(cbind(`ensemble mean accuracy`, `accuracy weighted mean`, `ppd weighted mean`, `sensitivity weighted mean`))
  return(out)
}

# convert fold_index df of training sample number X run to boolean matrix of run X sample number
samplesInTrainingSet <- function(fold_index) {
  runs <- names(fold_index)
  samples <- unique(unlist(fold_index))
  out <-
    array(
      data = logical(length(runs) * length(samples)), dim = c(length(runs), length(samples)), dimnames = list(runs, as.character(sort(samples)))
    )
  for(i in 1:length(runs)) out[i,] <- colnames(out) %in% as.character(fold_index[[rownames(out)[i]]])
  return(out)
}

# helper function to drop list elements not meeting condition
checklist <- function(list, cond) {

}

# using boolean matrix output of samplesInTrainingSet(), get accuracy of ensemble on testing/out-of-sample data
testCensXsample <- function(testClist_out, sits_out, part = "train", alpha = 0.05, score = "Accuracy", scoreMin = NULL, top = NULL) {
  stopifnot(score %in% c("Accuracy", "Pos Pred Value", "Sensitivity"))
  # drop not significant and worst combos from part (test or train) partition
  bestCombos <- lapply(testVtrain(testClist_out, c("Accuracy", "AccuracyPValue", "Pos Pred Value", "Sensitivity")), function(x) x[,, part])
  bestCombos <- lapply(bestCombos, function(x) x[x[,"AccuracyPValue"] < alpha, -2])
  if(!is.null(scoreMin)) bestCombos <- lapply(bestCombos, function(x) x[x[, score] >= scoreMin,])
  # bestCombos <- lapply(bestCombos, function(x) x[order(x[, score]),])
  if(!is.null(top)) bestCombos <- lapply(bestCombos, function(x) x[order(x[, score])[1:round(top * length(x[,1]))],])
  bestCombos <- lapply(bestCombos, function(x) c("case", rownames(x)))
  testClist_out <- mapply(function(x, y) x$result[y,], testClist_out$test, bestCombos, SIMPLIFY = FALSE)
  # make list whose elements are a list for each sample of combo testing run results for each run not in the training set of the sample
  out <- lapply(as.data.frame(sits_out), function(x) lapply(testClist_out[!x], name2col))
  # drop all results except for the named sample
  samples <- names(out)
  out <- mapply(function(x, y) lapply(x, function(z) z[, c("name", y)]), out, samples, SIMPLIFY = FALSE)
  # concatenate sample combo lists
  out <- lapply(out, function(x) unique(Reduce(rbind, x)))
  # drop empty sample result dfs
  out <- out[!sapply(out, is.null)]
  out <- out[sapply(out, function(x) dim(x)[1] > 2)]
  # calculate ensemble scores
  for(n in seq_along(out)) names(out[[n]])[2] <- "result"
  eScore <- lapply(out, function(x) data.frame(name = "mean", result = mean(x[-1, 2])))
#   aWeScore <- out$score$Accuracy %*% as.matrix(out$result[-1,]) / sum(out$score$Accuracy)
#   ppvWeScore <- out$score$`Pos Pred Value` %*% as.matrix(out$result[-1,]) / sum(out$score$`Pos Pred Value`)
#   senWeScore <- out$score$Sensitivity %*% as.matrix(out$result[-1,]) / sum(out$score$Sensitivity)
  out <- mapply(rbind, eScore, out, SIMPLIFY = FALSE) # Map(, aWeScore, ppvWeScore, senWeScore
#   for(n in seq_along(out)) rownames(out)[1] <- "mean" # , "acc_wt_mean", "ppd_wt_mean", "sen_wt_mean")
  scores <- sapply(out, function(x) x[1:2, 2])
  if(is.null(dim(scores))) {
    print("all combos have been filtered out")
    return(NULL)
    }
  print(paste("mean out-of-sample accuracy of combo mean ensembles is", 1 - sum(abs(round(scores[1,]) - scores[2,]))/dim(scores)[2]))
  print(paste(dim(sits_out)[2] - dim(scores)[2], "samples were in the training sets of all", dim(sits_out)[1], "cross-validation runs", "and not included in the mean ensemble score (out of", dim(sits_out)[2], "total samples)"))
  return(out)
}

## figure out size of moses output
# helper function to count occurances of a particular character in each string of a character vector

countCharOccurrences <- function(char, s) {
  s2 <- gsub(char, "", s)
  return (nchar(s) - nchar(s2))
}

# count variables, binary operators, and negations in combo set
comboSize <- function(combo) {
  c(features = countCharOccurrences("\\$", combo), operators = countCharOccurrences("\\(", combo), nots = countCharOccurrences("!", combo))
}

## misc helper functions
# function to get ith element of each of a list of n length vectors
ith_element_vector <- function(list, i = 1) {
  if(i > length(list[[1]])) return("i is greater than vector length")
  sapply(list, '[[', i)
}

ith_element_list <- function(list, i = 1) {
  if(i > length(list[[1]])) return("i is greater than vector length")
  lapply(list, '[[', i)
}

# get means from vector of combos
meanComboSize <- function(comboVector) rowMeans(sapply(comboVector, comboSize, USE.NAMES = FALSE))
# example output
# features Noperators       nots
#   11.139      6.924      5.865

# apply median norm to matrix by columns
med.normalize <- function(mat) {
  out <- mat
  for (i in seq(dim(mat)[2])) {
    vect <- mat[,i]
    med <- median(vect, na.rm = TRUE)
    out[,i] <- as.numeric(vect >= med)
  }
  return(out)
}

## ensemble validation?
# i can't remember how this works or where it is from.
getScores <- function(m2cOut, penalized = FALSE) {
  out <- lapply(m2cOut$score, function(x) x[1 + penalized,])
  return(out)
}

getBestCombos <- function(slist) {
  best <- lapply(slist, max)
  out <- mapply(function(x, y) Filter(function(z) z == y, x), slist, best)
  return(lapply(out, names))
}

ensemble.score <- function(resultMatrix) {
  score <- apply(resultMatrix, 2, function(x) round(median(x)))
  return(score)
}

testCElist <- function(celist, tdatlist) {
  case <- tdatlist$test[[1]]$case
  caseprev <- sum(case) / length(case)
  celist <- celist[order(names(celist))]
  tdatlist$test[["fold_index"]] <- NULL
  resultOut <- Map(testCstring, celist, tdatlist$test[order(names(tdatlist))], caserat = caseprev, return.cm = FALSE)
  score <- lapply(resultOut, function(x) ensemble.score(x[-1,]))
  resultOut <- Map(rbind, score = score, resultOut)
  m <- length(resultOut)
  metrics <- vector("list", m)
  for(i in 1:m){
    fresult <- as.factor(unlist(resultOut[[i]]["score",]))
    levels(fresult) <- c(0, 1)
    metrics[[i]] <- caret::confusionMatrix(fresult, as.factor(case), prev = caseprev)
  }
  metrics <- cml2df(metrics)
  rownames(metrics) <- names(resultOut)
  return(list(results = resultOut, confusionMatrix = metrics))
}

## unfinished and scrap code
#--------------------------

### wrap it up
# get list of input files and set up directories
make.dir <- function(subDir, mainDir = getwd()) {
	if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
		cat(paste(subDir, "exists in mainDir and is a directory\n"))
	} else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
		cat(paste(subDir, "exists in", mainDir, "but is a file\n"))
	} else {
		cat(paste(subDir, "does not exist in", mainDir, "- creating\n"))
		dir.create(file.path(mainDir, subDir))
	}
	if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
		# By this point, the directory either existed or has been successfully created
		setwd(file.path(mainDir, subDir))
	} else {
		cat(paste(subDir, "does not exist\n"))
		# Handle this error as appropriate
	}
}

setup <- function(inputDr, ...) {   # , outputDr = NULL, drName = "Rmoses.output"
	inputEnv <- new.env(parent = globalenv())
	input <- list.files(inputDr, pattern = ".csv")
	for(file in input) assign(unlist(strsplit(file, ".", fixed = TRUE))[1], read.csv(file.path(inputDr, file)), envir = inputEnv)
	input <- ls(envir = inputEnv)
# 	if(outputDr != getwd()) make.dir(file.path(outputDr, drName))
	dataList <- vector("list", length(input))
	names(dataList) <- input
	for(minput in input) {
		make.dir(minput)
		data <- makeMpartitions(get(minput, envir = inputEnv), k = 10, control = 1, dir = "./", namestr = minput, ...)
		dataList[[minput]] <- data
		saveName <- paste0(minput, "_partition")
		assign(saveName, data)
		save(list = saveName, file = paste0(saveName, ".RData"))
		setwd("..")
	}
return(dataList)
}

# run moses & save results
runMoses <- function(flags, testdat, ensemble = FALSE, inputDr = getwd()) {
	setwd(inputDr)
	runs <- dir()
	out <- vector("list", length(runs))
	for(n in runs) {
		setwd(n)
		cat(paste("running moses on .csv files in", n))
		combos <- moses2combo(runMfolder(flags))
		saveName <- paste0(n, "_out")
		assign(saveName, combos)
		save(saveName, file = paste0(saveName, ".RData"))
		setwd("..")
		out[[n]] <- if(ensemble) {testCElist(getBestCombos(getScores(combos)), testdat[[n]])
		} else aggResults(testClist(combos$combo, testdat[[n]]))
	}
	return(out)
}
# output

#for(i in 1:10) {
#	make.dir(mainD, paste("meta_run", i, sep = ""))
#
#	# # impute NAs and check for non-informative features
#	# moses.data$gpl81 <- bin.impute.matrix(gpl81)
#	# summary(colSums(moses.data$gpl81))
#	# #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#	# #   16.00   18.00   18.00   18.07   18.00   22.00
#	# dim(moses.data$gpl81)
#	# # [1]    34 12489
#
#	### dataset pgl81
#
#	# gpl81 <- data.frame(moses.data$gpl81, check.names = TRUE)
#	dataset <- "gpl81"
#	cr <- mean(gpl81[, "age"])
#	make.dir(getwd(), dataset)
#	gpl81_1.test <- makeMpartitions(gpl81, p = .5)
#	gpl81_1_hx5.train <- runMfolder(moses.flags)
#	save(gpl81_1.test, gpl81_1_hx5.train, file = paste("../", dataset, "_run", i, "_moses.Rdata", sep = ""))
#	gpl81_1_hx5 <- testClist(gpl81_1_hx5.train, gpl81_1.test, cr)
#	gpl81_1_hx5.best <- bestCombos(gpl81_1_hx5)
#	gpl81_1_hx5.bestN5 <- bestCombos(gpl81_1_hx5, N = 5)
#	save(gpl81_1_hx5.best, gpl81_1_hx5.bestN5, file = paste("gpl81_", i, "_best.rdata", sep = ""))
#	setwd("..")
#}

# calculate Escore: for down make count negative and sum by feature
Escore <- function(c2fc, add = TRUE) {
	if(!identical(names(c2fc)[1:3], c("feature", "Freq", "level"))) return("error: input is not output of combo2fcount(... split = FALSE)")
	out <- data.frame(feature = c2fc$feature, Escore = ifelse(c2fc$level == "down", -c2fc$Freq, c2fc$Freq), stringsAsFactors = FALSE)
	if(add) out <- aggregate(. ~ feature, data = out, function(x) sum(abs(x))) else
		out <- aggregate(. ~ feature, data = out, sum)
	if(length(c2fc) > 3) {
		out <- merge(out, c2fc[, c(1, 4:length(c2fc))], by = "feature")
		names(out) <- c("feature", "Escore", names(c2fc)[4:length(c2fc)])
	}
	unique(out[order(-abs(out$Escore), out$feature),])
}

# save combined combo2fcount result to csv file with option to return value (ret = TRUE) and save with deleted hi-lo features
combo2Fcsv <- function(combo, name = deparse(substitute(combo)), dir = ".", strip = FALSE, ret = FALSE, Escore = FALSE) {
	name <- paste(name, "csv", sep = ".")
	out <- combo2fcount(combo, stripX = strip, split = FALSE)
	if(Escore) out <- Escore(out)
	write.csv(out, file = paste(dir, name, sep = "/"), row.names = FALSE)
	if(ret) return(out)
}

# ### extract moses output from log files
# # get last n lines from file
# lastNlines <- function(filename, N = 12, drop = 2) {
# 	## filename is of mode character
# 	out <- system(sprintf("wc -l %s", filename), intern=TRUE)
# 	n <- as.integer(sub(sprintf("[ ]*([0-9]+)[ ]%s", filename), "\\1",out))
# 	print(n)
# 	scan(filename,what="",skip=n - N, nlines=N - drop,sep="\n", quiet=TRUE)
# }
#
# getMout <- function(dir = ".", type = ".log", lines = 12, drop = 2) {
# 	lfiles <- list.files(path = dir, pattern = type)
# 	out <- vector("list", length(lfiles))
# 	for(i in seq_along(lfiles)) {
# 		out[[i]] <- lastNlines(lfiles[i], N = lines, drop = drop)
# 	}
# 	return(out)
# }

# # take mean of duplicate columns and round.
# nms <- unique(colnames(testList$test[[1]]))
# testList$test <-  lapply(testList$test,  function(x) ifelse(class(x) == "data.frame", x,
# 																														t(apply(x, 1, function(y) sapply(nms, function(z) round(mean(y[nms == z])))))))
# testList$train <-  lapply(testList$train,  function(x) t(apply(x, 1, function(y) sapply(nms, function(z) round(mean(y[nms == z]))))))
# this results in 101 rows of NA  test case
# genenas <- rownames(testtestList1[sapply(testtestList1[,1], is.na),])

# }
# for(i in 1:10) detach(2)
# for(n in grep("file:", search(), fixed = TRUE, value = TRUE))

# aggregate binary matrix by rownames
combineByRow <- function(m) {
  m <- m[ order(rownames(m)), ]

  ## keep track of previous row name
  prev <- rownames(m)[1]
  i.start <- 1
  i.end <- 1

  ## cache the rownames -- profiling shows that it takes forever to look at them
  m.rownames <- rownames(m)
  stopifnot(all(!is.na(m.rownames)))

  ## go through matrix in a loop, as we need to combine some unknown set of rows
  for (i in 2:(1+nrow(m))) {

    curr <- m.rownames[i]

    ## if we found a new row name (or are at the end of the matrix), combine all rows and mark invalid rows
    if (prev != curr || is.na(curr)) {

      if (i.start < i.end) {
        m[i.start,] <- apply(m[i.start:i.end,], 2, max)
        m.rownames[(1+i.start):i.end] <- NA
      }

      prev <- curr
      i.start <- i
    } else {
      i.end <- i
    }
  }

  m[ which(!is.na(m.rownames)),]
}


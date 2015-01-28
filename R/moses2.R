#  wrapper/interface functions for moses binary

##### generate 2-fold cross validation run
#make k 2-fold partitions of moses input dfs, save training sets, and return list of testing dfs and df of column indices 
# TODO:  add checks & warnings when writing over files
makeMpartitions <- function(data, k = 10, control = 1, dir = "./", ...){
  require(caret)
  namestr <- deparse(substitute(data))
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
  return(list(train = TrainOut, test = TestOut))
}

# run moses on a file.  log file is named as input file name to first "." plus ".log".  output can be a file name.
moses <- function( flags = "", DSVfile = "", output = TRUE, ...) {
  if(flags == "help") flags <- "--help"          # moses(help) -> moses --help
  if(flags == "version")  flags <- "--version"    # moses() -> moses --version
  if(DSVfile != "") flags <- paste("--input-file", DSVfile, "--log-file", paste(word(DSVfile, sep = fixed(".")), ".log", sep = ""), flags)
  system2("moses", args=flags, stdout = output, ...)
}

# run moses on all csv files in a directory.  "output" argument can be a file name.
runMfolder <- function(flags = "", dir = getwd(),  output = TRUE) {
  require(stringr)
  files <- list.files(path = dir)[str_detect(list.files(path = dir), "csv$")]
  out <- vector("list", length(files))
  names(out) <- word(files, sep = fixed("."))
  for(i in seq_along(files)) out[[i]] <- moses(flags, files[i], output)
  return(out)
}

# make combo strings and feature dfs from moses output.  score = TRUE returns moses scoring data in brackets
# this won't work if moses is called with -S [ --output-score ] = 0
mscore2vector <- function(str) eval(parse(text = paste0("c(", gsub("\\[| |.$", "", str), ")")))

# convert list of vectors of moses output strings to list(combo = vector of combo strings, score = list of combo score component matrices)
moses2combo <- function(mout) {
  require(stringr)
	# check if coplexity score is included in output string
	if(length(grep("\\[", mout[[1]][1])) == 0) {
  	mout <- lapply(mout, function(x) str_split_fixed(x, fixed(" "), 2))
  	out <- list(combo = lapply(mout, function(x) str_trim(x[, 2])), score = lapply(mout, function(x) x[, 1]))
  	return(out)
  }
  # generate complexity score matrix
  mout <- lapply(mout, function(x) str_split_fixed(x, fixed("["), 2))
  score <- lapply(mout, function(x) sapply(x[, 2], mscore2vector, USE.NAMES = FALSE))
  mout <- lapply(mout, function(x) x[, 1])
  mout <- lapply(mout, function(x) str_split_fixed(x, fixed(" "), 2))
  mout <- lapply(mout, function(x) str_trim(x[, 2]))
  for(i in 1:length(score)) dimnames(score[[i]])[[2]] <- mout[[i]]
  return(list(combo = mout, score = score))
}

# ## combine folds keeping worst score for duplicate combos
# # extract duplicates
# duplicateCombos <- function(clist) {
# 	clist <- unlist(clist)
# 	unique(clist[duplicated(clist)])
# }
# 
# getIndex <- function(combo, clist) {
# 	which(Reduce(rbind, clist) == combo, arr.ind = TRUE)
# }
# 
# getScore <- function(Imatrix, sList, sIndex = 1) {
# 	score <- apply(Imatrix, 1, function(x) sList[[x[1]]][sIndex, x[2]])
# 	cbind(Imatrix, score = score)
# }
# 
# deleteIndex <- function(m2cout) {
# 	
# }

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
}

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
# PPV = (sensitivity * Prevalence)/((sensitivity*Prevalence) + ((1-specificity)*(1-Prevalence)))
# 
# NPV = (specificity * (1-Prevalence))/(((1-sensitivity)*Prevalence) + ((specificity)*(1-Prevalence)))
# 
# Detection Rate = A/(A+B+C+D)
# 
# Detection Prevalence = (A+B)/(A+B+C+D)
# 
# Balanced Accuracy = (Sensitivity+Specificity)/2

# translate n >=2 argument boolean operators
and <- function(x) Reduce("&", x)
or <- function(x) Reduce("|", x)

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

# evaluate combos from output of  moses2combo() on test dfs and return result lists using caret::confusionMatrix
testCstring <- function(combos, testdf, casecol = 1, caserat) {
  case <- testdf[[casecol]]
  n <- length(case)
  attach(testdf)
  m <- length(combos)
  results <- matrix(nrow = m,ncol = n, dimnames = list(sapply(combos, as.name), row.names(testdf)))
  metrics <- vector("list", m)
  for(i in 1:m){
    results[i,] <- as.numeric(evalstring(combo.edit(combos[i])))
    fresult <- as.factor(results[i,])
    metrics[[i]] <- caret::confusionMatrix(fresult, as.factor(case), prev = caserat)
  }
  detach(testdf)
  metrics <- cml2df(metrics)
  rownames(metrics) <- rownames(results)
  results <- as.data.frame(rbind(results, case))
  return(list(result = results, score = metrics))
}

# evaluate list of combo strings & compute catagorization metrics.  "cc_ratio" is cases/1's over cases + controls/0's
testClist <- function(clist, tdatlist) {
	case <- tdatlist$test[[1]]$case
	cc_ratio <- sum(case) / length(case)
	clist <- clist[order(names(clist))]
	tdatlist$test[["fold_index"]] <- NULL
	trainOut <- Map(testCstring, clist, tdatlist$train[order(names(tdatlist))], caserat = cc_ratio)
	testOut <- Map(testCstring, clist, tdatlist$test[order(names(tdatlist))], caserat = cc_ratio)
	return(list(train = trainOut, test = testOut))
}

### combine runs & extract best models
# extract, combine & recast results matrices from testClist()
name2col <- function(df, name = "name") {
	df <- cbind(gsub("`", "", rownames(df), fixed = TRUE), df, stringsAsFactors = FALSE)
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

getResults <- function(testOut) {
	TrainOut <- lapply(testOut$train, function(x) x[[1]])
	TrainOut <- lapply(TrainOut, name2col)
	TestOut <- lapply(testOut$test, function(x) x[[1]])
	TestOut <- lapply(TestOut, name2col)
	out <- c(train = TrainOut, test = TestOut)
	return(out)
}

sortResults <- function(aggOut) {
	out <- aggOut[c(grep("^case$", row.names(aggOut)), grep("^case$", row.names(aggOut), invert = TRUE)), gtools::mixedorder(names(aggOut))]
 	out <- cbind(out, score = apply(as.matrix(out), 1, function(x) vdist(out[1,], x)))
	return(out[order(out$score, rowSums(is.na(out))),])
}

vdist <- function(x, y) {
	if(length(x) != length(y)) return("vectors have different lengths")
	sum(x != y, na.rm = TRUE)
}

aggScore <- function(aggOut) {
	results <- aggOut[, -dim(aggOut)[2]]
	case <- as.factor(unlist(results[1,]))
	print(summary(case))
	results <- results[-1,]
	n <- dim(results)[1]
	metrics <- vector("list", n)
	for(i in 1:n){
		fresult <- as.factor(unlist(results[i,]))
#     levels(fresult) <- c("0", "1")
		metrics[[i]] <- caret::confusionMatrix(fresult, case, prev = summary(case)[2] / summary(case)[1])
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

##### put training and validation together and generate summary results
# extract the ith element from all list components
getCombo <- function(rlist, c) {
  out <- list(rlist$combo[c], rlist$Mscores[c], rlist$score[c,])
  names(out) <- c("combo", "mosesScores", "score")
  return(out)
}

# get indexes of all of the n highest m (column index of confusion matrix data from "col") scoring combos from score df
# TODO: add option to get all combos with m-score > cut-off
bestIndex <- function(df, n = 1, col = 1) {
  rindex <- order(df[[col]], decreasing = TRUE)
  rvalues <- unique(df[[col]][rindex])
  which(df[[col]] >= rvalues[n])
}

# combine fold results (results df is thrown out)
combineFolds <- function(rlist) {
  out <- tail(rlist[[1]], -1)
  for(i in seq(2, length(rlist))) {
    out[[1]] <- rbind(out[[1]], rlist[[i]][[2]])
    out[[2]] <- c(out[[2]], rlist[[i]][[3]])
    out[[3]] <- c(out[[3]], rlist[[i]][[4]])
  }
  return(out)
}
# get all the n best m-scoring combo programs from a k-fold validation set. default m is balanced accuracy
# TODO: add option to get all combos with m-score > cut-off
bestCombos <- function(rlist, N = 1, metric = 13) {
  allfolds <- combineFolds(rlist)
  best <- bestIndex(allfolds$score, n = N, col = metric)
  out <- getCombo(allfolds, best)
  out[["features"]] <- combo2fcount(out$combo, split = FALSE)
  return(out)
}

## apply median norm to matrix by columns

med.normalize <- function(mat) {
  out <- mat
  for (i in seq(dim(mat)[2])) { 
    vect <- mat[,i]
    med <- median(vect, na.rm = TRUE)
    out[,i] <- as.numeric(vect >= med)
  }
  return(out)
}

### make df of features from combo strings ordered by feature count
# set stripX = TRUE to remove first character from feature name.
# default split = TRUE outputs 2 data frames in a list: $up & $down, with 2 columns:  feature (string), Freq (count)
# $up are unmodified combo variables and $down are ! (not) combo variables
# set split = FALSE to output single dataframe with column "level" = "up" or "down"

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

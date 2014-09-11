#  wrapper/interface functions for moses binary

##### generate k-fold cross validation run
#make k 2-fold partitions of moses input dfs, save training sets, and return list of testing dfs and df of column indices 
makeMpartitions <- function(data, k = 10, control = 1, dir = "./", ...){
  require(caret)
  namestr <- deparse(substitute(data))
  flds <- createDataPartition(as.factor(data[, control]), times = k, ...)
  out <- vector("list", k)
  names(out) <- paste(namestr, "_f", 1:k, sep = "")
  names(flds) <- names(out)
  for(i in 1:k){
    write.csv(data[flds[[i]],], paste(dir, names(out)[i], ".csv", sep = ""), row.names = FALSE)
    out[[i]] <- data[-flds[[i]],]
  }
  out[["fold_index"]] <- as.data.frame(flds)
  return(out)
}

# run moses on a file.  log file is named as input file name to first "." plus ".log".  output can be a file name.
moses <- function( flags = "", DSVfile = "", output = TRUE) {
  if(flags == "help") flags <- "--help"          # moses(help) -> moses --help
  if(flags == "version")  flags <- "--version"    # moses() -> moses --version
  if(DSVfile != "") flags <- paste("--input-file", DSVfile, "--log-file", paste(word(DSVfile, sep = fixed(".")), ".log", sep = ""), flags)
  system2("moses", args=flags, stdout = output)
}

# run moses on all csv files in a directory.  "output" argument can be a file name.
runMfolder <- function(flags = "", dir = getwd(),  output = TRUE) {
  require(stringr)
  files <- list.files(path = dir)[str_detect(list.files(path = dir), "csv$")]
  out <- vector("list", length(files))
  names(out) <- paste(word(files, sep = fixed(".")), "_Mout", sep = "")
  for(i in seq_along(files)) out[[i]] <- moses(flags, files[i], output)
  return(out)
}

### make df of features from combo strings ordered by feature count
# set stripX = TRUE to remove first character from feature name.
# default split = TRUE outputs 2 data frames in a list: $up & $down, with 2 columns:  feature (string), Freq (count)
# $up are unmodified combo variables and $down are ! (not) combo variables
# set split = FALSE to output single dataframe with column "level" = "up" or "down"

combo2fcount <- function(combo, stripX = FALSE, split = TRUE) {
  out <- vector("list", 2)
  names(out) <- c("up", "down")
  combo <- gsub("and|or|[()$]", "", combo)
  combo <- unlist(strsplit(combo, split = " ", fixed = TRUE))
  out$up <- grep("!", combo, value = TRUE, fixed = TRUE, invert = TRUE)
  out$down <- substr(grep("!", combo, value = TRUE, fixed = TRUE), 2, 100)
  if(stripX) out <- lapply(out, substr, 2, 100)
  out <- lapply(out, function (feature) as.data.frame(table(feature), stringsAsFactors = FALSE))
  if(split) return(lapply(out, function(x) x[order(x$Freq, decreasing = TRUE),]))
  out$up$level <- "up"
  out$down$level <- "down"
  rbind(out$up, out$down)[order(rbind(out$up, out$down)$Freq, decreasing = TRUE),]
}

# save combined combo2fcount result to csv file with option to return value (ret = TRUE)
combo2csv <- function(combo, name = deparse(substitute(combo)), dir = ".", strip = FALSE, ret = FALSE) {
  out <- combo2fcount(combo, stripX = strip, split = FALSE)
  write.csv(out, file = paste(dir, name, sep = "/"), row.names = FALSE)
  if(ret) return(out)
}

# make combo strings and feature dfs from moses output.  score = TRUE returns moses scoring data in brackets
# TODO: function to put moses scoring data into data frame, check GEO meta data functions?
moses2combo <- function(mout, score = FALSE) {
  require(stringr)
  mout <- str_split_fixed(mout, fixed("["), 2)
  out <- str_trim(str_sub(mout[, 1], 5, -2))    # this will fail of moses score is ,<= -10
  if(score) return(paste("[", mout[, 2], sep = ""))
  return(out)
}

# make combo strings and feature dfs using moses2combo and combo2fcount
Mout2str <- function(mout, strip = FALSE) {
require(stringr)
out <- vector("list", 3)
names(out) <- c("combo", "features", "scores")
out[[1]] <- moses2combo(mout)
out[[2]] <- combo2fcount(out[[1]], stripX = strip, split = FALSE)
out[[3]] <- moses2combo(mout, score = TRUE)
return(out)
}

### extract moses output from log files
# get last n lines from file
lastNlines <- function(filename, N = 12, drop = 2) {
  ## filename is of mode character
  out <- system(sprintf("wc -l %s", filename), intern=TRUE)
  n <- as.integer(sub(sprintf("[ ]*([0-9]+)[ ]%s", filename), "\\1",out))
  print(n)
  scan(filename,what="",skip=n - N, nlines=N - drop,sep="\n", quiet=TRUE)
}

getMout <- function(dir = ".", type = ".log", n = 12, d = 2) {
  lfiles <- list.files(path = dir, pattern = type)
  out <- vector("list", length(lfiles))
  for(i in seq_along(lfiles)) {
    out[[i]] <- lastNlines(lfiles[i], N = n, drop = d)
  }
  return(out)
}

##### evaluate combo string vectors
# The formulas used here are:
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
  mc_ops <- list(c("true", "TRUE"), c("(", "(list("), c(")", "))"), c(" ", ", "),c("$", ""))
  for(i in seq_along(mc_ops)) str <- str_trim(str_replace_all(str, fixed(mc_ops[[i]][1]), mc_ops[[i]][2]))
  str
}

# evaluate translated expression
evalstring <- function(str){
  eval(parse(text=str))
}

#put confusionMatrix outputs in table
cmv <- function(cm) {
  c('[['(cm, 3)[c(1, 3:6)], '[['(cm, 4))
}

cml2df <- function(cmlist) {
  out <- t(vapply(cmlist, cmv, numeric(13)))
  row.names(out) <- paste("C", seq(1, length(cmlist)), sep = "")
  return(as.data.frame(out))
}

# evaluate translated combo programs on test dfs and return result lists using caret::confusionMatrix
testCstring <- function(rlist, testdf, concol = 1, conrat) {
  control <- testdf[[concol]]
  combo <- rlist[[1]]
  n <- length(control)
  attach(testdf)
  m <- length(combo)
  results <- matrix(nrow = m,ncol = n, dimnames = list(paste("C", 1:m, sep = ""), row.names(testdf)))
  metrics <- vector("list", m)
  for(i in 1:m){
    results[i,] <- as.numeric(evalstring(combo.edit(combo[i])))
    fresult <- as.factor(results[i,])
    levels(fresult) <- c("0", "1")
    metrics[[i]] <- caret::confusionMatrix(
      fresult, as.factor(control), prev = conrat)
  }
  detach(testdf)
  metrics <- cml2df(metrics)
  results <- as.data.frame(rbind(results, control))
  return(list(result = results, score = metrics, combo = rlist[[1]], Mscores = rlist[[3]], features = rlist[[2]]))
}

# evaluate list of combo strings & compute catagorization metrics.  "cc_ratio" is cases/1's over cases + controls/0's
testClist <- function(clist, tdatlist, cc_ratio = .5) {
 out <- lapply(clist, Mout2str)
 tdatlist[["fold_index"]] <- NULL
 return(Map(testCstring, out, tdatlist, conrat = cc_ratio))
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


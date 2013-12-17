#  wrapper/interface functions for moses binary

#make k-fold partitions of moses data set files and list of testing dfs
makeMpartitions <- function(data, k = 10, control = 1, ...){
  require(caret)
  namestr <- deparse(substitute(data))
  flds <- createDataPartition(as.factor(data[, control]), times = k, ...)
  out <- vector("list", k)
  names(out) <- paste(namestr, "_f", 1:k, sep = "")
  for(i in 1:k){
    write.csv(data[flds[[i]],], paste(names(out)[i], ".csv", sep = ""), row.names = FALSE)
    out[[i]] <- data[-flds[[i]],]
  }
  return(out)
}
# run moses on a file
moses <- function( flags = "", DSVfile = "", output = TRUE) {
  if(flags == "help") flags <- "--help"          # moses(help) -> moses --help
  if(flags == "version")  flags <- "--version"    # moses() -> moses --version
  if(DSVfile != "") flags <- paste("--input-file", DSVfile, "--log-file", paste(word(DSVfile, sep = fixed(".")), ".log", sep = ""), flags)
  system2("moses", args=flags, stdout = output)
}

# run moses on a directory
runMfolder <- function(flags = "",dir = getwd(),  output = TRUE) {
  files <- list.files(path = dir)
  out <- vector("list", length(files))
  names(out) <- paste(word(files, sep = fixed(".")), "_Mout", sep = "")
  for(i in seq_along(files)) out[[i]] <- moses(flags, files[i])
  return(out)
}

# make df of features from combo strings
combo2flist <- function(cstr, rank) {
require(stringr)
probe <- str_replace_all(cstr, "and+", "")
probe <- str_replace_all(probe, "or+", "")
probe <- str_replace_all(probe, "[()$]+", "")
flist <- str_split(probe, pattern = " ")
ranks <- rep(rank, vapply(flist, length, integer(1)))
fdf <- data.frame(feature = unlist(flist), score = ranks, stringsAsFactors = FALSE)
fdf$low <- str_detect(fdf$feature, "!")
fdf$combo <- rep(seq(length(probe)), vapply(flist, length, integer(1)))
fdf$feature <- str_replace(fdf$feature, "!", "")
fdf <- fdf[!duplicated(fdf[[1]]),]
return(fdf)
}

# make combo strings and feature dfs from moses output
Mout2str <- function(ostr) {
require(stringr)
out <- vector("list", 3)
names(out) <- c("combo", "features", "ranks")
out[[1]] <- str_trim(str_split_fixed(ostr, " ", 2)[,2])
rank <- as.numeric(str_split_fixed(hlf1_r, " ", 2)[,1])
out[[2]] <- combo2flist(out[[1]], rank)
out[[3]] <- rank
return(out)
}

###evaluate combo string vectors
# translate n >=2 argument boolean operators
and -> function(x) Reduce("&", x)
or -> function(x) Reduce("|", x)

# turn combo string vector into list of R function combinations
combo.edit <- function(str) {
  mc_ops <- list(c("(", "(list("), c(")", "))"), c(" ", ", "),c("$", ""))
  for(i in seq_along(mc_ops)) str <- str_replace_all(str, fixed(mc_ops[[i]][1]), mc_ops[[i]][2])
  str
}

# evaluate translated expression
evalstr -> function(str){
  eval(parse(text=str))
}

#put confusionMatrix outputs in table
cm2df <- function(cmlist) {
  vapply()
}
# evaluate translated combo programs on test dfs and return result lists
testCstring <- function(rlist, testdf, concol = 1, conrat) {
  control <- testdf[[concol]]
  combo <- rlist[[1]]
  n <- length(control)
  attach(testdf)
  m <- length(combo)
  results <- matrix(nrow = m,ncol = n, dimnames = list(paste("C", 1:m, sep = ""), row.names(testdf)))
  metrics <- vector("list", m)
  for(i in 1:m){
    results[i,] <- as.numeric(evalstr(combo.edit(combo[i])))
    metrics[[i]] <- confusionMatrix(
      as.factor(results[i,]), as.factor(control), prev = conrat)
  }
  detach(testdf)
  return(list(result = rbind(results, control), score = metrics, combo = rlist[[1]], ranks = rlist[[3]], features = rlist[[2]]))
}

# evaluate list of combo strings & compute catagorization metrics
testClist <- function(clist, tdatlist, cc_ratio = .5) {
 out <- lapply(clist, Mout2str)
 return(Map(testCstring, out, tdatlist, conrat = cc_ratio))
 }


# TODO:  guided menu system to set moses flags for data appropriate analysis

# #######  combo -> R failures
# # turn combo to lisp string
# combo.edit <- function(str) {
#   mc_ops <- list(c("and(", "('&' "), c("or(", "('|' "), c("$", ""))
#   for(i in seq_along(mc_ops)) str <- str_replace_all(str, fixed(mc_ops[[i]][1]), mc_ops[[i]][2])                   # translate logical operators
#   str_trim(str)
# }
# # convert lisp string to nested lists.
# combo2list <- function(str){ paste("list",
#                                    gsub("( \\()", " list\\(", gsub("[ ]{1,}", ", ",str), perl = TRUE),
#                                    sep = "")
# }
# # parse lisp list to r list
# is.int <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
# #
# lisp2r <- function(lst){
#   if(class(lst) != "list") return(lst)
#   rlst <- vector("list", 2 * length(lst) - 3)
#   for(i in seq_along(rlst)){
#     if(!is.int(i/2)) rlst[[i]] <- tail(lst, -1)[[(i + 1)/2]]
#     else rlst[[i]] <- lst[[1]]
#   }
#   return(rlst)
# }
# 
# combo_eval <- function(list){
#   state <- sapply(list, class)# if list is fully parsed to atomic vectors then return
#   if(str_detect(str_c(state, collapse = "TRUE"), fixed("list")))
#     return(toString(list))
#   list <- lapply(list, lisp2r)
#   list <- combo_eval(list)
# }
# 
# eval(parse(text=combo2list(str)))  # evaluate string to form list

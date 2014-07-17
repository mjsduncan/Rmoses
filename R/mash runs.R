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
sapply(lapply(ls(pattern = "_cum_"), get), function(x) summary(as.factor(x$rank)))
sapply(lapply(ls(pattern = "_cum_"), get), function(x) summary(as.factor(x$score$Accuracy)))

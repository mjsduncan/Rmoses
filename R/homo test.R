### more moses tests
# prepare data directory
setwd("~/Desktop/biodata/sf2_data/moses_runs/homo_moses")
load("~/R/Rmoses/testing.rdata")
homo_moses <- as.data.frame(homo_moses)
names(homo_moses)[1] <- "case"
rownames(homo_moses) <- NULL
homo_test <- makeMpartitions(homo_moses)
# homo_out <- runMfolder("-j8 --balance 1 -m 100000 -W 1 --output-cscore 1 --logical-perm-ratio 0 --result-count 100")
homo_combos <- vector("list", 10)
names(homo_combos) <- names(homo_test$train)
homo_combos <- lapply(homo_combos, function(x) x <- Homo_combo$combo)
homo_eval <- testClist(homo_combos, homo_test)

# column names of training & test sets form column names of complete data set
sapply(merge(as.data.frame(lapply(homo_test[[11]], as.character), stringsAsFactors = FALSE), as.data.frame(lapply(homo_eval, function(x) names(x[[1]])), stringsAsFactors = FALSE), homo = TRUE), function(x) length(unique(x)))
# homo_moses_f1  homo_moses_f2  homo_moses_f3  homo_moses_f4  homo_moses_f5  homo_moses_f6  homo_moses_f7  homo_moses_f8 
# 67              67              67              67              67              67              67              67 
# homo_moses_f9 homo_moses_f10 
# 67              67 

homo_names <- unlist(lapply(homo_eval[[1]], function(x) rownames(x[[1]])))
length(homo_names)
# [1] 1010
length(unique(homo_names))
# [1] 147

# combine results & get aggregate performance
homo_agg <- aggResults(homo_eval)

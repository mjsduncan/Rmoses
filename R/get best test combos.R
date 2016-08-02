### for each feature fulder, get eval results from each meta-run folder, get best test combo df, and save as RDS file
source("/root/Rmoses/R/moses2.R")
feature_folders <- paste0("feature", c(23, 24, 10, 25, 9, 11, 4, 22))
# setwd("/mnt/biodata/svm_check/")
# loop through folders
for(i in 1:8) {
  setwd(feature_folders[i])
  data_dirs <- list.dirs(full.names = FALSE, recursive = FALSE)
  for(j in data_dirs) {
    setwd(j)
    eval_file <- list.files(pattern = "_eval.rdata")
    if(length(eval_file) != 1) {
      print(paste("file error in folder", j))
      setwd("..")
      next
    }
    load(eval_file)
    feature_testCs <- bestTestCombos(feature_eval, 0.5)
    setwd("..")
    saveRDS(feature_testCs[, c("combo", "Sensitivity", "Pos Pred Value", "Balanced Accuracy", "AccuracyPValue", "Prevalence")], file = paste0(feature_folders[i], "_", j, "_combos.rds"))
  }
  rm(feature_eval)
  setwd("..")
}

testCombos <- vector("list", 8)
names(testCombos) <- feature_folders
for(n in feature_folders) {
  rdsFiles <- list.files(n, ".rds")
  evalRuns <- vector("list", length(rdsFiles))
  names(evalRuns) <- rdsFiles
  for(m in rdsFiles) {
    evalRuns[[m]] <- readRDS(paste0(n, "/", m))
  }
  testCombos[[n]] <- evalRuns
}
save(testCombos, file = "testCombos.rdata")

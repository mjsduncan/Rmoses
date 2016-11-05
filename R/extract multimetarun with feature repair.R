source("~/R/Rmoses/R/moses2.R")
library(stringr)
feature_folders <- paste0("feature", c(9, 10, 11, 23, 24, 25)) #
setwd("/mnt/biodata/svm_check2/basic")
# loop through folders
for(i in 1:6) {
  setwd(feature_folders[i])
  data_dirs <- list.dirs(full.names = FALSE, recursive = FALSE)
  data_dirs <- data_dirs[c(2, 5:10)]
  for(j in data_dirs) {
    setwd(j)
    print(paste("entering directory", getwd()))
    # evaluate combos
    feature_data <- readRDS("feature_input.rds")
    feature_combos <- readRDS(paste0(feature_folders[i], "_combos_", j, ".rds"))

        # rename features "and" & "or" to "and_" & "or_"
    for(n in names(feature_combos$combo)) {
      names(feature_data$train[[n]])[names(feature_data$train[[n]]) == "and"] <- "and_"
      names(feature_data$test[[n]])[names(feature_data$test[[n]]) == "and"] <- "and_"
      names(feature_data$train[[n]])[names(feature_data$train[[n]]) == "or"] <- "or_"
      names(feature_data$test[[n]])[names(feature_data$test[[n]]) == "or"] <- "or_"
      print("saving feature_data with renamed 'and' & 'or' features")
      saveRDS(feature_data, "feature_input_fixed.rds")
      # rename combo features
      feature_combos$combo[[n]] <- str_replace_all(feature_combos$combo[[n]], "\\$and ", "$and_ ")
      feature_combos$combo[[n]] <- str_replace_all(feature_combos$combo[[n]], "\\$or ", "$or_ ")
      feature_combos$combo[[n]] <- str_replace_all(feature_combos$combo[[n]], "\\$and\\)", "$and_)")
      feature_combos$combo[[n]] <- str_replace_all(feature_combos$combo[[n]], "\\$or\\)", "$or_)")
      print("saving feature_combos with renamed 'and' & 'or' features")
      saveRDS(feature_combos, paste0(feature_folders[i], "_combos_", j, "_fixed.rds"))
    }
    # evaluate fixed combos on fixed data & save results
    feature_eval <- testClist(feature_combos$combo, feature_data)
    saveRDS(feature_eval, file = paste0(feature_folders[i], "_", j, "_eval.rds"))
    setwd("..")
  }
  rm(feature_eval, feature_data, feature_combos)
  setwd("..")
}

# make a list of lists of test score matrices from the .rds file for each run of each feature
testCombos <- vector("list", 7)
names(testCombos) <- feature_folders
for(i in feature_folders) {
  run_folders <- list.dirs(i, full.names = FALSE, recursive = FALSE)
  evalRuns <- vector("list", length(run_folders))
  names(evalRuns) <- run_folders
  for(j in run_folders){
    evalRuns[[j]] <- bestTestCombos(readRDS(paste0(i, "/", j, "/", i, "_", j, "_eval.rds")))
  }
  testCombos[[i]] <- evalRuns
}

# rbind the list of matrices for each feature
testCombos <- lapply(testCombos, function(x) Reduce(rbind, x))

# sort matrices by balanced accuracy
testCombos <- lapply(testCombos, function(x) dplyr::arrange(x, dplyr::desc(`Balanced Accuracy`)))
save(testCombos, file = "testCombos.rdata")

# best combo
lapply(testCombos, function(x) x[1, c("combo", "Accuracy", "AccuracyPValue", "Balanced Accuracy", "Sensitivity", "Pos Pred Value", "Prevalence")])
# $feature4
# combo
# and(or(and($a !$i !$of) $second) or($come $on $second) or($hold $let) !$in.)
#  Accuracy AccuracyPValue Balanced Accuracy Sensitivity Pos Pred Value  Prevalence
# 0.9943788      0.4926618          0.504474    0.999966      0.0056887 0.005638082
#
# $feature9
# NA

# $feature10
# combo
# and(or(and(or($bunch $hey $oh $punch $uh) !$and_) $an $salina) $thank)
#  Accuracy AccuracyPValue Balanced Accuracy Sensitivity Pos Pred Value   Prevalence
# 0.9997468      0.4667315         0.6874578   0.9999156    0.000432034 0.0002700878
#
# $feature11
# NA

# $feature23
# combo
# 1 and(or(and(or($a $exact $for.) $yeah) and($agre $i) $definit) or(and(or($agre $exact $will) or($definit !$that $um $were $will) !$on) $doe $oh) !$dont !$fortun !$have !$him !$of !$parent !$societi !$the)
#    Accuracy AccuracyPValue Balanced Accuracy Sensitivity Pos Pred Value Prevalence
# 1 0.9825456       0.433615         0.5208274   0.9993471     0.01830531 0.01755571

# $feature24
# combo
# $uhuh
#  Accuracy AccuracyPValue Balanced Accuracy Sensitivity Pos Pred Value  Prevalence
# 0.9962188      0.4910838         0.5022222           1    0.003815001 0.003798109

# $feature25
# combo
# 1 or(and(or(and(or(and(!$about !$mani) $good $hope) !$anyon !$dalla !$do !$enough !$him !$know !$male !$of !$queen !$societi $talk !$ten !$that !$thought !$who) and(or($call $well) !$interest $thank) and(or($enjoy $good $nice) $hope !$look) and(!$degre !$have $luck)) !$can !$coat !$dont !$wish $you) and(!$but $righti))
#    Accuracy AccuracyPValue Balanced Accuracy Sensitivity Pos Pred Value  Prevalence
# 1 0.9924038   4.180617e-05         0.6082958    0.999523     0.01158647 0.009098582

# after 10 more metaruns
# make a list of lists of test score matrices from the .rds file for each run of each feature
checkCombos <- vector("list", 6)
names(checkCombos) <- feature_folders
for(i in feature_folders) {
  run_folders <- list.dirs(i, full.names = FALSE, recursive = FALSE)
  evalRuns <- vector("list", length(run_folders))
  names(evalRuns) <- run_folders
  for(j in run_folders){
    evalRuns[[j]] <- bestTestCombos(readRDS(paste0(i, "/", j, "/", i, "_", j, "_eval.rds")))
  }
  checkCombos[[i]] <- evalRuns
}

# rbind the list of matrices for each feature
checkCombos <- lapply(checkCombos, function(x) Reduce(rbind, x))

# sort matrices by balanced accuracy
checkCombos <- lapply(checkCombos, function(x) dplyr::arrange(x, dplyr::desc(`Balanced Accuracy`)))
save(checkCombos, file = "checkCombos.rdata")

# best combo
lapply(checkCombos, function(x) x[1, c("combo", "Accuracy", "AccuracyPValue", "Balanced Accuracy", "Sensitivity", "Pos Pred Value", "Prevalence")])
# $feature9
#                                                                                combo Accuracy AccuracyPValue Balanced Accuracy Sensitivity
# 1 and(or($about $drop $excus $keep $rope $um $well) !$and_ !$convict $sorri !$tanya) 0.999713      0.4686334         0.6110858   0.9999493
#   Pos Pred Value   Prevalence
# 1    0.000390609 0.0003038488
#
# $feature10
#                                                                    combo  Accuracy AccuracyPValue Balanced Accuracy Sensitivity
# 1 and(or(and(or($bunch $hey $oh $punch $uh) !$and_) $an $salina) $thank) 0.9997468      0.4667315         0.6874578   0.9999156
# Pos Pred Value   Prevalence
# 1  0.000432034 0.0002700878
#
# $feature11
# combo Accuracy AccuracyPValue Balanced Accuracy Sensitivity Pos Pred Value Prevalence
# NA  <NA>       NA             NA                NA          NA             NA         NA
#
# $feature23
#                                                           combo  Accuracy AccuracyPValue Balanced Accuracy Sensitivity
# 1 and(or(and(!$the !$uh) !$to) or(!$dont !$i) !$a $agre !$and_) 0.9825287       0.445972         0.5264849   0.9991237
# Pos Pred Value Prevalence
#     0.01852035 0.01755571
#
# $feature24
#                                      combo  Accuracy AccuracyPValue Balanced Accuracy Sensitivity Pos Pred Value  Prevalence
# 1 and(!$a $actual $no !$tax !$to !$troubl) 0.9962525      0.4379301         0.5066667           1    0.003849238 0.003798109
#
# $feature25
# combo
# 1 and(or(and(or(and(!$about !$someon) $get $time) !$can !$dalla !$did !$do !$into !$know !$long !$male !$mani !$of !$societi !$suppos $talk !$them !$who $you !$your) and($good $luck !$oh !$time) and($hi !$you)) !$had !$mean !$pretti !$we)
#    Accuracy AccuracyPValue Balanced Accuracy Sensitivity Pos Pred Value  Prevalence
# 1 0.9925557   6.864231e-06         0.6129681   0.9995911     0.01172456 0.009098582

# ensemble scores for pooled testing & training for feature25
load("/home/biocog/Desktop/biodata/svm_check2/basic/feature25/feature25.rdata")
names(feature_input)[grep("^and$", names(feature_input))] <- "and_"
names(feature_input)[grep("^or$", names(feature_input))] <- "or_"
ens_test <- testCensemble(checkCombos$feature25$combo, feature_input)
# [1] "ensemble mean accuracy = 0.740740740740741"
# [1] "accuracy weighted mean = 0.740740740740741"
# [1] "ppd weighted mean = 0.742017879948914"
# [1] "sensitivity weighted mean = 0.740740740740741"
ens_train <- testCensemble(wvigene_train05$combo, wvigene)
# [1] "ensemble mean accuracy = 0.743295019157088"
# [1] "accuracy weighted mean = 0.743295019157088"
# [1] "ppd weighted mean = 0.743295019157088"
# [1] "sensitivity weighted mean = 0.743295019157088"

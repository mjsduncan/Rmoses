### example metarun script
require(abind)
source("~/path/to/moses2.R")
setwd("/path/to/metarun/directory")
wvigene <- read.csv("/path/to/iniput/data/wvigene.csv", stringsAsFactors=FALSE)

wvigene_data <- makeMpartitions(wvigene, p = .8)

# fancy fs without sub-sampling ratio selection outputsaved in folder var2
wvigene_out <- runMfolder("-j8 --balance 1 -m 100000 -W 1 --output-cscore 1 --result-count 100 --reduct-knob-building-effort=1 --hc-widen-search=1 --enable-fs=1 --fs-algo=smd --fs-target-size=4 --hc-crossover-min-neighbors=5000 --fs-focus=all --fs-seed=init --complexity-ratio=3 --hc-max-nn-evals=10000 --hc-crossover-pop-size=1000 -l debug")

wvigene_combos <- moses2combo(wvigene_out)

wvigene_eval <- testClist(wvigene_combos$combo, wvigene_data)

# graph test vs train metrics
library(ggplot2)
wvigene_tvt <- testVtrain(wvigene_eval, c(1, 5, 8, 6))
wvigene_plot_dat <- tvtScatter(wvigene_tvt)

# this drops outlier combo that does worse than chance
wvigene_Accuracy <- qplot(data = wvigene_plot_dat$Accuracy[wvigene_plot_dat$Accuracy$test > 0.5,], x = train, y = test, color = run, main = "gene level data with delux feature selection & hill-climbing\naccuracy", size = I(3), alpha = I(0.2)) + stat_smooth(method="lm") + theme(legend.text=element_text(size=12), legend.title=element_text(size=12), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"))
wvigene_Accuracy

# save plot
ggsave("/mnt/biodata/sf2_data/moses_runs/wvi/gene4/wvigene4_accuracy.svg")

wvigene_sensitivity <- qplot(data = wvigene_plot_dat$Sensitivity[wvigene_plot_dat$Sensitivity$test < 1,], x = train, y = test, color = run, main = "gene level data with delux feature selection & hill-climbing\nsensitivity", size = I(3), alpha = I(0.2)) + stat_smooth(method="lm") + theme(legend.text=element_text(size=12), legend.title=element_text(size=12), axis.title.x=element_text(face="bold"), axis.title.y=element_text(face="bold"))
wvigene_sensitivity
ggsave("/mnt/biodata/sf2_data/moses_runs/wvi/gene4/wvigene4_sensitivity.svg")

# get combos scoring significantly better than chance (p < 0.05) by partition
wvigene_test05 <- bestTestCombos(wvigene_eval, 0.05)
wvigene_train05 <- bestTrainCombos(wvigene_eval, 0.05)

# combo complexity for testing and training sets
rowMeans(sapply(wvigene_test05$combo, comboSize, USE.NAMES = FALSE))
# features operators      nots
# 24.86325  12.92593  13.45442
rowMeans(sapply(wvigene_train05$combo, comboSize, USE.NAMES = FALSE))
# features operators      nots
# 25.22095  13.05394  13.41079

# combo scores for testing and training sets
colMeans(wvigene_test05[, c("Accuracy", "Sensitivity", "Pos Pred Value")])
# Accuracy    Sensitivity Pos Pred Value
# 0.6478377      0.6648167      0.6493377
colMeans(wvigene_train05[, c("Accuracy", "Sensitivity", "Pos Pred Value")])
# Accuracy    Sensitivity Pos Pred Value
# 0.7289139      0.7294646      0.7345652

summary(wvigene_test05$Accuracy)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.5769  0.6218  0.6603  0.6478  0.6731  0.6923
summary(wvigene_train05$Accuracy)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.6954  0.7225  0.7257  0.7289  0.7368  0.7576

# ensemble scores for pooled testing & training combos
ens_test <- testCensemble(wvigene_test05$combo, wvigene)
# [1] "ensemble mean accuracy = 0.740740740740741"
# [1] "accuracy weighted mean = 0.740740740740741"
# [1] "ppd weighted mean = 0.742017879948914"
# [1] "sensitivity weighted mean = 0.740740740740741"
ens_train <- testCensemble(wvigene_train05$combo, wvigene)
# [1] "ensemble mean accuracy = 0.743295019157088"
# [1] "accuracy weighted mean = 0.743295019157088"
# [1] "ppd weighted mean = 0.743295019157088"
# [1] "sensitivity weighted mean = 0.743295019157088"

# ensemble scores by meta-run
wvigene_eXr <- testCensXrun(wvigene_eval)
#             ensemble mean accuracy accuracy weighted mean ppd weighted mean sensitivity weighted mean
# wvigene_f1               0.6644737              0.6644737         0.6644737                 0.6644737
# wvigene_f2               0.6644737              0.6644737         0.6644737                 0.6644737
# wvigene_f3               0.5592105              0.5592105         0.5592105                 0.5592105
# wvigene_f4               0.5263158              0.5263158         0.5263158                 0.5263158
# wvigene_f5               0.6250000              0.6250000         0.6250000                 0.6250000
# wvigene_f6               0.5460526              0.5460526         0.5460526                 0.5460526
# wvigene_f7               0.6118421              0.6118421         0.6118421                 0.6118421
# wvigene_f8               0.6578947              0.6578947         0.6578947                 0.6578947
# wvigene_f9               0.6513158              0.6513158         0.6513158                 0.6513158
# wvigene_f10              0.6118421              0.6118421         0.6118421                 0.6118421

ensTest <- testCensXsample(wvigene_eval, samplesInTrainingSet(wvigene_data$test$fold_index), "test")
# [1] "mean out-of-sample accuracy of combo mean ensembles is 0.644478063540091"
# [1] "122 samples were in the training sets of all 783 cross-validation runs"
ensTrain <- testCensXsample(wvigene_eval, samplesInTrainingSet(wvigene_data$test$fold_index))
# [1] "mean out-of-sample accuracy of combo mean ensembles is 0.617021276595745"
# [1] "78 samples were in the training sets of all 783 cross-validation runs"

# use different scores and filter combos by score
ensTrainAcc7 <- testCensXsample(wvigene_eval, samplesInTrainingSet(wvigene_data$test$fold_index), scoreMin = 0.7)
# [1] "mean out-of-sample accuracy of combo mean ensembles is 0.622613803230543"
# [1] "102 samples were in the training sets of all 783 cross-validation runs"
ensTrainPPD7 <- testCensXsample(wvigene_eval, samplesInTrainingSet(wvigene_data$test$fold_index), score = "Pos Pred Value", scoreMin = 0.7)
# [1] "mean out-of-sample accuracy of combo mean ensembles is 0.617021276595745"
# [1] "78 samples were in the training sets of all 783 cross-validation runs"
ensTrainSens7 <- testCensXsample(wvigene_eval, samplesInTrainingSet(wvigene_data$test$fold_index), score = "Sensitivity", scoreMin = 0.7)
# [1] "mean out-of-sample accuracy of combo mean ensembles is 0.630769230769231"
# [1] "133 samples were in the training sets of all 783 cross-validation runs"


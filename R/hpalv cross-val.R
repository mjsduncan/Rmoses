## hpalv n-fold validation
require(plyr)
require(Biobase)
require(stringr)
load("~/github/mosesData/gsematrix.rdata")

hpalv <- read.table("~/github/openbiomind2/control datasets/alveolar macrophage/pigAndHumanMatchAlveolarMacrophage.txt", stringsAsFactors = FALSE)
p2s <- hpalv[, c(1, 2, 47, 72)]
# data clean up:  half of values are characters, 2 extra columns X.1 & humanSum3, 
rownames(hpalv) <- paste(hpalv[,1], hpalv[, 2], sep = " - ")
hpalv <- subset(hpalv, select = -c(1, 2, 47, 72))

# map names to gene symbols
p2s <- merge(p2s, fData(gsematrix$halmac[[1]])[, c(1, 3)], by.x = "human", by.y = "ID", all.x = TRUE)
hpalv <- cbind(p2s$'Gene symbol', hpalv)
names(hpalv)[1] <- "symbol"

library(doParallel)
nodes <- detectCores()
cl <- makeCluster(nodes)
registerDoParallel(cl)
hpalv.avg <- ddply(hpalv, .(symbol), colwise(mean, is.numeric), .parallel = TRUE, .progress = "text")
stopImplicitCluster()

hpalv <- med.normalize(hpalv.avg[, -1])
row.names(hpalv) <- hpalv.avg[, 1]

# make easy sample names
pigGSM <- names(hpalv)[1:44]
names(pigGSM) <- paste("pig", seq(1, length(pigGSM)), sep = "")
humGSM <- names(hpalv)[45:68]
names(humGSM) <- paste("human", seq(1, length(humGSM)), sep = "")
names(hpalv) <- c(names(pigGSM), names(humGSM))

pig <- c(rep.int(1, length(pigGSM)), rep.int(0, length(humGSM)))

hpalv.moses <- as.data.frame(t(rbind(pig, hpalv)))
hpalv <- hpalv.moses
names(hpalv.moses)[1] <- "pig"

p2h.ratio <- length(pigGSM)/(length(pigGSM) + length(humGSM))
# [1] 0.6470588  

# full run using .5 training/testing ratio
setwd("/home/cog/github/mosesData/hpalv/hpalv-hx5")
hpalv.test <- makeMpartitions(hpalv, p = .5)
hpalv_hx52 <- runMfolder("-j4 -W1 -u pig -Y sample --hc-crossover=1 -m 100000 -G 1 --enable-fs=1")
hpamp_hx52 <- testClist(hpalv_hx52, hpalv.test, 0.6470588)
hpamp_hx52.best <- bestCombos(hpamp_hx52)
# w/o feature selection - bad idea, one fold comleted before termination
RBMS2 
CCDC11 
MAPK1 
PRR22 
SCIN 
RAPH1 
MAP3K6 
ERC1 
C2orf15 
BCL2L14 
PANX2 
run1 <- c("KHK", "TNNI1", "STK17B", "APOH", "RBMS2",  "ADAM8",  "ACVR1B", "GALK2",  "FCN1")
hpalv2 <- hpalv[,!(names(hpalv) %in% run1)]
setwd("/home/cog/github/mosesData/hpalv/hpalv-hx52")
hpalv.test <- makeMpartitions(hpalv2, p = .5)
hpalv_hx5nf <- runMfolder("-j4 -W1 -u pig -Y sample --hc-crossover=1 -m 100000 -G 1 --enable-fs=0")
hpamp_hx5nf <- testClist(hpalv_hx5nf, hpalv.test, 0.6470588)
hpamp_hx5nf.best <- bestCombos(hpamp_hx5nf)





# full run using .6 training/testing ratio
setwd("~/github/mosesData/hpalv-hx6")
hpalv.test <- makeMpartitions(hpalv, p = .6)
hpalv_hx6 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 1000000 -G 1 --enable-fs=1")
hpamp_hx6 <- testClist(hpalv_hx6, hpalv.test, 0.2058824)
hpamp_hx6.best <- bestCombos(hpamp_hx6)

# full run using .8 training/testing ratio
setwd("~/github/mosesData/hpalv-hcr8")
hpalv.test <- makeMpartitions(hpalv, p = .8)
hpalv_hcr8 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 100000 -G 1 -T 1 --enable-fs=1")
hpamp_hcr8 <- testClist(hpalv_hcr8, hpalv.test, 0.2058824)
hpamp_hcr8.best <- bestCombos(hpamp_hcr8)

# full run using .6 training/testing ratio
setwd("~/github/mosesData/hpalv-hcr6")
hpalv.test <- makeMpartitions(hpalv, p = .6)
hpalv_hcr6 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 1000000 -G 1 -T 1 --enable-fs=1")
hpamp_hcr6 <- testClist(hpalv_hcr6, hpalv.test, 0.2058824)
hpamp_hcr6.best <- bestCombos(hpamp_hcr6)

mashed.features.hpamp <- mergeFdfs(list(hpamp_hx8.best[["features"]], hpamp_hx6.best[["features"]], hpamp_hcr6.best[["features"]], hpamp_hcr8.best[["features"]]))
best.features.hpamp <- as.data.frame(cbind(mashed.features.hpamp[[1]], mashed.features.hpamp[[3]], mashed.features.hpamp[[2]]), stringsAsF = FALSE)
names(best.features.hpamp) <- c("probes", "low_expression", "combo_rank")
setwd("~/github/mosesData")
write.csv(best.features.hpamp, "hpalvfeatures.hpamp.csv", row.names = FALSE)

# full run using .5 training/testing ratio
setwd("~/github/mosesData/hpalv-hx5")
hpalv.test <- makeMpartitions(hpalv, p = .5)
hpalv_hx5 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 1000000 -G 1 --enable-fs=1")
hpamp_hx5 <- testClist(hpalv_hx5, hpalv.test, 0.2058824)
hpamp_hx5.best <- bestCombos(hpamp_hx5)
hpamp_hx5.bestN2 <- bestCombos(hpamp_hx5, N = 2)

# full run using .5 training/testing ratio
setwd("~/github/mosesData/hpalv-hcr5")
hpalv.test <- makeMpartitions(hpalv, p = .5)
hpalv_hcr5 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 100000 -G 1 -T 1 --enable-fs=1")
hpamp_hcr5 <- testClist(hpalv_hcr5, hpalv.test, 0.2058824)
hpamp_hcr5.best <- bestCombos(hpamp_hcr5)
hpamp_hcr5.bestN2 <- bestCombos(hpamp_hcr5, N = 2)

mashed.features.hpamp <- mergeFdfs(list(hpamp_hx5.best[["features"]], hpamp_hcr5.best[["features"]]))
best.features.hpamp5 <- as.data.frame(cbind(mashed.features.hpamp[[1]], mashed.features.hpamp[[3]], mashed.features.hpamp[[2]]), stringsAsF = FALSE)
names(best.features.hpamp5) <- c("probes", "low_expression", "combo_rank")
setwd("~/github/mosesData")
write.csv(best.features.hpamp5, "hpalvfeatures5.hpamp.csv", row.names = FALSE)

mashed.features.hpamp <- mergeFdfs(list(hpamp_hx5.bestN2[["features"]], hpamp_hcr5.bestN2[["features"]]))
best.features.hpamp5N2 <- as.data.frame(cbind(mashed.features.hpamp[[1]], mashed.features.hpamp[[3]], mashed.features.hpamp[[2]]), stringsAsF = FALSE)
names(best.features.hpamp5N2) <- c("probes", "low_expression", "combo_rank")
setwd("~/github/mosesData")
write.csv(best.features.hpamp5N2, "hpalvfeatures5N2.hpamp.csv", row.names = FALSE)

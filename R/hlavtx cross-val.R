## hlavtx n-fold validation
# full run using .8 training/testing ratio
require(caret)
require(stringr)
setwd("~/Desktop/sandbox")
hlavtx <- read.csv("~/Desktop/sandbox/hlavtx_moses.csv")
rownames(hlavtx) <- hlavtx[,1]
hlavtx[,1] <- NULL

# full run using .8 training/testing ratio
setwd("~/github/mosesData/hlavtx-hx8")
hlavtx.test <- makeMpartitions(hlavtx, p = .8)
hlavtx_hx8 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 100000 -G 1 --enable-fs=1")
hlav_hx8 <- testClist(hlavtx_hx8, hlavtx.test, 0.2058824)
hlav_hx8.best <- bestCombos(hlav_hx8)

# full run using .6 training/testing ratio
setwd("~/github/mosesData/hlavtx-hx6")
hlavtx.test <- makeMpartitions(hlavtx, p = .6)
hlavtx_hx6 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 1000000 -G 1 --enable-fs=1")
hlav_hx6 <- testClist(hlavtx_hx6, hlavtx.test, 0.2058824)
hlav_hx6.best <- bestCombos(hlav_hx6)

# full run using .8 training/testing ratio
setwd("~/github/mosesData/hlavtx-hcr8")
hlavtx.test <- makeMpartitions(hlavtx, p = .8)
hlavtx_hcr8 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 100000 -G 1 -T 1 --enable-fs=1")
hlav_hcr8 <- testClist(hlavtx_hcr8, hlavtx.test, 0.2058824)
hlav_hcr8.best <- bestCombos(hlav_hcr8)

# full run using .6 training/testing ratio
setwd("~/github/mosesData/hlavtx-hcr6")
hlavtx.test <- makeMpartitions(hlavtx, p = .6)
hlavtx_hcr6 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 1000000 -G 1 -T 1 --enable-fs=1")
hlav_hcr6 <- testClist(hlavtx_hcr6, hlavtx.test, 0.2058824)
hlav_hcr6.best <- bestCombos(hlav_hcr6)

mashed.features.hlav <- mergeFdfs(list(hlav_hx8.best[["features"]], hlav_hx6.best[["features"]], hlav_hcr6.best[["features"]], hlav_hcr8.best[["features"]]))
best.features.hlav <- as.data.frame(cbind(mashed.features.hlav[[1]], mashed.features.hlav[[3]], mashed.features.hlav[[2]]), stringsAsF = FALSE)
names(best.features.hlav) <- c("probes", "low_expression", "combo_rank")
setwd("~/github/mosesData")
write.csv(best.features.hlav, "hlavtxfeatures.hlav.csv", row.names = FALSE)

# full run using .5 training/testing ratio
setwd("~/github/mosesData/hlavtx-hx5")
hlavtx.test <- makeMpartitions(hlavtx, p = .5)
hlavtx_hx5 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 1000000 -G 1 --enable-fs=1")
hlav_hx5 <- testClist(hlavtx_hx5, hlavtx.test, 0.2058824)
hlav_hx5.best <- bestCombos(hlav_hx5)
hlav_hx5.bestN2 <- bestCombos(hlav_hx5, N = 2)

# full run using .5 training/testing ratio
setwd("~/github/mosesData/hlavtx-hcr5")
hlavtx.test <- makeMpartitions(hlavtx, p = .5)
hlavtx_hcr5 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 100000 -G 1 -T 1 --enable-fs=1")
hlav_hcr5 <- testClist(hlavtx_hcr5, hlavtx.test, 0.2058824)
hlav_hcr5.best <- bestCombos(hlav_hcr5)
hlav_hcr5.bestN2 <- bestCombos(hlav_hcr5, N = 2)

mashed.features.hlav <- mergeFdfs(list(hlav_hx5.best[["features"]], hlav_hcr5.best[["features"]]))
best.features.hlav5 <- as.data.frame(cbind(mashed.features.hlav[[1]], mashed.features.hlav[[3]], mashed.features.hlav[[2]]), stringsAsF = FALSE)
names(best.features.hlav5) <- c("probes", "low_expression", "combo_rank")
setwd("~/github/mosesData")
write.csv(best.features.hlav5, "hlavtxfeatures5.hlav.csv", row.names = FALSE)

mashed.features.hlav <- mergeFdfs(list(hlav_hx5.bestN2[["features"]], hlav_hcr5.bestN2[["features"]]))
best.features.hlav5N2 <- as.data.frame(cbind(mashed.features.hlav[[1]], mashed.features.hlav[[3]], mashed.features.hlav[[2]]), stringsAsF = FALSE)
names(best.features.hlav5N2) <- c("probes", "low_expression", "combo_rank")
setwd("~/github/mosesData")
write.csv(best.features.hlav5N2, "hlavtxfeatures5N2.hlav.csv", row.names = FALSE)

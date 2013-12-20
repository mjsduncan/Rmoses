## practice n-fold validation
# full run using .8 training/testing ratio
require(caret)
require(stringr)
setwd("~/Desktop/sandbox")
hlungtx1 <- read.csv("~/Desktop/sandbox/hlungtx1_moses.csv")
rownames(hlungtx1) <- hlungtx1[,1]
hlungtx1[,1] <- NULL

# full run using .8 training/testing ratio
setwd("~/github/mosesData/hlungtx1-hx8")
hlungtx1.test <- makeMpartitions(hlungtx1, p = .8)
hlungtx1_hx8 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 100000 -G 1 --enable-fs=1")
hlung_hx8 <- testClist(hlungtx1_hx8, hlungtx1.test, .32)
hlung_hx8.best <- bestCombos(hlung_hx8)

# full run using .6 training/testing ratio
setwd("~/github/mosesData/hlungtx1-hx6")
hlungtx1.test <- makeMpartitions(hlungtx1, p = .6)
hlungtx1_hx6 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 1000000 -G 1 --enable-fs=1")
hlung_hx6 <- testClist(hlungtx1_hx6, hlungtx1.test, .32)
hlung_hx6.best <- bestCombos(hlung_hx6)

# full run using .8 training/testing ratio
setwd("~/github/mosesData/hlungtx1-hcr8")
hlungtx1.test <- makeMpartitions(hlungtx1, p = .8)
hlungtx1_hcr8 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 100000 -G 1 -T 1 --enable-fs=1")
hlung_hcr8 <- testClist(hlungtx1_hcr8, hlungtx1.test, .32)
hlung_hcr8.best <- bestCombos(hlung_hcr8)

# full run using .6 training/testing ratio
setwd("~/github/mosesData/hlungtx1-hcr6")
hlungtx1.test <- makeMpartitions(hlungtx1, p = .6)
hlungtx1_hcr6 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 1000000 -G 1 -T 1 --enable-fs=1")
hlung_hcr6 <- testClist(hlungtx1_hcr6, hlungtx1.test, .32)
hlung_hcr6.best <- bestCombos(hlung_hcr6)

### fix fuck up #################

setwd("~/github/mosesData/hlungtx1-hx6")
hx6.test <- lapply(read.csv("hlungtx1_test.csv", header = TRUE), as.numeric)
hx6.test <- lapply(hx6.test, function(v) hlungtx1[-v,])
hlungtx1_hx6[["hlungtx1_test_Mout"]] <- NULL
hlung_hx6 <- testClist(hlungtx1_hx6, hx6.test, .32)
hlung_hx6.best <- bestCombos(hlung_hx6)

setwd("~/github/mosesData/hlungtx1-hx8")
hx8.test <- lapply(read.csv("hlungtx1_test.csv", header = TRUE), as.numeric)
hx8.test <- lapply(hx8.test, function(v) hlungtx1[-v,])
hlungtx1_hx8[["hlungtx1_test_Mout"]] <- NULL
hlung_hx8 <- testClist(hlungtx1_hx8, hx8.test, .32)
hlung_hx8.best <- bestCombos(hlung_hx8)

setwd("~/github/mosesData/hlungtx1-hcr6")
hcr6.test <- lapply(read.csv("hlungtx1_test.csv", header = TRUE), as.numeric)
hcr6.test <- lapply(hcr6.test, function(v) hlungtx1[-v,])
hlungtx1_hcr6[["hlungtx1_test_Mout"]] <- NULL
hlung_hcr6 <- testClist(hlungtx1_hcr6, hcr6.test, .32)
hlung_hcr6.best <- bestCombos(hlung_hcr6)

setwd("~/github/mosesData/hlungtx1-hcr8")
hcr8.test <- lapply(read.csv("hlungtx1_test.csv", header = TRUE), as.numeric)
hcr8.test <- lapply(hcr8.test, function(v) hlungtx1[-v,])
hlungtx1_hcr8[["hlungtx1_test_Mout"]] <- NULL
hlung_hcr8 <- testClist(hlungtx1_hcr8, hcr8.test, .32)
hlung_hcr8.best <- bestCombos(hlung_hcr8)

mashed.features.hlung <- mergeFdfs(list(hlung_hx8.best[["features"]], hlung_hx6.best[["features"]], hlung_hcr6.best[["features"]], hlung_hcr8.best[["features"]]))
best.features.hlung <- as.data.frame(cbind(mashed.features.hlung[[1]], mashed.features.hlung[[3]], mashed.features.hlung[[2]]), stringsAsF = FALSE)
names(best.features.hlung) <- c("probes", "low_expression", "combo_rank")
setwd("~/github/mosesData")
write.csv(best.features, "lungtxfeatures.csv", row.names = FALSE)

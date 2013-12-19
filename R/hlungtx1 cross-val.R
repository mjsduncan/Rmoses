## practice n-fold validation
# full run using .8 training/testing ratio
require(caret)
require(stringr)
setwd("~/Desktop/sandbox")
hlungtx1 <- read.csv("~/Desktop/sandbox/hlungtx1_moses.csv")
rownames(hlungtx1) <- hlungtx1[,1]
hlungtx1[,1] <- NULL
setwd("~/github/mosesData/hlungtx1-8")
hlungtx1.test <- makeMpartitions(hlungtx1, p = .8)
hlungtx1.results <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 500000 --enable-fs=1")
hlungtx1.rlist <- lapply(hlungtx1.results, Mout2str)
hlungtx1.scores <- Map(testCstring, hlungtx1.rlist, hlungtx1.test, conrat = .32)

# full run using .6 training/testing ratio
setwd("~/github/mosesData/hlungtx1-sa8")
hlungtx1.test <- makeMpartitions(hlungtx1, p = .6)
hlungtx1_sa8 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 500000 --enable-fs=1")
hlung_sa8 <- testClist(hlungtx1_sa8, hlungtx1.test, .32)

combo.v1m <- moses("-j2 -W1 -u controls -Y sample --hc-single-step=1 -m 100000 --enable-fs=1","hlungtx1_moses.csv")
combo.sa <- moses("-j2 -W1 -u controls -Y sample -a sa --enable-fs=1 -m 1000000 ","hlungtx1_moses.csv")   # run moses on 2 cores sa algo



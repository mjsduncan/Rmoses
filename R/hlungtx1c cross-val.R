## hlungtx1c n-fold validation
require(caret)
require(stringr)
require(hgu133a2.db)

load("~/github/mosesData/hlungtx1c.moses.fdata")
hlungtx1 <- t(exprs(GSE8021c))
hlungtx1 <- med.normalize(hlungtx1)
# add control binary (cases are transplants resulting in primary graft disfunction)
controls <- c(0,0,1,0,0,0,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,1,0,1,1,1,1,1,1,1,1,0,1,1,1,1)
hlungtx1 <- cbind(controls, hlungtx1)

# convert col names to gene symbols
probes <- dimnames(hlungtx1)[[2]][-1]
x <- hgu133a2SYMBOL
# Get the probe identifiers that are mapped to a gene symbol
mapped_probes <- mappedkeys(x)
# Convert to a list
xx <- as.list(x[mapped_probes])
p2s <- unlist(xx[probes])
new.names <- p2s[match(probes, names(p2s))]
summary(is.na(new.names))
# Mode   FALSE    TRUE    NA's 
# logical   19953    2324       0 
# dimnames(hlungtx1.moses)[[2]][-1]
new.names[is.na(new.names)] <- probes[is.na(new.names)]
dimnames(hlungtx1)[[2]][-1] <- new.names
save(hlungtx1, file = "~/github/mosesData/hlungtx1c.moses.rdata")
hlungtx1 <- as.data.frame(hlungtx1)                                   
names(hlungtx1) <- make.names(names(hlungtx1))

# full run using .8 training/testing ratio
setwd("~/github/mosesData/hlungtx1c/hlungtx1-hx8")
hlungtx1.test <- makeMpartitions(hlungtx1, p = .8)
hlungtx1_hx8 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 100000 -G 1 --enable-fs=1")
hlung_hx8 <- testClist(hlungtx1_hx8, hlungtx1.test, .32)
hlung_hx8.best <- bestCombos(hlung_hx8)

# full run using .6 training/testing ratio
setwd("~/github/mosesData/hlungtx1c/hlungtx1-hx6")
hlungtx1.test <- makeMpartitions(hlungtx1, p = .6)
hlungtx1_hx6 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 1000000 -G 1 --enable-fs=1")
hlung_hx6 <- testClist(hlungtx1_hx6, hlungtx1.test, .32)
hlung_hx6.best <- bestCombos(hlung_hx6)

# full run using .8 training/testing ratio
setwd("~/github/mosesData/hlungtx1c/hlungtx1-hcr8")
hlungtx1.test <- makeMpartitions(hlungtx1, p = .8)
hlungtx1_hcr8 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 100000 -G 1 -T 1 --enable-fs=1")
hlung_hcr8 <- testClist(hlungtx1_hcr8, hlungtx1.test, .32)
hlung_hcr8.best <- bestCombos(hlung_hcr8)

# full run using .6 training/testing ratio
setwd("~/github/mosesData/hlungtx1c/hlungtx1-hcr6")
hlungtx1.test <- makeMpartitions(hlungtx1, p = .6)
hlungtx1_hcr6 <- runMfolder("-j4 -W1 -u controls -Y sample --hc-crossover=1 -m 1000000 -G 1 -T 1 --enable-fs=1")
hlung_hcr6 <- testClist(hlungtx1_hcr6, hlungtx1.test, .32)
hlung_hcr6.best <- bestCombos(hlung_hcr6)


mashed.features.hlung <- mergeFdfs(list(hlung_hx8.best[["features"]], hlung_hx6.best[["features"]], hlung_hcr6.best[["features"]], hlung_hcr8.best[["features"]]))
best.features.hlung <- as.data.frame(cbind(mashed.features.hlung[[1]], mashed.features.hlung[[3]], mashed.features.hlung[[2]]), stringsAsF = FALSE)
names(best.features.hlung) <- c("probes", "low_expression", "combo_rank")
setwd("~/github/mosesData")
write.csv(best.features.hlung, "hlungtx1cfeatures.csv", row.names = FALSE)

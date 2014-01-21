# moses on jane's combined human pig data set
hplung.moses <- cbind(c(rep.int(0, 39), rep.int(1, 108)), t(hplung))
dimnames(hplung.moses)[[2]][1] <- "is.human"
hplung.moses <- med.normalize(hplung.moses)
write.csv(hplung.moses, file = "~/github/openbiomind2/results/tissue_samples/hplung.csv")
hplung <- read.csv("~/github/openbiomind2/results/tissue_samples/hplung.csv", stringsAsFactors = F)
row.names(hplung) <- hplung[,1]
hplung[[1]] <- NULL
# replace NAs with random bits
hplung <- replace(hplung, is.na(hplung), rbinom(1, 1, .05))
write.csv(hplung, file = "~/github/openbiomind2/results/tissue_samples/hplung_moses.csv")

# full run using .5 training/testing ratio
setwd("~/github/mosesData/hplung-hx5")
hplung.test <- makeMpartitions(hplung, p = .5)
hplung_hx5 <- runMfolder("-j4 -W1 -u is.human -Y sample --hc-crossover=1 -m 100000 -G 1 --enable-fs=1")
hlung_hx5 <- testClist(hplung_hx5, hplung.test, 0.2653061)
hlung_hx5.best <- bestCombos(hlung_hx5)
write.csv(hlung_hx5.best$features, file = "~/github/openbiomind2/results/tissue_samples/hplungBestFeatures1.csv")

# full run using .5 training/testing ratio with -T 1 switch
setwd("~/github/mosesData/hplung-hcr5")
hplung.test <- makeMpartitions(hplung, p = .5)
hplung_hcr5 <- runMfolder("-j4 -W1 -u is.human -Y sample --hc-crossover=1 -m 100000 -G 1 -T 1 --enable-fs=1")
hlung_hcr5 <- testClist(hplung_hcr5, hplung.test, 0.2653061)
hlung_hcr5.best <- bestCombos(hlung_hcr5)
write.csv(hlung_hcr5.best$features, file = "~/github/openbiomind2/results/tissue_samples/hplungBestFeatures2.csv")

mashed.features.hplung <- mergeFdfs(list(hlung_hx5.best[["features"]], hlung_hcr5.best[["features"]]))
best.features.hplung <- as.data.frame(cbind(mashed.features.hplung[[1]], mashed.features.hplung[[3]], mashed.features.hplung[[2]]), stringsAsF = FALSE)
names(best.features.hplung) <- c("probes", "low_expression", "combo_rank")
best.features.hplung <- unique(best.features.hplung)
write.csv(best.features.hplung, "~/github/mosesData/hplungfeatures.csv", row.names = FALSE)


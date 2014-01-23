### compare moses results

load("~/github/mosesData/gsematrix.rdata")
hplungfeatures <- read.csv("~/github/openbiomind2/results/tissue_samples/hplungfeatures.csv")
hlungtx1cfeatures <- read.csv("~/github/mosesData/hlungtx1cfeatures.csv")
hlungtx1_diffex500 <- read.csv("~/github/openbiomind2/results/transplant_samples/hlungtx1_diffex500.csv")
hlungtx1_results <- read.csv("~/github/openbiomind2/results/transplant_samples/hlungtx1_results.csv")
lungtxfeatures <- read.csv("~/github/openbiomind2/results/transplant_samples/lungtxfeatures.csv")

summary(hlungtx1cfeatures$probes %in% hlungtx1_diffex500$Gene.symbol)
# Mode   FALSE    TRUE    NA's 
# logical      69       4       0 
#    Mode   FALSE    TRUE    NA's 
# logical      72       1       0 

hlungtx1features <- merge(lungtxfeatures, fData(gsematrix$hlungtx1[[1]])[, c(1, 3, 2)], 
  by.x = "probes", by.y = "ID", all.x = TRUE)
summary(hlungtx1features$probes %in% hlungtx1_diffex500$ID)
# Mode   FALSE    TRUE    NA's 
# logical     121      10       0 
summary(hlungtx1features$'Gene symbol' == "")
# Mode   FALSE    TRUE    NA's 
# logical      81      50       0 
summary(hlungtx1cfeatures$probes %in% hlungtx1features$'Gene symbol')
summary(hlungtx1cfeatures$probes %in% hlungtx1features$'Gene symbol')
# Mode   FALSE    NA's 
# logical      73       0 


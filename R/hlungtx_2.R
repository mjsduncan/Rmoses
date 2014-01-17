##### test hlungtx1 combo programs on hlungtx2
require(GEOquery)
tx1.es <- getGEO(GEO = "GSE8021", destdir = '~/github/openbiomind2/getgeo', AnnotGPL = T)
tx1.fd <- fData(tx1.es[[1]])
# > summary(best.features[[1]] %in% tx1.fd[[1]])
#    Mode   FALSE    NA's 
# logical     188       0 
# no matches !?! :(
BiocInstaller::biocLite("hgu133a2.db")
require(hgu133a2.db)
x <- hgu133a2UNIGENE
# Get the probe identifiers that are mapped to any Unigene ID
mapped_probes <- mappedkeys(x)
# Convert to a list
xx <- as.list(x[mapped_probes])
if(length(xx) > 0) {
  # Get the REFSEQ for the first five probes
  xx[1:5]
  # Get the first one
  xx[[1]]
}
require(stringr)
best.probes <- substr(best.features[[1]], 2, max(nchar(best.features)))
best.features$probes <- best.probes

summary(best.probes %in% mapped_probes)
#    Mode   FALSE    TRUE    NA's 
# logical      21     167       0
tx1bp_ug <- map_1(xx, best.probes, NA)
best.features[["unigene_hgu133a2.db"]] <- tx1bp_ug
summary(str_detect(best.features$unigene, fixed(",")))
#    Mode   FALSE    TRUE    NA's 
# logical     139      28      21 

## David query
require(DAVIDQuery)
chipTypes <- getAffyChipTypes()
#display choice dialog
item <- menu(graphics = TRUE, title = "Select Array Type",  chipTypes[,"name"]);
#retrieve array type for subsequent usage
tx1chip <- chipTypes[item,"value"]
tx1chip.probes <- getAffyProbesetList("Human Genome U133A 2.0")
tx1p_ug = convertIDList(best.probes, fromType = "AFFYMETRIX_3PRIME_IVT_ID" toType = "UNIGENE", verbose = TRUE)
best.unigene <- merge(best.features, tx1p_ug, by.x = "probes", by.y = "From", all = TRUE)
write.csv(best.unigene, file = "~/github/openbiomind2/results/transplant_samples/hlungtx1_best.csv")
hlungtx1_best <- read.csv("results/transplant_samples/hlungtx1_best.csv", stringsAsFactors = FALSE)
best.map <- merge(hlungtx, hlungtx1_best, by.x = "unigene", by.y = "To", all = TRUE)
best.map <- best.map[!is.na(best.map$probes),]
best.map <- best.map[!is.na(best.map$gb_acc),]
write.csv(best.map, file = "~/github/openbiomind2/results/transplant_samples/best_map.csv")

##### test hlungtx1 combo programs on hlungtx2
require(GEOquery)
tx1.es <- getGEO(GEO = "GSE8021", destdir = '~/GitHub/openbiomind2/getgeo', AnnotGPL = T)
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
summary(substr(best.features[[1]], 2, max(nchar(best.features))) %in% mapped_probes)
#    Mode   FALSE    TRUE    NA's 
# logical      21     167       0


## David query
require(DAVIDQuery)
chipTypes <- getAffyChipTypes()
#display choice dialog
item <- menu(graphics = TRUE, title = "Select Array Type",  chipTypes[,"name"]);
#retrieve array type for subsequent usage
tx1chip <- chipTypes[item,"value"]
tx1chip.array <- getAffyProbesetList(menu=TRUE, verbose=TRUE)
tx1p_ug = convertIDList(best.features[[1]], fromType = "", toType = "UNIGENE", verbose = TRUE)

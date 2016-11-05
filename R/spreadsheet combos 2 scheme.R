### spreadsheet combos 2 scheme
setwd("/mnt/biodata/artificial biologist/stevia2/elegant knockouts/")
library(XLConnect)
wb = loadWorkbook("superCvsWellfem.xlsx")
codingC <-  readWorksheet(wb, sheet="coding combos")$combo
mitoC <- readWorksheet(wb, sheet="mito combos")$combo
dnaC <- readWorksheet(wb, sheet="dna combos")$combo
dbC <- readWorksheet(wb, sheet="db combos")$combo

codingV <-  unique(readWorksheet(wb, sheet="coding variants")[, c("feature", "gene", "vcf_id", "impact_so")])
mitoV <- unique(readWorksheet(wb, sheet="mito variants")[, c("feature", "gene", "vcf_id", "impact_so")])
dnaV <- unique(readWorksheet(wb, sheet="dna variants")[, c("feature", "gene", "vcf_id", "impact_so")])
dbV <- unique(readWorksheet(wb, sheet="db variants")[, c("feature", "gene", "vcf_id", "impact_so")])

combo2sub <- function(combos, features, sub) {
  if(length(features) != length(sub)) return("error: features and substitutes are unequal lengths.")
  require(stringr)
  features <- paste0("X", features)
  names(sub) <- features
  str_replace_all(combos, sub)
}

for(n in c("coding", "mito", "dna", "db")) {
  out <- as.data.frame(combo2sub(get(paste0(n, "C"), 1), get(paste0(n, "V"), 1)$feature, get(paste0(n, "V"), 1)$gene))
  write.table(out, file = paste0(n, "_geneCombos.scm"), row.names = FALSE, col.names = FALSE, quote = FALSE)
  system2("combo-fmt-converter", args = paste0("-f scheme -C ", n, "_geneCombos.scm -o ", n, "_geneCombos.scm"))
}

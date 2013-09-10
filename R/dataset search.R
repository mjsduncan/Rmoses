## exploration of GEO data sets for comparing samples of pigs and humans

library(GEOmetadb, GEOquery, stringr)

# download and connect to metadata sql file

getSQLiteFile()
con <- dbConnect(SQLite(),'GEOmetadb.sqlite')

colDesc <- columnDescriptions()

dbDisconnect(con)
file.remove('GEOmetadb.sqlite')

## exploratory functions

# get get index vector of variables from gsexxx or gsmxxx record code.  TODO: add other types (gpl, gds, others?)

GEOVar <- function(geo, vars) {
  type <- tolower(str_sub(geo, end = 3))
  if(is.na(type)) return("missing GEO code")
  sql <- paste("SELECT ",
    paste(colDesc[colDesc$TableName == type,2][vars], collapse = ", "),
    " FROM ", type,
    " WHERE ", type, " = '", geo, "'",
    sep = "")
  return(dbGetQuery(con, sql))
}

# get variables from vector of record codes.

selVar <- function(char, var) {
  val <- character(length(char))
  for(i in seq_along(char)) val[i] <- as.character(GEOVar(char[i], var))
  return(val)
} 

# get vector of sample codes associated with a series code

gse2gsm <- function(gse) {
  out <- list()
  for(i in seq_along(gse)) {
    samples <- geoConvert(gse[i])$gsm[,2]
    out <- c(out, list(samples))
    print(length(samples))
  }
  return(out)
}

# get vector of dataset codes associated with a series code

gse2gds <- function(gse) {
  for(i in seq_along(gse)){
    sql <- paste("SELECT gds.gse, gds.gds, gds.title, gds.sample_count",
    " FROM gds WHERE gds.gse = '",
    gse[i], "'", sep = "")
    print(dbGetQuery(con, sql))
  }
}

# parse string of variable/value pairs to value vector with named as variable

string2char <- function(str) {
  lns <- str_count(str, ";\t") +1
  vars <- character(lns)
  for(i in seq_along(vars)) vars[i] <- word(str, i, sep = ";\t")
  var <- matrix(unlist(str_split(vars, ": ")), nrow = 2)
  vars <- var[2,]
  names(vars) <- var[1,]
  return(vars)
}
# version for "pig lung" data  TODO:  add separtor variable to function

string2char <- function(str) {
  lns <- str_count(str, "; ") +1
  vars <- character(lns)
  for(i in seq_along(vars)) vars[i] <- word(str, i, sep = "; ")
  var <- matrix(unlist(str_split(vars, ": ")), nrow = 2)
  vars <- var[2,]
  names(vars) <- var[1,]
  return(vars)
}

# get variable from variable string by indexes from geo record name

getSubVar <- function(str, var, sub) string2char(as.character(GEOVar(str, var)))[sub]

# vectorization of above function (assumes all records have same structure)

extSubVar <- function(str, var, sub) {
  val <- character(length(str))
  for(i in seq_along(val)) val[i] <- getSubVar(str[i], var, sub)
  return(val)
}

# separate string names by substring into string matrix and optionally return one column

split_char <- function(chr, sp, rw = 0) {
  vars <- str_split_fixed(chr, sp, n = str_count(chr[1], sp) + 1)
  if(rw > 0 && rw <= length(vars[1,])) return(vars[,rw]) else return(vars) 
}

# example variable string to test parsing

sptest1 <- "disease state: Control;\tSex: 1-Male;\tage: 75;\tgold stage: 0-At Risk;\tsmoker?: 2-Ever (>100);\t%predicted fev1 (pre-bd): 96;\t%predicted fvc (pre-bd): 97;\t%predicted dlco: 78"

# results

best <- c("GSE25935", "GSE47460", "GSE25445", "GSE12194")
names(best) <- c("human liver", "human lung", "pig liver", "pig lung")

best.samples <- gse2gsm(best)
names(best.samples) <- c("human liver", "human lung", "pig liver", "pig lung")

best.samples[['human lung']] <- best.samples[['human lung']][
  extSubVar(best.samples[['human lung']], 13, 1) == "Control"
  ]
best.samples[['human liver']] <- best.samples[['human liver']][
  split_char(selVar(best.samples[['human liver']], 2), "_", 2) == "rep1"
  ]
best.samples[['pig lung']] <- best.samples[['pig lung']][
  extSubVar(best.samples[['pig lung']], 13, 4) == "Uninfected"
  ]
best.samples[['pig lung']] <- best.samples[['pig lung']][
  str_detect(selVar(best.samples[['pig lung']], 2),"Lung ")
  ]

best.samples.backup <- best.samples

gse <- dbGetQuery(con, "select series_id from gsm")
gse.count <- as.data.frame(table(gse$series_id))

# sql searches of geo records that eliminates duplicate data references

human_lung <- paste("SELECT DISTINCT gse.title,gse.gse",
"FROM",
" gsm JOIN gse_gsm ON gsm.gsm=gse_gsm.gsm",
" JOIN gse ON gse_gsm.gse=gse.gse",
" JOIN gse_gpl ON gse_gpl.gse=gse.gse",
" JOIN gpl ON gse_gpl.gpl=gpl.gpl",
"WHERE",
" gsm.molecule_ch1 like '%total RNA%' AND",
" gse.title LIKE '%lung%' AND",
" gpl.organism LIKE '%Homo sapiens%'",sep=" ")
hl <- dbGetQuery(con, human_lung)

human_lung2 <- paste("SELECT DISTINCT gse.title,gse.gse",
"FROM",
" gsm JOIN gse_gsm ON gsm.gsm=gse_gsm.gsm",
" JOIN gse ON gse_gsm.gse=gse.gse",
" JOIN gse_gpl ON gse_gpl.gse=gse.gse",
" JOIN gpl ON gse_gpl.gpl=gpl.gpl",
"WHERE",
" gsm.molecule_ch1 like '%total RNA%' AND",
" gse.title LIKE '%lung%' AND",
" gse.title NOT LIKE '%cell%' AND",
" gse.title NOT LIKE '%cancer%' AND",
" gse.title NOT LIKE '%asthma%' AND",
" gpl.organism LIKE '%Homo sapiens%'",sep=" ")
hl2 <- dbGetQuery(con, human_lung2)

human_liver <- paste("SELECT DISTINCT gse.title,gse.gse",
"FROM",
" gsm JOIN gse_gsm ON gsm.gsm=gse_gsm.gsm",
" JOIN gse ON gse_gsm.gse=gse.gse",
" JOIN gse_gpl ON gse_gpl.gse=gse.gse",
" JOIN gpl ON gse_gpl.gpl=gpl.gpl",
"WHERE",
" gsm.molecule_ch1 like '%total RNA%' AND",
" gse.title LIKE '%liver%' AND",
" gse.title NOT LIKE '%cell%' AND",
" gse.title NOT LIKE '%cancer%' AND",
" gse.title NOT LIKE '%HCC%' AND",
" gse.title NOT LIKE '%cirrhosis%' AND",
" gpl.organism LIKE '%Homo sapiens%'",sep=" ")
hlv <- dbGetQuery(con, human_liver)

pig_lung <- paste("SELECT DISTINCT gse.title,gse.gse",
"FROM",
" gsm JOIN gse_gsm ON gsm.gsm=gse_gsm.gsm",
" JOIN gse ON gse_gsm.gse=gse.gse",
" JOIN gse_gpl ON gse_gpl.gse=gse.gse",
" JOIN gpl ON gse_gpl.gpl=gpl.gpl",
"WHERE",
" gsm.molecule_ch1 like '%total RNA%' AND",
" gse.title LIKE '%lung%' AND",
" gpl.organism LIKE '%Sus scrofa%'",sep=" ")
pl2 <- dbGetQuery(con, pig_lung)

pig_liver <- paste("SELECT DISTINCT gse.title,gse.gse",
"FROM",
" gsm JOIN gse_gsm ON gsm.gsm=gse_gsm.gsm",
" JOIN gse ON gse_gsm.gse=gse.gse",
" JOIN gse_gpl ON gse_gpl.gse=gse.gse",
" JOIN gpl ON gse_gpl.gpl=gpl.gpl",
"WHERE",
" gsm.molecule_ch1 like '%total RNA%' AND",
" gse.title LIKE '%liver%' AND",
" gpl.organism LIKE '%Sus scrofa%'",sep=" ")
plv <- dbGetQuery(con, pig_liver)

human_blood <- paste("SELECT DISTINCT gse.title,gse.gse",
"FROM",
" gsm JOIN gse_gsm ON gsm.gsm=gse_gsm.gsm",
" JOIN gse ON gse_gsm.gse=gse.gse",
" JOIN gse_gpl ON gse_gpl.gse=gse.gse",
" JOIN gpl ON gse_gpl.gpl=gpl.gpl",
"WHERE",
" gsm.molecule_ch1 like '%total RNA%' AND",
#" gse.title LIKE '%immune%' AND",
" gse.title LIKE '%blood%' AND",
#" gse.title LIKE '%macrophage%' AND",
" gpl.organism LIKE '%Homo sapiens%'",sep=" ")
hbl <- dbGetQuery(con, human_blood)



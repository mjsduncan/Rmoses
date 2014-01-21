### moses cross species lung run
# for moses
## apply median norm to object

med.normalize <- function(mat) {
  out <- mat
  for (i in seq(dim(mat)[2])) { 
    vect <- mat[,i]
    med <- median(vect, na.rm = TRUE)
    out[,i] <- as.numeric(vect >= med)
  }
  return(out)
}

# binary median human lung data sets
load("~/github/openbiomind2/control datasets/Lung/binarizedMedianGSE47460GPL6480.RData")
load("~/github/openbiomind2/control datasets/Lung/binarizedMedianGSE47460GPL14550.RData")
hlung1 <- binarizedMedianGSE47460GPL6480
hlung2 <- binarizedMedianGSE47460GPL14550C
rm(binarizedMedianGSE47460GPL14550C, binarizedMedianGSE47460GPL6480)
identical(row.names(hlung1), row.names(hlung2))
# [1] TRUE
hlung <- cbind(hlung1, hlung2)
rm(hlung1, hlung2)

require(DAVIDQuery)
bothHuman <- read.table("~/github/openbiomind2/control datasets/Lung/bothHuman.txt", header=T, quote="\"", stringsAsFactors = FALSE)
row.names(bothHuman) <- bothHuman[,1]
bothHuman[[1]] <- NULL
hlung <- as.data.frame(t(bothHuman))
rm(bothHuman)
hlung.gs = convertIDList(names(hlung), fromType = "ENTREZ_GENE_ID", toType = "OFFICIAL_GENE_SYMBOL", verbose = TRUE)
summary(duplicated(hlung.gs[,1]))
#    Mode   FALSE    TRUE    NA's 
# logical   15167    1500       0 
hlung.gsu <- hlung.gs[!(duplicated(hlung.gs[,1])),]

pigGSM <- row.names(plung)
names(pigGSM) <- paste("pig", seq(1, length(pigGSM)), sep = "")
humGSM <- row.names(hlung)
names(humGSM) <- paste("human", seq(1, length(humGSM)), sep = "")

bothLinearizedPigLung <- read.table("~/github/openbiomind2/control datasets/Lung/bothLinearizedPigLung.txt", header=T, quote="\"", stringsAsFactors = FALSE)
row.names(bothLinearizedPigLung) <- bothLinearizedPigLung[,1]
bothLinearizedPigLung[[1]] <- NULL
plung <- as.data.frame(t(bothLinearizedPigLung))
rm(bothLinearizedPigLung)
plung.gs = convertIDList(names(plung), fromType = "ENTREZ_GENE_ID", toType = "OFFICIAL_GENE_SYMBOL", verbose = TRUE)
summary(duplicated(plung.gs[,1]))
#    Mode   FALSE    NA's 
# logical    1641       0 

summary(plung.gs[,2] %in% hlung.gsu[,2])
#    Mode   FALSE    TRUE    NA's 
# logical     841     800       0 

# vs jane's combined data set
load("~/github/openbiomind2/control datasets/Lung/allpigAndHumanWoGSEsmall.rdata")

uniqgpl2.gs = convertIDList(uniqgpl2[,1],  fromType = "ENSEMBL_GENE_ID", toType = "OFFICIAL_GENE_SYMBOL", verbose = TRUE)
summary(duplicated(uniqgpl2.gs[,1]))
#    Mode   FALSE    TRUE    NA's 
# logical    2943     231       0 
uniqgpl2.gsu <- uniqgpl2.gs[!(duplicated(uniqgpl2.gs[,1])),]
row.names(uniqgpl2) <- uniqgpl2[,1]
uniqgpl2[,1] <- uniqgpl2.gsu[match(uniqgpl2[,1], uniqgpl2.gsu[,1]),2]
hplung <- uniqgpl2[!(duplicated(uniqgpl2[,1])),]
hplung<- subset(hplung, !is.na(Human.Ensembl.Gene.ID))
row.names(hplung) <- hplung[,1]
hplung[[1]] <- NULL
hplung[["ENTREZID"]] <- NULL
hplung.names <- names(hplung)
names(hplung)[1:39] <- paste("pig", 1:39, sep = "")
names(hplung)[40:147] <- paste("human", 1:108, sep = "")
hplung[, 29:39] <- lapply(hplung[,29:39], function(x) as.numeric(as.character(x)))
hplung <- replace(hplung, is.na(hplung), NA)

# moses on jane's combined human pig data set
hplung.moses <- cbind(c(rep.int(0, 39), rep.int(1, 108)), t(hplung))
dimnames(hplung.moses)[[2]][1] <- "is.human"
hplung.moses <- med.normalize(hplung.moses)
write.csv(hplung.moses, file = "~/github/openbiomind2/results/tissue_samples/hplung.csv")
hplung <- read.csv("~/github/openbiomind2/results/tissue_samples/hplung.csv", stringsAsFactors = F)
row.names(hplung) <- hplung[,1]
hplung[[1]] <- NULL
hplung <- replace(hplung, is.na(hplung), rbinom(1, 1, .05))

# run aracne
require(BUS)
require(igraph)
hlung.mim <- build.mim(t(med.normalize(hplung[,40:147])), estimator="mi.empirical")
hlung.net <- aracne(hlung.mim)
hlung.ig <- graph.adjacency(hlung.net, "undirected", weighted = TRUE, diag = FALSE)
hlung.ig
# IGRAPH UNW- 2935 7243 -- 
#   + attr: name (v/c), weight (e/n)

summary(E(hlung.ig)$weight)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.02196 0.18530 0.26760 0.27980 0.35410 6.90800 

is.connected(hlung.ig)
# [1] TRUE

summary(degree.distribution(hlung.ig))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.02196 0.18530 0.26760 0.27980 0.35410 6.90800 

plot(degree.distribution(hlung.ig))
hist(degree(hlung.ig))
lines(density(degree.distribution(hlung.ig)), col = "red")

graph.density(hlung.ig) # Density
# [1] 0.00168221

transitivity(hlung.ig) # Transitivity
# [1] 0.0005930319

hub.score(hlung.ig)$value
# [1] 50.67404

summary(hub.score(hlung.ig)$vector)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000000 0.0000002 0.0000016 0.0010470 0.0000155 1.0000000 
hlung.nsp <- gene.pvalue(EXP = thhlung[,1:39], measure="MI", n.replica=400, net.trim="aracne")
hlung.bun <- pred.network(hlung.nsp[[2]], hlung.net, thresh=0.05)
hlung.big <- graph.adjacency(hlung.bun, "undirected", weighted = TRUE, diag = FALSE)
hlung.big <- delete.vertices(hlung.big, V(hlung.big)[degree(hlung.big) < 1])
hlung.big
# IGRAPH UNW- 2935 7243 -- 
#   + attr: name (v/c), weight (e/n)

plung.big
save.image("~/github/aracne/hplungBUS.RData")


summary(E(hlung.big)$weight)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.02196 0.18530 0.26760 0.27980 0.35410 6.90800 

is.connected(hlung.big)
# [1] TRUE

summary(degree.distribution(hlung.big))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.02196 0.18530 0.26760 0.27980 0.35410 6.90800 

plot(degree.distribution(hlung.big))
hist(degree(hlung.big))
lines(density(degree.distribution(hlung.big)), col = "red")

graph.density(hlung.big) # Density
# [1] 0.00168221

transitivity(hlung.big) # Transitivity
# [1] 0.0005930319

hub.score(hlung.big)$value
# [1] 50.67404

summary(hub.score(hlung.big)$vector)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000000 0.0000002 0.0000016 0.0010470 0.0000155 1.0000000 

plung.mim <- build.mim(t(hplung[,1:39]), estimator="spearman")
plung.net <- aracne(plung.mim)
plung.ig <- graph.adjacency(plung.net, "undirected", weighted = TRUE, diag = FALSE)
plung.ig
IGRAPH UNW- 2935 400554 -- 
  + attr: name (v/c), weight (e/n)

summary(E(plung.ig)$weight)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.02820 0.08426 0.12470 0.74270 0.18190 6.90800 

is.connected(plung.ig)
# [1] TRUE

summary(degree.distribution(plung.ig))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.01667 0.06667 0.07143 0.12080 0.17500 

plot(degree.distribution(plung.ig))
hist(degree(plung.ig))
lines(density(degree.distribution(plung.ig)), col = "red")

graph.density(plung.ig) # Density
# [1] 0.04593838

transitivity(plung.ig) # Transitivity
# [1] 0.2230603

hub.score(plung.ig)$value
# [1] 764.0799

summary(hub.score(plung.ig)$vector)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000000 0.0000000 0.0000002 0.0423900 0.0000018 1.0000000 


plung.hubs90 <- 
  V(plung.ig)[hub.score(plung.ig)$vector > quantile(hub.score(plung.ig)$vector, .9)]
# Vertex sequence:
# [1] "NM_000263_at"    "NM_001040114_at" "NM_001517_at"    "NM_003942_at"    "NM_004994_at"   
# [6] "XM_931972_at"    "XM_931982_at"    "XM_931985_at"    "XM_934714_at"    "XM_935815_at"   
# [11] "XM_943709_at"    "XR_000538_at"   

degree(plung.ig, plung.hubs90)
# NM_000263_at NM_001040114_at    NM_001517_at    NM_003942_at    NM_004994_at    XM_931972_at 
# 10              10               8               8               8               8 
# XM_931982_at    XM_931985_at    XM_934714_at    XM_935815_at    XM_943709_at    XR_000538_at 
# 8               8               6               8               8               5

# plot top decile hub score nodes
plot.igraph(plung.ig,
            layout = layout.fruchterman.reingold,
            vertex.size = 10,
            mark.groups = plung.hubs90, # draws polygon around nodes
            mark.col = "green"
)
# plot top decile hubs & neighbors
V(plung.ig)$color<-"goldenrod"
V(plung.ig)$size<-10
V(plung.ig)$label.color<-"black"
V(plung.ig)[plung.hubs90]$color<-"darkgreen"
V(plung.ig)[plung.hubs90]$size<-14
V(plung.ig)[plung.hubs90]$label.color<-"white"
plot(plung.ig,  layout = layout.fruchterman.reingold)

# run BUS

plung.nsp <- gene.pvalue(EXP = t(hplung[,1:39]), measure="MI", n.replica=400, net.trim="aracne")
plung.bun <- pred.network(plung.nsp[[2]], plung.net, thresh=0.05)
plung.big <- graph.adjacency(plung.bun, "undirected", weighted = TRUE, diag = FALSE)
plung.big <- delete.vertices(plung.big, V(plung.big)[degree(plung.big) < 1])

plung.big

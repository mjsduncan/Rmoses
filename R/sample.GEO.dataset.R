# gse860 pmbc expression hours and 4 months after trauma, 17 with ptsd, 16 controls
library(GEOquery)
gse <- getGEO("GSE860", GSEMatrix=FALSE)
probesets <- as.character(Table(GPLList(gse)[[1]])$ID)
samplecode <- unlist(lapply(GSMList(gse), function(gsm) {slot(gsm, "header")$title}))
# requires as.data.frame.list.R
sample.matrix <- as.data.frame.list(strsplit(samplecode, "-"))
names(sample.matrix) <- c("out", "ID","symptom.evol")


abs.matrix <- do.call('cbind',lapply(GSMList(gse),function(x)
 														{tab <- Table(x)
																mymatch <- match(probesets,tab$ID_REF)
																return(tab$ABS_CALL[mymatch])
															}))
probesets <- probesets[rowSums(is.na(abs.matrix))==0]
abs.matrix <- abs.matrix[rowSums(is.na(abs.matrix))==0, ]
abs.matrix <- apply(abs.matrix,c(1, 2), function(x) {if(x>1) x <- 1 else x <- 0})
rownames(abs.matrix) <- probesets
colnames(abs.matrix) <- names(GSMList(gse))
moses.matrix <- t(abs.matrix)
moses.matrix <- cbind(sample.matrix, moses.matrix)

# subset by time of collection
moses.er <- moses.matrix[moses.matrix$ID=="ER",c(2:12603,1)]
moses.er$ID <- substr(as.character(moses.er$out), 1, nchar(as.character(moses.er$out))-1)
moses.er$out <- as.character(moses.er$out)
moses.er$out <-	substr(moses.er$out, nchar(moses.er$out),nchar(moses.er$out))
moses.er$out <- ifelse(moses.er$out=="C",0,1)
colsums.er <- unlist(lapply(moses.er[,3:12603], sum))
simple.er <- moses.er[,3:12603]
simple.er <- simple.er[,colsums.er[]!=0 & colsums.er[]!=15]
write.table(simple.er, file="er.txt", quote=FALSE, sep="\t", row.names=FALSE)

moses.4m <- moses.matrix[moses.matrix$ID=="M4",c(2:12603,1)]
moses.4m$ID <- substr(as.character(moses.4m$out), 1, nchar(as.character(moses.4m$out))-1)
moses.4m$out <- as.character(moses.4m$out)
moses.4m$out <-	substr(moses.4m$out, nchar(moses.4m$out),nchar(moses.4m$out))
moses.4m$out <- ifelse(moses.4m$out=="C",0,1)
simple.4m <- moses.4m[,3:12603]
simple.4m <- simple.4m[,colsums.4m[]!=0 & colsums.4m[]!=18]
write.table(simple.4m, file="4m.txt", quote=FALSE, sep="\t", row.names=FALSE)


# make expression set
# pdata <- data.frame(samples=names(GSMList(gse)))
# rownames(pdata) <- names(GSMList(gse))
# pheno <- as(pdata,"AnnotatedDataFrame")
# eset <- new('ExpressionSet', exprs=abs.matrix, phenoData=pheno)
probesets <- as.character(Table(GPLList(gse)[[1]])$ID)
value.matrix <- do.call('cbind',lapply(GSMList(gse),function(x)
 														{tab <- Table(x)
																mymatch <- match(probesets,tab$ID_REF)
																return(tab$VALUE[mymatch])
															}))
value.matrix <- apply(value.matrix,2,function(x) {as.numeric(as.character(x))})
# value.matrix <- log2(value.matrix)

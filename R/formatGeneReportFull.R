formatGeneReportFull <- function(result){
    ids <- strsplit(attr(result,"ids") , split=",")[[1]]
	annot <- attr(result,"annot") 
	type <- attr(result,"type") 
	idRows <- match(ids, result[,1])
	theFeatures <- c("Gene Name", "Species", "ENSEMBL_GENE_ID",
        "ENTREZ_GENE_ID", "GENE_SYMBOL")
	nFeatures <- length(theFeatures)
	nIds <- length(idRows)
	temp2 <- matrix(rep("", nIds*nFeatures), nrow=nFeatures,
		ncol=nIds, dimnames=list(theFeatures, ids))
	temp2[,ids] <- t(as.matrix(result[idRows,theFeatures]))
	invisible(sapply(ids, simplify=FALSE, function(id) {
            info <- as.list(temp2[, id])
            info[-c(1:2)] <- lapply(info[-c(1:2)],
                function(f) strsplit(f, split=",")[[1]])
            info
        }))
}

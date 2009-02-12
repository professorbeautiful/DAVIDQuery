formatGeneReportFull <- function(result){
	ids <- strsplit(attr(result,"ids") , split=",")[[1]]
	annot <- attr(result,"annot") 
	type <- attr(result,"type") 
	idRows <- match(ids, result[,1])
	theFeatures <- union(c("Gene Name", "species"),
	 	setdiff((result[,1]), ids))
	nFeatures <- length(theFeatures)
	nIds <- length(idRows)
	temp2 <- matrix(rep("", nIds*nFeatures), nrow=nFeatures,
		ncol=nIds, dimnames=list(theFeatures, ids))
	temp2[1:2,ids] <- as.matrix(result[idRows,3:2])
	infoRows <- cbind(idRows+1, 
				c(idRows[-1]-1, nrow(result)))
	for(iId in 1:nIds) {
		infoRowStart <- idRows[iId]+1 
		infoRowEnd <- ifelse(iId==nIds, 
				nrow(result), idRows[iId+1]-1) 
		infoRows <- infoRowStart:infoRowEnd
		for(iRow in infoRows)
		temp2[result[iRow,1], iId] <- result[iRow,2]
	}
	invisible(sapply(ids, simplify=FALSE, function(id)
		sapply(theFeatures, simplify=FALSE, 
		function(f) 
			strsplit(temp2[f, id], split=", ")[[1]]
	)))
}

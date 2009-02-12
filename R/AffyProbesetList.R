AffyProbesetList = function(chipname=NULL, 
	menu=TRUE, verbose=FALSE)
{
	DAVIDAffyHome <- paste(DAVIDURLBase, "ease/update/affyPopulations/", sep="/")
	myCurlHandle <- RCurl::getCurlHandle(cookiefile=" DAVIDAffyCookiefile.txt")
	firstStageAffyResult <- RCurl::getURL(DAVIDAffyHome, curl=myCurlHandle, verbose=FALSE)
	writeChar(firstStageAffyResult, "firstStageAffyResult.html")

	splitHTML <- strsplit(firstStageAffyResult, "\r\n")[[1]]
	splitHTML <- grep("href=\"", splitHTML, value=TRUE)
	### The visible chip names, plus ".txt"
	arrayNames <- 	unlist(bracketedStrings(splitHTML, "\"><tt>", "</tt>", verbose=F))
	txtIndices <- unlist(gregexpr(".txt", arrayNames)) - 1
	arrayNamesPretty <- arrayNames
	### Remove ".txt" from the end of the arrayNames.
	arrayNamesPretty[txtIndices>0] <- 	substring(			arrayNamesPretty[txtIndices>0], 1, 
				unlist(gregexpr(".txt", arrayNamesPretty[txtIndices>0])) - 1)
		arrayIndices <- unlist(gregexpr(" Array", arrayNamesPretty)) - 1
	arrayNamesPretty[arrayIndices>0] <- 	substring(arrayNamesPretty[arrayIndices>0], 1, 
					unlist(gregexpr(" Array", arrayNamesPretty[arrayIndices>0])) - 1)
	### Remove " Array" from the end of some arrayNames.

	arrayLinks <- 	unlist(bracketedStrings(splitHTML, "href=\"", "\"><tt>", verbose=F))

	if(menu == TRUE){
		if(!is.null(chipname)) {
			chipnameSubset <- which(regexpr(chipname, arrayNamesPretty) > 0)
			arrayNamesPretty <- arrayNamesPretty[chipnameSubset]
			arrayNames <- arrayNames[chipnameSubset]
			arrayLinks <- arrayLinks[chipnameSubset]
		}
		thePick <- menu(graphics=TRUE, title="Choose an array name", arrayNamesPretty)
		#print(thePick)
		if(length(thePick) == 0) stop("No choice made.")
		if(thePick == 0) stop("No choice made.")
		chipname <- arrayNames[thePick]
		chipnamePretty <- arrayNamesPretty[thePick]
	}
	if(chipname == "u133p2")
		chipname <-"Human Genome U133 Plus 2.0"
	arrayLink <- arrayLinks[match(chipname, arrayNames)]
	if(is.na(arrayLink))
			arrayLink <- arrayLinks[match(chipname, arrayNamesPretty)]
	if(verbose) cat("chipname=", chipname, "  arrayLink=", arrayLink)
	theURL <- paste(DAVIDURLBase, arrayLink, sep="")
 	if(verbose) cat("\n  theURL=", theURL, "\n")
	result <- strsplit(RCurl::getURL(theURL, curl=myCurlHandle, verbose=FALSE), split="\n")[[1]]
	return(result)
}

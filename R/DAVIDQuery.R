`DAVIDQuery` <-
function(ids="O00161,O75396", 
	type="UNIPROT_ACCESSION", 
	annot, 
	tool,
	URLlengthLimit=2048,
	details=TRUE,
	verbose=FALSE,
	writeHTML=FALSE,
	testMe=FALSE,
	graphicMenu=FALSE,
	formatIt=TRUE
)
{
####  Changelog: 2010-05-09  RD
###	 	-Coping with a change in DAVIDQuery API returnvalue format that broke this function.  The formation of the third URL was modified.
    if (testMe) {
        type <- "UNIPROT_ACCESSION"
        annot <- NULL
        tool <- "geneReportFull"
	} else {
        if (type == "menu") {
			type <-DAVIDTypeChoices[menu(DAVIDTypeChoices,
				graphics=graphicMenu, title="Choose an ID TYPE")]
        }
		if(is.numeric(type)) type <- DAVIDTypeChoices[type]

		if(missing(tool)) tool <- "menu"
        if (tool == "menu") {
			tool <- DAVIDToolChoices[menu(paste(names(DAVIDToolChoices), " (", DAVIDToolChoices, ")", sep=""),
				graphics=graphicMenu, title="Choose a TOOL")]
        }
		if(is.numeric(tool)) tool <- DAVIDToolChoices[tool]

        if (tool == "annotationReport") {
			if(missing(annot)) annot <- "menu"
            if (!is.null(annot)) {
                if (identical(annot, "menu")) {
					annot = select.list(DAVIDAnnotChoices,
						multiple=TRUE,
                    title = "Choose Annotations (0 = \"NULL\")")
                  if (length(annot) == 0) 
                    annot <- NULL
                }
            }
            if (is.numeric(annot)) 
                annot <- DAVIDAnnotChoices[annot]
            if (length(annot) > 1) 
                annot = paste(annot, collapse = ",")
        }
        if (missing(annot)) 
            annot <- NULL
    }
    ids <- paste(ids, collapse = ",")
	ids <- paste(strsplit(ids, " ")[[1]], sep="", collapse="")  ### remove blanks
    firstURLOK <- FALSE
    while (firstURLOK == FALSE) {
		firstURL <- paste(DAVIDURLBase, "/api.jsp?",
			"type=", type,
			"&ids=", ids,
			"&tool=", tool,
			sep="")
		if(!is.null(annot)) firstURL <- paste(firstURL, "&annot=", annot, sep="")
		if(verbose) cat("DAVIDQuery:  firstURL = ", firstURL, "\n")
        if (nchar(firstURL) < URLlengthLimit) 
            firstURLOK <- TRUE
		else
			ids <- ids[-length(ids)]
    }
    DAVIDQueryResult <- try({
        myCurlHandle <- RCurl::getCurlHandle(cookiefile = "DAVIDCookiefile.txt")
        firstStageResult <- RCurl::getURL(firstURL, curl = myCurlHandle, 
            verbose = verbose)
        if (writeHTML) 
            writeChar(firstStageResult, "firstStageResult.html")
        DAVIDaction <- bracketedStrings(firstStageResult, "document.apiForm.action = \"", 
            "\"")
        DAVIDvalues <- bracketedStrings(firstStageResult, "document.apiForm.[a-z]*.value=\"", 
            "\"", warn.if.gt.1 = FALSE)
        DAVIDfields <- bracketedStrings(firstStageResult, "document.apiForm.", 
            ".value=\"", warn.if.gt.1 = FALSE)
        secondURL <- paste(DAVIDURLBase, "/", DAVIDaction, "?", 
            paste(DAVIDfields, "=", DAVIDvalues, sep = "", collapse = "&"), 
            sep = "")
        if (verbose) 
            cat("DAVIDQuery:  secondURL = ", secondURL, "\n")
        if (nchar(secondURL) > URLlengthLimit) 
            stop(paste("nchar(secondURL) too long; ", nchar(secondURL), 
                ">", URLlengthLimit))
        secondStageResult <- RCurl::getURL(secondURL, curl = myCurlHandle, 
            verbose = verbose)
        hasSessionEnded <- length(grep("Your session has ended", 
            secondStageResult) > 0)
        if (hasSessionEnded) 
            warning("Warning: Session ended")
        if (writeHTML) 
            writeChar(secondStageResult, "secondStageResult.html")
        downloadFileName <- bracketedStrings(secondStageResult, 
            "<a href=\"", "\" ", warn.if.gt.1=FALSE)[1]  ###  Fixed  Saturday, May 8, 2010   RD
        if (length(downloadFileName) == 0) 
            warning("Warning: downloadFileName is not found in reply html. \n")
		downloadURL <- paste(DAVIDURLBase, 
								downloadFileName, sep="") 
		if(verbose) cat("downloadURL = ", downloadURL, "\n")
        read.delim(downloadURL, header = FALSE, stringsAsFactors = FALSE)
    })
	try(
		if(is.data.frame(DAVIDQueryResult) & (length(DAVIDQueryResult) > 0)){
			#print(DAVIDQueryResult[[1]])
			#print(grep("<title>Directory Listing For /UserDownload/</title>", DAVIDQueryResult[[1]]))
			if(length(grep("<title>Directory Listing For ", DAVIDQueryResult[[1]])) > 0) {
	     		DAVIDQueryResult <- paste("No result file was found. URL = ", DAVIDQueryResult$firstURL)
            class(DAVIDQueryResult) <- "try-error"
        }
    	}
    )
    attr(DAVIDQueryResult, "ids") <- ids
    attr(DAVIDQueryResult, "tool") <- tool
    attr(DAVIDQueryResult, "annot") <- annot
    attr(DAVIDQueryResult, "type") <- type
    if (formatIt & (class(DAVIDQueryResult) != "try-error")) {
		## attempt to make the result conform to a data structure.
        DAVIDQueryResult <- formatDAVIDResult(DAVIDQueryResult)
    }
    if (details) 
		return(list(
			ids=ids,   ### might be shorter than the original arg value.
			myCurlHandle=myCurlHandle,
			firstURL=firstURL,
			firstStageResult=firstStageResult,
			DAVIDaction=DAVIDaction,
			secondURL=secondURL,
			secondStageResult=secondStageResult,
			hasSessionEnded=hasSessionEnded,
			downloadFileName=downloadFileName,
			downloadURL=downloadURL,
			DAVIDQueryResult=DAVIDQueryResult
		))
    return(DAVIDQueryResult)
}


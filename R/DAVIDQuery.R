
DAVIDQuery<-function (ids = "O00161,O75396", type = "UNIPROT_ACCESSION", 

                      annot, tool="geneReportFull", URLlengthLimit = 2048, details = TRUE, verbose = FALSE, 
                      writeHTML = FALSE, testMe = FALSE, graphicMenu = FALSE, formatIt = TRUE) 
{
  if(length(sys.call()) == 1)  testMe <- TRUE
  
  options(RCurlOptions = list(verbose = verbose,
                              capath = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  
  #generate ID choices online from DAVID web site
  idChoices <- getIdConversionChoices(verbose=verbose);
  
  if (testMe) {
    if(tool == "gene2gene") {
      ids <- "33246_AT,32469_AT,1786_AT,32680_AT,1355_G_AT,37968_AT,33530_AT,31987_AT,35956_S_AT,35956_S_AT,1112_G_AT,33077_AT,1331_S_AT,40350_AT,37968_AT,38926_AT,37953_S_AT,34436_AT,37097_AT,32439_AT,35121_AT,40317_AT,39469_S_AT,32439_AT,33685_AT,40294_AT,1575_AT,39187_AT,34720_AT,41489_AT,35439_AT,39698_AT,40790_AT,33922_AT,39908_AT,41113_AT,34606_S_AT,37711_AT,38945_AT,32073_AT"
      type <- "AFFYMETRIX_3PRIME_IVT_ID"
    }
    else {  
      type <- "UNIPROT_ACCESSION"
      annot <- NULL
      tool <- "geneReportFull"
      verbose = TRUE
      writeHTML = TRUE
    }
  }
  else {
    if (type == "menu") {
      item <- menu(idChoices$from[,"name"], 
                   graphics = graphicMenu, 
                   title = "Choose an ID TYPE");
      type <- idChoices$from[item,"value"];
      if (length(type) == 0) 
        type <- NULL
    }
    
    if (is.numeric(type)) 
      type <- DAVIDTypeChoices[type]
    if (missing(tool)) 
      tool <- "menu"
    if (tool == "menu") {
      tool <- DAVIDToolChoices[menu(paste(names(DAVIDToolChoices), 
                                          " (", DAVIDToolChoices, ")", sep = ""), 
                                    graphics = graphicMenu, 
                                    title = "Choose a TOOL")]
    }
    if (is.numeric(tool)) 
      tool <- DAVIDToolChoices[tool]
    
    if (tool == "annotationReport") {
      if (missing(annot) || is.null(annot)) 
        annot <- "menu"
      if (!is.null(annot)) {
        if (identical(annot, "menu")) {
          annotChoices <- getAnnotationChoices(verbose=verbose);
          annot <- select.list(annotChoices, multiple = TRUE,
                               title <- "Choose Annotations (0 = \"NULL\")")
          if (length(annot) == 0) 
            annot <- NULL
        }
      }
      if (is.numeric(annot)) 
        annot <- DAVIDAnnotChoices[annot]
      if (length(annot) > 1) 
        annot <- paste(annot, collapse = ",")
    }
    
    if (tool == "geneIdConversion") {
      if (missing(annot) || is.null(annot)) 
        annot <- "menu";
      if (annot=="menu") {
        item <- max(1, 
                    menu(idChoices$to[,"name"], graphics = graphicMenu, 
                         title = "Choose Annotation (Cancel = \"DAVID\")"));
        annot <- idChoices$to[item,"value"];
      }		
      res <- convertIDList(idList=ids,
                           fromType=type,
                           toType=annot, 
                           annotChoices=idChoices, 
                           writeHTML=writeHTML,details=details,verbose=verbose);
      return(res);
    }
    
    if (missing(annot)) 
      annot <- NULL
  }
  ids <- paste(ids, collapse = ",")
  ids <- paste(strsplit(ids, " ")[[1]], sep = "", collapse = "")
  firstURLOK <- FALSE
  while (firstURLOK == FALSE) {
    firstURL <- paste(DAVIDURLBase, "api.jsp?", "type=", 
                      type, "&ids=", ids, "&tool=", tool, sep = "")
    if (!is.null(annot)) 
      firstURL <- paste(firstURL, "&annot=", annot, sep = "")
    if (verbose) 
      cat("DAVIDQuery:  firstURL = ", firstURL, "\n")
    if (nchar(firstURL) < URLlengthLimit) 
      firstURLOK <- TRUE
    else ids <- ids[-length(ids)]
  }
  firstURL = paste0("https://", firstURL)
  DAVIDQueryResult <- try({
    myCurlHandle <- RCurl::getCurlHandle(cookiefile = "DAVIDCookiefile.txt")
    firstStageResult <- RCurl::getURL(firstURL, curl = myCurlHandle, 
                                      ssl.verifypeer = FALSE,
                                      verbose = FALSE)
    if (writeHTML) 
      writeChar(firstStageResult, "firstStageResult.html")
    DAVIDaction <- bracketedStrings(firstStageResult, "document.apiForm.action = \"", 
                                    "\"")
    DAVIDvalues <- bracketedStrings(firstStageResult, "document.apiForm.[a-z]*.value=\"", 
                                    "\"", warn.if.gt.1 = FALSE)
    DAVIDfields <- bracketedStrings(firstStageResult, "document.apiForm.", 
                                    ".value=\"", warn.if.gt.1 = FALSE)
    secondURL <- paste(DAVIDURLBase, DAVIDaction, "?", 
                       paste(DAVIDfields, "=", DAVIDvalues, sep = "", collapse = "&"), 
                       sep = "")
    secondURL = paste0("https://", secondURL)
    
    if (verbose) 
      cat("DAVIDQuery:  secondURL = ", secondURL, "\n")
    if (nchar(secondURL) > URLlengthLimit) 
      stop(paste("nchar(secondURL) too long; ", nchar(secondURL), 
                 ">", URLlengthLimit))
    secondStageResult <- RCurl::getURL(secondURL, curl = myCurlHandle, 
                                       verbose = TRUE)
    hasSessionEnded <- length(grep("Your session has ended", 
                                   secondStageResult) > 0)
    if (hasSessionEnded) 
      warning("Warning: Session ended")
    if (writeHTML) 
      writeChar(secondStageResult, "secondStageResult.html")
    downloadFileName <- bracketedStrings(secondStageResult, 
                                         "href=\"data/download/", "\" target=")
    if (length(downloadFileName) == 0) 
      warning("Warning: downloadFileName is not found in reply html. \n")
    downloadURL <- paste0("https://", DAVIDURLBase, "data/download/", 
                         downloadFileName)
    if (verbose) 
      cat("downloadURL = ", downloadURL, "\n")
    thirdStageResult = RCurl::getURL(downloadURL,curl = myCurlHandle, 
                                     verbose = TRUE)
    writeChar(thirdStageResult, "thirdStageResult.html")
    if (tool=="geneReport"){
      # work around the format in which the file for 'geneReport' is returned by DAVID 
      read.delim("thirdStageResult.html",stringsAsFactors=FALSE,header=TRUE,nrows=0);
    } else {
      downloadedAnswer = RCurl::getURL(downloadURL, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE);
      write(x = downloadedAnswer, file = "downloadedAnswer.txt")
      read.delim("downloadedAnswer.txt", header = TRUE, check.names = FALSE, stringsAsFactors = FALSE);
    }
  })
  try(if (is.data.frame(DAVIDQueryResult) 
          & (length(DAVIDQueryResult) > 0)) {
    if (length(grep("<title>Directory Listing For /data/download/</title>", 
                    DAVIDQueryResult[[1]])) > 0) {
      DAVIDQueryResult <- paste("No result file was found. URL = ", 
                                DAVIDQueryResult$firstURL)
      class(DAVIDQueryResult) <- "try-error"
    }
  })
  attr(DAVIDQueryResult, "ids") <- ids
  attr(DAVIDQueryResult, "tool") <- tool
  attr(DAVIDQueryResult, "annot") <- annot
  attr(DAVIDQueryResult, "type") <- type
  if (formatIt & (class(DAVIDQueryResult) != "try-error")) {
    DAVIDQueryResult <- formatDAVIDResult(DAVIDQueryResult)
  }
  if (details) 
    return(list(ids = ids, firstURL = firstURL, firstStageResult = firstStageResult, 
                DAVIDaction = DAVIDaction, secondURL = secondURL, 
                secondStageResult = secondStageResult, hasSessionEnded = hasSessionEnded, 
                downloadFileName = downloadFileName, downloadURL = downloadURL, 
                DAVIDQueryResult = DAVIDQueryResult))
  return(DAVIDQueryResult)
}


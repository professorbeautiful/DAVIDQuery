
###################################################
#### DAVID specific using WebQuery functionality
###################################################

#' Upload an ID list to the DAVID query system
#'
#' @name DAVID.uploadIDList
#' @param url the url to post a request to
#' @param curl the RCurl handle
#' @param idList the list of IDs to upload
#' @param type the DAVID Identifier to convert from
#' @param sessionID the RCurl session ID to be submitted as part of ID list uploading process.
#' If NULL the session ID is retrieved from curl handle
#' @param verbose if TRUE enables diagnostic messages
#' @return a character string containing the source for a list upload html page
#' @usage 
#' myCurlHandle<-RCurl::getCurlHandle(cookiefile="DAVIDCookies.txt");
#' res1<-RCurl::getURL("http://david.abcc.ncifcrf.gov", curl = myCurlHandle,verbose = TRUE);
#' url="http://david.abcc.ncifcrf.gov/summary.jsp";
#' idList=c("P04264", "P13645", "P35908", "P08729", "P08727", "P25705", "P06576", "Q2KHP4", "Q14764", "P14625");
#' res2<-DAVID.uploadIDList(url,myCurlHandle,idList,type="UNIPROT_ACCESSION",verbose=TRUE);

DAVID.uploadIDList<-function(url,curl,idList,type="UNIPROT_ACCESSION",  verbose=TRUE){

	if (verbose) {
		cat("URL=",url,":\n");
		cat("Uploading the ID list for",type,"\n\n");
	}

	res<-RCurl::postForm(url,
        	.params=list(
		idType=type,
		uploadType="list",
		multiList="false",
		Mode="paste",
		useIndex="null",
		usePopIndex="null",
		demoIndex="null",
		ids=paste(idList,collapse=" "),
		removeIndex="null",
		renameIndex="null",
		renamePopIndex="null",
		newName="null",
		combineIndex="null",
		selectedSpecies="null",
		SESSIONID="",
		uploadHTML="",	
		managerHTML="",	
		sublist="",	
		rowids="",	
		convertedListName="null",
		convertedPopName="null",
		#pasteBox=paste(idList,collapse=" "),
		fileBrowser="",	
		Identifier=type,
		rbUploadType="list"),curl=curl
	);


	return(res);		
}


#' Request a conversion for a previously uploaded ID List
#' 
#' @name DAVID.requestConversion
#' @param url the url to post a request to
#' @param curl the RCurl handle
#' @param type the DAVID ID Identifier to convert to
#' @param verbose if TRUE enables diagnostic messages
#' @return a character string containing the source for a conversion results html page
#' including the url to the conversion results text file
#' @usage
#' myCurlHandle<-RCurl::getCurlHandle(cookiefile="DAVIDCookies.txt");
#' res1<-RCurl::getURL("http://david.abcc.ncifcrf.gov", curl = myCurlHandle,verbose = TRUE);
#' url1="http://david.abcc.ncifcrf.gov/summary.jsp";
#' idList=c("P04264", "P13645", "P35908", "P08729", "P08727", "P25705", "P06576", "Q2KHP4", "Q14764", "P14625");
#' res2<-uploadIDList(url1,myCurlHandle,idList,type="UNIPROT_ACCESSION",verbose=TRUE);
#' url2<-"http://david.abcc.ncifcrf.gov/conversion2.jsp";
#' res3<-DAVID.requestConversion(url2,myCurlHandle,type="AFFYMETRIX_3PRIME_IVT_ID",verbose=TRUE);
#' writeChar(res3, "thirdStageResult.html")

DAVID.requestConversion<-function(url,curl,type="AAFFYMETRIX_3PRIME_IVT_ID",verbose=TRUE){
	if (verbose) {
		cat("URL=",url,":\n");
		cat("Request conversion to",type,"\n\n");
	}

	res<-RCurl::getForm(url,
		.params=c(
		status="showResult",
		uploadType="",
		convertTo=type,
		Submit="Submit to Conversion Tool"),curl=curl
	);
	return(res);
}


#' Retrieves the set of unique pairs for the given ID list
#' performing the ID conversion based on the pair of DAVID ID types  
#' 
#' @name DAVID.convertIDChunk
#' @param idList the ID list
#' @param fromType the type of input IDs
#' @param toType the type to convert to
#' @param urlBase the DAVID main page url. Default is 'http://david.abcc.ncifcrf.gov'.
#' @param sessionID the RCurl session ID to be submitted as part of ID list uploading process. Default is NULL.
#' @param writeHTML if TRUE writes the conversion result html page into the 'conversionResult.html' file. Default is FALSE.
#' @param verbose if TRUE enables diagnostic messages
#' @return the data frame containing 2 columns: 'From' and 'To'
#' @usage
#' idList=c("P04264", "P13645");
#' data<-DAVID.convertIDChunk(idList,fromType="UNIPROT_ACCESSION",toType="AFFYMETRIX_3PRIME_IVT_ID",writeHTML=TRUE,verbose=TRUE);

DAVID.convertIDChunk<-function(idList,curl,fromType="UNIPROT_ACCESSION", toType="AFFYMETRIX_3PRIME_IVT_ID",
						urlBase="http://david.abcc.ncifcrf.gov", writeHTML=FALSE, verbose=FALSE){

	#upload ID list
	url1=paste(urlBase,"tools.jsp",sep="/");
	res2<-DAVID.uploadIDList(url1,curl,idList,type=fromType, verbose=verbose);

	#turn off the internal DAVID check 'at least 80% samples should be mapped'
	submitAnyway<-RCurl::getURL(paste(urlBase,"submitAnyway.jsp",sep="/"),curl=curl);

	#submit conversion request
	url2<-paste(urlBase,"conversion2.jsp",sep="/");
	res3<-DAVID.requestConversion(url2,curl,type=toType,verbose=verbose);
	if (writeHTML)
		writeChar(res3, "conversionResult.html");


	#retrieve download file name
	downloadFileName<-WebQuery.findFirstInBrackets(res3,"href=\"",".txt",includeRight=TRUE);

	if (nchar(downloadFileName)<1)
		return(NULL);

	downloadURL<-paste(urlBase,downloadFileName,sep="/");
	
	#retrieve unique pairs
	if (verbose){
		cat("Retrieving conversion file:",downloadURL,"\n\n");
	}

	data<-read.delim(downloadURL, header = TRUE, stringsAsFactors = FALSE)

	return(data[,c("From","To")]);
}

#' Retrieves the set of unique pairs for the given ID list
#' performing the ID conversion based on the pair of DAVID ID types  
#' 
#' the function retrieves data in chunks using DAVID.convertIDChunk to comply with the
#' DAVID ID list size limitation for online queries
#' If the ID list length exceeds the chunk size 
#' the function retrieves data in a few chunks reusing the RCurl handle initialized at the beginning of the session
#' 
#' @name DAVID.convertIDSession
#' @param idList the ID list
#' @param fromType the type of input IDs
#' @param toType the type to convert to
#' @param urlBase the DAVID main page url. Default is 'http://david.abcc.ncifcrf.gov'.
#' @param chunk maximum size of the portion of the input ID list the DAVID.convertIDChunk will use
#' @param writeHTML if TRUE writes the conversion result html page into the 'conversionResult.html' file. Default is FALSE.
#' @param verbose if TRUE enables diagnostic messages
#' @return the data frame containing 2 columns: 'From' and 'To'
#' @usage
#' idList=c("P04264", "P13645");
#' data<-DAVID.convertIDSession(idList,fromType="UNIPROT_ACCESSION",toType="AFFYMETRIX_3PRIME_IVT_ID",writeHTML=TRUE,verbose=TRUE);

DAVID.convertIDSession<-function(idList,curl=NULL,fromType="UNIPROT_ACCESSION", toType="AFFYMETRIX_3PRIME_IVT_ID",
					urlBase="http://david.abcc.ncifcrf.gov", chunk=15000, writeHTML=FALSE, verbose=FALSE){

	if (verbose)
		cat("Starting query session...\n\n");


	curl<-WebQuery.registerCurlSession(urlBase,"DAVIDCookies.txt",writeHTML,verbose);

	res<-WebQuery.loop(idList,curl,chunk,DAVID.convertIDChunk,fromType,toType,urlBase,writeHTML,verbose);
}

#' Retrieves the set of unique pairs for the given ID list
#' performing the ID conversion based on the pair of DAVID ID types  
#' 
#' the function retrieves data in sessions using DAVID.convertIDSession to comply with the
#' DAVID ID list size limitation for online queries
#' If the ID list length exceeds the total session size (chunksPerSession x chunk) 
#' the RCurl handle gets reinitialized which causes the new session to start
#' 
#' @name DAVID.convertID
#' @param idList the ID list
#' @param fromType the type of input IDs
#' @param toType the type to convert to
#' @param urlBase the DAVID main page url. Default is 'http://david.abcc.ncifcrf.gov'.
#' @param writeHTML if TRUE writes the conversion result html page into the 'conversionResult.html' file. Default is FALSE.
#' @param chunk maximum size of the protion of the input ID list the DAVID.convertIDChunk will use
#' @param chunksPerSession number of chunks to be retrieved using the same session. 
#' @param verbose if TRUE enables diagnostic messages
#' @return the data frame consisting of two columns ('From' and 'To'), the first containing UNIPROT IDs
#' and the second containing the Affymetrix IDs for 3-prime arrays (like 'HG133 Plus 2' etc).
#' The IDs for which conversion is not found (ambiguous IDs in DAVID terms) are not returned. 
#' The missing IDs can be found comparing the input ID set and the unique ID set in the 'From' column.
#' If no conversion at all is found the function returns NULL.
#' \item{column 1}{contains the IDs (possibly repeating) from the idList corresponding to the 'fromType' DAVID Identifier 
#' ('UNIPROT_ACCESSION' DAVID Identifier by default)}
#' \item{column 2}{contains the IDs obtained during conversion to the 'toType' DAVID Identifier
#' ('AFFYMETRIX_3PRIME_IVT_ID' DAVID Identifier by default)} 
#' @usage
#' idList=c("P04264", "P13645");
#' data<-DAVID.convertID(idList,fromType="UNIPROT_ACCESSION",toType="AFFYMETRIX_3PRIME_IVT_ID",writeHTML=TRUE,verbose=TRUE);

DAVID.convertID<-function(idList,fromType="UNIPROT_ACCESSION", toType="AFFYMETRIX_3PRIME_IVT_ID",
					urlBase="http://david.abcc.ncifcrf.gov", chunk=100000, chunksPerSession=10,
					writeHTML=FALSE, verbose=FALSE){

	if (verbose)
		cat("Performing ID conversion...\n\n");

	require(RCurl);

	res<-WebQuery.loop(idList,NULL,chunksPerSession*chunk,DAVID.convertIDSession,
					fromType,toType,urlBase,chunk,writeHTML,verbose);

	return(res);
}


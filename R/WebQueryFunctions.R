######################################################################
#
# requires R >= 10.1, RCurl >= 1.4, libidn >= 1.14, libcurl >= 7.19
#
# to automatically retrieve the .Rd documentation using Roxygen
# only for this particular file use Roxygen::make.Rd.roclet()        
#
######################################################################

#################################
#### Generic WebQuery functions
#################################

#' Register a session on the urlBase web site for subsequent queries using RCurl functionality
#'
#' @name WebQuery.registerCurlSession
#' @param urlBase URL address of the web site to register on
#' @param cookiefile the file containing cookies to be used for subsequent queries
#' @param writeHTML if TRUE writes the conversion result html page into the 'baseURL.html' file. Default is FALSE.
#' @param verbose if TRUE enables diagnostic messages
#' @return RCurl handle
#' @usage
#' WebQuery.registerCurlSession("http://david.abcc.ncifcrf.gov",cookiefile="DAVIDCookies.txt",verbose=TRUE);

WebQuery.registerCurlSession<-function(urlBase,cookiefile,writeHTML=FALSE,verbose=FALSE){
	if (verbose) {
		cat("Registering RCurl session...\n\n");
	}

	curl<-RCurl::getCurlHandle(cookiefile=cookiefile);
	res<-RCurl::getURL(urlBase, curl = curl);
	if(writeHTML)
		writeChar(res, "baseURL.html");
	return(curl);	
}



#' Get session ID for a given RCurlhandle
#' 
#' Used for subsequent HTTP posts to DAVID in case they require the SESSIONID value pair
#' (uploading the ID list for example)
#'
#' @name WebQuery.getSessionID
#' @param curl RCurl handle 
#' @param length if not NA the last 'length' symbols returned. Default is NA. 
#' @return character string containing the session ID (JSESSIONID value)
#' @usage 
#' myCurlHandle<-RCurl::getCurlHandle(cookiefile="DAVIDCookies.txt");
#' res<-RCurl::getURL("http://david.abcc.ncifcrf.gov", curl = myCurlHandle,verbose = TRUE);
#' sessionID<-DAVID.getSessionID(myCurlHandle);
#' @note works only with cUrl >= 1.19


WebQuery.getSessionID<-function(handle,length=NA){

	cookies<-RCurl::getCurlInfo(handle,which="cookielist");

	item<-grep("JSESSIONID",cookies);
	cookieItem<-cookies[[item]];
	splits<-strsplit(cookieItem,split="JSESSIONID\t")
	sessionID<-strsplit(splits[[1]][2],split="\t")[[1]];
	if (!is.na(length)){
		len<-nchar(sessionID);
		sessionID<-substr(sessionID,len-length+1,len);
	}
	return(sessionID);
}

#' extract the substring between the left and right brackets
#' where left is the last occurence of left bracket
#' before the first occurence of the right bracket
#'
#' @name WebQuery.findFirstInBrackets
#' @param string character string to extract substring from
#' @param left left bracket (character string)
#' @param right right bracket (character string)
#' @param includeLeft if TRUE include left bracket into the result string. Default is FALSE
#' @param includeRight if TRUE include right bracket into the result string. Default is FALSE
#' @return substring between the left and right brackets. If not founs, returns empty string.
#' @usage
#' # obtain an HTML page represented by a character string conversionPage
#' # extract the link to the data file with extension 'txt':
#' fileName<-DAVID.findFirstInBrackets(conversionPage,"href=\"",".txt");

WebQuery.findFirstInBrackets<-function(string,left,right,includeLeft=FALSE,includeRight=FALSE){
	pos1<-gregexpr(right,string)[[1]][1];
	if (pos1<1)
		return("");
	pos2<-gregexpr(left,string)[[1]];
	if (pos2[1]<1)
		return("");
	start<-pos2[pos2<pos1[1]];
	start<-start[length(start)]+(!includeLeft)*(nchar(left));
	end<-pos1+includeRight*nchar(right)-1;
	res<-substr(string,start,end);
	return(as.character(res));
}


#' Perform queries in chunks to comply with potential  limitations 
#' on the query size for a partivular online query system
#'
#' @name WebQuery.loop
#' @param idList the list of IDs to be used in query
#' @param curl the RCurl handle
#' @param chunk maximum size of the portion of the input ID list to be used in a single queryFun call
#' @param queryFun a functions submitting the query to th particular Web query system
#' @param ... arguments to be passed to queryFun
#' @return data frame containing the guery result
#' @usage
#' #convert the Uniprot accession IDs into Affymetrix Human Genome 3-prime IDs using DAVID Web query system
#' url="http://david.abcc.ncifcrf.gov/summary.jsp";
#' fromType="UNIPROT_ACCESSION";
#' toType="AFFYMETRIX_3PRIME_IVT_ID"
#' uids=c("P04264", "P13645");
#' curl=NULL; 
#' queryFun<-DAVID.convertIDSession;
#' chunk<-100000;
#' chunksPerSession=10;
#' res<-WebQuery.loop(uids,curl,chunksPerSession*chunk,queryFun,fromType,toType,url,chunk,writeHTML=FALSE,verbose=TRUE);

WebQuery.loop<-function(idList,curl,chunk,queryFun,...) {
	
	sz<-length(idList);

	res<-list();
	start<-1;
	while(start<=sz){
		end<-min(sz,start+chunk-1);
		IDs<-idList[start:end];
		chunk_res<-queryFun(IDs,curl,...);
		res<-rbind(res,chunk_res);
		start<-end+1;
	}
	gc();
	return(res);
}



#' Testing version of DAVIDQuery::uniprotToAffy based on postForm/getForm RCurl functionality
#'
#' @name  uniprotToAffy
#' @param uid Uniprot IDs, either a string with the IDs separated by commas, or else a character vector
#' @param ... Args to be passed to DAVIDQuery().
#' @return the data frame consisting of two columns ('From' and 'To'), the first containing UNIPROT IDs
#' and the second containing the Affymetrix IDs for 3-prime arrays (like 'HG133 Plus 2' etc)
#' \item{column 1}{contains UNIPROT IDs ('UNIPROT_ACCESSION' DAVID Identifier)}
#' \item{column2}{contains the Affymetrix IDs for 3-prime arrays ('AFFYMETRIX_3PRIME_IVT_ID' DAVID Identifier) 
#' @usage
#' uid=c("P04264", "P13645");
#' res<-uniprotToAffy(uid,writeHTML=TRUE,verbose=TRUE);

uniprotToAffy<-function(uid="O00161", ...){
	DAVID.convertID(uid,fromType="UNIPROT_ACCESSION",toType="AFFYMETRIX_3PRIME_IVT_ID",...);
}




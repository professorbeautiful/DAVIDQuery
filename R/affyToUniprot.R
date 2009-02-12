affyToUniprot = function(ids="88736_AT", ...){
	DAVIDQuery(ids=ids, annot="UNIPROT_ACCESSION",
		tool="annotationReport",
		type="AFFY_ID", ...)
}
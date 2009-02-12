uniprotToAffy = function(uid="O00161", ...){
	DAVIDQuery(ids=uid, type="UNIPROT_ACCESSION",
		tool="annotationReport",
		annot="AFFY_ID")
}
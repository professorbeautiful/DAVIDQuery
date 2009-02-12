catalogDAVIDResultsByTool = function(annot=NULL, sleepSeconds=10, ...) {
	# Purpose: to investigate and compare the consequences of different tools.
	# Results are saved in a list whose name includes the annotation parameter.
	# catalogDAVIDResultsByTool()
	
	getAResult <- function(tool, annot) {
		cat("tool = ", tool, "annot = ", ifelse(is.null(annot), "NULL", annot), "... ")
		queryReturnValue <- try(
			DAVIDQuery(tool=tool, annot=annot, detail=FALSE, ...) 
           )
#           if(class(queryReturnValue) == "try-error") 
#           {
#           	returnValue <- queryReturnValue
#           }
#           else returnValue <- queryReturnValue$DAVIDQueryResult
           timeElapsed <- difftime(Sys.time(), the.time, units = "secs")
           timeToSleep <- max(0, sleepSeconds - timeElapsed + 1)
           cat("DAVIDQueryLoop: Sleeping for ", timeToSleep, 
                  " seconds ...\n")
           Sys.sleep(timeToSleep)
	       the.time <<- Sys.time()
	       queryReturnValue
	  #      ifelse(class(result)=="error", NULL, result)
#		})
	}
	the.time <- Sys.time()
	sleepSeconds <- 10
	catalogOfDAVIDResultsByTool <- 	sapply(DAVIDToolChoices, simplify=FALSE, 
			function(tool) getAResult(tool=tool, annot=annot))
	if(is.null(annot)) catalogName <- "catalogOfDAVIDResultsByTool.NULL"
	else  catalogName <- paste("catalogOfDAVIDResultsByTool", annot, sep=".")
	invisible(assign(catalogName, catalogOfDAVIDResultsByTool, pos=1)  )
}

formatGene2Gene <- function(result){
	groupRows <- grep("Functional", result[ , 1])
	processDiagram <- function(diagramString){
		lapply(
		strsplit(
			strsplit(diagramString, split="\\$")[[1]]
			, split=";|:"),
			function(svec) {
				preSemi <- strsplit(svec[1], split=",")[[1]]
				preSemi <- preSemi[preSemi != ""]
				list(preSemi=preSemi,
					preColon=svec[2],
					postColon=as.numeric(svec[3]))
			}
		)
	}	
	returnValue <- lapply(1:length(groupRows),
		function(ind){
			theseRows <- groupRows[ind]:
				ifelse(ind == length(groupRows), 
						dim(result)[1],
						groupRows[ind+1]-1)
			#cat("formatGene2Gene: ind=", ind, " theseRows=", theseRows[1], ":", rev(theseRows)[1])
			chunk <- result[theseRows, ]
			list(
				median=as.numeric(substring(
					chunk[1, 2], nchar("Median: ")+1)),
				geo=as.numeric(substring(
					chunk[1, 3], nchar("Geo: ")+1)),
				diagram=processDiagram(chunk[1,4]),
				details=data.frame(
					gene=chunk[-1, 1],
					geneName=chunk[-1, 2],
					url=chunk[-1, 3]
					)
			)	
		}
	)  ### end of "lapply" function.
	invisible(returnValue)
}

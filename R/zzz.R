.onAttach = function(libname, pkgname) {
  desc <- packageDescription("DAVIDQuery")
	DQdate <-  desc$Date
	DQVersion =  desc$Version
	packageStartupMessage("This is DAVIDQuery Version ", DQVersion , " ", DQdate, "\n")
	return(invisible(NULL))
}

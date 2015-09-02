.onAttach = function(libname, pkgname) {
  data(DAVIDURLBase)
  data(DAVIDToolChoices)
  data(DAVIDTypeChoices)
  data(DAVIDAnnotChoices)
  data(DAVIDAffyChipChoices)
  data(idExampleList)
  desc <- packageDescription("DAVIDQuery")
	DQdate <-  desc$Date
	DQVersion =  desc$Version
	packageStartupMessage("This is DAVIDQuery Version ", DQVersion , " ", DQdate, "\n")
	return(invisible(NULL))
}

.onLoad <- function(libname, pkgname) {
	### .First.lib is NOT RECOMMENDED for packages with #namespace, but it works.
}

.onAttach = function(libname, pkgname) {
	desc <- packageDescription(pkgname)
	DQdate <-  desc$Date
	DQVersion =  desc$Version
	packageStartupMessage("This is ", pkgname, " ", desc$Version, " ", desc$Date)
	return(invisible(NULL))
}





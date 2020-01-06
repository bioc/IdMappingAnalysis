.onLoad <- function(libname, pkgname) {
	### .First.lib is NOT RECOMMENDED for packages with #namespace, but it works.
}

.onAttach <- function(libname, pkgname) {
    msg <- sprintf(
        "Package '%s' is deprecated and will be removed from Bioconductor
         version %s", pkgname, "3.12")
    .Deprecated(msg=paste(strwrap(msg, exdent=2), collapse="\n"))
}




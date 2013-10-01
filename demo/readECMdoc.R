# readECMdoc.R -- Displays a PDF file as a demo
#
file <- system.file("vignettes", 
                    "ECM_algorithm_for_two_clusters.pdf", 
                    package="IdMappingAnalysis")
isWindows <- (Sys.info()['sysname'] == 'Windows')
if(isWindows) { # Windows automatically finds executable based on file
type.
system(paste("CMD /C ", file, "\n"))
} else { # Change this to use path to Adobe reader if desired.
system(paste("xpdf ", file, "\n"))
}

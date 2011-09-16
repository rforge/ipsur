# Last modified Feb 15, 2008

# Note: the following function (with contributions from Richard Heiberger) 
# can be included in any Rcmdr plug-in package to cause the package to load
# the Rcmdr if it is not already loaded


.onAttach <- function(libname, pkgname){
       if (!interactive()) return()
       Rcmdr <- options()$Rcmdr
       plugins <- Rcmdr$plugins
       if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
               Rcmdr$plugins <- c(plugins, pkgname)
               options(Rcmdr=Rcmdr)
               closeCommander(ask=FALSE, ask.save=TRUE)
               Commander()
       }
}


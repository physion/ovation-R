.onLoad <- function(libname, pkgname) {
	if(Sys.getenv("OVATION_JAR") != "") { 
		jar.path <- Sys.getenv("OVATION_JAR")
	} else { 
		jar.path <- "ovation-assembly-2.0-jar-with-dependencies.jar"
	}
	
	.jpackage(pkgname, lib.loc=libname,morePaths=c(jar.path))
}

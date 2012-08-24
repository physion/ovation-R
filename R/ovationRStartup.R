.onLoad <- function(libname, pkgname) {
  
  if (Sys.info()["sysname"]=="Linux"){
    Sys.setenv(OBJY_ROOT="/usr/object/linux86_64")
    Sys.setenv(OVATION_ROOT="/usr/ovation")
    libraryPaths <- c("/usr/ovation/lib", "/usr/object/linux86_64/lib")
  } else if (Sys.info()["sysname"]=="Windows"){
    libraryPaths <- c(file.path(Sys.getenv("OVATION_ROOT"),"bin"),
                      file.path(Sys.getenv("OBJY_ROOT"),"lib"),
                      file.path(Sys.getenv("OBJY_ROOT"),"bin")) 
    
  } else if (Sys.info()["sysname"]=="Darwin"){
    Sys.setenv(OBJY_ROOT="/opt/object/mac86_64")
    Sys.setenv(OVATION_ROOT="/opt/ovation")
    libraryPaths <- c("/opt/ovation/lib", "/opt/object/mac86_64/lib")
    
  } else stop("Platform not supported")

  ovationJarPath <-file.path(Sys.getenv("OVATION_ROOT"),"ovation.jar")
  
  if (Sys.info()["sysname"]=="Windows"){
    ooJarPath <-file.path(Sys.getenv("OBJY_ROOT"), "lib", "oojava.jar") 
  } else {
	  ooJarPath <-file.path(Sys.getenv("OBJY_ROOT"),"java", "lib", "oojava.jar") 
  }
  
  .jpackage(pkgname, lib.loc=libname,morePaths=c(ovationJarPath,ooJarPath))
}

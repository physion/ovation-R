# Example data extraction from Inouye phenology data set


#' Collects all Epochs in an Experiment or EpochGroup
#' 
#' Recursively collects all Epochs in an Experiment or EpochGroup in to a
#' Vector. Epoch order is not guaranteed. You may want to sort the resulting
#' vector by Epoch \code{startTime}.
#' 
#' @param container Experiment or EpochGroup
#' @return Vector of Epochs
#' @export
CollectEpochs <- function(container) {
  # Collect Epochs, and then Epochs from all EpochGroups
  epochs <- as.list(container$getEpochs());
  
  for(g in as.list(container$getEpochGroups())) {
    epochs <- c(epochs, CollectEpochs(g));
  }
  
  
  return(epochs);
}

#' Collect Measurement data frames from legacy CSV data
#' 
#' @param epochs list of Epochs
#' @return Data Frame matching legacy CSV columns
#' @export
CollectLegacyMeasurements <- function(epochs) {
  result <- data.frame()
  for(e in epochs) {
    date <- e$getStart()
    
    plot.names <- las.vector(e$getInputSources()$keySet())
    epoch.df <- data.frame()
    
    for(m in as.list(e$getMeasurements())) {
      for(s in as.list(m$getSourceNames())) {
        #observer <- m$getProperty('Observer')$get(m$getDataContext)
        site <- e$getInputSources()$get(s)
        if(site$getProperty('habitat')$values()$size() > 0) {
          habitat <- IteratorNext(site$getProperty('habitat')$values()$iterator())  
        } else {
          habitat = 'unknown'
        }
        
        m.df <- read.csv(m$getLocalDataPath()$get())
        measurement.df <- data.frame(Date = date$toString(),
                                     PLOT = s$toString(),
                                     SPECIES=m$getName(),
                                     YEAR=date$getYear(),
                                     DOY=date$getDayOfYear(),
                                     `# FLOWERS` = df$FLOWER_COUNT,
                                     HABITAT=habitat)
        
        result <- rbind(result, measurement.df)
      }
    }
  }
    
  return(result)
}


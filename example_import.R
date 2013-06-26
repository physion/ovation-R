
# Load Rovation
library(Rovation)

# Connect to the database
context <- NewDataContext("ec2-107-22-113-107.compute-1.amazonaws.com::/var/lib/ovation/data/hofmann.connection", "ross")

# Insert a new project starting 6/20/2012 with given project name and purpose
project <- context$insertProject("project name", "project purpose", datetime(2012, 6, 20, timezone="America/New_York"))

# Next time, you can retrieve all projects with the given name
projects <- context$getProjects("project name")

# Create a Source. The label should identify the type of source (e.g. "genus")
genusSource <- context$insertSource("genus")
# Add a property to the source (you can retrieve the property with genusSource$getOwnerProperty("genus-name"))
genusSource$addProperty("genus-name", "A genus name")

# Add a child source
speciesSource <- genusSource$insertSource("species")
speciesSource$addProperty("species-name", "A species")

# Add a fish source
fish <- speciesSource$insertSource("fish")
fish$addProperty("animal-id", "Unique ID for this fish")

# Create an experiment which started 2/1/2012
experiment <- project$insertExperiment("experiment purpose", datetime(2012,2,1,timezone="America/New_York"))

# Create an EpochGroup starting 2/2/2012 @ 8am, endint 2/2/2012 @ 1600hrs
epochGroup <- experiment$insertEpochGroup(fish, "group label", datetime(2012,2,2,hour=8, timezone="America/New_York"), datetime(2012,2,2,hour=16,timezone="America/New_York"))

# Add a tag
epochGroup$addTag("my tag!")

# Add a child EpochGroup
childEpochGroup <- epochGroup$insertEpochGroup("child label", datetime(2012,2,2,hour=9,timezone="America/New_York"))

# Add an Epoch to childEpochGroup. protocolID should uniquely identify the protocol used for the data in this epoch, with protocolParameters specifying parameters of that protocol. We recommend the reverse domain-name for unique protocolIDs (e.g. edu.utexas.hofmann.some-protocol)
params <- list(paramKey1=1, paramKey2="abc")
epoch <- childEpochGroup$insertEpoch(datetime(2012,2,2,hour=9,minute=1,timezone="America/New_York"),
									datetime(2012,2,2,hour=9,minute=10, timezone="America/New_York"),
									"edu.utexas.hofmann.my-protocol",
									list2map(params)
									)
									
# Add a numeric measurement (Response) to the Epoch. There are a number of ways to insert a measurement, depending on its dimensions and whether you want to store the data in Ovation or just reference it. See the Epoch documentation for more info (run epoch.help() to open the documentation;  you can do this for any Ovation object too). The device is the device that made the measurement
data <- c(1,2,3)
samplingRate <- 10
response <- epoch$insertResponse(experiment$externalDevice("device-name", "manufacturer"),
								list2map(list()), # Device parameters can be empty
								numericData(data), # Pass an ovation.NumericData instance
								"response units",
								c("dimension label (e.g. time)"),
								c(samplingRate),
								c("Hz"),
								NUMERIC_DATA_UTI() # UTI for numeric data
								)

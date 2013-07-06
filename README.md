# Ovation API for the R Project

Ovation is the powerful data management service engineered specifically for scientists that liberates research through organization of multiple data formats and sources, the ability to link raw data with analysis and the freedom to safely share all of this with colleagues and collaborators.

The Ovation R API wraps the Ovation Java API for use with the R statistical computing environment. Through this R API, R users can access the full functionality of the Ovation ecosystem from within R. 


## Requirements

* Java 1.6+
* R 2.15.0+

## Installation

Download the Ovation R library from http://ovation.io/downloads. You can install the library archive directly via the R package manager or at the R command line:

	install.packages(pathToOvationR.tar.gz, repos=NULL)

## Usage

1. Load the Ovation R library:

	library(Rovation)

2. Open a new `DataContext` with the `NewDataContext` function:

	dataContext <- NewDataContext("<email>")


## License

The Ovaiton R API is provided under the terms of the GPLv3 license (see LICENSE.txt)

The Ovation R API is copyright (c) 2013 Physion LLC.
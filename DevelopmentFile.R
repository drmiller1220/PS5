
## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("C:\\Users\\drmiller1220\\Documents\\GitHub\\PS5") #This will need to be changed to match your directory

## This is run once when the package structure is first created

## At this point put the *.R files into the correct directories and edit the DESCRIPTION file

# Now the NAMESPACE

## This can be run many times as the code is updates
current.code <- as.package("fitR")
load_all(current.code)
document(current.code)
check(current.code)
install(pkg="fitR", local = TRUE)
library(fitR)


## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("C:\\Users\\drmiller1220\\Documents\\GitHub\\PS5") #This will need to be changed to match your directory

## This is run once when the package strcuture is first created


## At this point put the *.R files into the correcto directories and edit the DESCRIPTION file

## Let's look through the R directory in this order:

# squaresPack-package.r
# Squares.R
# addSquares.R
# subtractSquares.R
# AllSquares-class.R
# allSquares.R

# Now the NAMESPACE

## This can be run many times as the code is updates
current.code <- as.package("fitR")
load_all(current.code)
document(current.code)
check(current.code)
install(pkg="fitR", local = TRUE)
library(fitR)

## Let's look at a function
getSquares
getMethod(getSquares, "Squares")
getMethod(getSquares, "AllSquares")
getMethod(allSquares)

## Let's try it out
x<-c(1,2)
y<-c(3,4)
allObj <- allSquares(x, y)
allObj
getSquares(allObj)

## Moving between classes
addObj <- addSquares(x,y)
addObj
as(object=addObj, Class="AllSquares")




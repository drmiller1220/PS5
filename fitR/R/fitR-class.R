#' A fitR object 
#' 
#' Object of class \code{fitR} are created by the \code{fitR} function
#'
#' 
#' An object of the class `fitR' has the following slots:
#' \itemize{
#' \item \code{statistics} A matrix with one row for each competing model, and one column
#' for each fit statistic calculated.
#' }
#'
#' @author David R. Miller: \email{drmiller@@wustl.edu}
#' @rdname fitR-class
#' @export
setClass('fitR', # creating an S4 'fitR' object
         slots=c(statistics='matrix')
)

#' @export
setMethod("initialize", "fitR", 
     function(.Object, ...){
           value=callNextMethod()
           return(value)
         }
         ) 

#' @rdname fitR
#' @export 
setGeneric("getfitR",
     function(object="fitR")  {
         standardGeneric("getfitR")
       }
       )

#' @export
setMethod("getfitR", "fitR",
     function(object){ 
          return(object)
        }
        )

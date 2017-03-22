#' fitRstats
#'
#' Calculates the fit statistics for competing models by comparing predictive accuracy
#'
#' @param true A vector of the true values
#' @param predicted A matrix of values predicted by competing models, with one column for
#' each model
#' @param naive A vector of naive forecasts
#' @param statistics A vector of character strings indicating the fit statistics to
#' be calculated
#' @param ... Other arguments to be passed through
#'
#' @return A matrix with one row for each competing model, and one column for each fit 
#' statistic calculated
#' @author David R. Miller
#' 
#' @rdname fitRstats
#' @export
setGeneric(name="fitRstats",
           def=function(true, predicted, statistics, ...)
           {standardGeneric("fitRstats")} # creating a generic method for fitRstats
           )

#' @rdname fitRstats
#' @export
setMethod(f="fitRstats", # creating a method for fitRstats
          definition=function(true, predicted, statistics, naive=NULL, ...){
            # the true and predicted inputs must be numeric values
            # true must be vectors
            # predicted must be a matrix
            # the first two ``if'' statements check if these conditions are met, and
            # stop execution and throw errors if they are not
            if(!is.vector(true)==TRUE | !is.numeric(true)==TRUE){
              stop("`true' values must be a vector of numeric values")
            }
            if(!is.matrix(predicted)==TRUE | !is.numeric(predicted)==TRUE){
              stop("`predicted' values must be a matrix of numeric values")
            }
            if(sum(is.na(true)>0) | sum(is.na(predicted)>0)){
              warning("some inputted values are NAs; some fit statistics may not compute")
            }
            # creating a null object in which to store the fit statistics
            statistics_output <- NULL
            # calculating the absolute error for each observation for each model
            abs_error <- apply(predicted, MAR=2, FUN=function(x) abs(x - true))
            # calculating the arctangent absolute percent error for each observation 
            # for each model; the absolute percent error formula we were given does not
            # account for 0 values, which result in NaNs, so I implement the arctangent
            # absolute percent error instead, in which all zero values take the value pi/2
            # and all non-zero values are computed by taking the arctangent of the absolute
            # error over the true value; 
            # see http://www.sciencedirect.com/science/article/pii/S0169207016000121
            abs_pct_error <- apply(predicted, MAR=2, FUN=function(x) ifelse(x==0, pi/2, atan(abs((x - true)/true))))
            # if the user requested rmse,
            # calculate the rmse for each model, and cbind the fit statistics to the object
            if("rmse" %in% statistics){
              rmse <- apply(abs_error, MAR=2, FUN=function(x) sqrt(sum(x^2)/length(x)))
              statistics_output <- cbind(statistics_output, rmse)
            }
            # if the user requested mad, or did not make any requests,
            # calculate the mad for each model, and cbind the fit statistics to the object
            if("mad" %in% statistics){
              mad <- apply(abs_error, MAR=2, FUN=function(x) median(x))
              statistics_output <- cbind(statistics_output, mad)
            }
            # if the user requested rmsle, or did not make any requests,
            # calculate the rmsle for each model, and cbind the fit statistics to the object
            if("rmsle" %in% statistics){
              rmsle <- apply(predicted, MAR=2, FUN=function(x) sqrt(sum((log(x+1)-log(true+1))^2)/length(true))) 
              statistics_output <- cbind(statistics_output, rmsle)
              # rmsle cannot handle negative values within the log(); if we obtain negative
              # values, the resulting fit statistic will be NaN, so we display a more
              # meaningful warning message to explain to the user why they obtained an NaN
              if("NaN" %in% rmsle){
                warning("rmsle could not be computed for some models due to negative values")
                }
            }
            # if the user requested mape, or did not make any requests,
            # calculate the mape for each model, and cbind the fit statistics to the object
            if("mape" %in% statistics){
              mape <- apply(abs_pct_error, MAR=2, FUN=function(x) sum(x)/length(x))
              statistics_output <- cbind(statistics_output, mape)
            }
            # if the user requested mape, or did not make any requests,
            # calculate the mape for each model, and cbind the fit statistics to the object
            if("meape" %in% statistics){
              meape <- apply(abs_pct_error, MAR=2, FUN=function(x) median(x))
              statistics_output <- cbind(statistics_output, meape)
            }
            # if the user requested mape, or did not make any requests,
            # calculate the mape for each model, and cbind the fit statistics to the object
            if("mrae" %in% statistics){
              # checking to make sure user supplies necessary naive forecasts
              if(length(naive)==0){
                stop("To calculate mrae, you must supply naive forecasts")
              }
              # checking if naive values are of appropriate class
              if(!is.vector(naive)==TRUE | !is.numeric(naive)==TRUE){
                stop("`naive' values must be a vector of numeric values")
              }
              # calculating the baseline as directed
              baseline <- abs(naive-true)
              mrae <- apply(abs_pct_error, MAR=2, FUN=function(x) median(x/baseline))
              statistics_output <- cbind(statistics_output, mrae)
            }
            # converting the output object to a matrix, so as to conform to the fitR class
            statistics_output <- as.matrix(statistics_output)
            # assigning meaningful row names; if the input matrix has column names, we
            # take those column names and make them the row names to identify the model;
            # otherwise, we give them generic names of `Model 1,' `Model 2,' etc.
            rownames(statistics_output) <- ifelse(colnames(predicted)==NULL, 
                              paste0("Model ",1:length(dim(statistics_output)[2])), 
                              colnames(predicted))
            # we return an object of class fitR with the fit statistics matrix
            return(new("fitR", statistics=statistics_output))
          }
          )





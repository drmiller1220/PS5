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
           def=function(true, predicted, naive, statistics, ...)
           {standardGeneric("fitRstats")}
           )

#' @rdname fitRstats
#' @export
setMethod(f="fitRstats",
          definition=function(true, predicted, naive=NULL, statistics="rmse", ...){
            statistics_output <- NULL
            abs_error <- apply(predicted, MAR=2, FUN=function(x) abs(x - true))
            abs_pct_error <- apply(predicted, MAR=2, FUN=function(x) ifelse(x==0, pi/2, atan(abs((x - true)/true))))
            baseline <- abs(naive-true)
            if("rmse" %in% statistics){
              rmse <- apply(abs_error, MAR=2, FUN=function(x) sqrt(sum(x^2)/length(x)))
              statistics_output <- cbind(statistics_output, rmse)
            }
            if("mad" %in% statistics){
              mad <- apply(abs_error, MAR=2, FUN=function(x) median(x))
              statistics_output <- cbind(statistics_output, mad)
            }
            if("rmsle" %in% statistics){
              rmsle <- apply(predicted, MAR=2, FUN=function(x) sqrt(sum((log(x+1)-log(true+1))^2)/length(true))) 
              statistics_output <- cbind(statistics_output, rmsle)
              if("NaN" %in% rmsle){
                warning("rmsle could not be computed for some models due to negative values")
                }
            }
            if("mape" %in% statistics){
              mape <- apply(abs_pct_error, MAR=2, FUN=function(x) sum(x)/length(x))
              statistics_output <- cbind(statistics_output, mape)
            }
            if("meape" %in% statistics){
              meape <- apply(abs_pct_error, MAR=2, FUN=function(x) median(x))
              statistics_output <- cbind(statistics_output, meape)
            }
            if("mrae" %in% statistics){
              mrae <- apply(abs_pct_error, MAR=2, FUN=function(x) median(x/baseline))
              statistics_output <- cbind(statistics_output, mrae)
            }
            statistics_output <- as.matrix(statistics_output)
            rownames(statistics_output) <- ifelse(colnames(predicted)==NULL, paste0("Model ",1:length(dim(statistics_output)[2])), colnames(predicted))
            return(new("fitR", statistics=statistics_output))
          }
          )





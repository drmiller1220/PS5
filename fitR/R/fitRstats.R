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
#'
#' @return A matrix with one row for each competing model, and one column for each fit 
#' statistic calculated
#' @author David R. Miller
#' @examples
#' 
#' myX <- c(20, 3) 
#' myY <- c(-2, 4.1) 
#' subtractSquares(myX, myY)
#' @seealso \code{\link{addSquares}}
#' @rdname fitRstats
#' @aliases subtractSquares,ANY-method
#' @export
setGeneric(name="fitRstats",
           def=function(true, predicted, naive, statistics, ...)
           {standardGeneric("fitRstats")}
           )

#' @export
setMethod(f="fitRstats",
          definition=function(true, predicted, naive=NULL, statistics="rmse", ...){
            statistics_output <- NULL
            abs_error <- apply(predicted, MAR=2, FUN=function(x) abs(x - true))
            abs_pct_error <- apply(predicted, MAR=2, FUN=function(x) abs(x - true)/abs(true) * 100)
            baseline <- abs(naive-true)
            if("rmse" %in% statistics){
              rmse <- apply(abs_error, MAR=2, FUN=function(x) sqrt(sum(x^2)/length(x)))
              cbind(statistics_output, rmse)
            }
            if("mad" %in% statistics){
              mad <- apply(abs_error, MAR=2, FUN=function(x) median(x))
              cbind(statistics_output, mad)
            }
            if("rmsle" %in% statistics){
              rmsle <- apply(predicted, MAR=2, FUN=function(x) sqrt(sum((log(x+1)-log(true+1))^2)/length(true)), true=true) 
              cbind(statistics_output, rmsle)
            }
            if("mape" %in% statistics){
              mape <- apply(abs_pct_error, MAR=2, FUN=function(x) sum(x)/length(x))
              cbind(statistics_output, mape)
            }
            if("meape" %in% statistics){
              meape <- apply(abs_pct_error, MAR=2, FUN=function(x) median(x))
              cbind(statistics_output, meape)
            }
            if("mrae" %in% statistics){
              mrae <- apply(abs_pct_error, MAR=2, FUN=function(x) median(x/baseline), baseline=baseline)
              cbind(statistics_output, mrae)
            }
            colnames(statistics_output) <- ifelse(colnames(predicted)==NULL, paste0("Model ",1:length(dim(statistics_output)[2])), colnames(predicted))
            return(new("fitR", statistics=statistics_output))
          }
          )





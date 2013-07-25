#' The discount-to-rate function
#'
#' Return the discount-to-rate function.
#'
#' @param compounding Choice of compounding method, 
#' i.e. "continuous", "discrete", "annual", or "simple".
#' @param frequency Compounding frequency (discrete compounding).
#' 
#' @details 
#' The discount-to-rate function is defined as follows 
#' \code{discountToRate <- function(times, discountFactors) { ... }}
#' where \code{times} and rates are vectors.  
#' 
#' @return The discount-to-rate function.
#'
# @note 
#'
#' @author Carl Johan Rehn \email{care02@@gmail.com}.
#' 
#' @references
#' \url{http://www.treasurers.org/daycountconventions}.
#'
#' @seealso \code{\link{rateToDiscount}}.
#' 
#' @aliases discount2rate
#' 
#' @export
#' 
#' @examples
#'
#' times <- seq(1, 8, 2)
#' discountFactors <- c(0.99, 0.96, 0.93, 0.90)
#'
#' rates <- discountToRate("continuous")
#' 
#' r <- rates(times, discountFactors)

discountToRate <- discount2rate <- function(compounding = "continuous", frequency = 1, ...) {
  if ( compounding == "continuous" )
    function(times, discountFactors) { 
      -log(discountFactors) / times 
    }
  else if ( compounding == "discrete" )
    function(times, discountFactors) { 
      frequency * ( (1 / discountFactors) ^ ( 1 / (times * frequency) ) - 1 )
    }
  else if ( compounding == "annual" )
    function(times, discountFactors) { 
      (1 / discountFactors) ^ ( 1 / times ) - 1 
    }
  else if ( compounding == "simple" )                
    function(times, discountFactors) { 
      (1 / discountFactors - 1) / times 
    }
  else
    function(times, discountFactors, ...) {
      NULL
    }
}

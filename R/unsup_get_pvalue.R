#' Title
#'
#' @param u
#' @param Y_sorted
#'
#' @return
#' @export
#'
#' @examples
unsup_get_pvalue <- function(u, Y_sorted) {

  # Number of observations
  k <- length(Y_sorted)

  # Possible alpha values
  possible_alpha <- seq(2/(k+1), 2*floor((k+1)/2)/(k+1), by = 2/(k+1))

  # Initialize p_value
  p_value <- NA

  # Determine least upper bound on p-value
  for(i in 1:length(possible_alpha)) {

    if(u < Y_sorted[i] | u > Y_sorted[k + 1 - i]) {
      p_value <- possible_alpha[i]
    }

    if(!is.na(p_value)){
      break
    }
  }

  # If u is contained in all possible [Y_sorted[i], Y_sorted[k+1-i]] sets,
  # define p_value = 1. (Never reject H_0.)
  if(u >= Y_sorted[floor((k+1)/2)] & u <= Y_sorted[ceiling((k+1)/2)] ) {
    p_value <- 1
  }

  return(p_value)
}

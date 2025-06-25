#' @export

stan_traces <- function(object, pars = NULL, ...) {

  if (is.null(pars)) pars <- names(object)
  for (par_i in pars) {
    g <- rstan::stan_trace(object, pars = par_i, ...) +
      ggplot2::labs(title = par_i)
    print(g)
  }

}

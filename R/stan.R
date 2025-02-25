#' @export

stan_traces <- function(stan_res, par_names, inc_warmup = FALSE) {

  for (par_name in par_names) {
    g <- rstan::stan_trace(stan_res, pars = par_name, inc_warmup = inc_warmup) +
      ggplot2::labs(title = par_name)
    print(g)
  }

}

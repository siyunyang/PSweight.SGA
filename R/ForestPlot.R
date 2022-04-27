#' Plot the estimated subgroup causal effects via forest plot
#'
#' @param obj a list of \code{summary.PSweight_sga} objects obtained with \code{\link{summary.PSweight_sga}} function.
#'  This is done to plot multiple PS weighting methods.
#' @param logodds an indicator to specify whether the effects are estimated on the logodds scale. The default is FALSE.
#' @param xlab an optional character string indicating the xlab of the plot.
#' @param legend_label an optional character vector indicating the PS weighting methods corresponding to the \code{summary.PSweight_sga} object.
#'  The default is \code{method} returned from the \code{summary.PSweight_sga} function.
#' @param ... further arguments passed to or from other methods.
#'
#' @return Plot of the indicated type.
#'
#' @export
#'
#'
#' @import ggplot2 ggforestplot
#' @importFrom  stats binomial coef cov formula glm lm model.matrix plogis poisson predict qnorm quantile sd
#' @importFrom  utils capture.output combn
#' @importFrom  graphics hist legend par
#' @importFrom  grDevices rgb
#' @importFrom  rlang .data

ForestPlot <- function(obj, logodds=FALSE, xlab=NULL, legend_label=NULL,...){


  Std.Error  <- subgroup <- Estimate <- Method <-c()
  # if (length(obj)==0) {
  #   x <- x$estimates
  #   Subgroups <-rownames(x)}else{
  #   ary <-simplify2array(lapply(obj, function (obj) obj[c('estimates')]))
  #   x <-as.data.frame(do.call(rbind, ary))
  #   Subgroups <- rownames(ary$estimates)
  # }
  ary <-simplify2array(lapply(obj, function (obj) obj[c('estimates')]))
  x <-as.data.frame(do.call(rbind, ary))
  Subgroups <- rownames(ary$estimates)
  nsub <- length(Subgroups)
  if(is.null(legend_label)){method <-unlist(simplify2array(lapply(obj, function (obj) obj[c('method')]))) }else{
    method <- legend_label
  }
  fdata <- cbind(x, Method= rep(method,each = nsub), subgroup= Subgroups )

  # rmst_full <-df_linear_rmst %>%  dplyr::arrange(subgroup)

  fplot =ggforestplot::forestplot(
    df = fdata,
    name=subgroup,
    estimate = Estimate,
    se=Std.Error,
    logodds = logodds,
    colour = Method,
    title = "Subgroup causal effects and 95% CIs",
    xlab = xlab,
    ylab="Subgroups")+
    ggplot2::theme(legend.key.size = unit(10, 'cm'), #change legend key size
                   legend.key.height = unit(1, 'cm'), #change legend key height
                   legend.key.width = unit(1, 'cm'), #change legend key width
                   legend.title = element_text(size=14), #change legend title font size
                   legend.text = element_text(size=12)) #change legend text font size

  #splot <- fplot+ theme(panel.border = element_rect(linetype = "solid", fill = NA))

  print(fplot)
}



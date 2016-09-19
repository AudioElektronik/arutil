#' Discard outliers from a frame given value variable
#'
#' Some entries in datasets are outliers. This function filters them out.
#' Given frame can be grouped.
#'
#' @param frame \code{data.frame} variant dataset.
#' @param value_var name of the value variable that will be
#'    inspected for outliers
#' @param value_var name of the value variable
#' @param percentage what percentile will be considered outlier. This will be
#'    used for upper and lower percentile.
#'
#' @export
discard_outliers <- function(frame, value_var, percentile = 0.025) {
  # Preparing percentile probs
  probs <- c(percentile, 1 - percentile)

  # There may be a way to get rid of this duplication
  dots <- list(lazyeval::interp(~x > quantile(x, probs[1]),
                                .values = list(x = as.name(value_var))),
               lazyeval::interp(~x < quantile(x, probs[2]),
                                .values = list(x = as.name(value_var))))

  dplyr::filter_(frame, .dots = dots)
}



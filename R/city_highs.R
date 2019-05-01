#' High Temperatures by Month
#'
#' High Temperatures by month (violin plots) for several cities.
#'
#' @param city_name String giving name of the city. Possible values
#' are: Auckland, Mumbai, Beijing, Chicago, and San Diego.
#' @examples
#' city_highs("Auckland")
#' city_highs("Mumbai")
#' @import ggplot2
#' @export
city_highs <- function(city_name) {
  ggplot(subset(weather, city == city_name), aes(x = month_name, y = high_temp)) +
    geom_violin(fill = "burlywood") +
    geom_jitter(width = 0.25, size = 0.3, alpha = 0.5) +
    labs(x = "Month", y = "High Temperature",
         title = paste0("High Temperatures by Month in ", city_name))
}


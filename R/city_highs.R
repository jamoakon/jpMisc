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
#' @importFrom plyr mapvalues
#' @importFrom mosaicData Weather
#' @export
city_highs <- function(city_name) {
  month_name1 <-
    plyr::mapvalues(Weather$month, from = 1:12,
                    to = month.abb)
  month_name2 <- factor(month_name1, levels = month.abb)
  Weather$month_name <- month_name2
  ggplot(subset(Weather, city == city_name), aes(x = month_name, y = high_temp)) +
    geom_violin(fill = "burlywood") +
    geom_jitter(width = 0.25, size = 0.3, alpha = 0.5) +
    labs(x = "Month", y = "High Temperature",
         title = paste0("High Temperatures by Month in ", city_name))
}


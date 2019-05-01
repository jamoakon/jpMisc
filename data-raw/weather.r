w <- mosaicData::Weather
weather <- data.frame(
  city = w$city,
  high_temp = w$high_temp
)
month_name_1 <- plyr::mapvalues(
  w$month,
  from = 1:12,
  to = month.abb
)
weather$month_name <-
  factor(month_name_1, levels = month.abb)

usethis::use_data(weather, internal = TRUE, overwrite = TRUE)


library(highcharter)
library(geojsonio)
library(httr)
#library(purrr)


url <- "https://code.highcharts.com/mapdata/countries/in/custom/in-all-andaman-and-nicobar.geo.json"

tmpfile <- tempfile(fileext = ".json")
download.file(url, tmpfile)

ind <- readLines(tmpfile)

hcmap("countries/in/custom/in-all-andaman-and-nicobar")


mapdata <- get_data_from_map(download_map_data("countries/in/custom/in-all-andaman-and-nicobar"))
glimpse(mapdata)



india_map <- hcmap("countries/in/custom/in-all-andaman-and-nicobar", data = append_table, value = "confirmed",
      joinBy = c("name", "state"), name = "Confirmed Cases",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "black", borderWidth = 0.1) %>%
  hc_mapNavigation(enabled = T) %>%
  hc_colorAxis(minColor = "#FDEDEC", maxColor = "#CB4335")


#hc_colorAxis(stops = color_stops(10, rev(inferno(10))))
#hc_colorAxis(stops = color_stops(10, rev(inferno(n=10,direction = -1,alpha = 1,begin =0.2))))
#hc_colorAxis(stops = color_stops(10, rev(inferno(n=10,begin =0.1))))
#hc_colorAxis(stops = color_stops(10, rev(inferno(n=10,direction = -1,alpha = 1,begin =0.2))))
#hc_colorAxis(stops = color_stops(10, rev(inferno(10))))
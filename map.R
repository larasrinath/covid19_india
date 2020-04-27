library(highcharter)
library(geojsonio)
library(httr)
library(tidyverse)
#library(purrr)


st=format(Sys.time(), "%Y-%m-%d")
file = str_replace_all(paste("C:/Users/laras/Documents/R/Projects/covid19_india/files/daily_files/cases_",st,".csv"), fixed(" "), "")

yday_file <- read_csv(file)


url <- "https://code.highcharts.com/mapdata/countries/in/custom/in-all-andaman-and-nicobar.geo.json"

tmpfile <- tempfile(fileext = ".json")
download.file(url, tmpfile)
ind <- readLines(tmpfile)


mapdata <- get_data_from_map(download_map_data("countries/in/custom/in-all-andaman-and-nicobar"))


india_map <- hcmap("countries/in/custom/in-all-andaman-and-nicobar", data = yday_file, value = "confirmed",
      joinBy = c("name", "state"), name = "Confirmed Cases",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "black", borderWidth = 0.1) %>%
  hc_mapNavigation(enabled = T) %>%
  hc_colorAxis(minColor = "#FDEDEC", maxColor = "#CB4335")



############# DT ###################

library(DT)
library(gt)

dt <- yday_file %>%
  rename(State = "state",
         Confirmed  = "confirmed",
         Recovered = "recovered",
         Dead = "dead",
         "Active Cases" = "active") %>% .[,-1]

dt_view <- datatable(dt, 
                     caption = "Coronavirus Cases by each State",
                     class = 'cell-border stripe',
                     rownames = T,
                     options = list (pageLength = 50,dom = 'tip'))

st_dt=format(Sys.time() , "%Y-%m-%d")
subtitle = paste("Cases as of",st_dt)

gt_tbl <- gt(data = dt)
gt_tbl <-
  gt_tbl %>%
  tab_header(
    title = md("**Coronavirus Cases by each State**"),
    subtitle = subtitle
  )%>%
  tab_source_note(
    source_note = "Source: Ministry of Health and Family Welfare, India"
  ) %>%
  tab_source_note(
    source_note = md("Link: https://www.mohfw.gov.in/")
  )
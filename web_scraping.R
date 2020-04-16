library(rvest)
library(tidyverse)

mohfw <- read_html("https://www.mohfw.gov.in/") # read html file 


#tbls <- html_nodes(mohfw, "table")

tbls_ls <- mohfw %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE)

India_tbl <- tbls_ls[[1]]




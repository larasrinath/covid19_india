library(DT)
library(tidyverse)

dt <- clean_table %>%
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
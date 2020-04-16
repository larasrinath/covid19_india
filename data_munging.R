######## cleaning Table #############

clean_table <- India_tbl %>%
  rename(rowid = "S. No.",                                               
         state = "Name of State / UT" ,                                  
         confirmed = "Total Confirmed cases (Including 76 foreign Nationals)",
         recovered = "Cured/Discharged/Migrated"  ,                         
         dead = "Death" ) %>%
  mutate(rowid = as.numeric(rowid)) %>%
  filter(rowid != is.na(rowid)) %>%
  mutate(confirmed = as.numeric(confirmed),
         recovered = as.numeric(recovered),
         dead = as.numeric(dead)) %>%
  mutate(active = confirmed - (recovered + dead),
         state = str_replace(state, "Islands", "")) %>%
  mutate(state = str_trim(state))

########## saving csv file ###########

st=format(Sys.time(), "%Y-%m-%d")
file = paste("C:/Users/laras/Documents/R/Projects/covid19_india/files/daily_files/cases_",st, ".csv")
write_csv(clean_table,file)


########## Appended Table ############

append_table <- clean_table %>%
         mutate(date = format(Sys.time(), "%Y-%m-%d"))

extfile <- read_csv("C:/Users/laras/Documents/R/Projects/covid19_india/files/daily_append.csv")

new_append <- rbind(extfile,append_table)

write_csv(new_append,"C:/Users/laras/Documents/R/Projects/covid19_india/files/daily_append.csv")

########## changing data str ############

append_file <- read_csv("C:/Users/laras/Documents/R/Projects/covid19_india/files/daily_append.csv")

new_str <- append_file %>%
  .[,-1] %>%  
  gather(key = status, value = "cases", c(-state, - date) ) %>%
  spread(key = date , value = cases)




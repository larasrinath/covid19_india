###### whole india stats #######
library(RCurl)
library(lubridate)
library(highcharter)
library(tidyverse)

recovered_url<- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
recovered <- read_csv(recovered_url)

confirmed_url <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
confirmed<- read_csv(confirmed_url)


death_url <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
death<- read_csv(death_url)



################# data cleansing ##############

confirmed_data <- confirmed %>% 
  rename(State = "Province/State", Country = "Country/Region", Latitude = Lat, Longitude = Long)%>%
  gather(key= "Date", value = "Count", - c(State,Country,Latitude,Longitude)) %>%
  mutate(Date = gsub("X","",Date),
         Date = gsub("\\.","-",Date),
         Date = mdy(Date)) %>%
  mutate(Country = str_replace_all(Country, "\\*", ""),
         Country = str_replace_all(Country, ", The", ""),
         Country = str_replace_all(Country, "The", ""),
         Country = str_trim(Country)) 

confirmed_data <- confirmed_data[,c(2,1,3,4,5,6)]


recovered_data <- recovered %>% 
  rename(State = "Province/State", Country = "Country/Region", Latitude = Lat, Longitude = Long) %>%
  gather(key= "Date", value = "Count", - c(State,Country,Latitude,Longitude)) %>%
  mutate(Date = gsub("X","",Date),
         Date = gsub("\\.","-",Date),
         Date = mdy(Date)) %>%
  mutate(Country = str_replace_all(Country, "\\*", ""),
         Country = str_replace_all(Country, ", The", ""),
         Country = str_replace_all(Country, "The", ""),
         Country = str_trim(Country)) 

recovered_data <- recovered_data[,c(2,1,3,4,5,6)]

death_data <- death %>% 
  rename(State = "Province/State", Country = "Country/Region", Latitude = Lat, Longitude = Long)%>%
  gather(key= "Date", value = "Count", - c(State,Country,Latitude,Longitude)) %>%
  mutate(Date = gsub("X","",Date),
         Date = gsub("\\.","-",Date),
         Date = mdy(Date)) %>%
  mutate(Country = str_replace_all(Country, "\\*", ""),
         Country = str_replace_all(Country, ", The", ""),
         Country = str_replace_all(Country, "The", ""),
         Country = str_trim(Country)) 

death_data <- death_data[,c(2,1,3,4,5,6)]


confirmed_recovered<- confirmed_data %>% 
  full_join(recovered_data,by = c("Country" = "Country","Date" = "Date", "State" = "State","Latitude"= "Latitude", "Longitude"="Longitude"), suffix=c("_confirmed","_recovered"))

full_table <- confirmed_recovered %>% 
  full_join(death_data,by = c("Country" = "Country","Date" = "Date", "State" = "State","Latitude"= "Latitude", "Longitude"="Longitude")) %>%
  rename(recovered = Count_recovered,
         confirmed = Count_confirmed,
         death = Count) %>% 
  mutate(confirmed = replace_na(confirmed,0),
         recovered =replace_na(recovered,0),
         death = replace_na(death,0),
         Country = factor(Country)) %>%
  select(Country,Date,confirmed,recovered,death) %>%
  group_by(Country,Date) %>%
  summarise(confirmed = sum(confirmed),
         recovered = sum(recovered),
         death = sum(death))

Countrytbl <- full_table %>%
  select(Country, Date, confirmed) %>%
  filter(Country %in% c("US", "India", "United Kingdom", "China", "Spain" ,"Italy")) %>%
  spread(key = Country, value = confirmed) %>%
  mutate(id = row_number())


ustbl <- Countrytbl %>%
  select(id, US) %>%
  filter(US > 100)

####################### data manipulation ###############

daily_change_full <- full_table %>%
  filter(Country == "India") %>%
  ungroup() %>%
  arrange(desc(Date)) %>% 
  mutate(last_day=lead(confirmed),
         Change = (confirmed - last_day)/last_day,
         New_case = (confirmed - last_day)) %>%
  mutate(Change = replace_na(Change,0),
         New_case = replace_na(New_case,0),
         Change = as.numeric(sprintf("%.2f ",100*(Change))))


############### chart #############
options(scipen = 999)

p <- highchart() %>%
  hc_xAxis(type = "datetime", labels = list(format = '{value:%b %d}')) %>%
  hc_yAxis_multiples(
    list(title = list(text = "Total Cases"),
         showFirstLabel = TRUE,showLastLabel=TRUE,opposite = FALSE),
    list(title = list(text = "Daily Cases"),
         showLastLabel = FALSE, opposite = TRUE)) %>% 
  hc_add_series(daily_change_full,type="line",hcaes(x=Date,y=confirmed), name = "Cumulative Cases") %>%
  hc_plotOptions(line = list(
    pointWidth=10,
    dataLabels = list(enabled = FALSE),
    color = "red"
  )) %>%
  hc_add_series(daily_change_full,type="column",hcaes(x=Date,y=New_case),yAxis=1 , name = "New Cases")%>%
  hc_plotOptions(column = list(
    pointWidth=10,
    dataLabels = list(enabled = FALSE),
    color = "red",
    opacity = 0.5
  )) %>%
  hc_title(text = "Outbreak Timeline in India",align = "center")%>%
  hc_add_theme(hc_theme_smpl())%>%
  hc_legend(enabled = F) 

############### country comparison ################
  
Countrytbl <- full_table %>%
  select(Country, Date, confirmed) %>%
  filter(Country %in% c("US", "India", "United Kingdom", "China", "Spain" ,"Italy")) %>%
  spread(key = Country, value = confirmed) %>%
  mutate(id = row_number()) %>%
  rename(uk = "United Kingdom")


indtbl <- Countrytbl %>%
  select(id, India) %>%
  filter(India > 100)%>%
  mutate(id = row_number())


Italytbl <- Countrytbl %>%
  select(id, Italy) %>%
  filter(Italy > 100)%>%
  mutate(id = row_number())


uktbl <- Countrytbl %>%
  select(id, uk) %>%
  filter(uk > 100)%>%
  mutate(id = row_number())


ustbl <- Countrytbl %>%
  select(id, US) %>%
  filter(US > 100)%>%
  mutate(id = row_number())


Spaintbl <- Countrytbl %>%
  select(id, Spain) %>%
  filter(Spain > 100)%>%
  mutate(id = row_number())

Chinatbl <- Countrytbl %>%
  select(id, China) %>%
  filter(China > 100)%>%
  mutate(id = row_number())

days_100 <- indtbl %>% 
  full_join(Italytbl, by = "id") %>% 
  full_join(uktbl, by = "id")%>% 
  full_join(ustbl, by = "id")%>% 
  full_join(Spaintbl, by = "id")#%>% 
 # full_join(Chinatbl, by = "id")


cmp_plot <- highchart() %>%
  hc_xAxis(type = "number", title = list(text = "Days after 100 Cases"), format = '{value:%b %d}') %>%
  hc_yAxis(title = list(text = "Cases")) %>%
  hc_add_series(days_100,type="line",hcaes(x=id,y=India), name = "India", color = "#5DADE2") %>%
  hc_add_series(days_100,type="line",hcaes(x=id,y=Italy), name = "Italy", color = "#28B463") %>%
  hc_add_series(days_100,type="line",hcaes(x=id,y=uk), name = "UK", color = "#0258FF") %>%
  hc_add_series(days_100,type="line",hcaes(x=id,y=US), name = "US", color = "#CB4335") %>%
  hc_add_series(days_100,type="line",hcaes(x=id,y=Spain), name = "Spain", color = "#F1C40F") %>%
  hc_title(text = "Comparison with Countries",align = "center")%>%
  hc_plotOptions(line = list(
    pointWidth=20,
    dataLabels = list(enabled = FALSE)
  )) %>%
  hc_add_theme(hc_theme_smpl())


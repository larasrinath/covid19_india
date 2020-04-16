###### whole india stats #######
library(RCurl)
library(lubridate)
library(highcharter)

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
         Country = factor(Country))

####################### data manipulation ###############
gather_table <- full_table %>% 
  gather(key= "Status", value = "Count", - c(Country,State, Latitude, Longitude,Date)) %>% 
  mutate(Count = replace_na(Count,0)) %>%
  filter(Country == "India")

Timelinechart <- gather_table %>%
  group_by(Date,Status) %>%
  summarise(Affected = sum(Count))



############### chart #############
options(scipen = 999)


p <-  hchart(Timelinechart, "line", hcaes(x = Date, y = Affected, group = Status) ) %>%
  hc_plotOptions(line = list(
    pointWidth=10,
    dataLabels = list(enabled = F)
  )) %>%
  hc_title(text = "Outbreak Timeline",align = "center") %>% 
  hc_xAxis(title = list(text = "Date"))%>%
  hc_yAxis(title = list(text = "Cases")) %>%
  hc_legend(enabled = F)


########## Latest Data ############
latest_data<- Timelinechart %>% spread(Status,Affected) %>% arrange(desc(Date)) %>% head(1)
latest_recdata<- Timelinechart %>% spread(Status,Affected) %>% arrange(desc(recovered)) %>% head(1)
latest_recdate <- latest_data$Date


latest_date <- latest_data$Date
latest_Confirmed <- latest_data$confirmed
latest_Death <- latest_data$death
latest_Recovered <- latest_recdata$recovered
fatality <- sprintf("%.2f %%",100*(latest_Death/latest_Confirmed))


daily_change_full <- Timelinechart %>%
  ungroup() %>%
  filter(Status =="confirmed") %>% 
  arrange(desc(Date)) %>% 
  mutate(last_day=lead(Affected),
         Change = (Affected - last_day)/last_day,
         New_case = (Affected - last_day)) %>%
  mutate(Change = replace_na(Change,0),
         New_case = replace_na(New_case,0),
         Change = as.numeric(sprintf("%.2f ",100*(Change))))


p2 <- hchart(daily_change_full, "column", hcaes(x = Date, y = New_case), name = "confirmed") %>% 
  hc_title(text = "New Cases per Day",
           margin = 20, align = "center",
           style = list( useHTML = TRUE))%>%
  hc_yAxis(title = list(text ="New Cases")) %>%
  hc_plotOptions(column = list(
    pointWidth=8,
    dataLabels = list(enabled = F)))


p2 <- p2 %>% hc_add_theme(hc_theme_smpl())


#highchart() %>% 
##  hc_yAxis_multiples() %>% 
#  hc_add_series(daily_change_full, "line", hcaes(x = Date, y = Affected), name = "Confirmed #Cases") %>%
#  hc_add_series(daily_change_full, "column", hcaes(x = Date, y = New_case)) %>%#
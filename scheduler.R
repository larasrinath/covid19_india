library(taskscheduleR)
ind_script <- "C:/Users/laras/Documents/R/Projects/covid19_india/source.R"

## run script once within 62 seconds
#taskscheduler_create(taskname = "myfancyscript", rscript = "C:/Users/laras/Documents/R/Projects/covid19_india/source.R", 
#                     schedule = "ONCE", starttime = format(Sys.time() + 30, "%H:%M"))
#taskscheduler_delete(taskname = "india_tbl")


taskscheduler_create(taskname = "india_tbl", rscript = ind_script, 
                     schedule = "DAILY", starttime = "23:58", startdate = format(Sys.Date(), "%d/%m/%Y"))

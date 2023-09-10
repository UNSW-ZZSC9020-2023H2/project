#------------------------------------------------------------------------#
# file: data exploration
#
# author: Hao
# 
# last modified by: Louis
# 
# description: load initial data sets and perform preliminary data
#              preparation and exploration
#------------------------------------------------------------------------#

# load packages -----------------------------------------------------------

# relevant packages for script
package_list <- c(
  "tidyverse"
  , "lubridate"
  , "data.table"
  , "crayon"
)

# list of packages not installed
required_packages <- setdiff(
  package_list
  , rownames(installed.packages())
)

# install any package not installed
lapply(required_packages, install.packages)

# load required packages
lapply(package_list, library, character.only = T)



# read files --------------------------------------------------------------
temperature_nsw    <- fread("data/temperature_nsw.csv")
totaldemand_nsw    <- fread("data/totaldemand_nsw.csv")
forecastdemand_nsw <- fread("data/forecastdemand_nsw.csv")


# data preparation --------------------------------------------------------

##need to change to full outer join
data <- totaldemand_nsw %>% full_join(temperature_nsw,by=c('DATETIME')) %>% select(-LOCATION,-REGIONID);

##filter rows that do not have actual demand
data <- data %>% filter(!is.na(TOTALDEMAND))

##arrange by date in ascending
data <- data %>% arrange(DATETIME);

##fill in missing temperature (down fill)
data <- data %>% fill(TEMPERATURE)

##convert DATETIME to date
data$DATETIME = ymd_hms(data$DATETIME);

##create year/month/day/hour/minute fields
data <- data %>% mutate(YEAR=year(DATETIME), MONTH=month(DATETIME),DAY=day(DATETIME));
data <- data %>% mutate(HOUR=hour(DATETIME), MINUTE=minute(DATETIME));

##season
data <- data %>%mutate(SEASON = case_when(
  MONTH %in%  9:11 ~ "SPRING",
  MONTH %in%  c(12, 1, 2)  ~ "SUMMER",
  MONTH %in%  3:5  ~ "AUTUMN",
  TRUE ~ "WINTER"));
##create boolean for 8am~8pm timeslot
data <- data %>%mutate(timeframe_8amto8pm = case_when(
  HOUR %in%  8:20 ~ TRUE,
  TRUE ~ FALSE));
##Get days of week, monday = 1,...,sunday=7
data<- data%>%mutate(DaysOfWeek=lubridate::wday(DATETIME,week_start = getOption("lubridate.week.start", 1)));
##get weekday&weekend
data <- data %>%mutate(WEEKDAYWEEKEND = case_when(
  DaysOfWeek %in%  1:5 ~ "WEEKDAY",
  DaysOfWeek %in%  6:7  ~ "WEEKEND"
))


# data exploration --------------------------------------------------------


#
# 0. basic set-up of an R work session
#
rm(list=ls())

#
THESIS_FOLDER=dirname(rstudioapi::getActiveDocumentContext()$path)
BEHAVIOUR_DATA_SOURCE="multinomial_group_behaviour.csv"
setwd(THESIS_FOLDER)
library(dplyr)

#
# 1.0 data ingestion
#

#
from_folder.load_monkeys_data=function(
    monkeys_data_file=BEHAVIOUR_DATA_SOURCE
){
  setwd(THESIS_FOLDER)
  list.files()
  read.csv(monkeys_data_file) %>%
    dplyr::select(
      grep(names(.), pattern="^[^X]")
    )
}
from_folder.load_monkeys_data() %>% View()
from_folder.load_monkeys_data() %>% names()
from_folder.load_monkeys_data() %>% dim()


#
# 1.2 data curation and modelling (transformations)
#


#
monkeys_data.curation_and_feature_engineering=
  function(
    
    monkeys_data=from_folder.load_monkeys_data(),
    
    months_in_order=
      c("Jan", "Feb", "Mar",
        "Apr", "May", "Jun",
        "Jul", "Aug", "Sep",
        "Oct", "Nov", "Dec"),
    
    Excluded_Behaviours="Walking"
    
  ){
    monkeys_data %>% 
      
      dplyr::filter(!(Behaviour %in%  Excluded_Behaviours)) %>%
      
      dplyr::mutate(
        Behaviour=trimws(Behaviour)
      ) %>%
      
      dplyr::mutate(
        hour_of_day=stringr::str_extract(Hour, "^[0-9]+") %>% as.numeric()
      ) %>%
      
      dplyr::mutate(
        day=stringr::str_extract(Date, "^[0-9]+") %>% as.numeric(),
        
        month=stringr::str_extract(tolower(Date), "[a-z]+"),
        month=plyr::mapvalues(month,
                              from=tolower(months_in_order),
                              to=1:length(months_in_order)),
        
        year=stringr::str_extract(tolower(Month), "[0-9]+"),
        year=paste("20",year, sep="") %>% as.numeric()
      ) %>%
      dplyr::mutate(
        date= as.Date(paste(year, month,day, sep="-" ))
      ) %>%
      dplyr::mutate(
        year= as.character( lubridate::year(date))
      ) %>%
      dplyr::mutate(
        year_season= as.character( ceiling((as.numeric(month))/3))
      ) %>%
      dplyr::mutate(
        Weather=ifelse(stringr::str_detect(Weather, "Rain"), "Rain", Weather)
      ) %>% 
      dplyr::mutate(
        Weather=factor(Weather, levels=c("Windy","Rain","Sunny","Cloudy"))
      )%>% 
      dplyr::mutate(
        Time_frame=factor(Time_frame, levels=c("Morning","Midday","Afternoon"))
      )%>%
      dplyr::select(-c("Time","Hour","Date", "Month", "day","month")) 
  }
#
monkeys_data.curation_and_feature_engineering() %>%
  View()

#
# 2.0 data ingestion
#

monkeys_data.features= monkeys_data.curation_and_feature_engineering()
monkeys_data.features %>% View()
list.files()
save.image(file="monkeys_features_cache.RData")

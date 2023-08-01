
#
# 0. basic set-up of an R work session
#
rm(list=ls())

#
#
THESIS_FOLDER=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(THESIS_FOLDER)
library(dplyr)

#
# 1.0 data ingestion
#

#
#
from_folder.load_monkeys_data=function(
  monkeys_data_file="multinomial_group_behaviour.csv"
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
# 1.3 data description (Exploratory Data Analysis)
#

#
#
Curated_monkeys_data.describe_variable_wise_on_type=
  function(
    Curated_monkeys_data=monkeys_data.curation_and_feature_engineering()
  ){
    Curated_monkeys_data %>%
      lapply(function(data_col){
        
        if(length(unique(data_col))>10)
          {
            data.frame(
              min_value=min(data_col),
              standard_dev=round(sd(data_col),3),
              median_value=round(median(data_col),3),
              max_value=max(data_col)
            )
        } else  {
            sort(table(data_col), decreasing=TRUE)
          }
        })
  }
Curated_monkeys_data.describe_variable_wise_on_type()



#
# 2.Graphical exploration of model covariates
#

#
# 2.1 description of y (dependent) variable

#
monkeys_proportions_plot.neat_style=function(
    monkeys_proportions_plot,
    caption_=paste(
      "Proportions were computed as the fraction of time of each behaviour, within subsample",
      "Samples of less than 15 monkeys were excluded to facilitate interpretation",
      sep="\n"
    )
    ){
  monkeys_proportions_plot +
  labs(
    subtitle = 
      "Proportions of monkeys in each behaviour, per Group",
    caption = caption_
  )+
    ylab("Proportion")+
    guides(alpha="none")
}
#
self_as_names=function(x){names(x)=x;x}
#
Curated_monkeys_data.Behaviour_AssestmentXGroup=
  function(
    Curated_monkeys_data=monkeys_data.curation_and_feature_engineering()
  ){
    
    Groups=
    c(
      unique(Curated_monkeys_data$Group) %>% self_as_names(),
      "All"=list(unique(Curated_monkeys_data$Group))
      ) 
    #
    library(ggplot2)
    pl=
    lapply(names(Groups),function(Group_L){
      df=
        Curated_monkeys_data %>%
        dplyr::filter(Group %in% Groups[[Group_L]]) 
      
      f=round(prop.table(table(df$Behaviour))*100,3)
      
      data.frame(
        Group=dplyr::first(Group_L),
        Behaviour=names(f),
        Prop=f %>% as.numeric()
      )
    }) %>% 
      bind_rows() %>%
      dplyr::mutate(
        Behaviour=factor(Behaviour, 
                         levels =table(Curated_monkeys_data$Behaviour) %>% sort() %>% names() )
      ) %>%
      dplyr::mutate(
        alpha_=as.numeric(.7)
      ) %>% 
      dplyr::mutate(
        alpha_=dplyr::case_when(
          Group=="All"~"a",
          TRUE~"b"
        )) %>%
      ggplot(aes(
        y=Prop,
        x=Behaviour,
        group=Group,
        color=Group
      ))+
      geom_point(
        aes(shape=Group,alpha=alpha_),
        size=2.5
      )+
      geom_line(
        aes(linetype=Group,alpha=alpha_),
        linewidth=1.0
      )+
      scale_alpha_manual(values = c(1,.4)) 
      monkeys_proportions_plot.neat_style(
        pl
        )
  }
#
Curated_monkeys_data.Behaviour_AssestmentXGroup(
  
)


#
# 2.2 description of x's (independent) variables
#


#
#
monkeys_data.behaviour_per_secondary_factor=
  function(
    monkeys_data=monkeys_data.curation_and_feature_engineering(),
    secondary_factor="Weather"
  ){
    
    #
    Groups=
      c(unique(monkeys_data$Group) %>% self_as_names(),
        "All"=list(unique(monkeys_data$Group))
        ) 
    
    #
    library(ggplot2)
    pl=
    lapply(names(Groups),function(Group_L){
        monkeys_data %>%
        dplyr::filter(Group %in% Groups[[Group_L]]) %>%
        split(.[[secondary_factor]]) %>%
        lapply(function(df_){
          n=table(df_$Behaviour)
          f=round(prop.table(n)*100,3)
          data.frame(
            Group=dplyr::first(Group_L),
            Secondary=dplyr::first(df_[[secondary_factor]]),
            Behaviour=names(f),
            Size=n %>% as.numeric(),
            Prop=f %>% as.numeric()
          )
        }) %>%
        bind_rows()
    }) %>%
      bind_rows() %>% 
      dplyr::mutate(
        Behaviour=factor(Behaviour, 
                         levels =table(monkeys_data$Behaviour) %>% sort(decreasing = TRUE) %>% names() )
      ) %>% 
      dplyr::mutate(
        alpha_=dplyr::case_when(
          Group=="All"~"a",
          TRUE~"b"
        ))  %>% 
      
      dplyr::filter(
        Size>=15
      ) %>%
      
      ggplot(aes(x=Secondary , y=Prop, color= Group, group=Group))+
        geom_point(
          aes(shape=Group,
              alpha=alpha_),
          size=2.5
        ) +
      
      facet_wrap(~Behaviour, scales="free_y")+
      
      geom_line(
        aes(linetype=Group, alpha=alpha_),
        linewidth=.5
      )+
      scale_alpha_manual(values = c(1,.4))+
      xlab(secondary_factor)
    monkeys_proportions_plot.neat_style(
      pl)
  }
#
monkeys_data.behaviour_per_secondary_factor(
  secondary_factor= "Weather"
)
#
monkeys_data.behaviour_per_secondary_factor(
  secondary_factor= "Time_frame"
)
#
monkeys_data.behaviour_per_secondary_factor(
  secondary_factor= "year_season"
)
#


#
#
Curated_monkeys_data.XGroup.daily_behaviour_plot=
  function(
    Curated_monkeys_data=monkeys_data.curation_and_feature_engineering(),
    interpolating="y~poly(x, 3)"
  ){
    
    library(ggplot2)
    pl=
    Curated_monkeys_data %>% 
      split(.$Group) %>%
      lapply(function(group_subsample){
        group_subsample %>%
        split(.$"hour_of_day") %>%
          lapply(function(hour_df){
            
            behavour_counts=table(hour_df$Behaviour)
            data.frame(
              
              Group=dplyr::first(hour_df$"Group"),
              
              hour_of_day= dplyr::first(hour_df$"hour_of_day"),
              #
              number_of_monkeys=nrow(hour_df),
              #
              behavour_label= names(behavour_counts) %>% as.character(),
              behavour_count= behavour_counts %>% as.numeric()
              
            ) %>%
              dplyr::mutate( behavour_rate=(behavour_count/number_of_monkeys)*100) 
          }) %>%
          bind_rows()
      }) %>%
      bind_rows() %>%
      ggplot(aes(x= hour_of_day, y=behavour_rate, colour= behavour_label), alpha=.3)+
      geom_point(aes(shape=behavour_label), alpha=.3,size=4)+
      geom_line(lty="dashed", alpha=.3)+
      geom_smooth(formula=interpolating,method="lm",se=FALSE)+
      labs(
        subtitle = "Evolution of shares of each behaviour trough a tipical day"
      )+
      facet_wrap(~Group)
      monkeys_proportions_plot.neat_style(
        pl,
        caption_=
          "Proportions were computed as the fraction of time of each behaviour, within subsample"
      )
  }
#
Curated_monkeys_data.XGroup.daily_behaviour_plot()


#
Curated_monkeys_data.XGroup.long_term_behaviour_plot=
  function(
    Curated_monkeys_data=monkeys_data.curation_and_feature_engineering(),
    interpolating="y~poly(x, 3)"
  ){
    
    pl=
      Curated_monkeys_data %>% 
      split(.$Group) %>%
      lapply(function(group_subsample){
        group_subsample %>%
          split(.$"date") %>%
          lapply(function(hour_df){
            
            behavour_counts=table(hour_df$Behaviour)
            data.frame(
              
              Group=dplyr::first(hour_df$"Group"),
              #
              date= dplyr::first(hour_df$"date"),
              #
              number_of_monkeys=nrow(hour_df),
              #
              behavour_label= names(behavour_counts) %>% as.character(),
              behavour_count= behavour_counts %>% as.numeric()
              
            ) %>%
              dplyr::mutate( behavour_rate=(behavour_count/number_of_monkeys)*100) 
            
          }) %>%
          bind_rows()
      })%>%
      bind_rows() %>%
      
      ggplot(aes(x= date, y=behavour_rate, colour= behavour_label), alpha=.3)+
        geom_point(aes(shape=behavour_label), alpha=.3,size=4)+
        geom_line(lty="dashed", alpha=.3)+
        geom_smooth(formula=interpolating,method="lm",se=FALSE)+
        labs(
          subtitle = "Evolution of shares of each behaviour over the long term"
        )+
      facet_wrap(~Group)
      monkeys_proportions_plot.neat_style(
        pl,
        caption_=
          "Proportions were computed as the fraction of time of each behaviour, within subsample"
      )
  }
#
Curated_monkeys_data.XGroup.long_term_behaviour_plot()


#
# 3.0 Modelling trough multinomial logistic model
#
monkeys_data.model_behavour=
  function(
    Curated_monkeys_data=monkeys_data.curation_and_feature_engineering()
  ){
    
    monkeys_model_data=
      Curated_monkeys_data %>%
      dplyr::mutate(
        Behaviour=factor(Behaviour)
      ) %>%
      dplyr::mutate(
        Behaviour=relevel(Behaviour, ref = "Resting")
      ) %>%
      dplyr::mutate(
        weather_=factor(Weather)
      ) %>%
      # dplyr::mutate(
      #   weather_=relevel(weather_, ref = "Sunny")
      # ) %>%  
      dplyr::mutate(
        Time_frame_=Time_frame
      ) %>%  
      dplyr::mutate(
        ln_of_date_=log(date %>% as.numeric())
      ) %>%  
      dplyr::mutate(
        year_=year %>% as.character()
      ) %>%
      dplyr::mutate(
        year_season_=factor(year_season)
      ) %>%  
      # dplyr::mutate(
      #   year_season_=relevel(year_season_, ref = "4")
      # ) %>%
      dplyr::select(
        Behaviour, grep(names(.), pattern="_$")
      )
    nnet::multinom( Behaviour ~ .,  data =  monkeys_model_data)
  } 
#
Model_Fit_Cache=monkeys_data.model_behavour()
#
# 3.1 Summary of the model as relative risk
#
Multinomial_Logistic_Regression_Fit.summarize_Relative_Risks=
  function(
    Multinomial_Logistic_Regression_Fit=Model_Fit_Cache
  ){
    options(scipen=999)
    a=
    exp(coef(Multinomial_Logistic_Regression_Fit)) %>%
      as.data.frame() %>%
      dplyr::select(-c("(Intercept)")) %>%
      dplyr::mutate(Behaviour=rownames(.) ) %>%
      tidyr::pivot_longer( 
        setdiff( names(.),"Behaviour"),
        names_to="field",
        values_to="Relative_Risk_Ratio"
        ) %>%
      dplyr::mutate(
        Relative_Risk_Ratio=round(Relative_Risk_Ratio,3)
      ) %>%
      dplyr::arrange(
        Behaviour, 
        field
      )
    b=
    (1 -
    pnorm(
    abs(
    (
    summary( Multinomial_Logistic_Regression_Fit)$coefficients/
      summary(Multinomial_Logistic_Regression_Fit)$standard.errors
    )
    ),
    0, 1))*2  
    
    b=
    b %>%
      as.data.frame() %>%
      dplyr::select(-c("(Intercept)")) %>%
      dplyr::mutate(Behaviour=rownames(.) ) %>%
      tidyr::pivot_longer( 
        setdiff( names(.),"Behaviour"),
        names_to="field",
        values_to="P_val"
      ) %>%
      dplyr::mutate(
        P_val=round(P_val,5)
      )
    dplyr::left_join(a, b) 
  }
#
Multinomial_Logistic_Regression_Fit.summarize_Relative_Risks() %>% View()
#

#
# single evaluation loop
#
Curated_sample_cache=
monkeys_data.curation_and_feature_engineering()
#
table(Curated_sample_cache$Group)
#
Curated_sample_cache %>%
  split(.$Group) %>%
  lapply(function(sample_group){
    sample_group %>%
      monkeys_data.model_behavour() %>%
      Multinomial_Logistic_Regression_Fit.summarize_Relative_Risks() %>%
      dplyr::mutate(
       Group=dplyr::first(sample_group$Group),
       Sample_size=nrow(sample_group)
      )
  }) %>%
  #bind_rows() %>%
  openxlsx::write.xlsx("results.xlsx")

  
  




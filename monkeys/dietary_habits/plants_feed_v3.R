#
# 0.1 set-up
#

rm(list=ls())
THESIS_FOLDER=dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(THESIS_FOLDER)
PLANTS_DATA_SOURCE="Annual string group feeding v2.xlsx"
library(dplyr)

#
# 1. load feeding records
#

#
Standardize_strings=
  function(
    strings
  ){
    strings %>%
      tolower() %>%
      return()
  }
#
label_per_code=
  PLANTS_DATA_SOURCE %>%
  readxl::read_excel(sheet=2) %>% 
  {
    names(.)=c("code", "label");.
  } %>%
  dplyr::mutate(
    code=Standardize_strings(code),
    label=Hmisc::capitalize( Standardize_strings(label))
    
  ) %>%
  {
    l= .$label
    
    names(l)=
      .$code %>%
      sapply(function(k){
        sprintf("%s(s)*",k)
      })
    
    l
  } 
#
label_per_code 
#
from_folder.load_plants_data=function(
    plants_data_file="Annual string group feeding v2.xlsx"
){
  
  #
  setwd(THESIS_FOLDER)
  list.files()
  readxl::read_excel(plants_data_file) %>% 

    dplyr::mutate(
      "scientific_name_"=Hmisc::capitalize(Standardize_strings(.[["Scientific name"]]))
    ) %>% 
    
    dplyr::mutate(
      date_=tolower(Date),
      date_=stringr::str_replace(date_, "2022", "2019")
    ) %>% 
    
    dplyr::mutate(
      year_=stringr::str_extract(date_, "[0-9]{4}" ),
      month_=
        stringr::str_extract( 
          stringr::str_extract(date_, "[-][0-9]{2}[-]" ),
          "[0-9]{2}"
        )) %>%
    
    dplyr::mutate(
      part_=Standardize_strings(Part)
    ) %>%
    {
      new_df=.
      for (a_code in names(label_per_code) ) {
        new_df=
          new_df %>%
          dplyr::mutate(
            part_=stringr::str_replace_all(part_, a_code, label_per_code[[a_code]] )
          )
      }
      new_df
    } %>%
    
    dplyr::select(
      c(Group,grep("_$", names(.),value=TRUE))
    )
}
#
from_folder.load_plants_data() %>% View()

#
# 3. transformation of features
#

#
Plants_data.curate_features=function(
    plants_data=from_folder.load_plants_data(),
    categorical_features=c("scientific_name_", "part_"),
    
    min_freq_threshold=10,
    
    rare_value_label="Rare",
    missing_value_label="Not reported"
    
){
  
  #
  for (some_feature in categorical_features){
    #print(some_feature)
    
    #
    feature_values=
      plants_data[[some_feature]] %>%
      stringr::str_split(",") %>%
      unlist() %>%
      trimws() 
    feature_values_freqs= sort(table(feature_values), decreasing = TRUE)
    rare_feature_values=names(feature_values_freqs[feature_values_freqs<min_freq_threshold])
    rare_feature_values=stringr::str_replace_all(rare_feature_values, "[()]",".")
    rare_regex=
      paste(
        paste("^(",rare_feature_values,")$", collapse="|", sep=""),
        paste("^(",rare_feature_values,",)", collapse="|", sep=""),
        paste("(, ",rare_feature_values,")$", collapse="|", sep=""),
        paste("(, ",rare_feature_values,",)", collapse="|", sep=""),
        collapse="|", sep="|"
      )
    rare_regex
    
    #
    plants_data=
      plants_data %>%
      dplyr::mutate(
        temp=stringr::str_replace_all(plants_data[[some_feature]],
                                      rare_regex, 
                                      rare_value_label )
      )
    plants_data[["temp"]][is.na(plants_data[["temp"]])]=missing_value_label
    plants_data[[some_feature]]=plants_data[["temp"]]
    # plants_data
  }
  plants_data %>%
    dplyr::select(-temp)
}
Plants_data.curate_features() %>% View()

curated_plants_data=Plants_data.curate_features() 
sort(table(curated_plants_data$scientific_name_))
sort(table(curated_plants_data$part_))
#table(plants_data$"Scientific name", plants_data$Group ) 


#
# 4. statistical test on plant feeded
#

#
Plants_data.contingency_table=function(
    plants_data=curated_plants_data,
    feature="part_"
){
  
  #
  plants_data.unrolled_feature=
    plants_data %>%
    dplyr::select(
      c(Group,feature)
    ) %>%
    split(1:nrow(.)) %>%
    lapply(function(row){
      data.frame(
        Group=dplyr::first(row[["Group"]]),
        feature=stringr::str_split(row[[feature]], ",") %>% unlist()
      )
    }) %>%
    bind_rows()
  
  #
  con_table=table(
    plants_data.unrolled_feature$feature %>%
      stringr::str_split(",") %>%
      unlist() %>%
      trimws(),
    plants_data.unrolled_feature$Group )
  
  #
  chisq_test=
    chisq.test(con_table) %>%
    {
      t=.
      t[
      sapply(t,function(a){
        class(a) %in% c("numeric", "integer", "character")
        #class(a)
      }) %>% unlist()
      ]
    } %>%
    as.data.frame() %>%
    {
      row.names(.)=NULL;.
    }
  
  #con_table
  #
  con_table=
    con_table %>%
    as.data.frame() %>%  
    tidyr::pivot_wider(names_from=Var2, values_from =Freq ) %>% 
    {
      df=.
      df[[feature]]=df[["Var1"]]
      df %>% dplyr::select(-Var1)
      #dplyr::rename(Species=Var1) 
    } %>% 
    
    {
      df=.
      list(
        df %>% dplyr::select(c(feature)),
        df %>% dplyr::select(-c(feature)) %>%
          {
            apply(.,MARGIN=2, FUN=function(col){
              
              tryCatch(
                {
                  
                  col[1]+col[2]
                  n=as.numeric(col)
                  p=round(as.numeric(col/sum(n))*100,3)
                  sprintf("%s (%s)", n,p)  
                },
                error=function(e){
                  col
                })
              
            })
          }
      ) %>%
        bind_cols()
    } %>%
    as.data.frame() %>%
    dplyr::mutate(
      Total=
        apply(con_table,MARGIN=1, FUN=function(row){
          sum(row)
        })
    )  %>%
    dplyr::arrange(
      Total*(-1)
    ) %>%
      dplyr::mutate(
        Total=
          lapply(Total, function(t){
            sprintf("%s (%s)", t, round((t/sum(Total))*100,3)) 
          })
        
      ) 
  
  list(
    "con_table"=con_table,
    "chisq_test"=chisq_test
  )
  
}
#
ct=Plants_data.contingency_table(
  feature="part_"
) 
ct$con_table %>%
  View()
ct$chisq_test 
#
ct=Plants_data.contingency_table(
  feature="scientific_name_"
) 
ct$con_table %>%
  View()
ct$chisq_test


#
Plants_data.statistical_tests=function(
    plants_data=curated_plants_data,
    Feature="part_"
){
  ct=Plants_data.contingency_table(
    feature=Feature
  )
  
  df= plants_data %>% dplyr::select(c(Group,Feature))
  
  logit_results=
  as.character(ct$con_table[[Feature]]) %>%
  lapply(function(feat_val){
    
    test_data=
      df %>%
      dplyr::mutate(
        is_value=
          as.numeric(stringr::str_detect(df[[Feature]], feat_val))
      ) %>%
      na.omit()
    
    # First Run
    lm_fit1 <- glm(is_value ~ Group-1, family = binomial(link='logit'), data = test_data)
    
    # Get coefficients
    coef1 <- coef(summary(lm_fit1))
    
    # Find the term with the largest coefficient (excluding intercept)
    max_coef_group <- 
      coef1 %>%
      as.data.frame() %>%
      dplyr::arrange( Estimate*(-1)) %>%
      {
      stringr::str_replace(row.names(.)[1], "Group", "")
      }
    
    # Convert 'Group' to a factor if it's not already
    test_data$Group <- as.factor(test_data$Group)
    
    # Make the category with the largest coefficient as the reference category for the second run
    test_data$Group <- relevel(test_data$Group, ref = max_coef_group)
    
    # Second Run
    lm_fit2 <- glm(is_value ~ Group, family = binomial(link='logit'), data = test_data)
  
    
    # Get model summary
    summary_lmf2 <- summary(lm_fit2)
    
    # Extract standard errors, z-values, and p-values
    std_error <- summary_lmf2$coefficients[, "Std. Error"]
    z_value <- summary_lmf2$coefficients[, "z value"]
    p_value <- summary_lmf2$coefficients[, "Pr(>|z|)"]
    
    # Convert coefficients to odds ratios
    odds_ratios <- exp(coef(lm_fit2))
    
    # Convert to a dataframe
    results_df <- data.frame(OddsRatio = odds_ratios,
                             StdError = std_error,
                             ZValue = z_value,
                             PValue = p_value)
    
    # Create a row for the reference category
    reference_row <- data.frame(OddsRatio = 1, StdError = NA, ZValue = NA, PValue = NA)
    rownames(reference_row) <- max_coef_group
    
    # Combine this row with the original dataframe
    results_df <- rbind(reference_row, results_df)
    
    list(
      Tested_value= feat_val,
      SumValue_= sum(test_data$is_value),
      Group=row.names(results_df),
      results_df %>%
        round(4)
    ) %>%
      bind_cols() %>%
      {
        rownames(.)=NULL;.
      }
     
  })  %>%
    bind_rows() %>%
    dplyr::filter(Group!="(Intercept)" ) %>%
    dplyr::mutate(
      Group=stringr::str_replace(Group, "Group", "")
    ) %>%
    dplyr::filter(SumValue_>=10) %>%
    dplyr::select(-SumValue_) 
  
  ct[["logit_results"]]=logit_results
  ct
}
  
#
ct=
Plants_data.statistical_tests(
  Feature="part_"
) 
ct$logit_results %>%View()
openxlsx::write.xlsx(
  ct,
  "statistical_analysis_of_parts_of_the_plants.xlsx"
)
#
ct=
Plants_data.statistical_tests(
  Feature="scientific_name_"
) 
openxlsx::write.xlsx(
  ct,
  "statistical_analysis_of_scientific_names.xlsx"
)


# 
# 5. computation of group summaries
#

plants_data.group_summary=function(
    
  plants_data=curated_plants_data,
  feature="part_"
    
){
  #
  feature_values=
    plants_data[[feature]] %>%
      stringr::str_split(",") %>%
      unlist() %>%
      trimws() %>%
      unique()
  #
  plants_data %>%
    split(.$Group) %>%
    lapply(function(df){
      data.frame(
        Group=dplyr::first(df$Group),
        monkeys_in_group=nrow(df)
      )
      feature_values %>%
        lapply(function(value){
          data.frame(
            Group=dplyr::first(df$Group),
            GroupSize=nrow(df),
            Feat_Value=value,
            Num_Monkeys=sum(stringr::str_detect(df[[feature]],value), na.rm = TRUE)
          )
          }) %>%
        bind_rows() %>%
        dplyr::mutate(
          Fraction= round((Num_Monkeys/GroupSize)*100,3) 
        ) %>%
        dplyr::arrange(
          Fraction*(-1)
        ) %>%
        dplyr::mutate(
          Rank=1:nrow(.) 
        )
    }) %>%
  bind_rows() 
}
#
plants_data.group_summary() %>% View()
plants_data.group_summary(feat="Scientific name") %>% View()



#
# 5. graphical analysis
#

#
# (0)
#
label_per_feature=list(
  "part_"= "Part of the plant that was feed",
  "scientific_name_"="Scientific name of plant species"
)
#
color_per_feature=list(
  "part_"= rgb(53/275, 136/275, 86/275),
  "scientific_name_"=rgb(148/563,199/563,216/563)
)

#
# (1)
#
monkeys_plants_proportions_plot.neat_style=function(
    monkeys_proportions_plot,
    caption_=paste(
      "Computed from number of times each value was observed, wtihin subsample",
      sep="\n"
    )
){
  monkeys_proportions_plot +
    labs(
      #subtitle = 
      #  "Proportions of monkeys with each species, per Group",
      caption = caption_
    )+
    guides(alpha="none")
}
#
Plants_data.counts.plot=function(
    plants_data=curated_plants_data,
    Feature="part_",
    rank_size=10,
    method="Fraction"
){
  
  #
  feature_values=
    plants_data[[Feature]] %>%
    stringr::str_split(",") %>%
    unlist() %>%
    trimws() %>%
    unique()
  
  #
  Summary=plants_data.group_summary(feature=Feature)
  df =
    Summary %>% 
    
    dplyr::mutate(
      feat_value_=.[[method]]
    ) %>%
    
    dplyr::filter(rank_size>Rank) %>%
    
    split(.$Group) %>%
    lapply(function(df){
      df %>%
        dplyr::mutate(
          alpha_=(feat_value_-min(feat_value_))/(max(feat_value_)-min(feat_value_)),
          alpha_=alpha_*.7
        )  
    }) %>%
    bind_rows() %>% 
    dplyr::arrange(feat_value_) %>%
    dplyr::mutate(
      Unique_Sample=1:nrow(.),
      Feat_Value_=factor(Unique_Sample, levels=Unique_Sample, labels=Feat_Value)
    )
  
  
  #
  library(ggplot2)
  pl=
    df %>%
    ggplot(aes(x=Feat_Value_, y=feat_value_))+
    geom_col(
      aes(
        alpha=alpha_,
      ),
      fill=color_per_feature[[Feature]],
      color="white",
      width = .5
    )+
    facet_wrap(~Group, scales="free")+
    coord_flip()+
    labs(
      title=sprintf("%s-Rank in counts (per group) for the %s", rank_size, label_per_feature[[Feature]])
    )+
    xlab(Feature)+
    ylab("count")+
    theme(
      legend.position="none",
      axis.text=element_text(size=6)
    )
  #
  monkeys_plants_proportions_plot.neat_style(pl) 
}
#
Plants_data.counts.plot(
  rank_size=10,
  Feature="part_",
) ##-> look this chart
# 
Plants_data.counts.plot(
  rank_size=10,
  Feature="scientific_name_"
) ##-> look this chart



#
# (2)
#
Plants_data.freqs.plot=function(
    
    plants_data=from_folder.load_plants_data(),
    Feature="part_",
    rank_size=20
    
){
  #
  Summary=plants_data.group_summary(feature=Feature)
  #
  library(ggplot2)
  pl=
    Summary %>% 
      dplyr::filter(
        Rank<=rank_size
      ) %>% 
      dplyr::mutate(
        Group=factor(Group)
      ) %>%
      dplyr::mutate(
        label_=
          sprintf(
            "%s: (%s)", Feat_Value, Fraction
          )
      ) %>%
    dplyr::mutate(
      alpha_=(Fraction-mean( Fraction))/(max(Fraction)-min(Fraction)),
      size_=(Fraction-mean( Fraction))/(max(Fraction)-min(Fraction))*.7,
      size_=.7
    ) %>%
    ggplot(aes(x=Group,
               y=Fraction,
               group=Feat_Value
    ))+
        ggrepel::geom_label_repel(
            aes(
              
              
              fill=Feat_Value,
              label=label_,
              
              #alpha=alpha_,
              
              ),
              #fontface = 'bold',
              
              alpha=.5,
              color = "black",
              size=2.0
          )+
          geom_line(
            aes(col=Feat_Value),
            alpha=.7,
            linewidth=.6,
            linetype="dashed"
          )+
        geom_vline(
          xintercept = 1:15,
          color="black",
          linetype="dashed",
          linewidth=.1,
          alpha=.5
        )+
        theme(
          legend.position="none",
          axis.text=element_text(size=8)
        )+
        labs(
          title=sprintf("%s-Rank in fractions (per group) for %s", rank_size, label_per_feature[[Feature]] )
        )+
        ylab("Percentage")+
        xlab("Group")
  #
  monkeys_plants_proportions_plot.neat_style(pl) 
}
#
Plants_data.freqs.plot(
  rank_size=20,
  Feature="part_",
) ##-> look this chart
#
Plants_data.freqs.plot(
  rank_size=20,
  Feature="scientific_name_"
) ##-> look this chart

#
# (3)
#
plants_data.per_date_freqs.plot=function(
    
    plants_data=from_folder.load_plants_data(),
    Feature="part_",
    rank_size=4,
    
    ordered_months_labels=
      c(
        "january",
        "february", 
        "march", 
        "april",  
        "may",
        "june",
        "july",      
        "august", 
        "september",
        "october",
        "november",
        "december"
      )
                
){
  #
  Main_plants_per_group=
    plants_data.group_summary(
      feature=Feature
      ) %>% 
      dplyr::filter(
        Rank<=rank_size
      ) %>%
      split(.$Group) %>%
      lapply(function(df){
        df[["Feat_Value"]]
      }) 
  #
  library(ggplot2)
  df=
    #
    plants_data  %>% View()
      dplyr::mutate(
        month_=as.numeric(month_)
        ) %>% 
      dplyr::mutate(
        date_=sprintf("%s-%s-15", year_, month_  ),
        date_=as.Date(date_)
      ) %>%  
    
    #
    split(.$date_) %>%
    lapply(function(df){
      data.frame(
        dplyr::first(df$date_),
        nrow(df)
      )
       df %>%
         plants_data.group_summary(
           feature=Feature
         ) %>%
         dplyr::mutate(
           date_=dplyr::first(df$date_)
         )
    }) %>%
    bind_rows() 
  
  pl=
    df %>%  
      dplyr::mutate(
        #
        Feat_Value_label_alpha_=
          1:nrow(df) %>%
          sapply(function(i){
            df[["Feat_Value"]][i] %in% Main_plants_per_group[[df[["Group"]][i]]] 
          }),
        #
        Feat_Value_label_=
          1:nrow(df) %>%
          sapply(function(i){
            ifelse(
              df[["Feat_Value"]][i] %in% Main_plants_per_group[[df[["Group"]][i]]],
              df[["Feat_Value"]][i],""
            )
          })
      ) %>% 
    
    dplyr::sample_frac() %>%
    split(paste(.$Group, .[["Feat_Value"]])) %>%
    lapply(function(ts_line_data){
      ts_line_data %>%
        dplyr::mutate(row_=1:nrow(ts_line_data)) %>%
        dplyr::mutate(
          Feat_Value_label_= ifelse(row_==1,Feat_Value_label_, "")
        )
    }) %>%
    bind_rows() %>% 
    
    ggplot(aes(x=date_, 
               y=Fraction, 
               fill=Feat_Value,
               group=Feat_Value,
               alpha= Feat_Value_label_alpha_
    ))+
    # geom_point(
    #   aes(color=Scientific_name)
    #   )+
    geom_line(aes(color=Feat_Value), linewidth=.6)+
    
    theme(
      legend.position="none",
      axis.text=element_text(size=6)
    )+
    facet_wrap(~Group)+
    
    ggrepel::geom_text_repel(
      aes(
        label=Feat_Value_label_,
        color=Feat_Value_label_
      ),
      
      alpha=1,
      
      segment.linetype = 2,
      #alpha=.5,
      
      #force=100,
      force_pull=5,
      size=2.5,
      
      max.overlaps = Inf,
      linewidth=.05
      
    )+
    labs(
      title=sprintf("%s-Rank in fractions (per group, over time) for %s", rank_size, label_per_feature[[Feature]] )
    )
    monkeys_plants_proportions_plot.neat_style(pl)
}
#
plants_data.per_date_freqs.plot(
  rank_size = 4,
  Feature="part_"
) ##-> look this chart
#
plants_data.per_date_freqs.plot(
  rank_size = 4,
  Feature="scientific_name_"
) ##-> look this chart

#
# (4)
#
plants_data.per_month_freqs.plot=function(
    
    plants_data=from_folder.load_plants_data(),
    Feature="part_",
    rank_size=3,
    
    ordered_months_labels=
      c(
        "january",
        "february", 
        "march", 
        "april",  
        "may",
        "june",
        "july",      
        "august", 
        "september",
        "october",
        "november",
        "december"
      )
    
){
  #
  Main_plants_per_group=
    plants_data.group_summary(
      feature=Feature
    ) %>% 
    dplyr::filter(
      Rank<=rank_size
    ) %>%
    split(.$Group) %>%
    lapply(function(df){
      df[["Feat_Value"]]
    }) 
  
  
  #
  library(ggplot2)
  df=
    #
    plants_data  %>%
      dplyr::mutate(
        month_=as.numeric(month_)
      ) %>% 
      
      dplyr::mutate(
        date_=factor(as.numeric(month_),
                     levels= 1:length(ordered_months_labels),
                     labels= ordered_months_labels)
      ) %>%
    # {
    #   .[["Scientific name"]]=.[["Scientific_Name_L"]];.
    # } %>%

    #
    split(.$date_) %>%
    lapply(function(df){
      
      data.frame(
        dplyr::first(df$date_),
        nrow(df)
      )
      df %>%
        plants_data.group_summary(
          feature = Feature
        ) %>%
        dplyr::mutate(
          date_=dplyr::first(df$date_)
        ) %>%
        dplyr::mutate(
          date_=dplyr::first(df$date_)
        )
    }) %>%
    bind_rows() 
  
  pl=
    df %>% 
      dplyr::mutate(
        #
        Feat_Value_label_alpha_=
          1:nrow(df) %>%
          sapply(function(i){
            df[["Feat_Value"]][i] %in% Main_plants_per_group[[df[["Group"]][i]]] 
          }),
        #
        Feat_Value_label_=
          1:nrow(df) %>%
          sapply(function(i){
            ifelse(
              df[["Feat_Value"]][i] %in% Main_plants_per_group[[df[["Group"]][i]]],
              df[["Feat_Value"]][i],""
            )
          })
      ) %>% 
      sample_frac() %>%
      split(.$Group) %>%
      lapply(function(
      sample
      ){
        sample %>%
          dplyr::mutate(
            Feat_Value_label_=
              ifelse(date_==dplyr::first(date_),Feat_Value_label_, NA)
          )
        
      }) %>%
      bind_rows() %>%
      
      ggplot(aes(x=date_, 
                 y=Fraction, 
                 fill=Feat_Value,
                 group=Feat_Value,
                 #alpha=Scientific_Name_B
      ))+
      geom_col(
         position = "stack",
         color="white",
         linewidth=.4,
         width=.7,
         alpha=.7
              
      )+
    #scale_fill_discrete (palette = "Set1")+
    facet_wrap(~Group) +
    
    theme(
      legend.position="none",
      axis.text=element_text(size=6)
    )+ 
    theme(axis.text.x = element_text (angle = 45, vjust = .8))+
  
  
    ggrepel::geom_label_repel(
      aes(
        label=Feat_Value_label_
      ),
      alpha=.7,
      position = "stack",
      size=1.8,
      max.overlaps = 100,
      force=50
    )+
    labs(
      title=sprintf("%s-Rank in fractions (per group, by month) for %s", rank_size, label_per_feature[[Feature]] )
    )
    monkeys_plants_proportions_plot.neat_style(pl)
}
#
plants_data.per_month_freqs.plot(
  rank_size=5
  ) ## -> look this chart 
# 
plants_data.per_month_freqs.plot(
  rank_size=4,
  Feature="scientific_name_"
) ## -> look this chart

#
# 5. loop to create charts
#

setwd(THESIS_FOLDER)
# dir.create("charts")
# setwd("charts")
# #
# charts=grep(names(.GlobalEnv), pattern="plot$", value=TRUE) 
# #
# charts %>%
#   lapply(function(some_chart_name){
#     #
#     chart_func=.GlobalEnv[[some_chart_name]] 
#     #
#     setwd(THESIS_FOLDER)
#     dir.create("charts")
#     setwd("charts")
#     dir.create("tests")
#     setwd("tests")
#     #
#     png(sprintf("%s.png",some_chart_name))
#     pl=chart_func()
#     plot(pl)
#     dev.off()
#   })
#
charts=grep(names(.GlobalEnv), pattern="plot$", value=TRUE) 
#
setwd(THESIS_FOLDER)
c("part_", "scientific_name_") %>%
  lapply(function(a_feat){
    charts %>%
      lapply(function(some_chart_name){
        #
        chart_func=.GlobalEnv[[some_chart_name]] 
        #
        pl=chart_func(Feature=a_feat)
        plot(pl)
        ggsave(filename= sprintf("chart_%s_%s.png",some_chart_name,a_feat),
               path = sprintf("charts/%s",a_feat ),
               width =8,
               height =6,
               device='tiff', 
               dpi=3000)
        # plot(pl)
        dev.off()
      })
  })



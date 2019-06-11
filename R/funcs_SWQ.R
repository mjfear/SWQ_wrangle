# Date 20190313 Matt Fear
# Function defined using methods defined SOE SWQ report 2016 - CM record 16206728
# Sec B provides rough insight into parametric contribution to total Factor (F1, F2, F3) contributions. This should later be modified to a dot product treatment to highlight parametric contributions to F2 and F3

func_SWQ_calcWQI <- function(data, guidelines){
  #### Section A:
  data_long <- data %>% 
    gather("parameter", "value", Ammonia:SINitrogen) %>% 
    inner_join(guidelines, by = c("parameter" = "shortParameterName")) %>% 
    ### Lines below to account for seasonal variation in Ammonia guideline value
    mutate(guideline_upper = if_else(parameter != "Ammonia",
                                   guideline_upper,
                                   if_else(Time %>% month() %>% between(4, 10),
                                           0.76, guideline_upper))) %>%
    ### Lines above to account for seasonal variation in Ammonia guideline value 
    mutate(fail = value < guideline_lower | value > guideline_upper,
           excursion = if_else(fail == FALSE, 0,
                               if_else(value > guideline_lower & value > guideline_upper,
                                       value/guideline_upper - 1,
                                       guideline_lower/value - 1))) %>% 
    
    # return(data_long)
    select(-c("guideline_lower", "guideline_upper"))
  
  year_min <- data$Time %>% min %>% year()
  year_max <- data$Time %>% max %>% year()
  
  data_WQI_full <- tibble(yearStart = c(year_min:year_max-2),
                     yearEnd = yearStart + 2,
                     data = list(data_long)) %>% 
    unnest %>% 
    filter(year(Time) >= yearStart & year(Time) <= yearEnd) %>% 
    group_by(yearStart, yearEnd, parameter) 
  
  func_F1_aggregate <- function(vector){
    boo <- if_else(vector > 0, 1, 0)
    boo_summary <- sum(boo) / length(vector) * 100
  }
  
  data_WQI_aggregate <- data_WQI_full %>% 
    summarise(n_sample = n(),
              n_not_na = n_sample - sum(is.na(value)),
              fail_sum = sum(fail, na.rm = TRUE),
              excursion_sum = sum(excursion, na.rm = TRUE)) %>% 
    ungroup() %>%
    nest(parameter:excursion_sum) %>%
    mutate(n_group = map(data, "n_sample") %>% map_dbl(max),
           n_all_samples = map(data, "n_not_na") %>% map_dbl(sum),
           F1_tot = map(data, "fail_sum") %>% map_dbl(func_F1_aggregate),
           F2_tot = map(data, "fail_sum") %>% map_dbl(sum) / n_all_samples * 100,
           excursion_tot = map(data, "excursion_sum") %>% map_dbl(sum),
           nse = map(data, "excursion_sum") %>% map_dbl(sum) / n_all_samples,
           F3_tot = nse / (0.01 * nse + 0.01),
           WQI = 100 - sqrt(F1_tot ** 2 + F2_tot ** 2 + F3_tot ** 2)/1.732) %>%
    #### Section B:
    unnest() %>%
    mutate(F2_param = fail_sum / n_all_samples * 100,
           F3_param = excursion_sum / excursion_tot * F3_tot,
           F2F3 = (F2_param/F2_tot + F3_param/F3_tot) * -1 * 0.5 * (100 - WQI)) %>%
    select(-c(excursion_tot, contains("nse"), n_sample:F3_param)) %>% 
    mutate(parameter = factor(parameter, levels = param_order)) #include to set order of plots
    

}



# Backup code -------------------------------------------------------------

# 
# 
# # Date 20190313 Matt Fear
# # Function defined using methods defined SOE SWQ report 2016 - CM record 16206728
# # Sec B provides rough insight into parametric contribution to total Factor (F1, F2, F3) contributions. This should later be modified to a dot product treatment to highlight parametric contributions to F2 and F3
# 
# func_SWQ_calcWQI <- function(data, guidelines){
#   #### Section A:
#   data_long <- data %>% 
#     gather("parameter", "value", Ammonia:SINitrogen) %>% 
#     inner_join(guidelines, by = c("parameter" = "shortParameterName")) %>% 
#     mutate(fail = value < guideline_lower | value > guideline_upper,
#            excursion = if_else(fail == FALSE, 0,
#                                if_else(value > guideline_lower & value > guideline_upper,
#                                        value/guideline_upper - 1,
#                                        guideline_lower/value - 1))) %>% 
#     select(-c("guideline_lower", "guideline_upper"))
#   
#   year_min <- data$Time %>% min %>% year()
#   year_max <- data$Time %>% max %>% year()
#   
#   data_WQI_full <- tibble(yearStart = c(year_min:year_max-2),
#                           yearEnd = yearStart + 2,
#                           data = list(data_long)) %>% 
#     unnest %>% 
#     filter(year(Time) >= yearStart & year(Time) <= yearEnd) %>% 
#     group_by(yearStart, yearEnd, parameter) 
#   
#   func_F1_aggregate <- function(vector){
#     boo <- if_else(vector > 0, 1, 0)
#     boo_summary <- sum(boo) / length(vector) * 100
#   }
#   
#   data_WQI_aggregate <- data_WQI_full %>% 
#     summarise(n_sample = n(),
#               n_not_na = n_sample - sum(is.na(value)),
#               fail_sum = sum(fail, na.rm = TRUE),
#               excursion_sum = sum(excursion, na.rm = TRUE)) %>% 
#     ungroup() %>%
#     nest(parameter:excursion_sum) %>%
#     mutate(n_group = map(data, "n_sample") %>% map_dbl(max),
#            n_all_samples = map(data, "n_not_na") %>% map_dbl(sum),
#            F1_tot = map(data, "fail_sum") %>% map_dbl(func_F1_aggregate),
#            F2_tot = map(data, "fail_sum") %>% map_dbl(sum) / n_all_samples * 100,
#            nse = map(data, "excursion_sum") %>% map_dbl(sum) / n_all_samples,
#            F3_tot = nse / (0.01 * nse + 0.01),
#            WQI = 100 - sqrt(F1_tot ** 2 + F2_tot ** 2 + F3_tot ** 2)/1.732) %>% 
#     #### Section B:       
#     unnest() %>%
#     mutate(ratio = (100 - WQI) / (F1_tot + F2_tot + F3_tot),
#            F1 = if_else(fail_sum > 0, 1/9, 0) / 3,
#            F2 = fail_sum / n_all_samples / 3,
#            nse_param = excursion_sum / n_all_samples,
#            F3 = nse_param / (0.01 * nse_param + 0.01) / 300,
#            param_contribution = (F1 + F2 + F3) / (100 - WQI))  %>% 
#     select(-contains("nse"), -n_sample:-excursion_sum) 
#   
# }

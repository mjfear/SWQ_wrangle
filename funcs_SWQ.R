func_SWQ_calcWQI <- function(data, guidelines){
  data_long <- data %>% 
    gather("parameter", "value", Ammonia:SINitrogen) %>% 
    inner_join(guidelines, by = c("parameter" = "shortParameterName")) %>% 
    mutate(fail = value < guideline_lower | value > guideline_upper,
           excursion = if_else(fail == FALSE, 0,
                               if_else(value > guideline_lower & value > guideline_upper,
                                       value/guideline_upper - 1,
                                       guideline_lower/value - 1))) %>% 
    select(-c("guideline_lower", "guideline_upper"))
  
  year_min <- data$Time %>% min %>% year()
  year_max <- data$Time %>% max %>% year()
  
  data_WQI_full <- tibble(yearStart = c(year_min:year_max-2),
                     yearEnd = yearStart + 2,
                     data = list(data_long)) %>% #changed to long format
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
           nse = map(data, "excursion_sum") %>% map_dbl(sum) / n_all_samples,
           F3_tot = nse / (0.01 * nse + 0.01),
           WQI = 100 - sqrt(F1_tot ** 2 + F2_tot ** 2 + F3_tot ** 2)/1.732)
  ### Come back to look at parameter specific contribution later.
  # ,
  #          ratio = (100 - WQI) / (F1_tot + F2_tot + F3_tot)) %>%
  #   unnest() %>% 
  #   mutate(F2 = fail_sum / n_not_na * ratio,
  #          F3 = excursion_sum / n_not_na * ratio)
    #-starts_with("n_"), 
    # rename(F2 = fail_sum / n_not_na, F3 = excursion_sum) #%>%
    # nest(F2:F3) %>%
    # spread(parameter, data) %>%
    # unnest(.sep = "_")
  
  # ratio <- X2/X1
  # X1 <-  sum(F1) + sum(F2) + sum(F3)
  # X2 <- 100 - WQI

  # return(data_wide)
  # return(data_WQI_full)
  return(data_WQI_aggregate)
  # 
  # # data_WQI_aggregate <- data_WQI_full %>%
  # #   select(-contains("value")) %>% 
  # #   gather(key = "key", value = "value", Ammonia_F2:Turbidity_F3) %>% 
  # #   group_by(yearStart, yearEnd) #%>%
  # #   gather()
  # #   summarise(n_sample = n(),
  # #             F2 = )
  # #   
  # # data_WQI_summary
  # 
  # 
  # 
  # # data_wide <- data_long %>%
  # #   select(Time:value, F2 = fail, F3 = excursion) %>%
  # #   nest(value:F3) #%>%
  # # spread(parameter, data) %>%
  # # unnest(.sep = "_")
  # 
  # 
  # 
  # 
  # boos.df <- data %>%
  #   select(contains())
  # 
  # return(boos.df)
  # 
  # names(boos.df) <- names(guide.lines)
  # 
  # func_totaltests <- function(tests.vector){sum(!is.na(tests.vector))}
  # 
  # total.tests <- boos.df %>% 
  #   map_dbl(.,func_totaltests)
  # 
  # # F2 <- boos.df %>% 
  # #   map_dbl(.,sum,na.rm = "True")/sum(total.tests) *100
  # # 
  # F2 <- boos.df %>% 
  #   map_dbl(.,sum,na.rm = "True")/sum(total.tests) *100 #%>% 
  # 
  # # print(names(F2))
  # # print(names(guide.lines))
  # #set_names(.,nm = names(guide.lines))
  # # return(F2)
  # 
  # F1 <- F2 %>% {ifelse(.>0,1,0)}
  # F1 <- F1/length(F1) *100
  # 
  # #F2 <- sum(F2) ## Comment this line out if wanting parameter specific components of 
  # 
  # func_SWQ_calcF3 <- function(vector.in,vector.boo,guide.lines){
  #   true.rows=which(vector.boo == T, arr.ind = T)
  #   vector.out <- double(length(vector.in))
  #   for(i in true.rows){
  #     if (vector.in[i]>guide.lines[2]){
  #       vector.out[i] <- (vector.in[i]/guide.lines[2])-1
  #     } 
  #     else if (vector.in[i]<guide.lines[1]){
  #       vector.out[i] <- (guide.lines[1]/vector.in[i])-1
  #     }
  #   }
  #   vector.out <- sum(vector.out,na.rm = "True")
  #   return(vector.out)
  # }
  # 
  # excursion <- pmap_dbl(
  #   list(select(tbl.in,one_of(colnames(guide.lines))),
  #        select(tbl.in,one_of(paste0(colnames(guide.lines),"_boo"))),
  #        guide.lines),
  #   func_SWQ_calcF3
  # )
  # #nse <- sum(excursion)/sum(total.tests)
  # nse <- (excursion)/sum(total.tests)
  # 
  # F3_param <- nse/(0.01*nse+0.01)
  # F3 <- nse/(0.01*sum(nse)+0.01)
  # 
  # WQI <- 100-(sqrt((sum(F1))^2 + (sum(F2))^2 + (sum(F3))^2)/1.732) 
  # 
  # X1 <-  sum(F1) + sum(F2) + sum(F3)
  # X2 <- 100 - WQI
  # 
  # ratio <- X2/X1
  # 
  # F1_param <- F1 * ratio
  # F2_param <- F2 * ratio
  # F3_param <- F3 * ratio
  # Ftot_param <- (F1+F2+F3) * ratio #add this
  # 
  # names(F1_param) <- paste0("F1_",names(F1_param))
  # names(F2_param) <- paste0("F2_",names(F2_param))
  # names(F3_param) <- paste0("F3_",names(F3_param))
  # names(Ftot_param) <- paste0("Ftot_",names(Ftot_param)) #add this
  # 
  # # print(nrow(tbl.in))
  # completeSet <- tbl.in %>% {ifelse(nrow(.)<30,FALSE,TRUE)}
  # 
  # return.list <- list(WQI=WQI, completeSet=completeSet, F1=F1_param, F2=F2_param, F3=F3_param, Ftot=Ftot_param) #add last
  # 
  # return(return.list)
}

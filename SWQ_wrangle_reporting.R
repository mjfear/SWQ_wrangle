# Prereqs and definitions -------------------------------------------------
library(Hmisc)
library(lubridate)
library(Hilltop)
library(hillr)
library(tidyverse)

datestr <- stamp("24/02/1985")

endpoint <- "http://hydro.marlborough.govt.nz/soewq.hts?"
# endpoint <- "http://hydro.marlborough.govt.nz/mdc.hts?"

source("funcs_SWQ.R")

yearReport <- 2016
yearNow <- today() %>% year

runReports <- TRUE
param_order <- c("Turbidity", "SINitrogen", "Phosphorus", "Ecoli", "pH", "Oxygen", "Temperature", "Ammonia", "Nitrate")

# Wrangle data ------------------------------------------

sites_list <- read_csv("data/siteText.csv") %>% 
  select(hilltopName:reportRegion)

sites <- sites_list %>% 
  left_join(getHilltopSites(endpoint), by = c("hilltopName" = "site"))
  
parameters_list <- read_csv("data/parameterText.csv") %>% 
  mutate(parameterUnits = str_replace_na(parameterUnits, replacement = "")) 
  

measurement_query <-  parameters_list %>% 
  select(hilltopMeasurementName) %>% filter(is.na(.) == FALSE) %>% pull

guidelines_tbl <- parameters_list %>% 
  select(shortParameterName, guideline_lower, guideline_upper) %>% 
  filter(is.na(guideline_lower) == FALSE | is.na(guideline_upper) == FALSE)

measurements_raw <- sites_list %>% 
  group_by(hilltopName, sitePublicName, reportRegion) %>% 
  mutate(data = map(.x = hilltopName, .f = fullGetHilltopData, 
                    endpoint = endpoint, measurements = measurement_query, 
                    from = "01/01/2007", to = datestr(today()), 
                    option = "WQ")) %>% 
  unnest() %>% ungroup %>% select(-hilltopName)

measurements <- measurements_raw %>% 
  select(sitePublicName:Time, Measurement, Value, QualityCode, QAComment = `QA Comment`, Project, SampleID = `Sample ID`) %>% 
  mutate(Value = Value %>% str_remove(">|<") %>% as.double(),
         QualityCode = QualityCode %>% as.integer()) %>% 
  left_join(parameters_list %>% select(hilltopParameterName, parameter = shortParameterName), 
              by = c("Measurement" = "hilltopParameterName")) %>% 
  select(-Measurement) %>% 
  filter(Project == "SW SoE" | is.na(Project) == TRUE) 

measurements_spread <- measurements %>% 
  filter(QualityCode >= 500) %>% 
  select(-QualityCode, -QAComment) %>% 
  spread(parameter, Value) %>%
  mutate(SINitrogen = Ammonia + Nitrite) %>% 
  select(-Nitrite) %>% 
  arrange(Site, Time)

measurements_long <- measurements_spread %>% 
  gather(param, value, Ammonia:SINitrogen) %>% 
  arrange(Site, Time)

measurements_WQI_long <- measurements_spread %>%
  group_by(sitePublicName, reportRegion, Site) %>% 
  nest %>% 
  mutate(WQI_calcs = map(data, func_SWQ_calcWQI, guidelines_tbl)) %>% 
  unnest(WQI_calcs) %>% 
  spread(parameter, F2F3) %>% 
  mutate(param_contrib = 100 - WQI) %>% 
  gather(param, value, WQI:Nitrate) %>% 
  mutate(isnotWQI = if_else(param != "WQI", "Parameter Contribution", "WQI") %>% factor(levels = c("WQI", "Parameter Contribution")))

# measurements_WQI_wide <- measurements_WQI_long %>%
#   spread(parameter, F2F3) %>% 
#   gather(key, value, WQI:Nitrate) %>% 
#   spread(key, value)
# %>%  unnest(.sep = "_")

# Compare data with reported values ---------------------------------------

# measurements_WQI_HTlive <- measurements_WQI_wide
# measurements_WQI_SWQClean <- measurements_WQI_wide
# 
# reportedWQI <- read_csv("data/reportedWQIs.csv", col_names = TRUE) %>%
#   gather(key = yearStart, value = reportedWQI, "2007":"2013") %>%
#   rename(sitePublicName = publicName) %>%
#   mutate(yearStart = yearStart %>% as.double())
# 
# compare_WQI_HTlive <- measurements_WQI_HTlive %>%
# # compare_WQI_SWQClean <- measurements_WQI_SWQClean %>%
#   left_join(sites_list) %>%
#   left_join(reportedWQI) %>%
#   select(-hilltopName, -starts_with("contrib_")) %>%
#   mutate(difference = (WQI-reportedWQI)/reportedWQI) %>%
#   filter(abs(difference) > 0.02 & n_all_samples >= 30 * 9) %>%
#   arrange(desc(difference))

# compare_WQI_HTlive %>%
#   ggplot(aes(x = yearStart, y = difference, colour = sitePublicName, fill = sitePublicName)) +
#     geom_point() +
#     geom_line() +
#     facet_wrap(~reportRegion)

# # compare_WQI_SWQClean %>% write_csv("output/SWQClean_compareWQI.csv")
# compare_WQI_HTlive %>% write_csv("output/HTlive_compareWQI.csv")

# Data visualisation ------------------------------------------------------

reportTheme <- theme_grey()
WQI_boundaries <- geom_hline(yintercept = c(45,65,80,95), colour = c("orange","gold","green", "lightblue"), size = c(1, 1, 1, 0.5))

param_colours <- parameters_list %>% select(parameterColours) %>% filter(parameterColours %>% is.na == F) %>% pull
param_names <- parameters_list %>% select(shortParameterName) %>% filter(shortParameterName != "Nitrite") %>% pull
param_colours <- setNames(param_colours, param_names)

param_colour_scale <- scale_fill_manual(drop = FALSE, values = param_colours)

# All sites WQI
measurements_WQI_long %>% 
  group_by(sitePublicName, yearStart) %>% 
  filter(yearEnd == yearReport) %>% 
  ggplot(aes(x = reorder(sitePublicName, desc(param_contrib)), y = abs(value), fill = param)) + #add/remove abs to vary plot
    coord_flip() +
    geom_col(width = 0.5) +
    # geom_col(aes(y = (WQI)/9), fill = "blue", width = 0.5) +
    WQI_boundaries +
    scale_fill_manual(drop = FALSE, breaks = c("WQI", names(param_colours)), 
                      values = c(WQI = "blue", param_colours)) + 
    scale_y_continuous(breaks = seq(-100, 100, 10), position = "bottom",
                       sec.axis = sec_axis(~.-100, breaks = seq(-100, 0, 10), name = "Parameter subtraction from WQI")) +
    labs(y = paste0("Water Quality Index "), x = NULL) +
    theme(legend.title = element_blank())

# All sites WQI - facet grid
measurements_WQI_long %>% 
  filter(yearEnd == yearReport) %>% 
  ggplot(aes(x = reorder(sitePublicName, desc(param_contrib)), y = value, fill = param)) +
  geom_col(position = "stack", width = 0.5) +
  coord_flip() +
  scale_fill_manual(drop = FALSE, breaks = c("WQI", names(param_colours)), 
                    values = c(WQI = "blue", param_colours)) + 
  # scale_y_continuous(breaks = seq(-70, 0, 10)) +
  labs(y = NULL, x = NULL) +
  reportTheme + 
  facet_grid(. ~ isnotWQI, scale = "free", space = "free") +
  geom_hline(data = data.frame(yin = c(45,65,80,95), 
                               isnotWQI = factor("WQI", levels = c("WQI", "Parameter Contribution"))), 
             aes(yintercept = yin), colour = c("orange","gold","green", "lightblue"), size = c(1, 1, 1, 0.5)) 




# # Select region WQI
boxPlot_params <- param_order[1:6]
guidelines_low <- guidelines_tbl$guideline_lower %>% set_names(nm = guidelines_tbl$shortParameterName)
# guidelines_low                                                                                                                                                                 [guidelines_low < 0.01] <- -Inf
guidelines_high <- guidelines_tbl$guideline_upper %>% set_names(nm = guidelines_tbl$shortParameterName)

measurements_long %>%
  filter(year(Time) >= yearReport - 3 & year(Time) <= yearReport - 1) %>% 
  filter(reportRegion == "South Marlborough" & param %in% boxPlot_params) %>%
  ggplot(aes(x = sitePublicName, y = abs(value), fill = param, color = param)) +
  stat_summary(geom = "linerange", fun.data = median_hilow, fun.args = list(conf.int = 1), size = 0.75) +
  geom_boxplot(outlier.shape = NA, alpha = 0.2, size = 0.75) +
  geom_hline(data = data.frame(yin = guidelines_high, param = names(guidelines_high)), aes(yintercept = yin, col = param)) + 
  coord_flip() +
  scale_color_manual(name = "Measured Parameter", breaks = c(names(param_colours)), 
                     values = c(param_colours), aesthetics = c("colour", "fill")) +
  # scale_fill_manual(name = "Measured Parameter", breaks = c(names(param_colours)), values = c(param_colours)) + 
  theme(axis.title = element_blank()) +
  facet_wrap(. ~ param, scales = "free_x", nrow = 3) 
  



  # geom_col(aes(y = (WQI)/9), fill = "blue", width = 0.5) +
  # WQI_boundaries +
  # scale_fill_manual(drop = FALSE, values = param_colours) +
  # scale_y_continuous(breaks = seq(0, 100, 10),
  #                    sec.axis = sec_axis(~.,
  #                                        breaks = seq(-60, 0, 10),
  #                                        name = "Parameter subtraction from WQI"),) +
  # labs(y = paste0("Water Quality Index "), x = NULL) +
  # facet_wrap(~reportRegion)
  # theme(axis.title.x.bottom = element_text(hjust = 0.75),
  #       axis.title.x.top = element_text(hjust = 0., colour = "grey"))



# older coding ------------------------------------------------------------
# 
# mutate(data = hilltopName %>% 
#            safely(map_df(hilltopName, getHilltopMeasurements(endpoint, hilltopName))))
#   
# getHilltopMeasurements(endpoint, sites_list[1,1])
# ### check data can be got ### 
# 
# sites <- getHilltopSites(endpoint = endpoint)
# 
# n <- 1 + n
# colNamesBoo <- TRUE
# savePath <- now() %>% 
# {paste0("output/log_",year(.), str_pad(month(.),width=2, side="left", pad="0"),str_pad(day(.),width=2, side="left", pad="0"),"_",n,".csv")}
# 
# for(i in 31:100){
#   measurements <- getHilltopMeasurements(endpoint = endpointMdc, site = sites[i,1])
#   measurementsFiltered <- measurements %>% 
#     filter(MeasurementName != "Item2")
#   for(j in 1:length(measurementsFiltered$MeasurementName)){
#     if(exists("errorOut")==TRUE){rm(errorOut)}
#     # print("pause")
#     Sys.sleep(0.2)
#     # print("start")
#     errorOut <- fullGetHilltopData(endpoint = endpointMdc,
#                                    site = sites[i,1],
#                                    measurements = measurementsFiltered$MeasurementName[j])
#     if(dim(errorOut)[1]>0){
#       printStr <- paste0(sites[i,1], "(i=",i,") and measurement ", measurements[j,1], "j=",j," return a value from Webserver")
#       errorStr <- "Return OK"
#       errorCode <- 0
#     } else {
#       printStr <- paste0(sites[i,1], " and measurement ", measurements[j,1], " do not return a value from Webserver")
#       errorStr <- "Data returned from webserver is empty"
#       errorCode <- 1
#     }
#       print(printStr)
#       saveStr <- tibble(siteName=sites[i,1], measurement=measurementsFiltered$MeasurementName[j], errorCode=errorCode, errorString=errorStr)
#       write_csv(saveStr, savePath, append=TRUE, col_names = colNamesBoo)
#       if(colNamesBoo==TRUE){colNamesBoo <- FALSE}
#     
#   }
# }
# 
# 
# 
# 
# measurementsBySite <- tibble(siteName=sites[,1]) %>%
#   mutate(measurementsData=map(.x=siteName, .f=getHilltopMeasurements, endpoint = endpointMdc)) 
# 
#  
# # commonMeasurementString <- measurementsBySite$measurementsData %>% 
# #   map(select, datasource) %>% 
# #   reduce(intersect) %>% 
# #   pull ## Find the common datasource names between all sites listed in sites, so that common measurements for sites list can be queried by fullGetHilltopData
# 
# 
# #Having built site list and common measurements list, query this subset from Hilltop.
# data_WQ <- fullGetHilltopData(endpoint = endpointMdc,
#                              sites = sites$site,#measurementsBySite$siteName,
#                              measurements = "pH",#commonMeasurementString,
#                              from = "1/1/1907",
#                              to = "1/1/19",
#                              option = "None")
# 
# dataTest <- fullGetHilltopData(endpoint = endpointMdc,
#                            site = "Opawa River at Elizabeth St",
#                            measurement = "Conductivity (Field)", #pH
#                            from = "1/1/1907",
#                            to = "1/1/19",
#                            option = "None")
# 
# ### Wrangle ### 
# ## wrangle the dataWQ
# 
# data_tidy <- data_WQ %>% 
#   select(Site, Time, Measurement, Value, `Sample ID.y`) %>% 
#   spread(Measurement, Value) %>% 
#   arrange(Time) %>% 
#   group_by(Site) %>% 
#   nest
# 
# dataTest_tidy <- dataTest %>% 
#   arrange(Time) %>% 
#   group_by(Site) %>% 
#   nest
# 
# 
# ## test the data
# testTimeStamp <- function(data){
#   Time <- data$Time
#   errorsList <- if_else(as.numeric(Time-lag(Time))<3600*24,TRUE,FALSE)
#   if(sum(errorsList, na.rm=TRUE)==0){
#     return(list(errorCount=as.integer(0),errorText="No errors"))}
#   else{
#     errorInfo <- which(errorsList)
#     errorString <- paste0("Check entries circa ", Time[errorInfo])
#     return(errorCount=sum(errorsList, na.rm = TRUE), errorText=errorString)
#   }
# }
# 
# # testTimeStamp(dataWQ_tidy$data[[1]]$Time)
#   
# data_results <- dataTest %>% 
#   mutate(errorAlert=map(data, testTimeStamp))
# 
# testTimeStamp(dataTest)
# 

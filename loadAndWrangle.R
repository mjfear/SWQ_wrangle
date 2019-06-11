# Wrangle pre-requisite ---------------------------------------------------

sites_list <- read_csv("siteText.csv") %>% 
  select(hilltopName:reportRegion)

sites <- sites_list %>% 
  left_join(getHilltopSites(endpoint), by = c("hilltopName" = "site"))

parameters_list <- read_csv("parameterText.csv") %>% 
  mutate(parameterUnits = str_replace_na(parameterUnits, replacement = ""),
         parameterLabel = str_c(parameterPublicName, 
                                if_else(parameterUnits == "", 
                                        "", 
                                        str_c(" [", parameterUnits, "]")))) 


# Query data from hilltop webservice --------------------------------------

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


# Wrangle data ------------------------------------------------------------


measurements <- measurements_raw %>% 
  select(sitePublicName:Time, Measurement, Value, QualityCode, 
         QAComment = `QA Comment`, Project, SampleID = `Sample ID`) %>% 
  mutate(Value = Value %>% str_remove(">|<") %>% as.double(),
         QualityCode = QualityCode %>% as.integer()) %>% 
  left_join(parameters_list %>% 
              select(hilltopParameterName, 
                     parameter = shortParameterName), 
            by = c("Measurement" = "hilltopParameterName")) %>% 
  select(-Measurement) %>% 
  filter(Project == "SW SoE" | is.na(Project) == TRUE) 

measurements_spread <- measurements %>% 
  filter(QualityCode >= quality_code_acceptable_minimum | 
           is.na(QualityCode) == TRUE) %>% 
  select(-QualityCode, -QAComment) %>% 
  spread(parameter, Value) %>%
  mutate(SINitrogen = Ammonia + Nitrite) %>% 
  select(-Nitrite) %>% 
  arrange(Site, Time) 


# Filter none SoE manually identified -------------------------------------
# section to filter manually entered samples - should be automated later

removeSampleNo <- c(20141098) #Kaituna spurious March 2014 - this needs to be cleaned from dataset or identified as not SoE data somehow
measurements_spread_filtered <- measurements_spread %>% 
  filter(!SampleID %in% removeSampleNo)


# Continue wrangle --------------------------------------------------------

measurements_long <- measurements_spread_filtered %>% 
  gather(param, value, Ammonia:SINitrogen) %>% 
  arrange(Site, Time) %>% 
  left_join(parameters_list %>% select(shortParameterName, parameterLabel), 
            c("param" = "shortParameterName"))

measurements_WQI_long <- measurements_spread_filtered %>% 
  filter(!SampleID %in% removeSampleNo) %>%
  group_by(sitePublicName, reportRegion, Site) %>% 
  nest %>% 
  mutate(WQI_calcs = map(data, func_SWQ_calcWQI, guidelines_tbl)) %>% 
  unnest(WQI_calcs) %>% 
  spread(parameter, F2F3) %>% 
  mutate(param_contrib = 100 - WQI) %>% 
  gather(param, value, WQI:Nitrate) %>% 
  mutate(isnotWQI = if_else(param != "WQI", "Parameter Contribution", "WQI") %>% 
           factor(levels = c("WQI", "Parameter Contribution"))) %>% 
  arrange(Site, yearStart) %>% 
  left_join(parameters_list %>% select(shortParameterName, parameterLabel), 
            c("param" = "shortParameterName"))

if(exportWrangle){
  saveRDS(measurements_spread, "output/wrangled_all_spread.RDS")
  saveRDS(measurements_WQI_long, "output/wrangled_WQI.RDS")
}

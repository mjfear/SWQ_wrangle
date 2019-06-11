# Visualisation pre-requisites --------------------------------------------

if(loadFromSaved){measurements_spread <- readRDS("output/wrangled_all_spread.RDS")
measurements_WQI_long <- readRDS("output/wrangled_WQI.RDS")}

reportTheme <- theme_grey()

WQI_boundaries <- data.frame(cat = c("Poor", "Marginal", "Fair", "Good",  "Excellent"),
                             cat_low = c(0, 45, 65, 80, 95),
                             cat_high = c(45, 65, 80, 95, 100)
                             # cat_colour = c("red", "orange","gold","green", "lightblue")
)

# WQI_boundaries <- geom_hline(yintercept = c(45,65,80,95), colour = c("orange","gold","green", "lightblue"), size = c(1, 1, 1, 0.5))

param_colours <- parameters_list %>% 
  select(parameterColours) %>% 
  filter(parameterColours %>% is.na == F) %>%
  pull

param_names <- parameters_list %>% 
  select(shortParameterName) %>% 
  filter(shortParameterName != "Nitrite") %>% 
  pull

param_colours <- setNames(param_colours, param_names)

param_colour_scale <- scale_fill_manual(drop = FALSE, values = param_colours)

saveFig <- function(fname){
  ggsave(paste0("figs/", fname, filestampstr(today()),".pdf"), 
         width = 16.54, height = 11.57, units = "in", dpi = "retina")}



# Graphs ------------------------------------------------------------------


# All sites WQI -----------------------------------------------------------

measurements_WQI_long %>% 
  group_by(sitePublicName, yearStart) %>% 
  filter(yearEnd == yearReport-1) %>% 
  ggplot(aes(x = reorder(sitePublicName, desc(param_contrib)), 
             y = abs(value), 
             fill = param)) + #add/remove abs to vary plot
  coord_flip() +
  geom_col(width = 0.5) +
  geom_hline(data = WQI_boundaries[2:5,], #aes(yintercept = cat_low, colour = cat), 
             aes(yintercept = cat_low, colour = cat), 
             size = c(1,1,1,0.4)
             # colour = c("Marginal" = "orange", "Fair" = "gold", "Good" = "green", "Excellent" = "lightblue")
  ) +
  scale_fill_manual(drop = FALSE, breaks = c("WQI", names(param_colours)), 
                    values = c(WQI = "blue", param_colours)) + 
  scale_y_continuous(breaks = seq(-100, 100, 10), position = "bottom",
                     sec.axis = sec_axis(~.-100, breaks = seq(-100, 0, 10), 
                                         name = "Parameter subtraction from WQI")) +
  labs(y = paste0("Water Quality Index "), x = NULL) +
  theme(legend.title = element_blank()) 

if(saveGraphs){saveFig("fig_AllWQI_")}

  

# All sites WQI - facet grid ----------------------------------------------

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

if(saveGraphs){saveFig("fig_AllWQI_facet_")}

# Select region WQI -------------------------------------------------------

boxPlot_params <- param_order#[1:6]

guidelines_low <- guidelines_tbl$guideline_lower %>% set_names(nm = guidelines_tbl$shortParameterName)
guidelines_low[guidelines_low < 0.01] <- NA
guidelines_high <- guidelines_tbl$guideline_upper %>% set_names(nm = guidelines_tbl$shortParameterName)
guidelines_high["Oxygen"] <- NA
param_labels <- parameters_list$parameterLabel %>% set_names(nm = parameters_list$shortParameterName)

measurements_long %>%
  filter(year(Time) >= yearReport - 3 & year(Time) <= yearReport - 1) %>%
  filter(reportRegion == "South Marlborough" & param %in% boxPlot_params) %>%
  ggplot(aes(x = sitePublicName, y = abs(value), fill = param, color = param)) +
  stat_summary(geom = "linerange", fun.data = median_hilow, fun.args = list(conf.int = 1), size = 0.75) +
  geom_boxplot(outlier.shape = NA, alpha = 0.2, size = 0.75) +
  geom_hline(data = data.frame(yin = guidelines_high, param = names(guidelines_high)), aes(yintercept = yin, col = param)) +
  geom_hline(data = data.frame(yin = guidelines_low, param = names(guidelines_low)), aes(yintercept = yin, col = param)) +
  coord_flip() +
  scale_color_manual(name = "Parameter", breaks = c(names(param_colours)),
                     values = c(param_colours), aesthetics = c("colour", "fill")) +
  theme(axis.title = element_blank()) +
  facet_wrap(. ~ param, scales = "free_x", nrow = 3, labeller = labeller(param = param_labels))

if(saveGraphs){saveFig("fig_BoxPlot_")}


# WQI subtraction by region -----------------------------------------------

measurements_WQI_long %>%
  filter(param == "WQI") %>%
  ggplot(aes(x = yearEnd, y = 100 - param_contrib, colour = sitePublicName)) +
  geom_point() +
  geom_line() + 
  facet_wrap(~reportRegion)

if(saveGraphs){saveFig("fig_WQIbyRegion_")}


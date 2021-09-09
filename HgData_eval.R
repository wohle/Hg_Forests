
require(dplyr)
require(ggplot2)
require(modern)
require(ggpubr)
require(latex2exp)
require(ggpmisc)
require(grid)

# --------------------------------------------------------------------------

# Read data from Zenodo repository

text_connect <- 
  readLines("https://zenodo.org/record/5495180/files/FoliarHg.csv?download=1")
skip_info <- text_connect[-2] #skip explanatory row

dat <- read.csv(textConnection(skip_info),
                header = TRUE, stringsAsFactors = FALSE)

# Store explanatory information on parameters in dat in separate data frame

data_explanation <- read.csv(textConnection(text_connect[1:2]), sep = ",")
parameter <- colnames(data_explanation)
info <- as.character(data_explanation[1,])
data_explanation <- data.frame(parameter, info)


#Assign correct sampling day DOY for current-season 
#needle samples harvested in winter after 31st Dec

for (i in 1:length(dat$Sampling_date_DOY)) {
  if(dat$Sampling_date_DOY[i] < 50){
    dat$Sampling_date_DOY[i] <- 
      dat$Sampling_date_DOY[i] + 356
  }
}

#Convert sampling date to date
dat$Sampling_date <- as.Date(dat$Sampling_date, "%d/%m/%Y")

# --------------------------------------------------------------------------

# Calculate additional parameters: 
# proportion of daytime hours exceeding a VPD threshold to all daytime hours

dat <- dat %>% dplyr::mutate(prop_dayVPD_1.2kPa =
                  (exhrs_dayVPD_1.2kPa/12)/Sampling_interval_d,
                prop_dayVPD_1.6kPa = 
                  (exhrs_dayVPD_1.6kPa/12)/Sampling_interval_d,
                prop_dayVPD_2kPa = 
                  (exhrs_dayVPD_2kPa/12)/Sampling_interval_d,
                prop_dayVPD_3kPa =
                  (exhrs_dayVPD_3kPa/12)/Sampling_interval_d)

# Summarize data

#data has to aggregated (averaged) by forest site in order to perform 
#correlations with site-specific parameters
#create dat_comp from data set

dat_comp <- dat %>% dplyr::group_by(Sampling_year, Site_name, Species,
                                    Needle_age_class) %>%
  dplyr::summarize(Avg_Hg_daily = mean(Hg_daily, na.rm = T),
                   Std_Hg_daily = sd(Hg_daily, na.rm = T),
                   n_Samples = length(Hg_daily),
                   Avg_Hg_conc_ng_g = mean(Hg_ng_g, na.rm = T),
                   Std_Hg_conc_ng_g = sd(Hg_ng_g, na.rm = T),
                   Country = first(Country),
                   ICPF_country_plot = first(ICPF_country_plot),
                   Latitude = first(Latitude), 
                   Longitude = first(Longitude),
                   Sampling_date_DOY = first(Sampling_date_DOY),
                   Species_short = first(Species_short),
                   Foliage_type = first(Foliage_type),
                   Altitude_m = first(Altitude_m),
                   Sampling_interval_d = first(Sampling_interval_d),
                   BeginGS_final_DOY = first(BeginGS_final_DOY),
                   GLEAM_transpiration_avg = first(GLEAM_transpiration_avg),
                   exhrs_dayVPD_1.2kPa = first(exhrs_dayVPD_1.2kPa),
                   exhrs_dayVPD_1.6kPa = first(exhrs_dayVPD_1.6kPa),
                   exhrs_dayVPD_2kPa = first(exhrs_dayVPD_2kPa),
                   exhrs_dayVPD_3kPa = first(exhrs_dayVPD_3kPa),
                   prop_dayVPD_1.2kPa = first(prop_dayVPD_1.2kPa),
                   prop_dayVPD_1.6kPa = first(prop_dayVPD_1.6kPa),
                   prop_dayVPD_2kPa = first(prop_dayVPD_2kPa),
                   prop_dayVPD_3kPa = first(prop_dayVPD_3kPa),
                   prop_hours_low_wc = first(prop_hours_low_wc),
                   ERA5Land_avgTemp_C = first(ERA5Land_avgTemp_C))
#ICP Forests proprietory data can be obtained from
#the ICP Forests Database (http://icp-forests.net/page/data-requests) upon 
#request from the Programme Co-ordinating Center (PCC) in Eberswalde, Germany.
#This concerns the main tree species on the forest plot (Main_tree_species),
#average age of trees on plot (Mean_age_years), the average diameter at breast
#height (DBH) of trees on plot, the basal area of each plot (Basal_area), the
#number of trees per hectare on plot (Trees_per_hectare), and the respective
#soil texture at each plot (Soil_texture)
                  # Main_tree_species = first(Main_tree_species),
                  # Mean_age_years = first(Mean_age_years),
                  # Mean_DBH = mean(DBH, na.rm = T),
                  # Basal_area = first(Basal_area),
                  # Trees_per_hectare = first(Trees_per_hectare),
                  # Soil_texture = first(Soil_texture))

# --------------------------------------------------------------------------

# Separate data set into sub-data sets of interest

#Filter for foliage of the current-season (needle y0)
dat_y0 <- dat %>% dplyr::filter(Needle_age_class == 0)

#Current-season data aggregated by forest site
dat_comp_y0 <- dat_comp %>% dplyr::filter(Needle_age_class == 0)

# --------------------------------------------------------------------------

#Calculate median Hg values per tree species group

sum_dat_comp_y0 <- dat_comp_y0 %>%
  dplyr::group_by(Species_short) %>%
  dplyr::summarise(Foliage_tpye = first(Foliage_type),
                   Median_Hg_daily = median(Avg_Hg_daily),
                   Min_Hg_daily = min(Avg_Hg_daily),
                   Max_Hg_daily = max(Avg_Hg_daily),
                   n = length(Avg_Hg_daily))


#Calculate median Hg values of needle samples, current-season (y0)
needles_y0 <- dat_comp_y0 %>%
  dplyr::filter(Foliage_type == "needle") 
median_Hg_needles_y0 <- median(needles_y0$Avg_Hg_daily, na.rm = T)
remove(needles_y0)

#Calculate median Hg values of leaf samples
leaves <- dat_comp_y0 %>%
  dplyr::filter(Foliage_type == "leaf") 
median_Hg_leaves <- median(leaves$Avg_Hg_daily, na.rm = T)
remove(leaves)

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

# The following code section can only be run after obtaining the necessary
# ICP Forests proprietory data (foliar nitrogen concentrations, leaf mass area) 
# in the ICP Forests Database (http://icp-forests.net/page/data-requests) 
# from the Programme Co-ordinating Center (PCC) in Eberswalde, Germany 

# # Analysis of leaf nitrogen contents
# 
# #Leaf N changes at the beginning and end of the growing season
# #-> solely include samples harvested during a relatively
# #stable leaf N period: 1st July - 31st Aug (broad leaves); 
# #1st Sept - winter (needles)
# 
# leaves_stableN <- dat_y0 %>% dplyr::filter(Foliage_type == "leaf") %>%
#   dplyr::filter(between(Sampling_date_DOY, 182, 243))
# needles_stableN <- dat_y0 %>% dplyr::filter(Foliage_type == "needle") %>%
#   dplyr::filter(Sampling_date_DOY > 244)
# 
# dat_y0_stableN <- rbind(leaves_stableN, needles_stableN) %>%
#   dplyr::filter(Species_short != "larch") #remove larch since there is only
# #one measurement available
# remove(leaves_stableN, needles_stableN)
# 
# #Average leaf N and daily foliar Hg uptake per tree species
# Avg_leafN <- dat_y0_stableN %>% dplyr::group_by(Species_short) %>%
#   dplyr::summarise(Avg_leafN = mean(Ntot_mg_g, na.rm = T),
#                    Sd_leafN = sd(Ntot_mg_g, na.rm = T),
#                    Avg_Hgdaily = mean(Hg_daily, na.rm = T),
#                    Sd_Hgdaily = sd(Hg_daily, na.rm = T),
#                    n_samples = n())
# 
# # ----------------------
# 
# #Simple linear regression by tree species: 
# #daily foliar Hg uptake ~ leaf N;
# #only current-season (y0) needles
# lm_Hg_N_perspecies <- dat_y0_stableN %>%
#   dplyr::group_by(Species_short) %>%
#   do(model = lm(Hg_daily ~ Ntot_mg_g, data = .)) %>%
#   dplyr::mutate(coef_HgmassN = 0) %>%
#   dplyr::mutate(R2_mass = 0) %>% dplyr::mutate(n_fit = 0) %>%
#   dplyr::mutate(p_mass = 0) %>% dplyr::mutate(BP_test = 0)
# 
# lm_Hg_N_perspecies$coef_HgmassN <- 
#   lapply(lm_Hg_N_perspecies$model, function(x) summary(x)$coefficients[2])
# lm_Hg_N_perspecies$R2_mass <- 
#   lapply(lm_Hg_N_perspecies$model, function(x) summary(x)$r.squared)
# lm_Hg_N_perspecies$n_fit <-
#   lapply(lm_Hg_N_perspecies$model, function(x) nobs(x))
# lm_Hg_N_perspecies$n_fit <- unlist(lm_Hg_N_perspecies$n_fit)
# lm_Hg_N_perspecies$p_mass <-
#   lapply(lm_Hg_N_perspecies$model, function(x) summary(x)$coefficients[2,4])
# lm_Hg_N_perspecies$BP_test <- #Breusch-Pagan test for heteroscedasity
#   lapply(lm_Hg_N_perspecies$model, function(x) lmtest::bptest(x)) 
# 
# n_sites <- dat_y0_stableN %>% dplyr::group_by(Species_short) %>%
#   dplyr::summarise(n_sites = length(unique(Site_name)))
# 
# lm_Hg_N_perspecies <- merge(lm_Hg_N_perspecies, n_sites,
#                             by = "Species_short") 
# lm_Hg_N_perspecies <- as.data.frame(lm_Hg_N_perspecies)
# 
# remove(n_sites)
# 
# # ----------------------
# 
# #Simple linear regression by forest plot and tree species: 
# #daily foliar Hg uptake ~ leaf N;
# #only current-season (y0) needles
# 
# lm_Hg_N_perspecies_site <- dat_y0_stableN %>%
#   dplyr::group_by(Species_short, Site_name) %>%
#   do(model = lm(Hg_ng_g ~ Ntot_mg_g, data = .)) %>%
#   dplyr::mutate(R2_mass = 0) %>% dplyr::mutate(n_fit = 0) %>%
#   dplyr::mutate(p_mass = 0) %>% dplyr::mutate(BP_test = 0)
# 
# lm_Hg_N_perspecies_site$R2_mass <- 
#   lapply(lm_Hg_N_perspecies_site$model, function(x) summary(x)$r.squared)
# lm_Hg_N_perspecies_site$n_fit <-
#   lapply(lm_Hg_N_perspecies_site$model, function(x) nobs(x))
# lm_Hg_N_perspecies_site$n_fit <- unlist(lm_Hg_N_perspecies_site$n_fit)
# lm_Hg_N_perspecies_site$p_mass <-
#   lapply(lm_Hg_N_perspecies_site$model, function(x) summary(x)$coefficients[8])
# lm_Hg_N_perspecies_site$p_mass <- unlist(lm_Hg_N_perspecies_site$p_mass)
# lm_Hg_N_perspecies_site$BP_test <- #Breusch-Pagan test for heteroscedasity
#   lapply(lm_Hg_N_perspecies_site$model, function(x) lmtest::bptest(x))
# 
# #Filter for forest plots with 20 or more foliage samples
# lm_Hg_N_perspecies_site_n20 <- lm_Hg_N_perspecies_site %>%
#   dplyr::filter(n_fit > 19)
# 
# remove(lm_Hg_N_perspecies_site)
# 
# # --------------------------------------------------------------------------
# # --------------------------------------------------------------------------
# 
# # Analysis of Hg uptake and LMA (leaf mass per area)
# 
# #Filter for available SLA (specific leaf area) values (cm2/g)
# #LMA = 1/SLA; with LMA being leaf mass per area (g/m2)
# dat_y0_LMA <- dat_y0_stableN %>%
#   dplyr::filter(!is.na(SLA_ICPF_cm2_g)) %>%
#   dplyr::mutate(LMA_g_m2 =
#                   (1/SLA_ICPF_cm2_g)*10000) %>%
#   dplyr::distinct(Sample_code, .keep_all = TRUE)
# 
# #Exclude LMA needle values from Switzerland from the data set 
# #as needles were dried before area measurement
# dat_y0_LMA <- dat_y0_LMA %>%
#   dplyr::filter(!((Country == "Switzerland") & (Foliage_type == "needle")))
# 
# #Median LMA, N and Hg daily per foliage type (leaf/needle)
# Median_LMA_N_Hgdaily <- dat_y0_LMA %>% 
#   dplyr::group_by(Foliage_type) %>%
#   dplyr::summarise(Median_LMA = median(LMA_g_m2, na.rm = T),
#                    Min_LMA = min(LMA_g_m2, na.rm = T),
#                    Max_LMA = max(LMA_g_m2, na.rm = T),
#                    Median_Hgdaily = median(Hg_daily, na.rm = T),
#                    Min_Hgdaily = min(Hg_daily, na.rm = T),
#                    Max_Hgdaily = max(Hg_daily, na.rm = T),
#                    Median_N = median(Ntot_mg_g, na.rm = T),
#                    Min_N = min(Ntot_mg_g, na.rm = T),
#                    Max_N = max(Ntot_mg_g, na.rm = T),
#                    n_samples = n())
# 
# 
# # --------------------------------------------------------------------------
# 
# # Calculate average values of Hg, LMA and N for subset of the
# # data set for which LMA values are available
# 
# # Average LMA values per species
# avg_LMA_species <- dat_y0_LMA %>%
#   dplyr::group_by(Species_short) %>%
#   dplyr::summarise(meanLMA = mean(LMA_g_m2, na.rm = TRUE),
#                    sdLMA = sd(LMA_g_m2, na.rm = TRUE),
#                    relsdLMA = sdLMA/meanLMA,
#                    medianLMA = median(LMA_g_m2, na.rm = TRUE),
#                    n_LMA_values = length(LMA_g_m2),
#                    n_sites = length(unique(Site_name)),
#                    Foliage_type = first(Foliage_type))
# relsd_LMA_allspecies <- sd(dat_y0_LMA$SLMA_g_m2)/
#   mean(dat_y0_LMA$LMA_g_m2)
# 
# # Average N value per species
# 
# avg_N_species <- dat_y0_LMA %>%
#   dplyr::group_by(Species_short) %>%
#   dplyr::summarise(meanN = mean(Ntot_mg_g, na.rm = TRUE),
#                    sdN = sd(Ntot_mg_g, na.rm = TRUE),
#                    relsdN = sdN/meanN,
#                    medianN = median(Ntot_mg_g, na.rm = TRUE),
#                    n_N_values = length(Ntot_mg_g),
#                    n_sites = length(unique(Site_name)),
#                    Foliage_type = first(Foliage_type))
# relsd_N_allspecies <- sd(dat_y0_LMA$Ntot_mg_g)/
#   mean(dat_y0_LMA$Ntot_mg_g)
# 
# # Average Hg value per species
# 
# avg_Hgdaily_species <- dat_y0_LMA %>%
#   dplyr::group_by(Species_short) %>%
#   dplyr::summarise(meanHgdaily = mean(Hg_daily, na.rm = TRUE),
#                    sdHgdaily = sd(Hg_daily, na.rm = TRUE),
#                    varHgdaily = var(Hg_daily, na.rm = TRUE),
#                    relsdHgdaily = sdHgdaily/meanHgdaily,
#                    medianHgdaily = median(Hg_daily, na.rm = TRUE),
#                    n_Hgdaily_values = length(Hg_daily),
#                    n_sites = length(unique(Site_name)),
#                    Foliage_type = first(Foliage_type))
# relsd_Hgdaily_allspecies <- sd(dat_y0_LMA$Hg_daily)/
#   mean(dat_y0_LMA$Hg_daily)
# 
# avg_Hg_LMA_N_species <- merge(avg_Hgdaily_species, avg_N_species)
# avg_Hg_LMA_N_species <- merge(avg_Hg_LMA_N_species, avg_LMA_species)
# 
# remove(avg_Hgdaily_species, avg_N_species, avg_LMA_species)

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

# Analysis of Hg uptake and hourly daytime vapor pressure deficit (dayVPD) 
# values exceeding a certain threshold value (exhrs)
# threshold values are 1.2 kPa; 1.6 kPa; 2 kPa and 3 kPa

#Filter for available exceedance hours of daytime VPD > threshold value
#VPD threshold values: 1.2 kPa; 1.6 kPa; 2 kPa; 3 kPa
#Calculate ratio of exceedance hrs day VPD > 1.2 kPa to all daytime hours

dat_comp_y0_VPD <- dat_comp_y0 %>%
  dplyr::filter(!is.na(exhrs_dayVPD_1.2kPa)) %>%
  dplyr::mutate(ratio_exhrs_dayVPD_1.2kPa = 
                  exhrs_dayVPD_1.2kPa/(Sampling_interval_d*12)) %>%
  dplyr::mutate(Plot_year = paste(Site_name, Sampling_year))

#number of sites, for which VPD exceedance hours are available
n_sites_VPD <- length(unique(dat_comp_y0_VPD$Plot_year))

#Average ratio per foliage functional group (leaf/needle) of
#exceedance hrs day VPD > 1.2 kPa /all hrs of daytime life period
avg_ratio_dayVPDexhrs1.2kPa_allhrs <- dat_comp_y0_VPD %>%
  dplyr::group_by(Foliage_type) %>%
  dplyr::summarise(avg_ratio = mean(ratio_exhrs_dayVPD_1.2kPa, na.rm = T),
                   sd_ratio = sd(ratio_exhrs_dayVPD_1.2kPa, na.rm = T))

# ----------------------

# Calculation of linear regression parameters per tree species: 
# daily Hg uptake rate ~ proportion of VPD exceedance hours > threshold

proportions <- c("prop_dayVPD_1.2kPa", "prop_dayVPD_1.6kPa", 
                 "prop_dayVPD_2kPa", "prop_dayVPD_3kPa")

#Initialize empty data frame for linear regression parameters
lm_avgHgdaily_propVPD <- list()

#Linear regression of avg. daily Hg uptake vs. respective prop. VPD 
for (i in 1:length(proportions)) {
  
  regr <- paste("Avg_Hg_daily ~", proportions[i])
  
  lm <- dat_comp_y0_VPD %>%
    dplyr::filter(!is.na(Avg_Hg_daily)) %>% #lm() does not allow NA
    dplyr::filter(Species_short %in% c("beech", "oak",
                                       "pine", "spruce")) %>%
    dplyr::group_by(Species_short) %>%
    do(model = lm(regr, data = .)) %>%
    dplyr::mutate(coef = 0) %>%
    dplyr::mutate(R2 = 0) %>% 
    dplyr::mutate(p_value_coef = 0) %>% dplyr::mutate(n_fit = 0) %>%
    dplyr::mutate(VPD_proportion = proportions[i]) %>%
    dplyr::mutate(BP_test = 0) #Breusch-Pagan test for homoscedasticity
  
  lm$coef <- lapply(lm$model, function(x) summary(x)$coefficients[2])
  lm$R2 <- lapply(lm$model, function(x) summary(x)$r.squared)
  lm$p_value_coef <- lapply(lm$model, function(x) summary(x)$coefficients[8])
  lm$n_fit <- lapply(lm$model, function(x) nobs(x))
  lm$BP_test <- lapply(lm$model, function(x) lmtest::bptest(x))
  
  lm_avgHgdaily_propVPD[[i]] <- lm
}

lm_avgHgdaily_propVPD <- dplyr::bind_rows(lm_avgHgdaily_propVPD) %>%
  dplyr::filter(n_fit > 5) #filter out tree species with low n

lm_avgHgdaily_propVPD$model <- NULL

#check for homoscedasticity using output from Breusch-Pagan test
for (i in 1:length(lm_avgHgdaily_propVPD$Species_short)) {
  print(lm_avgHgdaily_propVPD$BP_test[[i]])
}

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

# Linear regression of daily foliar Hg uptake and proportion of sample life
# period, during which soil water fell below a soil texture specific threshold

dat_comp_y0_soil <- dat_comp %>% dplyr::filter(Needle_age_class == 0) %>%
  dplyr::filter(!is.na(prop_hours_low_wc)) %>%
  dplyr::filter(Species_short != "larch") %>%
  dplyr::filter(prop_hours_low_wc > 0)

#Linear regression: Avg. Hg uptake ~ proportion of hours soil cont. < threshold
#Per tree species, excluding proportions = 0

lm_Hg_undercuthrs_perspecies <- dat_comp_y0_soil %>%
  dplyr::group_by(Species_short) %>%
  do(model = lm(Avg_Hg_daily ~ prop_hours_low_wc, data = .)) %>%
  dplyr::mutate(coef = 0) %>%
  dplyr::mutate(R2 = 0) %>% dplyr::mutate(n_fit = 0) %>%
  dplyr::mutate(p = 0) %>% dplyr::mutate(BP_test = 0)

lm_Hg_undercuthrs_perspecies$coef <- 
  lapply(lm_Hg_undercuthrs_perspecies$model, function(x) summary(x)$coefficients[2])
lm_Hg_undercuthrs_perspecies$R2 <- 
  lapply(lm_Hg_undercuthrs_perspecies$model, function(x) summary(x)$r.squared)
lm_Hg_undercuthrs_perspecies$n_fit <-
  lapply(lm_Hg_undercuthrs_perspecies$model, function(x) nobs(x))
lm_Hg_undercuthrs_perspecies$n_fit <- unlist(lm_Hg_undercuthrs_perspecies$n_fit)
lm_Hg_undercuthrs_perspecies$p <-
  lapply(lm_Hg_undercuthrs_perspecies$model, function(x) summary(x)$coefficients[2,4])
lm_Hg_undercuthrs_perspecies$BP_test <- #Breusch-Pagan test for heteroscedasity
  lapply(lm_Hg_undercuthrs_perspecies$model, function(x) lmtest::bptest(x)) 

lm_Hg_undercuthrs_perspecies <- as.data.frame(lm_Hg_undercuthrs_perspecies) %>%
  dplyr::filter(n_fit >= 19)

lm_Hg_undercuthrs_perspecies$model <- NULL

#check for homoscedasticity using output from Breusch-Pagan test
for (i in 1:length(lm_Hg_undercuthrs_perspecies$Species_short)) {
  print(lm_Hg_undercuthrs_perspecies$BP_test[[i]])
}


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

# Linear regression of daily foliar Hg uptake and geographic and 
# tree-specific parameters

# To calculate all linear regression parameters, ICP Forests proprietory
# data (Mean_age_years, Mean_DBH) have to be requested from the ICP Forests
# Database (http://icp-forests.net/page/data-requests)

# parameters_oi <- c("Mean_age_years", "Altitude_m", "Latitude", "Mean_DBH", 
#                    "GLEAM_transpiration_avg",
#                    "ERA5Land_avgTemp_C")

parameters_oi <- c("Altitude_m", "Latitude", 
                   "GLEAM_transpiration_avg",
                   "ERA5Land_avgTemp_C")

#Initialize empty data frame for linear regression parameters
lm_avgHgdaily_paroi <- list()

#Linear regression of avg. daily Hg uptake vs. every parameter of interest
for (i in 1:length(parameters_oi)) {
  
  regr <- paste("Avg_Hg_daily ~", parameters_oi[i])
  
  lm <- dat_comp_y0 %>%
    dplyr::group_by(Species_short) %>%
    do(model = lm(regr, data = .)) %>%
    dplyr::mutate(coef = 0) %>%
    dplyr::mutate(R2 = 0) %>% 
    dplyr::mutate(p_value_coef = 0) %>% dplyr::mutate(n_fit = 0) %>%
    dplyr::mutate(ind_var = parameters_oi[i]) %>%
    dplyr::mutate(BP_test = 0)
  
  lm$coef <- lapply(lm$model, function(x) summary(x)$coefficients[2])
  lm$R2 <- lapply(lm$model, function(x) summary(x)$r.squared)
  lm$p_value_coef <- lapply(lm$model, function(x) summary(x)$coefficients[8])
  lm$n_fit <- lapply(lm$model, function(x) nobs(x))
  lm$BP_test <- lapply(lm$model, function(x) lmtest::bptest(x))
  
  lm_avgHgdaily_paroi[[i]] <- lm
}

lm_avgHgdaily_paroi <- dplyr::bind_rows(lm_avgHgdaily_paroi)

lm_avgHgdaily_paroi$model <- NULL

lm_avgHgdaily_paroi <- as.data.frame(lm_avgHgdaily_paroi) %>%
  dplyr::filter(p_value_coef < 0.05) %>% dplyr::filter(n_fit > 30)

#check for homoscedasticity using output from Breusch-Pagan test
for (i in 1:length(lm_avgHgdaily_paroi$Species_short)) {
  print(lm_avgHgdaily_paroi$BP_test[[i]])
}

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

# Assessment of differences in foliar Hg uptake between sampling years

#Create data frame that contains Hg uptake values per forest plot and
#tree species for both sampling years 2015 and 2017 in separate columns

species <- unique(dat_comp_y0$Species_short)

dat_duplicated <- list()

for (i in 1:length(species)) {
  
  dat_species <- dat_comp_y0 %>%
    dplyr::filter(Species_short == species[i])
  
  species_duplicated <-
    subset(dat_species, Site_name %in% Site_name[duplicated(Site_name)]) %>%
    dplyr::mutate(key = paste(Site_name, Species, Sampling_year)) %>%
    dplyr::distinct(key, .keep_all = TRUE) %>% dplyr::select(-c("key"))
  
  species_duplicated_2015 <- species_duplicated %>% 
    dplyr::filter(Sampling_year == 2015) %>%
    dplyr::mutate(key = paste(Site_name, Species))
  species_duplicated_2017 <- species_duplicated %>% 
    dplyr::filter(Sampling_year == 2017) %>%
    dplyr::mutate(key = paste(Site_name, Species))
  
  species_duplicated_pairs <- merge(species_duplicated_2017,
                                    species_duplicated_2015,
                                    by = "key", all.y = F)
  
  dat_duplicated[[i]] <- species_duplicated_pairs
  
}

dat_duplicated <- dplyr::bind_rows(dat_duplicated)
#data frame with corresponding values per plot and tree species 
#from 2015 and 2017 in Avg_Hg_daily.y and Avg_Hg_daily.x
dat_duplicated <- as.data.frame(dat_duplicated) 

#Paired t-test for difference in means of foliar Hg uptake from 2015 and 2017
t.test(dat_duplicated$Avg_Hg_daily.x, 
       dat_duplicated$Avg_Hg_daily.y, 
       paired = TRUE)

#Calculate average values per tree species and sampling year
avgHgdaily_per_year <- dat_duplicated %>%
  dplyr::group_by(Species_short.x) %>%
  dplyr::summarise(Avg_Hgdaily_2017 = mean(Avg_Hg_daily.x, na.rm = T),
                   Sd_Hgdaily_2017 = sd(Avg_Hg_daily.x, na.rm = T),
                   Avg_Hgdaily_2015 = mean(Avg_Hg_daily.y, na.rm = T),
                   Sd_Hgdaily_2015 = sd(Avg_Hg_daily.y, na.rm = T),
                   n = n()) %>%
  dplyr::mutate(Diff_2015_2017 = Avg_Hgdaily_2015 - Avg_Hgdaily_2017) %>%
  dplyr::rename(Species_short = Species_short.x)

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

# Evaluation of foliar Hg uptake per needle age class

spruces_needle_age_summary <- dat %>% 
  dplyr::filter(Species_short == "spruce") %>%
  dplyr::group_by(Needle_age_class) %>%
  dplyr::summarise(Median_Hgdaily = median(Hg_daily, na.rm = T),
                   Min_Hgdaily = min(Hg_daily, na.rm = T),
                   Max_Hgdaily = max(Hg_daily, na.rm = T),
                   Median_Hgconc = median(Hg_ng_g, na.rm = T),
                   Min_Hgconc = min(Hg_ng_g, na.rm = T),
                   Max_Hgconc = max(Hg_ng_g, na.rm = T),
                   Avg_Hgconc = mean(Hg_ng_g, na.rm = T),
                   Sd_Hgconc = sd(Hg_ng_g, na.rm = T),
                   n = n()) %>%
  dplyr::mutate(Needle_age = paste("y", Needle_age_class, sep = ""))


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

# Plots

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

#Plot of average leaf Hg concentrations per forest plot
#versus sampling day-of-year (Fig. 2)

leaf_Hgconc_DOY_p <- ggplot(subset(dat_comp_y0,
                                   Species_short %in% c("oak", "beech")),
                            aes(x = Sampling_date_DOY,
                                y = Avg_Hg_conc_ng_g)) +
  geom_smooth(formula = y ~ x, method = "lm", se = F,
              color = "darkgrey", linetype = "dashed") +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., sep = "~~~")), 
               label.x = "left", label.y = 0.96, size = 4, parse = TRUE) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")), 
               label.x = "left", label.y = 0.89, size = 4, parse = TRUE) +
  geom_errorbar(aes(ymin = Avg_Hg_conc_ng_g - Std_Hg_conc_ng_g, 
                    ymax = Avg_Hg_conc_ng_g + Std_Hg_conc_ng_g),
                position = position_dodge2(width = 0.2, preserve = "single"),
                color = "aquamarine4") +
  geom_point(aes(shape = factor(Sampling_year)),
             color = "aquamarine4", size = 2) +
  #facet_grid(rows = vars(Species_short)) +
  facet_wrap(~ Species_short, ncol = 1) +
  xlab(TeX("Sampling date (DOY)")) +
  ylab(TeX("Average leaf Hg concentration $ $ $($$\\ng$ Hg g$^{-1}_{d.w.}$$)$")) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 14),
        strip.background = element_rect(color = "black", fill = "white", 
                                        size = 1, linetype = "solid"),
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, vjust = - 1),
        axis.title.y = element_text(size = 16, vjust = 4),
        strip.text.y = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"))
leaf_Hgconc_DOY_p


#Plot of median daily Hg uptake per tree species group (Fig. 3)

summary_species_short_p <- 
  ggplot(sum_dat_comp_y0, aes(x = reorder(Species_short,
                                          - Median_Hg_daily),
                              y = Median_Hg_daily)) +
  geom_bar(stat = "identity", aes(fill = Species_short), 
           position = position_dodge2(width = 3, preserve = "single")) +
  xlab("") + 
  ylab(TeX("Median daily Hg uptake $($$\\ng$ Hg g$^{-1}_{d.w.}$  d$^{-1}$$)$")) +
  #scale_fill_manual(values = wes_palette("Cavalcanti1", n = 4)) +
  #scale_x_discrete(labels = as.character(sum_dat_comp_y0$Species_short)) +
  geom_errorbar(aes(ymin = Min_Hg_daily, ymax = Max_Hg_daily),
                position = position_dodge2(width = 0.2, preserve = "single")) +
  geom_text(aes(x = factor(Species_short),
                y = 0, label = paste("n =", n)), vjust = -0.2, size = 6) +
  #geom_text(aes(label = n), vjust = -0.2, size = 6) + #n samples on top of bars
  theme_bw() +
  theme(axis.text.x = element_text(size = 18, angle = 0, vjust = 0.7), 
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 18, vjust = 2),
        axis.ticks.x = element_blank(),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        #        plot.margin = unit(c(0.3, 0, 0, 1), "cm"),
        legend.position="none")
summary_species_short_p

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

# This plot requires data from the ICP Forests Database
# http://icp-forests.net/page/data-requests


# #Plot of average Hg, LMA, N per tree species in three subplots (Fig. 4)
# 
# HgN_p <- ggplot(avg_Hg_LMA_N_species, aes(x = meanN, y = meanHgdaily,
#                                           color = Species_short,
#                                           shape = Foliage_type)) +
#   geom_point(size = 4) +
#   geom_errorbar(aes(ymin = meanHgdaily - sdHgdaily, ymax = meanHgdaily + sdHgdaily)) + 
#   geom_errorbarh(aes(xmin = meanN - sdN, xmax = meanN + sdN)) +
#   xlab(TeX("Average foliar nitrogen concentration $($$\\mg$ N g$^{-1}_{d.w.}$$)$")) +
#   ylab(TeX("Average daily Hg uptake $ $ $($$\\ng$ Hg g$^{-1}_{d.w.}$  d$^{-1}$$)$")) +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 11), 
#         axis.text.y = element_text(size = 11),
#         axis.title.x = element_text(size = 11, vjust = - 1),
#         axis.title.y = element_text(size = 11, vjust = 4),
#         strip.text.y = element_text(size = 11),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 11),
#         plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"))
# 
# HgLMA_p <- ggplot(avg_Hg_LMA_N_species, aes(x = meanLMA, y = meanHgdaily,
#                                             color = Species_short,
#                                             shape = Foliage_type)) +
#   geom_point(size = 4) +
#   geom_errorbar(aes(ymin = meanHgdaily - sdHgdaily, ymax = meanHgdaily + sdHgdaily)) + 
#   geom_errorbarh(aes(xmin = meanLMA - sdLMA, xmax = meanLMA + sdLMA)) +
#   xlab(TeX("Average LMA $($$\\g$ m$^{-2}_{leaf}$$)$")) +
#   ylab(TeX("Average daily Hg uptake $ $ $($$\\ng$ Hg g$^{-1}_{d.w.}$  d$^{-1}$$)$")) +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 11), 
#         axis.text.y = element_text(size = 11),
#         axis.title.x = element_text(size = 11, vjust = - 1),
#         axis.title.y = element_text(size = 11, vjust = 4),
#         strip.text.y = element_text(size = 11),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 10),
#         plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"))
# 
# NLMA_p <- ggplot(avg_Hg_LMA_N_species, aes(x = meanLMA, y = meanN,
#                                            color = Species_short,
#                                            shape = Foliage_type)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = meanN - sdN, ymax = meanN + sdN)) + 
#   geom_errorbarh(aes(xmin = meanLMA - sdLMA, xmax = meanLMA + sdLMA)) +
#   xlab(TeX("Average LMA $($$\\g$ m$^{-2}_{leaf}$$)$")) +
#   ylab(TeX("Average foliar nitrogen concentration $ $ $($$\\mg$ N g$^{-1}_{d.w.}$$)$")) +
#   theme_bw() +
#   theme(axis.text.x = element_text(size = 11), 
#         axis.text.y = element_text(size = 11),
#         axis.title.x = element_text(size = 11, vjust = - 1),
#         axis.title.y = element_text(size = 11, vjust = 4),
#         strip.text.y = element_text(size = 11),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 11),
#         plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"))
# 
# HgNLMA_p <- ggarrange(HgLMA_p, HgN_p, NLMA_p,
#                       labels = c("(a)", "(b)", "(c)"),
#                       font.label = list(size = 11),
#                       ncol = 2, nrow = 2,
#                       common.legend = TRUE, legend = "bottom")
# 
# HgNLMA_p

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------


#Plot of average daily Hg uptake rates of pine and spruce needles 
#per forest site versus number of hours during life period 
#VPD > 1.2 kPa (pine) and VPD > 3 kPa (spruce) (Fig. 5)
 
HgVPDprop1.2kPa_pine_p <- ggplot(subset(dat_comp_y0_VPD,
                                        Species_short %in% c("pine")),
                                 aes(x = prop_dayVPD_1.2kPa,
                                     y = Avg_Hg_daily)) +
  geom_smooth(formula = y ~ x, method = "lm", se = F,
              color = "darkgrey", linetype = "dashed") +
  geom_errorbar(aes(ymin = Avg_Hg_daily - Std_Hg_daily,
                    ymax = Avg_Hg_daily + Std_Hg_daily),
                color = "cyan4") +
  geom_point(aes(shape = factor(Sampling_year)),
             color = "cyan4", size = 3) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., sep = "~~~")),
               label.x = "right", label.y = 0.98, size = 5, parse = TRUE) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
               label.x = "right", label.y = 0.92, size = 5, parse = TRUE) +
  facet_grid(rows = vars(Species_short)) +
  xlim(c(0,0.49)) +
  ylim(c(0, 0.18)) +
  xlab("Daytime proportion of hourly VPD > 1.2 kPa") +
  ylab(TeX("Average daily Hg uptake $ $ $($$\\ng$ Hg g$^{-1}_{d.w.}$  d$^{-1}$$)$")) +
  theme_bw() +
  theme(strip.background = element_rect(color = "black", fill = "gold1",
                                        size = 1.5, linetype = "solid"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, vjust = - 1),
        axis.title.y = element_text(size = 16, vjust = 4),
        strip.text.y = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"))
#HgVPDprop1.2kPa_pine_p
HgVPDprop3kPa_spruce_p <- ggplot(subset(dat_comp_y0_VPD,
                                        Species_short %in% c("spruce")),
                                 aes(x = prop_dayVPD_3kPa,
                                     y = Avg_Hg_daily)) +
  geom_smooth(formula = y ~ x, method = "lm", se = F,
              color = "darkgrey", linetype = "dashed") +
  geom_errorbar(aes(ymin = Avg_Hg_daily - Std_Hg_daily,
                    ymax = Avg_Hg_daily + Std_Hg_daily),
                color = "cyan4") +
  geom_point(aes(shape = factor(Sampling_year)),
             color = "cyan4", size = 3) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., sep = "~~~")),
               label.x = "right", label.y = 0.98, size = 5, parse = TRUE) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
               label.x = "right", label.y = 0.92, size = 5, parse = TRUE) +
  facet_grid(rows = vars(Species_short)) +
  ylim(c(0, 0.18)) +
  xlab("Daytime proportion of hourly VPD > 3 kPa") +
  ylab(TeX("Average daily Hg uptake $ $ $($$\\ng$ Hg g$^{-1}_{d.w.}$  d$^{-1}$$)$")) +
  theme_bw() +
  theme(strip.background = element_rect(color = "black", fill = "gold1",
                                        size = 1.5, linetype = "solid"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, vjust = - 1),
        axis.title.y = element_text(size = 16, vjust = 4),
        strip.text.y = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"))
#HgVPDprop3kPa_spruce_p
HgVPD_p <- ggarrange(HgVPDprop1.2kPa_pine_p + rremove("ylab"), 
                     HgVPDprop3kPa_spruce_p + rremove("ylab"),
                     labels = c("(a)", "(b)"),
                     label.x = 0.05,
                     font.label = list(size = 16),
                     ncol = 2, nrow = 1,
                     common.legend = TRUE, legend = "right")
annotate_figure(HgVPD_p, 
                left = textGrob(TeX("Average daily Hg uptake $ $ $($$\\ng$ Hg g$^{-1}_{d.w.}$  d$^{-1}$$)$"), 
                                rot = 90, vjust = 1, 
                                gp = gpar(cex = 1.3)))

#Plot of average daily Hg uptake rates per forest site versus 
#proportion of hours during life period soil moisture < threshold (Fig. 6)

Hgsoilmoistprop_species_p <- ggplot(subset(dat_comp_y0_soil,
                                           Species_short %in% 
                                             c("pine", "beech", "oak")),
                                    aes(x = prop_hours_low_wc,
                                        y = Avg_Hg_daily)) +
  
  geom_smooth(formula = y ~ x, method = "lm", se = F,
              color = "darkgrey", linetype = "dashed") +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., sep = "~~~")),
               label.x = "right", label.y = 0.98, size = 4, parse = TRUE) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
               label.x = "right", label.y = 0.85, size = 4, parse = TRUE) +
  geom_errorbar(aes(ymin = Avg_Hg_daily - Std_Hg_daily,
                    ymax = Avg_Hg_daily + Std_Hg_daily),
                color = "cyan4") +
  geom_point(aes(shape = factor(Sampling_year)),
             color = "cyan4", size = 3) +
  facet_grid(rows = vars(Species_short)) +
  xlab(TeX("Proportion of hours soil water < PAW$_{crit}$")) +
  ylab(TeX("Average daily Hg uptake $ $ $($$\\ng$ Hg g$^{-1}_{d.w.}$  d$^{-1}$$)$")) +
  theme_bw() +
  theme(strip.background = element_rect(color = "black", fill = "gold1",
                                        size = 1.5, linetype = "solid"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, vjust = - 1),
        axis.title.y = element_text(size = 16, vjust = 4),
        strip.text.y = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"))
Hgsoilmoistprop_species_p


#Plot of average spruce needle Hg concentrations per needle age class (Fig. 7)

spruceneedle_age_conc_p <- 
  ggplot(spruces_needle_age_summary, aes(x = Needle_age,
                                         y = Avg_Hgconc)) +
  geom_bar(stat = "identity", fill = "lightpink", color = "black",
           position = position_dodge2(width = 3, preserve = "single")) + 
  xlab("Needle age class") +
  ylab(TeX("Avgerage spruce needle Hg conc.  $$ $($$\\ng$ Hg g$^{-1}_{d.w.}$$)$")) +
  #scale_fill_manual(values = wes_palette("Cavalcanti1", n = 4)) +
  #scale_x_discrete(labels = as.character(sum_dat_comp_y0$Species_short)) +
  geom_errorbar(aes(ymin = Avg_Hgconc - Sd_Hgconc, 
                    ymax = Avg_Hgconc + Sd_Hgconc),
                position = position_dodge2(width = 0.2, preserve = "single")) +
  geom_text(aes(x = factor(Needle_age),
                y = 0, label = paste("n =", n)), vjust = -0.2, size = 5.5) +
  #geom_text(aes(label = n), vjust = -0.2, size = 6) + #n samples on top of bars
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, vjust = 0.3), 
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16, vjust = -0),
        axis.title.y = element_text(size = 16, vjust = 2),
        axis.ticks.x = element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        #        plot.margin = unit(c(0.3, 0, 0, 1), "cm"),
        legend.position="none")
spruceneedle_age_conc_p

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

# Supplementary plot pdf for linear regression parameters of
# foliar Hg uptake rates versus parameters of interest

# # Linear regression plots of Hg daily with parameters 
# #of interest for each species group individually
# 
# Species_group <- as.factor(c("beech", "oak", "pine", "spruce"))
# Species_group <- as.data.frame(Species_group)
# colnames(Species_group) <- "Species_group"
# 
# Parameters <- c("Latitude", "Altitude_m", 
#                 "GLEAM_transpiration_avg",
#                 "ERA5Land_avgTemp_C")
# Parameters <- as.data.frame(Parameters)
# 
# Par_Labels <- c("Latitude", "Altitude (m)",
#                 "Avg. of GLEAM transpiration $($mm day$^{-1}$$)$",
#                 "Avg. ERA5-Land 2 m air temperature (°C)")
# Par_Labels <- as.data.frame(Par_Labels)
# 
# graphics.off() #close possibly open graphics
# #Creation of pdf
# pdf("LinRegression_Hgdaily_vs_Parameters.pdf")
# par(mfrow = c(3,2))
# 
# for (i in 1:length(Parameters[,1])) {
#   for (j in 1:length(Species_group[,1])) {
#     
#     dat_regr_plots <- dat_comp_y0 %>%
#       dplyr::filter(Species_short == Species_group[j,1]) %>%
#       dplyr::ungroup() %>%
#       dplyr::select(Avg_Hg_daily, Parameters[i,1], Std_Hg_daily)
#     
#     dat_regr_plots <- as.data.frame(dat_regr_plots)
#     
#     print(ggplot(dat_regr_plots, aes_string(x = names(dat_regr_plots)[2],
#                                             y = names(dat_regr_plots)[1])) +
#             geom_errorbar(aes(ymin = Avg_Hg_daily - Std_Hg_daily,
#                               ymax = Avg_Hg_daily + Std_Hg_daily),
#                           color = "brown") +
#             geom_point(color = "brown", size = 2) +
#             geom_smooth(formula = y ~ x, method = "lm", se = F, na.rm = T,
#                         color = "black", linetype = "dashed") +
#             stat_poly_eq(formula = y ~ x, 
#                          aes(label = paste(..eq.label.., ..rr.label..,
#                                            ..p.value.label.., sep = "~~~")), 
#                          parse = TRUE) +
#             xlab(TeX(as.character(Par_Labels[i,1]))) +
#             ylab(TeX(paste(as.character(Species_group[j,1]),
#                            "Hg uptake $($$\\ng$ Hg g$^{-1}_{d.w.}$ day$^{-1}$$)$", sep = " "))) +
#             theme_bw() +
#             theme(axis.text.x = element_text(size = 18), 
#                   axis.text.y = element_text(size = 18),
#                   axis.title.x = element_text(size = 18, vjust = - 0.5),
#                   axis.title.y = element_text(size = 18, vjust = 2),
#                   strip.text.y = element_text(size = 18),
#                   legend.title = element_blank(),
#                   legend.text = element_text(size = 18)))
#     
#   }
#   
# }
# 
# #close Plot-Pdf
# dev.off()

# --------------------------------------------------------------------------








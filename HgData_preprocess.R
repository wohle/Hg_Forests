require(dplyr)
require(ggplot2)
require(openxlsx)
require(modern)
require(TDPanalysis)

# --------------------------------------------------------------------------

# Read data

dat <- read.csv("https://zenodo.org")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

#Correction of Hg concentrations using available drying factors
#Explanation: samples were dried at various temperatures (40 C - 80 C),
#thus measured concentration values have to be normalized to dry 
#weight after drying at 105 C

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

#Exclude Polish samples from drying factor calculation, since samples from
#Poland were already dried at 105 C

dat_Poland <- dat %>% dplyr::filter(Country == "Poland")
dat <- dat %>% dplyr::filter(Country != "Poland")


#Correct Hg conc. for which drying factors are available

for (i in 1:length(dat$Hg_ng_g)) {
  
  if(!is.na(dat$drying_factor[i])){
    dat$Hg_ng_g[i] <- dat$Hg_ng_g[i]*dat$drying_factor[i]
  }
  
}

#Filter dataset for available drying factors (old_dryfs) and for
#missing drying factors (dat_new_dryfs)

dat_old_dryfs <- dat %>%
  dplyr::filter(!is.na(drying_factor))
dat_new_dryfs <- dat %>% dplyr::filter(is.na(drying_factor)) %>%
  dplyr::mutate(key = paste(Species, Needle_age_class))


#Calculate average drying factors per tree species and needle age class
#to use for correction of Hg concentration values, for which no
#drying factor was determined

dryfs <- dat %>%
  dplyr::filter(!is.na(drying_factor)) %>%
  dplyr::group_by(Species, Needle_age_class) %>%
  dplyr::summarise(mean_df = mean(drying_factor),
                   sd_df = sd(drying_factor),
                   n_df = length(drying_factor)) %>%
  dplyr::mutate(key = paste(Species, Needle_age_class))

#Calculate average drying factor per tree species group (e.g. oak)

dryfs_species_short <- dat %>%
  dplyr::filter(!is.na(drying_factor)) %>%
  dplyr::group_by(Species_short, Needle_age_class) %>%
  dplyr::summarise(mean_df = mean(drying_factor),
                   sd_df = sd(drying_factor),
                   n_df = length(drying_factor)) %>%
  dplyr::mutate(key = paste(Species_short, Needle_age_class))

#Calculate average drying factor per foliage type (leaf/needle)

dryfs_foliage_type <- dat %>%
  dplyr::filter(!is.na(drying_factor)) %>%
  dplyr::group_by(Foliage_type, Needle_age_class) %>%
  dplyr::summarise(mean_df = mean(drying_factor),
                   sd_df = sd(drying_factor),
                   n_df = length(drying_factor)) %>%
  dplyr::mutate(key = paste(Foliage_type, Needle_age_class))

#Assign the correct drying factor of tree species and needle age class to
#respective sample, for which no drying factor is availabe (dat_new_dryfs)

for (i in 1:length(dat_new_dryfs$drying_factor)) {
  for (j in 1:length(dryfs$mean_df)) {
    
    if(dat_new_dryfs$key[i] == dryfs$key[j]){
      dat_new_dryfs$drying_factor[i] <- dryfs$mean_df[j]
    }
    
  }
}

#For samples of some tree species (e.g. Abies borisii regis) no specific
#drying factor were available (dat_rest_dryfs). Assign drying factor per
#species group and needle age class (e.g. fir, current-season) to these samples

dat_rest_dryfs <- dat_new_dryfs %>%
  dplyr::filter(is.na(drying_factor)) %>%
  dplyr::mutate(key = paste(Species_short, Needle_age_class))


for (i in 1:length(dat_rest_dryfs$drying_factor)) {
  for (j in 1:length(dryfs_species_short$mean_df)) {
    
    if(dat_rest_dryfs$key[i] == dryfs_species_short$key[j]){
      dat_rest_dryfs$drying_factor[i] <- dryfs_species_short$mean_df[j]
    }
    
  }
}

#For poplar there was no tree species or tree species group specific 
#drying factor available. Assign average drying factor of deciduous leaves
#to poplar

dryfs_foliage_type <- as.data.frame(dryfs_foliage_type)

for (i in 1:length(dat_rest_dryfs$Species_short)) {
  
  if(dat_rest_dryfs$Species_short[i] == "poplar"){
    dat_rest_dryfs$drying_factor[i] = 
      (dryfs_foliage_type %>% 
         dplyr::filter(Foliage_type == "leaf") %>%
         dplyr::select(mean_df))
  }
  
}

#Merge datasets with calculated drying factors

dat_new_dryfs <- dat_new_dryfs %>% dplyr::select(-c("key"))
dat_new_dryfs <- dat_new_dryfs %>%
  dplyr::filter(!is.na(drying_factor))
dat_rest_dryfs <- dat_rest_dryfs %>% dplyr::select(-c("key"))

dat_new_dryfs <- rbind(dat_new_dryfs, dat_rest_dryfs)

dat_new_dryfs$drying_factor <- unlist(dat_new_dryfs$drying_factor)

#Correct Hg content for drying temperature at 105 C by multiplication
#with drying factor

dat_new_dryfs$Hg_ng_g <- dat_new_dryfs$Hg_ng_g*dat_new_dryfs$drying_factor

#Merge data frames

dat_dryfs_corrected <- rbind(dat_old_dryfs, dat_new_dryfs)

#Correct values of C and N of three ICP Forests members
#Explanation: C and N values of Baden-Wurttemberg, Switzerland and UK had
#not been corrected for drying temperature previously

dat_dryfs_corrected$Corg_mg_g <-
  as.numeric(dat_dryfs_corrected$Corg_mg_g)
dat_dryfs_corrected$Ntot_mg_g <-
  as.numeric(dat_dryfs_corrected$Ntot_mg_g)

for (i in 1:length(dat_dryfs_corrected$Sample_code)) {
  
  if((dat_dryfs_corrected$Country[i] == "Baden-Wurttemberg") |
     (dat_dryfs_corrected$Country[i] == "Switzerland")  |
     (dat_dryfs_corrected$Country[i] == "UK")){
    dat_dryfs_corrected$Corg_mg_g[i] <-
      dat_dryfs_corrected$Corg_mg_g[i]*dat_dryfs_corrected$drying_factor[i]
    dat_dryfs_corrected$Ntot_mg_g[i] <-
      dat_dryfs_corrected$Ntot_mg_g[i]*dat_dryfs_corrected$drying_factor[i]
  }
  
}

#Merge to data set, of which all values (Hg, C, N) are corrected for 
#drying temperature

dat_final_dryf_corrected <- rbind(dat_dryfs_corrected, dat_Poland) %>%
  dplyr::arrange(Country)

dat <- dat_final_dryf_corrected

remove(dat_final_dryf_corrected, dat_dryfs_corrected, dat_new_dryfs,
       dat_old_dryfs, dat_Poland, dat_rest_dryfs, dryfs,
       dryfs_foliage_type, dryfs_species_short)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

#Calculation of daily foliar Hg uptake rates from foliar Hg concentrations
#by dividing Hg conc. by the number of days between the beginning of the
#growing season and the sampling date (or in the case of a few winter 
#needles the 15th of November instead of the sampling date).
#To this, the appropriate start-of-season date has to be assigned to the
#respective foliage sample. This is the ICP Forests start-of-season date, where
#available, the start-of-season date of the closest observation location within 
#the phenological database PEP725 for needles (BeginGS_PEP725_DOY) and the 
#start-of-season date resulting from PROBA-V LAI modelling for leaves
#(BeginGS_LAImethod_DOY).

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

# Assign appropriate DOY for beginning of growing season to each foliage sample

#Initialize
dat <- dat %>% dplyr::mutate(BeginGS_final_DOY = 0)

#
for (i in 1:length(dat$BeginGS_final_DOY)) {
  
  if (!is.na(dat$BeginGS_ICPF_DOY[i])){ #if there exists an ICPF value, assign
    dat$BeginGS_final_DOY[i] <- dat$BeginGS_ICPF_DOY[i]
  }
  else if ((dat$Foliage_type[i] == "needle") && #assign PEP725 value to needles
           (is.na(dat$BeginGS_ICPF_DOY[i]))) {
    dat$BeginGS_final_DOY[i] <- dat$BeginGS_PEP725_DOY[i]
  } #assign LAImethod value to leaves
  else {dat$BeginGS_final_DOY[i] <- dat$BeginGS_LAImethod_DOY[i]} 
}

#Round DOY
dat$BeginGS_final_DOY <- round(dat$BeginGS_final_DOY, digits = 0)


#Account for special case:
#there are four unreasonable start of growing season dates:
#SoS at one oak site in Northwestern Germany (Ehrhorn_Ei) 2015 is in July
#SoS at one oak site in NRW (Stadtlohn) 2015 is in almost June
#SoS at one beech site in France (Maron) 2015 is in June
#SoS at one oak site in Belgium (Gontrode) 2017 is in early March
#SoS at Ehrhorn_Ei, Stadtlohn, Maron and Gontrode will be set to 
#29th of April (DOY: 119)
#Ehrhorn_Ei key: 4-308 2015
#Stadtlohn key: 4-620 2015
#Maron key: 1-60 2015
#Gontrode key: 2-16 2017

#Create key to identify the right forest plots of the right sampling year
dat$key <- paste(dat$ICPF_country_plot, dat$Sampling_year)

#Correction of Ehrhorn_Ei, Stadtlohn, Maron and Gontrode
for (i in 1:length(dat$BeginGS_final_DOY)) {
  if((dat$key[i] == "4-308 2015") | 
     (dat$key[i] == "4-620 2015") | 
     (dat$key[i] == "1-60 2015") |
     (dat$key[i] == "2-16 2017")){
    dat$BeginGS_final_DOY[i] <- 119
  }
}

dat$key <- NULL

# Evaluate average Start of Season DOY
#for sampling years and species group

Avg_SoS_DOY <- dat %>% dplyr::mutate(Site_year = 
                                       paste(as.character(Site_name),
                                             as.character(Sampling_year),
                                             sep = "")) %>%
  dplyr::distinct(Site_year, .keep_all = TRUE) %>%
  dplyr::group_by(Species_short) %>%
  dplyr::summarise(Avg_DOY = round(mean(BeginGS_final_DOY)),
                   Std_DOY = round(sd(BeginGS_final_DOY)),
                   n_sites = n()) 

# Calculate daily Hg uptake

#Convert DOY of Begin GS to Date Begin GS for improved data handling

dat$origin <- "NA"

for (i in 1:length(dat$origin)) {
  dat$origin[i] <- paste(as.character(dat$Sampling_year[i]-1),"-12-31", sep = "")
}

#Initialize BeginGS date column and format as date
dat$BeginGS_date <- as.Date("1900-01-01")

for (i in 1:length(dat$BeginGS_date)) {
  dat$BeginGS_date[i] <- as.Date(dat$BeginGS_final_DOY[i], origin = dat$origin[i])
}

# Account for needle samples of different age classes

#Beginning of growing season is antedated for one year (365 d) for needle
#age class 1, two years (730 d) for needle age class 2 etc.

dat$BeginGS_date <- dat$BeginGS_date - dat$Needle_age_class * 365


#Define the date when foliar Hg uptake ends
#in most cases this is the sampling date
#in cases of needle sampling in winter (e.g. January) define
#end of uptake as 15th Nov (DOY = 319)

#Convert Sampling Date to DOY
dat$Sampling_date_character <- 
  as.character.Date(dat$Sampling_date, format = "%d/%m/%Y")
dat <- dat %>% 
  dplyr::mutate(Sampling_date_DOY = 
                  date.to.DOY(Sampling_date_character, format = "dd/mm/yyyy"))

#Delete rows without sampling date
dat <- dat[-c(which(is.na(dat$Sampling_date_DOY))),]

#Initialize column with date when Hg uptake ends and format
dat$EndDate <- as.Date("01.01.2014", format = "%d.%m.%Y")

for (i in 1:length(dat$EndDate)) {
  
  if ((dat$Sampling_date_DOY[i] > 319) |
      (dat$Sampling_date_DOY[i] < 100)){
    dat$EndDate[i] <- as.Date(paste("15.11.",as.character(dat$Sampling_year[i]), 
                                    sep = ""), 
                              format = "%d.%m.%Y")
  }
  else {dat$EndDate[i] <- dat$Sampling_date[i]}
  
}

#Calculate difference in days

dat$Sampling_interval_d <- dat$EndDate - dat$BeginGS_date
dat$Sampling_interval_d <- as.numeric(dat$Sampling_interval_d)

#Calculate daily Hg uptake rate

dat$Hg_daily <- dat$Hg_ng_g / dat$Sampling_interval_d

dat <- dat %>% dplyr::select(-c("origin", "Sampling_date_character"))


#Account for special case:
#some sites in Baden-Wuerttemberg have negative day differences
#due to a sampling date before the defined start of season

for (i in 1:length(dat$Sampling_interval_d)) {
  if((!is.na(dat$Sampling_interval_d[i])) &
     (dat$Sampling_interval_d[i] < 0)) {
    dat$Sampling_interval_d[i] <- abs(dat$Sampling_interval_d[i]) + 365
  }
}

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

# Outlier detection within daily foliar Hg uptake values using method of
#modified Z scores after Iglewicz and Hoaglin with a threshold of 3.5
#function iglewicz_hoaglin within R modern package

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

#Filter NA
dat <- dat[-c(which(is.na(dat$Hg_daily))),]

#Calculate modified Z scores within each species group and needle year class
modZ_scores <- dat %>% dplyr::group_by(Species_short, Needle_age_class) %>%
  dplyr::arrange(Sample_code, .by_group = TRUE) %>%
  dplyr::do(modZ = iglewicz_hoaglin(.$Hg_daily, threshold = 3.5,
                                    return_scores = TRUE)) %>%
  tidyr::unnest(cols = c(modZ))

dat_arranged <- dat %>% dplyr::group_by(Species_short, Needle_age_class) %>%
  dplyr::arrange(Sample_code, .by_group = TRUE) %>%
  tidyr::unnest()

dat_modZ <- cbind(dat_arranged, modZ_scores[,3])

#Test to check if sample is assigned to correctly corresponding modified Z score
# ash <- dat %>% dplyr::filter(Species_short == "ash") %>%
#   dplyr::arrange(Sample_code)
# test_ash <- iglewicz_hoaglin(ash$Hg_daily, threshold = 3.5,
#                              return_scores = TRUE)

dat_outlier_corrected <- dat_modZ %>% #remove values with mod. Z score of
  dplyr::filter(modZ >= -3.5) %>% # < -3.5 and
  dplyr::filter(modZ <= 3.5) # > 3.5

percent_outlier_values = (1 - (length(dat_outlier_corrected$Sample_code)/
                                 length(dat$Sample_code)))*100
#Please note:
#the percent_outlier_values does not correspond exactly to the percentage
#mentioned in the paper which is due to the fact that foliage samples from
#the Austrian Bio-Indicator Grid are excluded from the data set

dat_processed <- dat_outlier_corrected

remove(dat, dat_arranged, dat_modZ, dat_outlier_corrected, modZ_scores)

dat_processed <- as.data.frame(dat_processed)

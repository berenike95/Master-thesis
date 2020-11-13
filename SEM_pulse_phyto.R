#---------------------------------------------------------------------------------------------------------#
#### SEM for pulse disturbance & phytoplankton ####
#---------------------------------------------------------------------------------------------------------#

# load dataset
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/master_dataset.csv")

# filtering dataset for phytoplankton data only 
dataset_phyto <- dataset_final[which(dataset_final$community=='phyto'), ]

# filtering data for pulse disturbance only 
dataset_phyto_pulse <- dataset_phyto[which(dataset_phyto$Treatment=='F'), ]

#---------------------------------------------------------------------------------------------------------#

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for press disturbance only 
dataset_zoopl_pulse <- dataset_zoopl[which(dataset_zoopl$Treatment=='F'), ]

# filtering for body size and biomass 
dataset_zoopl_pulse <- select(dataset_zoopl_pulse, Lake, Experiment, Treatment, Enclosure, 
                              initial_zoop_body_size, mean_biomass)

# renaming columns
names(dataset_zoopl_pulse)[names(dataset_zoopl_pulse)=="mean_biomass"] <- "mean_zoopl_biomass"
names(dataset_zoopl_pulse)[names(dataset_zoopl_pulse)=="initial_zoop_body_size"] <- "zoopl_body_size"

# merging both datasets 
dataset_phyto_pulse <- merge(x=dataset_phyto_pulse, y=dataset_zoopl_pulse, by= c("Lake", "Experiment", 
                                                                                 "Treatment", "Enclosure"))

#---------------------------------------------------------------------------------------------------------#

# load packages

library('lme4')

library('lmerTest')

library('MuMIn')

library(tidyverse)

library(dplyr)

#---------------------------------------------------------------------------------------------------------#
#### 0. normal distribution and scaling of variables ####
#---------------------------------------------------------------------------------------------------------#
# Variables:

# TN, TP, DOC, PAR, Temp, TN/TP ratio, DN/TP ratio, Chl a

# checking normal distribution of TN
hist(dataset_phyto_pulse$mean_TN)
shapiro.test(dataset_phyto_pulse$mean_TN)
# p-value = 1.406e-05


# checking normal distribution of TP
hist(dataset_phyto_pulse$mean_TP)
shapiro.test(dataset_phyto_pulse$mean_TP)
# p-value = 0.003156
dataset_phyto_pulse$mean_TP_log <- log(dataset_phyto_pulse$mean_TP)
hist(dataset_phyto_pulse$mean_TP_log)
shapiro.test(dataset_phyto_pulse$mean_TP_log)
# p-value = 0.1248


# checking normal distribution of DOC
hist(dataset_phyto_pulse$mean_DOC)
shapiro.test(dataset_phyto_pulse$mean_DOC)
# p-value = 0.002989
dataset_phyto_pulse$mean_DOC_log <- log(dataset_phyto_pulse$mean_DOC)
hist(dataset_phyto_pulse$mean_DOC_log)
shapiro.test(dataset_phyto_pulse$mean_DOC_log)
# p-value = 0.003619


# checking normal distribution of PAR
hist(dataset_phyto_pulse$mean_PAR)
shapiro.test(dataset_phyto_pulse$mean_PAR)
# p-value = 5.475e-07
dataset_phyto_pulse$mean_PAR_log <- log(dataset_phyto_pulse$mean_PAR)
hist(dataset_phyto_pulse$mean_PAR_log)
shapiro.test(dataset_phyto_pulse$mean_PAR_log)
# p-value = 0.1951


# checking normal distribution of Temperature 
hist(dataset_phyto_pulse$mean_Temp)
shapiro.test(dataset_phyto_pulse$mean_Temp)
# p-value = 1.809e-12
dataset_phyto_pulse$mean_Temp_log <- sqrt(dataset_phyto_pulse$mean_Temp)
hist(dataset_phyto_pulse$mean_Temp_log)
shapiro.test(dataset_phyto_pulse$mean_Temp_log)


# checking normal distribution of TN/TP ratio
hist(dataset_phyto_pulse$mean_TN_TP_ratio)
shapiro.test(dataset_phyto_pulse$mean_TN_TP_ratio)
# p-value = 0.003622
dataset_phyto_pulse$mean_TN_TP_ratio_log <- log(dataset_phyto_pulse$mean_TN_TP_ratio*100)
hist(dataset_phyto_pulse$mean_TN_TP_ratio_log)
shapiro.test(dataset_phyto_pulse$mean_TN_TP_ratio_log)
# p-value = 0.01473


# checking normal distribution of DN/TP ratio
hist(dataset_phyto_pulse$mean_DN_TP_ratio)
shapiro.test(dataset_phyto_pulse$mean_DN_TP_ratio)
#  p-value = 0.0005185
dataset_phyto_pulse$mean_DN_TP_ratio_log <- log(dataset_phyto_pulse$mean_DN_TP_ratio*100)
hist(dataset_phyto_pulse$mean_DN_TP_ratio_log)
shapiro.test(dataset_phyto_pulse$mean_DN_TP_ratio_log)
# p-value = 0.05559



# checking normal distribution of Chl a
hist(dataset_phyto_pulse$mean_Chla)
shapiro.test(dataset_phyto_pulse$mean_Chla)
# p-value = 8.299e-09
dataset_phyto_pulse$mean_Chla_log <- log(dataset_phyto_pulse$mean_Chla)
hist(dataset_phyto_pulse$mean_Chla_log)
shapiro.test(dataset_phyto_pulse$mean_Chla_log)
# p-value = 0.2532

#---------------------------------------------------------------------------------------------------------#
# Biovolume, Evenness, ENS, Richness, Zooplankton biomass, Zooplankton body size

# checking normal distribution of biomass
hist(dataset_phyto_pulse$mean_biomass)
shapiro.test(dataset_phyto_pulse$mean_biomass)
# p-value = 7.695e-12
dataset_phyto_pulse$mean_biomass_log <- log(dataset_phyto_pulse$mean_biomass)
hist(dataset_phyto_pulse$mean_biomass_log)
shapiro.test(dataset_phyto_pulse$mean_biomass_log)
# p-value = 0.1764


# checking normal distribution of evenness
hist(dataset_phyto_pulse$mean_J)
shapiro.test(dataset_phyto_pulse$mean_J)
# p-value = 0.1123


# checking normal distribution of ENS
hist(dataset_phyto_pulse$mean_ENS_D)
shapiro.test(dataset_phyto_pulse$mean_ENS_D)
# p-value = 0.04182


# checking normal distribution of richness
hist(dataset_phyto_pulse$mean_s)
shapiro.test(dataset_phyto_pulse$mean_s)
# p-value = 0.01863
dataset_phyto_pulse$mean_s_log <- log(dataset_phyto_pulse$mean_s)
hist(dataset_phyto_pulse$mean_s_log)
shapiro.test(dataset_phyto_pulse$mean_s_log)
# p-value = 0.005852


# checking normal distribution of mean zooplankton biomass
hist(dataset_phyto_pulse$mean_zoopl_biomass)
shapiro.test(dataset_phyto_pulse$mean_zoopl_biomass)
# p-value = 7.328e-06
dataset_phyto_pulse$mean_zoopl_biomass_log <- log(dataset_phyto_pulse$mean_zoopl_biomass)
hist(dataset_phyto_pulse$mean_zoopl_biomass_log)
shapiro.test(dataset_phyto_pulse$mean_zoopl_biomass_log)
#  p-value = 0.2326


# checking normal distribution of initial zooplankton body size
hist(dataset_phyto_pulse$zoopl_body_size)
shapiro.test(dataset_phyto_pulse$zoopl_body_size)
# p-value = 1.455e-05
dataset_phyto_pulse$zoopl_body_size_log <- log(dataset_phyto_pulse$zoopl_body_size)
hist(dataset_phyto_pulse$zoopl_body_size_log)
shapiro.test(dataset_phyto_pulse$zoopl_body_size_log)
# p-value = 0.002341


#---------------------------------------------------------------------------------------------------------#
# final recovery, Resistance, resilience, area under the curve

# checking normal distribution of final recovery
hist(dataset_phyto_pulse$final_stab)
shapiro.test(dataset_phyto_pulse$final_stab)
# p-value = 2.113e-05
dataset_phyto_pulse$final_stab_log <- log(dataset_phyto_pulse$final_stab*-1)
hist(dataset_phyto_pulse$final_stab_log)
shapiro.test(dataset_phyto_pulse$final_stab_log)
# p-value = 0.02855


# checking normal distribution of resistance
hist(dataset_phyto_pulse$initial_stab)
shapiro.test(dataset_phyto_pulse$initial_stab)
# p-value = 9.272e-05
dataset_phyto_pulse$initial_stab_log <- log(dataset_phyto_pulse$initial_stab*-1)
hist(dataset_phyto_pulse$initial_stab_log)
shapiro.test(dataset_phyto_pulse$initial_stab_log)
# p-value = 0.1027


# checking normal distribution of resilience
hist(dataset_phyto_pulse$rate_change_ort)
shapiro.test(dataset_phyto_pulse$rate_change_ort)
# p-value = 0.00785
dataset_phyto_pulse$rate_change_ort_log <- log(dataset_phyto_pulse$rate_change_ort*-1)
hist(dataset_phyto_pulse$rate_change_ort_log)
shapiro.test(dataset_phyto_pulse$rate_change_ort_log)
# p-value = 0.07997

# checking normal distribution of AUC
hist(dataset_phyto_pulse$total_impact)
shapiro.test(dataset_phyto_pulse$total_impact)
# p-value = 0.0004286
dataset_phyto_pulse$total_impact_log <- log(dataset_phyto_pulse$total_impact)
hist(dataset_phyto_pulse$total_impact_log)
shapiro.test(dataset_phyto_pulse$total_impact_log)
# p-value = 0.5397

#---------------------------------------------------------------------------------------------------------#
#### 1. resistance for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resistance 
# explanatory = Chl a, TP, evenness, ENS, DOC 

resistance <- lmer(initial_stab_log ~ mean_Chla_log + mean_TP_log + mean_J + 
                     mean_zoopl_biomass_log + (1|Lake), data=dataset_phyto_pulse)

summary(resistance)
# Chl a 
# TP
# evenness
# mean zoopl biomass

r.squaredGLMM(resistance)
# R2m       R2c
# [1,] 0.3705117 0.3705117

#---------------------------------------------------------------------------------------------------------#
#### 2. final recovery for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = final recovery 
# explanatory = Chl a, TP, evenness, biomass, ENS, evenness, DOC 

recovery <- lmer(final_stab_log ~ mean_TP_log+ mean_DOC_log +
                   mean_zoopl_biomass_log + zoopl_body_size_log + (1|Lake), data=dataset_phyto_pulse)

summary(recovery)
# TP
# DOC
# mean zoopl biomass
# zoopl body size

r.squaredGLMM(recovery)
# R2m       R2c
# [1,] 0.2534717 0.2638958

#---------------------------------------------------------------------------------------------------------#
#### 3. resilience for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience
# explanatory = Chl a, Temp, DOC, TP 

resilience <- lmer(rate_change_ort_log ~ mean_Chla_log + mean_DOC_log + mean_TP_log + 
                     mean_zoopl_biomass_log + zoopl_body_size_log + (1|Lake), data=dataset_phyto_pulse)

summary(resilience)
# Chl a 
# Temp
# DOC
# TP 
# mean zoopl biomass
# zoopl body size

r.squaredGLMM(resilience)
# R2m       R2c
# [1,] 0.4667674 0.4667674

#---------------------------------------------------------------------------------------------------------#
#### 4. AUC for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = AUC
# explanatory = Chl a, Temp, DOC, TP 

AUC <- lmer(total_impact_log ~ mean_DOC_log + mean_TP_log + mean_zoopl_biomass_log + 
              zoopl_body_size_log + (1|Lake), data=dataset_phyto_pulse)

summary(AUC)
# DOC
# TP
# mean zoopl biomass
# zoopl body size

r.squaredGLMM(AUC)
# R2m      R2c
# [1,] 0.4767788 0.507721

#---------------------------------------------------------------------------------------------------------#
#### 5. Biomass for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = biomass
# explanatory = Chl a, TP, TN/TP ratio, DOC, mean zoopl biomass, zooplankton body size

biomass <- lmer(mean_biomass_log ~  mean_Chla_log + mean_TP + mean_TN_TP_ratio_log + zoopl_body_size_log + 
                  (1|Lake), data=dataset_phyto_pulse)

summary(biomass)
# Chl a 
# TP
# TN/TP ratio
# zoopl body size

r.squaredGLMM(biomass)
# R2m       R2c
# [1,] 0.416157 0.7125465

#---------------------------------------------------------------------------------------------------------#
#### 6. Evenness for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = Chl a, TP, TN/TP ratio, DOC, mean zoopl biomass, zooplankton body size

evenness <- lmer(mean_J ~  mean_Chla_log + mean_TP + mean_TN_TP_ratio_log + mean_DOC_log + 
                   mean_zoopl_biomass_log  + (1|Lake), data=dataset_phyto_pulse)
summary(evenness)
# Chl a 
# TP
# TN/TP ratio
# mean zoopl biomass

r.squaredGLMM(evenness)
#   R2m       R2c
# [1,] 0.5647679 0.5835105

#---------------------------------------------------------------------------------------------------------#
#### 7. ENS for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = Chl a, TP, TN/TP ratio, DOC, mean zoopl biomass, zooplankton body size

ENS <- lmer(mean_ENS_D ~  mean_Chla_log + mean_TP + mean_DOC_log + mean_zoopl_biomass_log + 
              zoopl_body_size_log + (1|Lake), data=dataset_phyto_pulse)

summary(ENS)

# Chl a 
# TP
# DOC
# mean zoopl biomass
# zoopl body size

r.squaredGLMM(ENS)
# R2m       R2c
# [1,] 0.4470678 0.9620057

#---------------------------------------------------------------------------------------------------------#






#---------------------------------------------------------------------------------------------------------#
#### SEM for pulse disturbance & bacteria ####
#---------------------------------------------------------------------------------------------------------#

# load dataset
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/master_dataset.csv")

# filtering dataset for phytoplankton data only 
dataset_bact <- dataset_final[which(dataset_final$community=='bact'), ]

# filtering data for pulse disturbance only 
dataset_bact_pulse <- dataset_bact[which(dataset_bact$Treatment=='F'), ]

dataset_bact_pulse = subset(dataset_bact_pulse, select = -c(15:17))

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
dataset_bact_pulse <- merge(x=dataset_bact_pulse, y=dataset_zoopl_pulse, by= c("Lake", "Experiment", 
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
hist(dataset_bact_pulse$mean_TN)
shapiro.test(dataset_bact_pulse$mean_TN)
# p-value = 1.406e-05


# checking normal distribution of TP
hist(dataset_bact_pulse$mean_TP)
shapiro.test(dataset_bact_pulse$mean_TP)
# p-value = 0.003156
dataset_bact_pulse$mean_TP_log <- log(dataset_bact_pulse$mean_TP)
hist(dataset_bact_pulse$mean_TP_log)
shapiro.test(dataset_bact_pulse$mean_TP_log)
# p-value = 0.1248


# checking normal distribution of DOC
hist(dataset_bact_pulse$mean_DOC)
shapiro.test(dataset_bact_pulse$mean_DOC)
# p-value = 0.002989
dataset_bact_pulse$mean_DOC_log <- log(dataset_bact_pulse$mean_DOC)
hist(dataset_bact_pulse$mean_DOC_log)
shapiro.test(dataset_bact_pulse$mean_DOC_log)
# p-value = 0.003619


# checking normal distribution of PAR
hist(dataset_bact_pulse$mean_PAR)
shapiro.test(dataset_bact_pulse$mean_PAR)
# p-value = 5.475e-07
dataset_bact_pulse$mean_PAR_log <- log(dataset_bact_pulse$mean_PAR)
hist(dataset_bact_pulse$mean_PAR_log)
shapiro.test(dataset_bact_pulse$mean_PAR_log)
# p-value = 0.1951


# checking normal distribution of Temperature 
hist(dataset_bact_pulse$mean_Temp)
shapiro.test(dataset_bact_pulse$mean_Temp)
# p-value = 1.809e-12
dataset_bact_pulse$mean_Temp_log <- log(dataset_bact_pulse$mean_Temp)
hist(dataset_bact_pulse$mean_Temp_log)
shapiro.test(dataset_bact_pulse$mean_Temp_log)
#  p-value = 3.205e-10


# checking normal distribution of TN/TP ratio
hist(dataset_bact_pulse$mean_TN_TP_ratio)
shapiro.test(dataset_bact_pulse$mean_TN_TP_ratio)
# p-value = 0.003622
dataset_bact_pulse$mean_TN_TP_ratio_log <- log(dataset_bact_pulse$mean_TN_TP_ratio*100)
hist(dataset_bact_pulse$mean_TN_TP_ratio_log)
shapiro.test(dataset_bact_pulse$mean_TN_TP_ratio_log)
# p-value = 0.01473


# checking normal distribution of DN/TP ratio
hist(dataset_bact_pulse$mean_DN_TP_ratio)
shapiro.test(dataset_bact_pulse$mean_DN_TP_ratio)
#  p-value = 0.0005185
dataset_bact_pulse$mean_DN_TP_ratio_log <- log(dataset_bact_pulse$mean_DN_TP_ratio*100)
hist(dataset_bact_pulse$mean_DN_TP_ratio_log)
shapiro.test(dataset_bact_pulse$mean_DN_TP_ratio_log)
# p-value = 0.05559



# checking normal distribution of Chl a
hist(dataset_bact_pulse$mean_Chla)
shapiro.test(dataset_bact_pulse$mean_Chla)
# p-value = 8.299e-09
dataset_bact_pulse$mean_Chla_log <- log(dataset_bact_pulse$mean_Chla)
hist(dataset_bact_pulse$mean_Chla_log)
shapiro.test(dataset_bact_pulse$mean_Chla_log)
# p-value = 0.2532


#---------------------------------------------------------------------------------------------------------#
# Evenness, ENS, Richness, Zooplankton biomass, Zooplankton body size

# checking normal distribution of evenness
hist(dataset_bact_pulse$mean_J)
shapiro.test(dataset_bact_pulse$mean_J)
# p-value = 0.2041


# checking normal distribution of ENS
hist(dataset_bact_pulse$mean_ENS_D)
shapiro.test(dataset_bact_pulse$mean_ENS_D)
# p-value = 0.09903

# checking normal distribution of richness
hist(dataset_bact_pulse$mean_s)
shapiro.test(dataset_bact_pulse$mean_s)
# p-value = 0.02031


# checking normal distribution of mean zooplankton biomass
hist(dataset_bact_pulse$mean_zoopl_biomass)
shapiro.test(dataset_bact_pulse$mean_zoopl_biomass)
# p-value = 7.328e-06
dataset_bact_pulse$mean_zoopl_biomass_log <- log(dataset_bact_pulse$mean_zoopl_biomass)
hist(dataset_bact_pulse$mean_zoopl_biomass_log)
shapiro.test(dataset_bact_pulse$mean_zoopl_biomass_log)
#  p-value = 0.2326


# checking normal distribution of initial zooplankton body size
hist(dataset_bact_pulse$zoopl_body_size)
shapiro.test(dataset_bact_pulse$zoopl_body_size)
# p-value = 1.455e-05
dataset_bact_pulse$zoopl_body_size_log <- log(dataset_bact_pulse$zoopl_body_size)
hist(dataset_bact_pulse$zoopl_body_size_log)
shapiro.test(dataset_bact_pulse$zoopl_body_size_log)
# p-value = 0.002341


#---------------------------------------------------------------------------------------------------------#
# final recovery, Resistance, resilience, area under the curve


# checking normal distribution of final recovery
hist(dataset_bact_pulse$final_stab)
shapiro.test(dataset_bact_pulse$final_stab)
# p-value = 2.001e-06
dataset_bact_pulse$final_stab_log <- log(dataset_bact_pulse$final_stab*-1)
hist(dataset_bact_pulse$final_stab_log)
shapiro.test(dataset_bact_pulse$final_stab_log)
# p-value = 0.4408


# checking normal distribution of resistance
hist(dataset_bact_pulse$initial_stab)
shapiro.test(dataset_bact_pulse$initial_stab)
# p-value = 0.003217
dataset_bact_pulse$initial_stab_log <- log(dataset_bact_pulse$initial_stab*-1)
hist(dataset_bact_pulse$initial_stab_log)
shapiro.test(dataset_bact_pulse$initial_stab_log)
# p-value = 0.0395


# checking normal distribution of resilience
hist(dataset_bact_pulse$rate_change_ort)
shapiro.test(dataset_bact_pulse$rate_change_ort)
# p-value = 0.1575


# checking normal distribution of AUC
hist(dataset_bact_pulse$total_impact)
shapiro.test(dataset_bact_pulse$total_impact)
# p-value = 0.02301


#---------------------------------------------------------------------------------------------------------#
#### 1. resistance for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = resistance 
# explanatory = TP, evenness, richness, ENS, DOC, zooplankton biomass, zooplankton body size 

resistance <- lmer(initial_stab_log ~ mean_TP_log + mean_s + mean_DOC_log + zoopl_body_size_log + 
                     (1|Lake), data=dataset_bact_pulse)

summary(resistance)
# TP
# richness
# DOC
# zoopl body size 


r.squaredGLMM(resistance)
# R2m       R2c
# [1,] 0.3170081 0.3170081

#---------------------------------------------------------------------------------------------------------#
#### 2. final recovery for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = final recovery 
# explanatory = TP, evenness, richness, ENS, DOC, zooplankton biomass, zooplankton body size 

recovery <- lmer(final_stab_log ~ mean_s + mean_ENS_D + mean_DOC_log +
                   mean_zoopl_biomass_log + (1|Lake), data=dataset_bact_pulse)

summary(recovery)
# richness
# ENS
# DOC
# zoopl biomass

r.squaredGLMM(recovery)
# R2m       R2c
# [1,] 0.4280992 0.4280992


#---------------------------------------------------------------------------------------------------------#
#### 3. resilience for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience
# explanatory = TP, evenness, richness, ENS, DOC, zooplankton biomass, zooplankton body size 

resilience <- lmer(rate_change_ort ~ mean_TP_log + mean_J + mean_s + (1|Lake), data=dataset_bact_pulse)

summary(resilience)
# TP
# evenness 
# richness

r.squaredGLMM(resilience)
# R2m       R2c
# [1,] 0.2022971 0.2022971

#---------------------------------------------------------------------------------------------------------#
#### 4. AUC for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = AUC
# explanatory = TP, evenness, richness, ENS, DOC, zooplankton biomass, zooplankton body size 

AUC <- lmer(total_impact ~ mean_TP_log + mean_s + mean_DOC_log + 
              mean_zoopl_biomass_log + (1|Lake), data=dataset_bact_pulse)

summary(AUC)
# TP
# richness
# DOC

r.squaredGLMM(AUC)
# R2m      R2c
# [1,] 0.3669249 0.376792

#---------------------------------------------------------------------------------------------------------#
#### 5. Evenness for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = Chl a, TP, DOC, TN/TP ratio, zooplankton biomass, zooplankton body size 

evenness <- lmer(mean_J ~  mean_Chla_log + mean_TP + mean_DOC_log +  
                   mean_zoopl_biomass_log + zoopl_body_size_log  + (1|Lake), data=dataset_bact_pulse)
summary(evenness)
# Chl a 
# TP
# DOC
# zoopl biomass
# zoopl body size

r.squaredGLMM(evenness)
# R2m       R2c
# [1,] 0.5242024 0.5242024

#---------------------------------------------------------------------------------------------------------#
#### 6. ENS for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = Chl a, TP, DOC, TN/TP ratio, zooplankton biomass, zooplankton body size 

ENS <- lmer(mean_ENS_D ~  mean_Chla_log + mean_TP + mean_TN_TP_ratio_log + (1|Lake), data=dataset_bact_pulse)

summary(ENS)
# Chl a 
# TP
# TN/TP ratio


r.squaredGLMM(ENS)
# R2m       R2c
# [1,] 0.4072486 0.9343338

#---------------------------------------------------------------------------------------------------------#


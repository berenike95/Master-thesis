#---------------------------------------------------------------------------------------------------------#
#### SEM for press disturbance & bacteria ####
#---------------------------------------------------------------------------------------------------------#

# load dataset
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/master_dataset.csv")

# filtering dataset for phytoplankton data only 
dataset_bact <- dataset_final[which(dataset_final$community=='bact'), ]

# filtering data for pulse disturbance only 
dataset_bact_press <- dataset_bact[which(dataset_bact$Treatment=='S'), ]

dataset_bact_press = subset(dataset_bact_press, select = -c(15:17))

#---------------------------------------------------------------------------------------------------------#

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for press disturbance only 
dataset_zoopl_press <- dataset_zoopl[which(dataset_zoopl$Treatment=='S'), ]

# filtering for body size and biomass 
dataset_zoopl_press <- select(dataset_zoopl_press, Lake, Experiment, Treatment, Enclosure, 
                              initial_zoop_body_size, mean_biomass)

# renaming columns
names(dataset_zoopl_press)[names(dataset_zoopl_press)=="mean_biomass"] <- "mean_zoopl_biomass"
names(dataset_zoopl_press)[names(dataset_zoopl_press)=="initial_zoop_body_size"] <- "zoopl_body_size"

# merging both datasets 
dataset_bact_press <- merge(x=dataset_bact_press, y=dataset_zoopl_press, by= c("Lake", "Experiment", 
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


# checking normal distribution of TP
hist(dataset_bact_press$mean_TP)
shapiro.test(dataset_bact_press$mean_TP)
# p-value = 0.005709
dataset_bact_press$mean_TP_log <- log(dataset_bact_press$mean_TP)
hist(dataset_bact_press$mean_TP_log)
shapiro.test(dataset_bact_press$mean_TP_log)
# p-value = 0.3459


# checking normal distribution of DOC
hist(dataset_bact_press$mean_DOC)
shapiro.test(dataset_bact_press$mean_DOC)
# p-value = 0.0008227
dataset_bact_press$mean_DOC_sqrt <- sqrt(dataset_bact_press$mean_DOC)
hist(dataset_bact_press$mean_DOC_sqrt)
shapiro.test(dataset_bact_press$mean_DOC_sqrt)
# p-value = 0.001198


# checking normal distribution of PAR
hist(dataset_bact_press$mean_PAR)
shapiro.test(dataset_bact_press$mean_PAR)
# p-value = 1.545e-06
dataset_bact_press$mean_PAR_log <- log(dataset_bact_press$mean_PAR)
hist(dataset_bact_press$mean_PAR_log)
shapiro.test(dataset_bact_press$mean_PAR_log)
# p-value = 0.00313

# checking normal distribution of TN  
hist(dataset_bact_press$mean_TN)
shapiro.test(dataset_bact_press$mean_TN)
dataset_bact_press$mean_TN_log <- log(dataset_bact_press$mean_TN*100)
hist(dataset_bact_press$mean_TN_log)
shapiro.test(dataset_bact_press$mean_TN_log)
# p-value = 0.0001328

# checking normal distribution of Chl a 
hist(dataset_bact_press$mean_Chla)
shapiro.test(dataset_bact_press$mean_Chla)
# p-value = 1.67e-08
dataset_bact_press$mean_Chla_log <- log(dataset_bact_press$mean_Chla)
hist(dataset_bact_press$mean_Chla_log)
shapiro.test(dataset_bact_press$mean_Chla_log)
# p-value = 0.3784

# checking normal distribution of TN/TP ratio
hist(dataset_bact_press$mean_TN_TP_ratio)
shapiro.test(dataset_bact_press$mean_TN_TP_ratio)
#  p-value = 0.004568
dataset_bact_press$mean_TN_TP_ratio_100 <- dataset_bact_press$mean_TN_TP_ratio*100
hist(dataset_bact_press$mean_TN_TP_ratio_100)
shapiro.test(dataset_bact_press$mean_TN_TP_ratio_100)

# checking normal distribution of DN/TP ratio 
hist(dataset_bact_press$mean_DN_TP_ratio)
shapiro.test(dataset_bact_press$mean_DN_TP_ratio)
# p-value = 0.0007845
dataset_bact_press$mean_DN_TP_ratio_log <- log(dataset_bact_press$mean_DN_TP_ratio*100)
hist(dataset_bact_press$mean_DN_TP_ratio_log)
shapiro.test(dataset_bact_press$mean_DN_TP_ratio_log)
#  p-value = 0.02709


#---------------------------------------------------------------------------------------------------------#
# Evenness, ENS, Richness, Zooplankton biomass, Zooplankton body size

# checking normal distribution of evenness
hist(dataset_bact_press$mean_J)
shapiro.test(dataset_bact_press$mean_J)
# p-value = 0.1147


# checking normal distribution of ENS
hist(dataset_bact_press$mean_ENS_D)
shapiro.test(dataset_bact_press$mean_ENS_D)
# p-value = 0.1954

# checking normal distribution of richness
hist(dataset_bact_press$mean_s)
shapiro.test(dataset_bact_press$mean_s)
# p-value = 0.009283


# checking normal distribution of mean zooplankton biomass
hist(dataset_bact_press$mean_zoopl_biomass)
shapiro.test(dataset_bact_press$mean_zoopl_biomass)
# p-value = 0.0007597
dataset_bact_press$mean_zoopl_biomass_log <- log(dataset_bact_press$mean_zoopl_biomass)
hist(dataset_bact_press$mean_zoopl_biomass_log)
shapiro.test(dataset_bact_press$mean_zoopl_biomass_log)
#  p-value = 0.04232


# checking normal distribution of initial zooplankton body size
hist(dataset_bact_press$zoopl_body_size)
shapiro.test(dataset_bact_press$zoopl_body_size)
# p-value = 0.7595

#---------------------------------------------------------------------------------------------------------#
# final recovery, Resistance, resilience, area under the curve


# checking normal distribution of final recovery
hist(dataset_bact_press$final_stab)
shapiro.test(dataset_bact_press$final_stab)
# p-value = 1.422e-07
dataset_bact_press$final_stab_log <- log(dataset_bact_press$final_stab*-1)
hist(dataset_bact_press$final_stab_log)
shapiro.test(dataset_bact_press$final_stab_log)
# p-value = 0.4624


# checking normal distribution of resilience
hist(dataset_bact_press$rate_change_ort)
shapiro.test(dataset_bact_press$rate_change_ort)
# p-value = 0.09742


# checking normal distribution of AUC
hist(dataset_bact_press$total_impact)
shapiro.test(dataset_bact_press$total_impact)
# p-value = 0.01544

#---------------------------------------------------------------------------------------------------------#
#### 1. final recovery for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = final recovery 
# explanatory = TP, TN/TP ratio, DN/TP ratio, evenness, richness, ENS, DOC, zooplankton biomass, zooplankton body size 

recovery <- lmer(final_stab_log ~ zoopl_body_size + mean_DOC_log + mean_TN_TP_ratio_100 + mean_DN_TP_ratio_log +
                   mean_ENS_D + (1|Lake), data=dataset_bact_press)

summary(recovery)
# zooplankton body size 
# DOC 
# TN/TP ratio

r.squaredGLMM(recovery)
# R2m       R2c
# [1,] 0.1837593 0.1837593


#---------------------------------------------------------------------------------------------------------#
#### 2. resilience for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience
# explanatory = TP, TN/TP ratio, evenness, richness, ENS, DOC, zooplankton biomass, zooplankton body size 

resilience <- lmer(rate_change_oti ~ mean_TP + mean_TN_TP_ratio_100 + mean_s + mean_zoopl_biomass_log + (1|Lake), data=dataset_bact_press)

summary(resilience)
# richness


r.squaredGLMM(resilience)
#  R2m       R2c
# [1,] 0.1895576 0.4138963


#---------------------------------------------------------------------------------------------------------#
#### 3. AUC for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = AUC
# explanatory = TP, TN/TP ratio, evenness, richness, ENS, DOC, zooplankton biomass, zooplankton body size 

AUC <- lmer(total_impact ~ mean_TP + mean_TN_TP_ratio_100 + mean_DN_TP_ratio_log + mean_J + mean_s + 
              mean_DOC_log + zoopl_body_size + (1|Lake), data=dataset_bact_press)

summary(AUC)
# TP
# TN/TP ratio
# DN/TP ratio
# evenness
# richness
# DOC
# zoopl body size 

r.squaredGLMM(AUC)
# R2m       R2c
# [1,] 0.4591336 0.4591336

#---------------------------------------------------------------------------------------------------------#
#### 4. Evenness for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = Chl a, TP, DOC, TN/TP ratio, zooplankton biomass, zooplankton body size 

evenness <- lmer(mean_J ~  mean_Chla_log + mean_TP + mean_DOC_log + zoopl_body_size  + (1|Lake), 
                 data=dataset_bact_press)
summary(evenness)
# Chl a
# TP
# DOC
# zoopl body size 

r.squaredGLMM(evenness)
# R2m       R2c
# [1,] 0.6229686 0.6229686

#---------------------------------------------------------------------------------------------------------#
#### 5. ENS for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = Chl a, TP, DOC, TN/TP ratio, DN/TP ratio, zooplankton biomass, zooplankton body size 

ENS <- lmer(mean_ENS_D ~  mean_Chla_log + mean_TP + mean_TN_TP_ratio_100 + mean_DN_TP_ratio_log +
          mean_zoopl_biomass_log + zoopl_body_size  + (1|Lake), data=dataset_bact_press)

summary(ENS)
# Chl a 
# TP
# TN/TP ratio
# DN/TP ratio
# zoopl biomass
# zoopl body size 

r.squaredGLMM(ENS)
# R2m       R2c
# [1,] 0.404867 0.9238743



#---------------------------------------------------------------------------------------------------------#
#### 6. richness for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = richness
# explanatory = Chl a, TP, DOC, TN/TP ratio, DN/TP ratio, zooplankton biomass, zooplankton body size 

richness <- lmer(mean_s ~  mean_Chla_log + mean_TP + mean_TN_TP_ratio_100 + mean_DN_TP_ratio_log +
              mean_zoopl_biomass_log + (1|Lake), data=dataset_bact_press)

summary(richness)
# Chl a 
# TP
# TN/TP
# DN/TP
# zoopl biomass






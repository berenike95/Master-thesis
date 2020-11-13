#---------------------------------------------------------------------------------------------------------#
#### SEM for pulse & press disturbance for bacteria ####
#---------------------------------------------------------------------------------------------------------#


# load dataset
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/master_dataset.csv")

# filtering dataset for phytoplankton data only 
dataset_bact <- dataset_final[which(dataset_final$community=='bact'), ]

# filtering data for pulse disturbance only 
bact_pulse_press <- dataset_bact[which(dataset_bact$Treatment=='FS'), ]

bact_pulse_press = subset(bact_pulse_press, select = -c(15:17))

#---------------------------------------------------------------------------------------------------------#

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for press disturbance only 
dataset_zoopl_pulse <- dataset_zoopl[which(dataset_zoopl$Treatment=='FS'), ]

# filtering for body size and biomass 
dataset_zoopl_pulse <- select(dataset_zoopl_pulse, Lake, Experiment, Treatment, Enclosure, 
                              initial_zoop_body_size, mean_biomass)

# renaming columns
names(dataset_zoopl_pulse)[names(dataset_zoopl_pulse)=="mean_biomass"] <- "mean_zoopl_biomass"
names(dataset_zoopl_pulse)[names(dataset_zoopl_pulse)=="initial_zoop_body_size"] <- "zoopl_body_size"

# merging both datasets 
bact_pulse_press <- merge(x=bact_pulse_press, y=dataset_zoopl_pulse, by= c("Lake", "Experiment", 
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

# TN, TP, DOC, PAR, Temp, TN/TP ratio, DN/TP ratio


# checking normal distribution of TN
hist(bact_pulse_press$mean_TN)
shapiro.test(bact_pulse_press$mean_TN)
# p-value = 2.56e-05
bact_pulse_press$mean_TN_100 <- bact_pulse_press$mean_TN*100


# checking normal distribution of TP
hist(bact_pulse_press$mean_TP)
shapiro.test(bact_pulse_press$mean_TP)
# p-value = 0.03724
bact_pulse_press$mean_TP_log <- log(bact_pulse_press$mean_TP)
hist(bact_pulse_press$mean_TP_log)
shapiro.test(bact_pulse_press$mean_TP_log)
# p-value = 0.3883


# checking normal distribution of DOC
hist(bact_pulse_press$mean_DOC)
shapiro.test(bact_pulse_press$mean_DOC)
#  p-value = 0.001361
bact_pulse_press$mean_DOC_sqrt <- sqrt(bact_pulse_press$mean_DOC)
hist(bact_pulse_press$mean_DOC_sqrt)
shapiro.test(bact_pulse_press$mean_DOC_sqrt)
#  p-value = 0.001937


# checking normal distribution of PAR
hist(bact_pulse_press$mean_PAR)
shapiro.test(bact_pulse_press$mean_PAR)
# p-value = 1.844e-06
bact_pulse_press$mean_PAR_log <- log(bact_pulse_press$mean_PAR)
hist(bact_pulse_press$mean_PAR_log)
shapiro.test(bact_pulse_press$mean_PAR_log)
# p-value = 0.01372


# checking normal distribution of Temp
hist(bact_pulse_press$mean_Temp)
shapiro.test(bact_pulse_press$mean_Temp)
# p-value = 3.616e-06
bact_pulse_press$mean_Temp_10 <- bact_pulse_press$mean_Temp/10


# checking normal distribution of TN/TP ratio 
hist(bact_pulse_press$mean_TN_TP_ratio)
shapiro.test(bact_pulse_press$mean_TN_TP_ratio)
# p-value = 0.002027
bact_pulse_press$mean_TN_TP_ratio_log <- log(bact_pulse_press$mean_TN_TP_ratio * 100)
hist(bact_pulse_press$mean_TN_TP_ratio_log)
shapiro.test(bact_pulse_press$mean_TN_TP_ratio_log)
# p-value = 0.003649


# checking normal distribution of DN/TP ratio 
hist(bact_pulse_press$mean_DN_TP_ratio)
shapiro.test(bact_pulse_press$mean_DN_TP_ratio)
# p-value = 0.0003171
bact_pulse_press$mean_DN_TP_ratio_log <- log(bact_pulse_press$mean_DN_TP_ratio * 100)
hist(bact_pulse_press$mean_DN_TP_ratio_log)
shapiro.test(bact_pulse_press$mean_DN_TP_ratio_log)
#  p-value = 0.01289


# checking normal distribution of Chl a  
hist(bact_pulse_press$mean_Chla)
shapiro.test(bact_pulse_press$mean_Chla)
# p-value = 1.133e-08
bact_pulse_press$mean_Chla_log <- log(bact_pulse_press$mean_Chla)
hist(bact_pulse_press$mean_Chla_log)
shapiro.test(bact_pulse_press$mean_Chla_log)
# p-value = 0.07454

#---------------------------------------------------------------------------------------------------------#
# Evenness, ENS, Richness, Zooplankton biomass, Zooplankton body size

# checking normal distribution of evenness
hist(bact_pulse_press$mean_J)
shapiro.test(bact_pulse_press$mean_J)
# p-value = 0.8084


# checking normal distribution of ENS
hist(bact_pulse_press$mean_ENS_D)
shapiro.test(bact_pulse_press$mean_ENS_D)
# p-value = 0.02186

# checking normal distribution of richness
hist(bact_pulse_press$mean_s)
shapiro.test(bact_pulse_press$mean_s)
# p-value = 0.003706
bact_pulse_press$mean_s_log <- log(bact_pulse_press$mean_s)
hist(bact_pulse_press$mean_s_log)
shapiro.test(bact_pulse_press$mean_s_log)

# checking normal distribution of mean zooplankton biomass
hist(bact_pulse_press$mean_zoopl_biomass)
shapiro.test(bact_pulse_press$mean_zoopl_biomass)
# p-value = 3.378e-06
bact_pulse_press$mean_zoopl_biomass_log <- sqrt(bact_pulse_press$mean_zoopl_biomass)
hist(bact_pulse_press$mean_zoopl_biomass_log)
shapiro.test(bact_pulse_press$mean_zoopl_biomass_log)
#  p-value = 0.0282


# checking normal distribution of initial zooplankton body size
hist(bact_pulse_press$zoopl_body_size)
shapiro.test(bact_pulse_press$zoopl_body_size)
# p-value = 0.0009254

#---------------------------------------------------------------------------------------------------------#
# final recovery, Resistance, resilience, area under the curve


# checking normal distribution of final recovery
hist(bact_pulse_press$final_stab)
shapiro.test(bact_pulse_press$final_stab)
# p-value = 3.257e-08
bact_pulse_press$final_stab_log <- log(bact_pulse_press$final_stab*-1)
hist(bact_pulse_press$final_stab_log)
shapiro.test(bact_pulse_press$final_stab_log)
# p-value = 0.8049


# checking normal distribution of resistance
hist(bact_pulse_press$initial_stab)
shapiro.test(bact_pulse_press$initial_stab)
# p-value = 1.014e-06
bact_pulse_press$initial_stab_log <- log(bact_pulse_press$initial_stab*-1)
hist(bact_pulse_press$initial_stab_log)
shapiro.test(bact_pulse_press$initial_stab_log)
# p-value = 0.3968


# checking normal distribution of resilience
hist(bact_pulse_press$rate_change_ort)
shapiro.test(bact_pulse_press$rate_change_ort)
# p-value = 0.000209


# checking normal distribution of AUC
hist(bact_pulse_press$total_impact)
shapiro.test(bact_pulse_press$total_impact)
# p-value = 0.0009905
bact_pulse_press$total_impact_log <- log(bact_pulse_press$total_impact)
hist(bact_pulse_press$total_impact_log)
shapiro.test(bact_pulse_press$total_impact_log)
# p-value = 0.6084

#---------------------------------------------------------------------------------------------------------#
#### 1. resistance for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = resistance 
# explanatory = TP, TN, TN/TP, DN/TP, evenness, richness, ENS, DOC, zooplankton biomass, zooplankton body size  

resistance <- lmer(initial_stab_log ~ mean_TP_log + mean_DN_TP_ratio_log + 
              mean_s_log + mean_ENS_D + mean_DOC_sqrt + zoopl_body_size_log + (1|Lake), data=bact_pulse_press)

summary(resistance)
# TP
# DN/TP ratio 
# richness
# DOC 

r.squaredGLMM(resistance)
# R2m       R2c
# [1,] 0.2207936 0.2207936


#---------------------------------------------------------------------------------------------------------#
#### 2. final recovery for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = final recovery 
# explanatory = TP, TN, TN/TP, DN/TP, evenness, richness, ENS, DOC, zooplankton biomass, zooplankton body size 

recovery <- lmer(final_stab_log ~ mean_TP_log + mean_DN_TP_ratio_log + mean_s_log + mean_ENS_D + 
                   mean_DOC_sqrt + zoopl_body_size + mean_zoopl_biomass_log + (1|Lake), data=bact_pulse_press)

summary(recovery)
# TP
# richness
# ENS
# DOC
# zoopl body size
# zoopl biomass

r.squaredGLMM(recovery)
# R2m       R2c
# [1,] 0.3868729 0.5055213


#---------------------------------------------------------------------------------------------------------#
#### 3. resilience for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience
# explanatory = TP, TN, TN/TP, DN/TP, evenness, richness, ENS, DOC, zooplankton biomass, zooplankton body size 

resilience <- lmer(rate_change_ort ~ mean_TP_log + mean_TN_TP_ratio_log + mean_DN_TP_ratio_log +
            mean_J + mean_s + (1|Lake), data=bact_pulse_press)

summary(resilience)
# TN/TP ratio
# DN/TP ratio 
# evenness
# richness

r.squaredGLMM(resilience)
# R2m        R2c
# [1,] 0.07381362 0.07381362

#---------------------------------------------------------------------------------------------------------#
#### 4. AUC for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = AUC
# explanatory = TP, TN, TN/TP, DN/TP, evenness, richness, ENS, DOC, zooplankton biomass, zooplankton body size 

AUC <- lmer(total_impact_log ~ mean_DN_TP_ratio_log + mean_J + mean_s + mean_ENS_D + zoopl_body_size + 
              mean_zoopl_biomass_log + (1|Lake), data=bact_pulse_press)

summary(AUC)
# evenness
# richness
# DOC
# zoopl body size 

r.squaredGLMM(AUC)
# R2m       R2c
# [1,] 0.1965265 0.4053822

#---------------------------------------------------------------------------------------------------------#
#### 5. Evenness for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = Chl a, TP, TN, DOC, TN/TP ratio, DN/TP ratio, zooplankton biomass, zooplankton body size 

evenness <- lmer(mean_J ~  mean_Chla_log + mean_TP_log + mean_TN_100 + mean_TN_TP_ratio_log + 
         mean_DN_TP_ratio_log + mean_DOC_sqrt + mean_zoopl_biomass_log + zoopl_body_size_log  + (1|Lake), data=bact_pulse_press)
summary(evenness)
# Chl a
# TP
# TN
# TN/TP
# DN/TP
# DOC
# zoopl biomass
# zoopl body size 

r.squaredGLMM(evenness)
# R2m       R2c
# [1,] 0.54008 0.5866821

#---------------------------------------------------------------------------------------------------------#
#### 6. ENS for bacteria ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = Chl a, TP, TN, DOC, TN/TP ratio, DN/TP ratio, zooplankton biomass, zooplankton body size 

ENS <- lmer(mean_ENS_D ~  mean_TN_100 + mean_TN_TP_ratio_log + 
    mean_DN_TP_ratio_log + mean_DOC_sqrt + mean_zoopl_biomass_log + zoopl_body_size + (1|Lake), data=bact_pulse_press)

summary(ENS)
# TN/TP ratio
# DN/TP ratio
# DOC
# zoopl body size 

r.squaredGLMM(ENS)
# R2m       R2c
# [1,] 0.3794835 0.9922483

#---------------------------------------------------------------------------------------------------------#


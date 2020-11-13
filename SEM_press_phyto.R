#---------------------------------------------------------------------------------------------------------#
#### SEM for press disturbance & phytoplankton####
#---------------------------------------------------------------------------------------------------------#

# load dataset
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/master_dataset.csv")

# filtering dataset for phytoplankton data only 
dataset_phyto <- dataset_final[which(dataset_final$community=='phyto'), ]

# filtering data for press disturbance only 
dataset_phyto_press <- dataset_phyto[which(dataset_phyto$Treatment=='S'), ]

dataset_phyto_press = subset(dataset_phyto_press, select = -c(16,17))

#---------------------------------------------------------------------------------------------------------#

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for press disturbance only 
dataset_zoopl_press <- dataset_zoopl[which(dataset_zoopl$Treatment=='S'), ]

# filtering for body size and biomass 
dataset_zoopl_press <- select(dataset_zoopl_press, Lake, Experiment, Treatment, Enclosure, 
                              initial_zoop_body_size, mean_initial_zoop_biomass)

# renaming columns
names(dataset_zoopl_press)[names(dataset_zoopl_press)=="mean_initial_zoop_biomass"] <- "zoopl_biomass"
names(dataset_zoopl_press)[names(dataset_zoopl_press)=="initial_zoop_body_size"] <- "zoopl_body_size"

# merging both datasets 
dataset_phyto_press <- merge(x=dataset_phyto_press, y=dataset_zoopl_press, by= c("Lake", "Experiment", 
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


# checking normal distribution of DOC
hist(dataset_phyto_press$mean_DOC)
shapiro.test(dataset_phyto_press$mean_DOC)
dataset_phyto_press$mean_DOC_sqrt <- sqrt(dataset_phyto_press$mean_DOC)
hist(dataset_phyto_press$mean_DOC_sqrt)
shapiro.test(dataset_phyto_press$mean_DOC_sqrt)


# checking normal distribution of PAR
hist(dataset_phyto_press$mean_PAR)
shapiro.test(dataset_phyto_press$mean_PAR)
dataset_phyto_press$mean_PAR_log <- log(dataset_phyto_press$mean_PAR)
hist(dataset_phyto_press$mean_PAR_log)
shapiro.test(dataset_phyto_press$mean_PAR_log)


# checking normal distribution of TN  
hist(dataset_phyto_press$mean_TN)
shapiro.test(dataset_phyto_press$mean_TN)
dataset_phyto_press$mean_TN_100 <- dataset_phyto_press$mean_TN*100


# checking normal distribution of Chl a 
hist(dataset_phyto_press$mean_Chla)
shapiro.test(dataset_phyto_press$mean_Chla)
dataset_phyto_press$mean_Chla_log <- log(dataset_phyto_press$mean_Chla)
hist(dataset_phyto_press$mean_Chla_log)
shapiro.test(dataset_phyto_press$mean_Chla_log)


# checking normal distribution of TN/TP ratio
hist(dataset_phyto_press$mean_TN_TP_ratio_100)
shapiro.test(dataset_phyto_press$mean_TN_TP_ratio_100)
dataset_phyto_press$mean_TN_TP_ratio_100 <- dataset_phyto_press$mean_TN_TP_ratio*100


# checking normal distribution of DN/TP ratio 
hist(dataset_phyto_press$mean_DN_TP_ratio)
shapiro.test(dataset_phyto_press$mean_DN_TP_ratio)
dataset_phyto_press$mean_DN_TP_ratio_100 <- dataset_phyto_press$mean_DN_TP_ratio*100
hist(dataset_phyto_press$mean_DN_TP_ratio_100)
shapiro.test(dataset_phyto_press$mean_DN_TP_ratio_100)

#---------------------------------------------------------------------------------------------------------#
# Biovolume, Evenness, ENS, Richness, Zooplankton biomass, Zooplankton body size


# checking normal distribution of biomass
hist(dataset_phyto_press$mean_biomass)
shapiro.test(dataset_phyto_press$mean_biomass)
dataset_phyto_press$mean_biomass_log <- log(dataset_phyto_press$mean_biomass)
hist(dataset_phyto_press$mean_biomass_log)
shapiro.test(dataset_phyto_press$mean_biomass_log)


# checking normal distribution of zooplankton initial body size 
hist(dataset_phyto_press$zoopl_biomass)
shapiro.test(dataset_phyto_press$zoopl_biomass)
dataset_phyto_press$zoopl_biomass_log <- log(dataset_phyto_press$zoopl_biomass)
hist(dataset_phyto_press$zoopl_biomass_log)
shapiro.test(dataset_phyto_press$zoopl_biomass_log)


# checking normal distribution of zooplankton initial biomass 
hist(dataset_phyto_press$zoopl_body_size)
shapiro.test(dataset_phyto_press$zoopl_body_size)

#---------------------------------------------------------------------------------------------------------#
# Final reovery, resilience, area under the curve


# checking normal distribution of resilience
hist(dataset_phyto_press$rate_change_oti)
shapiro.test(dataset_phyto_press$rate_change_oti)
dataset_phyto_press$rate_change_oti_trans <- sqrt(dataset_phyto_press$rate_change_oti*-1)
shapiro.test(dataset_phyto_press$rate_change_oti_trans)


# checking normal distribution of AUC
hist(dataset_phyto_press$total_impact)
shapiro.test(dataset_phyto_press$total_impact)
# p-value = 3.368e-06
dataset_phyto_press$total_impact_sqrt <- sqrt(dataset_phyto_press$total_impact)


# checking normal distribution of final recovery
hist(dataset_phyto_press$final_stab)
shapiro.test(dataset_phyto_press$final_stab)
# p-value = 0.000172
dataset_phyto_press$final_stab <- (dataset_phyto_press$final_stab*-1)
dataset_phyto_press$final_stab <- sqrt(dataset_phyto_press$final_stab)



#---------------------------------------------------------------------------------------------------------#
#### 1. final recovery for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# final recovery = response
# Chl a, Evenness, ENS, TP, TN/TP ratio, light = explanatory
# Chl a = proxy for phyto biomass


# running linear mixed-effects model for final recovery

final_recovery <- lmer(final_stab ~ mean_Chla_log + mean_J + mean_TN_TP_ratio_100 + zoopl_biomass_log + 
                         zoopl_body_size + (1|Lake), data=dataset_phyto_press)

summary(final_recovery)
# evenness t-value = -2.053
# Chl a t-value = -2.763 
# TN/TP t-value = -1.266
# zoopl biomass t-value = 1.874
# zoopl body size t-value = -2.634


?isSingular

# explained variance
r.squaredGLMM(final_recovery)
#            R2m      R2c
# [1,] 0.500044 0.500044

#---------------------------------------------------------------------------------------------------------#
#### 2. resilience for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#

resilience <- lmer(rate_change_oti_trans ~ mean_Chla_log + mean_TP + mean_TN_TP_ratio_100 + 
                     zoopl_biomass_log + zoopl_body_size + (1|Lake),data=dataset_phyto_press)

summary(resilience)
# Chl a t-value= -1.917  
# TP t-value =  1.155
# TN/TP ratio t-value =  0.930 
# zoopl biomass t-value = 1.000
# zoopl body size t-value = -0.975 

r.squaredGLMM(resilience)
#           R2m       R2c
# [1,] 0.1977085 0.1977085

#---------------------------------------------------------------------------------------------------------#
#### 3. AUC for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#

AUC <- lmer(total_impact_sqrt ~  mean_ENS_D + mean_TP + mean_TN_TP_ratio_100 + mean_DN_TP_ratio_100 
            + zoopl_biomass_log + zoopl_body_size +(1|Lake), data=dataset_phyto_press)

summary(AUC)
# mean_ENS_D t-value: -2.491 
# mean_TN_TP_ratio  t-value:  3.295
# mean_DN_TP_ratio t-value: -2.431 
# mean_TP t-value:  1.419

#---------------------------------------------------------------------------------------------------------#
#### 4. Biovolume for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#

biovolume <- lmer(mean_biomass_log ~  mean_TP + mean_TN_TP_ratio_100 + mean_TN_100  +(1|Lake), data=dataset_phyto_press)

summary(biovolume)
# TP t-value = 2.495
# TN/TP ratio t-vlue = 1.447
# TN t-value t-value = -1.591

r.squaredGLMM(biovolume)
#      R2m       R2c
# [1,] 0.3936048 0.7523003

#---------------------------------------------------------------------------------------------------------#
#### 5. Evenness for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#

evenness <- lmer(mean_J ~  mean_DOC_sqrt + mean_PAR_log + mean_TN_100 + (1|Lake), 
                  data=dataset_phyto_press)

summary(evenness)
# mean_PAR t-value: -2.361
# mean_TN t-value: -1.915
# mean_DOC t-value: -2.295

dataset_phyto_press$mean_TN <- scale(dataset_phyto_press$mean_TN)
dataset_phyto_press$mean_PAR <- scale(dataset_phyto_press$mean_PAR)
dataset_phyto_press$mean_DOC <- scale(dataset_phyto_press$mean_DOC)

r.squaredGLMM(evenness)
#    R2m       R2c
# [1,] 0.3600201 0.9682142

#---------------------------------------------------------------------------------------------------------#
#### 6. ENS for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
ENS <- lmer(mean_ENS_D ~  mean_DOC_sqrt + mean_PAR_log + mean_TN_100 + mean_TP + mean_TN_TP_ratio_100 + mean_DN_TP_ratio_100 + (1|Lake), 
                 data=dataset_phyto_press)

summary(ENS)
# DOC t-value = -3.987
# TN t-value = -2.339
# TN/TP ratio t-value =2.842
# TP t-value t-value = 1.613
# DN/TP ratio t-value = -1.277

r.squaredGLMM(ENS)
#     R2m       R2c
# [1,] 0.4048513 0.9962715

#---------------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------#
#### SEM for pulse& press disturbance for zooplankton ####
#---------------------------------------------------------------------------------------------------------#

# load packages

library(lme4)

library(lmerTest)

library(MuMIn)

library(tidyverse)

library(dplyr)

library(faraway)

#---------------------------------------------------------------------------------------------------------#

# load dataset
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/master_dataset3.csv")

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for pulse disturbance only 
zoopl_pulse_press <- dataset_zoopl[which(dataset_zoopl$Treatment=='FS'), ]

#---------------------------------------------------------------------------------------------------------#

# filtering dataset for phytoplankton data only 
dataset_phyto <- dataset_final[which(dataset_final$community=='phyto'), ]

# filtering data for pulse disturbance only 
phyto_pulse_press <- dataset_phyto[which(dataset_phyto$Treatment=='FS'), ]

# renaming columns for phyto biovolume and chl a
names(phyto_pulse_press)[names(phyto_pulse_press)=="mean_biomass"] <- "phyto_biovolume"

phyto_pulse_press <- select(phyto_pulse_press, Lake, Experiment, Treatment, Enclosure, phyto_biovolume)

zoopl_pulse_press <- merge(x=phyto_pulse_press, y=zoopl_pulse_press, by= c("Lake", "Experiment", 
                                                                                       "Treatment", "Enclosure"))

#---------------------------------------------------------------------------------------------------------#
#### 0. normal distribution and scaling of variables ####
#---------------------------------------------------------------------------------------------------------#
# Variables:

# TN, TP, DOC, PAR, Temp, TN/TP ratio, DN/TP ratio


# checking normal distribution of TN
hist(zoopl_pulse_press$mean_TN)
shapiro.test(zoopl_pulse_press$mean_TN)
#   p-value = 2.56e-05

# checking normal distribution of TP
hist(zoopl_pulse_press$mean_TP)
shapiro.test(zoopl_pulse_press$mean_TP)
# p-value = 0.03724
zoopl_pulse_press$mean_TP_log <- log(zoopl_pulse_press$mean_TP)
hist(zoopl_pulse_press$mean_TP_log)
shapiro.test(zoopl_pulse_press$mean_TP_log)
# p-value = 0.3883


# checking normal distribution of DOC
hist(zoopl_pulse_press$mean_DOC)
shapiro.test(zoopl_pulse_press$mean_DOC)
#  p-value = 0.001361


# checking normal distribution of PAR
hist(zoopl_pulse_press$mean_PAR)
shapiro.test(zoopl_pulse_press$mean_PAR)
# p-value = 1.844e-06
zoopl_pulse_press$mean_PAR_log <- log(zoopl_pulse_press$mean_PAR)
hist(zoopl_pulse_press$mean_PAR_log)
shapiro.test(zoopl_pulse_press$mean_PAR_log)
# p-value = 0.01372


# checking normal distribution of Temp
hist(zoopl_pulse_press$mean_Temp)
shapiro.test(zoopl_pulse_press$mean_Temp)
# p-value = 3.616e-06

# checking normal distribution of TN/TP ratio 
hist(zoopl_pulse_press$mean_TN_TP_ratio)
shapiro.test(zoopl_pulse_press$mean_TN_TP_ratio)
# p-value = 0.002027
zoopl_pulse_press$mean_TN_TP_ratio_100 <- zoopl_pulse_press$mean_TN_TP_ratio * 100
hist(zoopl_pulse_press$mean_TN_TP_ratio_100)


# checking normal distribution of DN/TP ratio 
hist(zoopl_pulse_press$mean_DN_TP_ratio)
shapiro.test(zoopl_pulse_press$mean_DN_TP_ratio)
# p-value = 0.0003171
zoopl_pulse_press$mean_DN_TP_ratio_100 <- zoopl_pulse_press$mean_DN_TP_ratio *100
hist(zoopl_pulse_press$mean_DN_TP_ratio_100)

#---------------------------------------------------------------------------------------------------------#
# Biomass, Evenness, ENS, Richness, Phytoplankton biovolume, Chl a 


# checking normal distribution of biomass
hist(zoopl_pulse_press$mean_biomass)
shapiro.test(zoopl_pulse_press$mean_biomass)
# p-value = 3.378e-06
zoopl_pulse_press$mean_biomass_sqrt <- sqrt(zoopl_pulse_press$mean_biomass)
hist(zoopl_pulse_press$mean_biomass_sqrt)
shapiro.test(zoopl_pulse_press$mean_biomass_sqrt)
# p-value = 0.0282

# checking normal distribution of evenness
hist(zoopl_pulse_press$mean_J)
shapiro.test(zoopl_pulse_press$mean_J)
# p-value = 0.04245


# checking normal distribution of ENS
hist(zoopl_pulse_press$mean_ENS_D)
shapiro.test(zoopl_pulse_press$mean_ENS_D)
# p-value = 0.8125


# checking normal distribution of richness
hist(zoopl_pulse_press$mean_s)
shapiro.test(zoopl_pulse_press$mean_s)
# p-value = 0.1654


# checking normal distribution of phytoplankton biovolume
hist(zoopl_pulse_press$phyto_biovolume)
shapiro.test(zoopl_pulse_press$phyto_biovolume)
# p-value = 1.432e-06
zoopl_pulse_press$phyto_biovolume_log <- log(zoopl_pulse_press$phyto_biovolume)
hist(zoopl_pulse_press$phyto_biovolume_log)
shapiro.test(zoopl_pulse_press$phyto_biovolume_log)
# p-value = 0.1644


# checking normal distribution of Chl a 
hist(zoopl_pulse_press$mean_Chla)
shapiro.test(zoopl_pulse_press$mean_Chla)
# p-value = 1.133e-08
zoopl_pulse_press$mean_Chla_log <- log(zoopl_pulse_press$mean_Chla)
hist(zoopl_pulse_press$mean_Chla_log)
shapiro.test(zoopl_pulse_press$mean_Chla_log)
# p-value = 0.07454


#---------------------------------------------------------------------------------------------------------#
# Resistance, final reovery, resilience, area under the curve


# checking normal distribution of resistance
hist(zoopl_pulse_press$initial_stab)
shapiro.test(zoopl_pulse_press$initial_stab)
# p-value = 0.004312
zoopl_pulse_press$initial_stab_log <- log(zoopl_pulse_press$initial_stab*-1)
hist(zoopl_pulse_press$initial_stab_log)
shapiro.test(zoopl_pulse_press$initial_stab_log)
#  p-value = 0.6004


# checking normal distribution of final recovery
hist(zoopl_pulse_press$final_stab)
shapiro.test(zoopl_pulse_press$final_stab)
# p-value = 2.133e-07
zoopl_pulse_press$final_stab_log <- log(zoopl_pulse_press$final_stab*-1)
hist(zoopl_pulse_press$final_stab_log)
shapiro.test(zoopl_pulse_press$final_stab_log)
# p-value = 0.004258


# checking normal distribution of resilience
hist(zoopl_pulse_press$rate_change_ort)
shapiro.test(zoopl_pulse_press$rate_change_ort)
# p-value = 1.444e-06


# checking normal distribution of AUC
hist(zoopl_pulse_press$total_impact)
shapiro.test(zoopl_pulse_press$total_impact)
# p-value = 8.193e-05
zoopl_pulse_press$total_impact_log <- log(zoopl_pulse_press$total_impact)
hist(zoopl_pulse_press$total_impact_log)
shapiro.test(zoopl_pulse_press$total_impact_log)
# p-value = 0.9235

#---------------------------------------------------------------------------------------------------------#
#### 1. resistance for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resistance 
# explanatory = TP, TN, TN/TP, DN/TP, DOC, PAR, Temp, biomass, evenness, ens, richness, phyto biovolume, chl a

# testing significance of all variables individually 
resistance <- lmer(initial_stab_log ~ phyto_biovolume_log + (1|Lake), data=zoopl_pulse_press)

summary(resistance)
# Chl a p-value = 0.784 
# TP p-value = 0.706 
# TN p-value = 0.831 
# TN/TP p-value = 0.831 
# DN/TP p-value =  0.945 
# DOC p-value = 0.0369 !
# PAR p-value = 0.649 
# Temp p-value = 0.756 
# biovol p-value = 0.01268 !
# evenness p-value = 0.589
# ENS p-value = 0.420 
# richness p-value = 0.00627 !
# phyto biovol p-value = 0.963 

# ---> DOC, bivolume, richness
test <- lm(initial_stab_log ~ mean_DOC + phyto_biovolume_log + mean_s, data=zoopl_pulse_press)
vif(test)
#  mean_DOC_10      phyto_biovolume_log              mean_s 
#  1.141092            1.543848                    1.617455 

resistance <- lmer(initial_stab_log ~ mean_DOC + (1|Lake), data=zoopl_pulse_press)

summary(resistance)

r.squaredGLMM(resistance)
#  R2m       R2c
# [1,] 0.2426139 0.5349752

#---------------------------------------------------------------------------------------------------------#
#### 2. final recovery for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = recovery 
# explanatory = TP, TN, TN/TP, DN/TP, DOC, PAR, Temp, biomass, evenness, ens, richness, phyto biovolume, chl a

# testing significance of all variables individually 
recovery <- lmer(final_stab_log ~ phyto_biovolume_log + (1|Lake), data=zoopl_pulse_press)

summary(recovery)
# Chl a p-value = 0.833
# TP p-value =  0.278 
# TN p-value = 0.197 
# TN/TP p-value = 0.826 
# DN/TP p-value = 0.530 
# DOC p-value = 0.000464 --> singular fit
# PAR p-value = 0.0869 
# Temp p-value = 0.945 
# biomass p-value = 0.717 
# evenness p-value = 0.429
# ENS p-value = 0.494
# richness p-value = 0.511 
# phyto biovol p-value = 0.204

#---------------------------------------------------------------------------------------------------------#
#### 3. AUC for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = AUC 
# explanatory = TP, TN, TN/TP, DN/TP, DOC, PAR, Temp, biomass, evenness, ens, richness, phyto biovolume, chl a

# testing significance of all variables individually 
AUC <- lmer(total_impact_log ~ phyto_biovolume_log + (1|Lake), data=zoopl_pulse_press)

summary(AUC)
# Chl a p-value = 0.710574  
# TP p-value =  0.9954  
# TN p-value = 0.95808  
# TN/TP p-value = 0.683  
# DN/TP p-value = 0.955  
# DOC p-value = 0.01005 !
# PAR p-value = 0.797  
# Temp p-value = 0.1623 
# biomass p-value = 0.0957  
# evenness p-value = 0.052 
# ENS p-value = 0.104  
# richness p-value = 0.0203 !
# phyto biovol p-value = 0.438270 

# --> DOC, richness

AUC <- lmer(total_impact_log ~ mean_DOC + (1|Lake), data=zoopl_pulse_press)

summary(AUC)

r.squaredGLMM(AUC)
# R2m       R2c
# [1,] 0.4912176 0.5062719

#---------------------------------------------------------------------------------------------------------#
#### 4. resilience for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience 
# explanatory = TP, TN, TN/TP, DN/TP, DOC, PAR, Temp, biomass, evenness, ens, richness, phyto biovolume, chl a

# testing significance of all variables individually 
resilience <- lmer(rate_change_ort ~ phyto_biovolume_log + (1|Lake), data=zoopl_pulse_press)

summary(resilience)
# Chl a p-value = 0.519 
# TP p-value = 0.701 
# TN p-value = 0.920 
# TN/TP p-value = 0.761 
# DN/TP p-value =  0.845 
# DOC p-value = 0.559 
# PAR p-value = 0.751 
# Temp p-value = 0.712 
# biomass p-value = 0.3303 
# evenness p-value = 0.879 
# ENS p-value = 0.855 
# richness p-value =  0.304 
# phyto biovol p-value = 0.696 


#---------------------------------------------------------------------------------------------------------#
#### 5. Biomass for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = mean zoopl biomass
# explanatory = TP, TN, TN/TP, DN/TP, DOC, PAR, Temp,phyto biovolume, chl a

# testing significance of all variables individually 
biomass <- lmer(mean_biomass_sqrt ~ phyto_biovolume_log + (1|Lake), data=zoopl_pulse_press)

summary(biomass)
# Chl a p-value = 0.0179 !
# TP p-value =  0.0635  
# TN p-value = 7.56e-09 !
# TN/TP p-value = 0.974 
# DN/TP p-value = 0.5803  
# DOC p-value = 0.5693   
# PAR p-value = 0.00202 !
# Temp p-value = 0.5080 
# phyto biovol p-value = 0.00631 !

# --> Chl a, TN, PAR, phyto biovolume

test <- lm(mean_biomass_sqrt ~ mean_Chla_log + mean_TN + mean_PAR_log + phyto_biovolume_log, data=zoopl_pulse_press)
vif(test)
# mean_Chla_log          mean_TN_10        mean_PAR_log       phyto_biovolume_log 
# 2.274548            1.652292            1.123387            1.762708 

biomass <- lmer(mean_biomass_sqrt ~ mean_TN + mean_PAR_log + phyto_biovolume_log + (1|Lake), data=zoopl_pulse_press)

summary(biomass)

r.squaredGLMM(biomass)
# R2m      R2c
# [1,] 0.4802795 0.984169


#---------------------------------------------------------------------------------------------------------#
#### 6. Evenness for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = TP, TN, TN/TP, DN/TP, DOC, PAR, Temp, phyto biovolume, chl a

# testing significance of all variables individually 
evenness <- lmer(mean_J ~ phyto_biovolume_log + (1|Lake), data=zoopl_pulse_press)

summary(evenness)
# Chl a p-value = 0.91601   
# TP p-value = 0.0555 
# TN p-value =  0.94567  
# TN/TP p-value = 0.00587 !
# DN/TP p-value = 0.286970  
# DOC p-value = 0.20500 
# Temp p-value = 0.0957 
# phyto biovol p-value = 0.9558   

evenness <- lmer(mean_J ~ mean_TN_TP_ratio_100 + (1|Lake), data=zoopl_pulse_press)

summary(evenness)

r.squaredGLMM(evenness)
# R2m       R2c
# [1,] 0.2688603 0.5684715

#---------------------------------------------------------------------------------------------------------#
#### 7. ENS for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = TP, TN, TN/TP, DN/TP, DOC, PAR, Temp, phyto biovolume, chl a

# testing significance of all variables individually 
ENS <- lmer(mean_ENS_D ~ phyto_biovolume_log + (1|Lake), data=zoopl_pulse_press)

summary(ENS)
# Chl a p-value = 0.217091 
# TP p-value =  0.14644  
# TN p-value = 0.8495  
# TN/TP p-value = 0.0437 !
# DN/TP p-value = 0.866060 
# DOC p-value =  0.13404   
# PAR p-value = 0.348  
# Temp p-value = 0.9135 
# phyto biovol p-value = 0.983 


ENS <- lmer(mean_ENS_D ~ mean_TN_TP_ratio_100 + (1|Lake), data=zoopl_pulse_press)

summary(ENS)

r.squaredGLMM(ENS)
# R2m       R2c
# [1,] 0.1476205 0.5429453

#---------------------------------------------------------------------------------------------------------#
#### 8. SEM####
#---------------------------------------------------------------------------------------------------------#
## final individual models 
resistance <- lmer(initial_stab_log ~ mean_DOC + (1|Lake), data=zoopl_pulse_press)
AUC <- lmer(total_impact_log ~ mean_DOC + (1|Lake), data=zoopl_pulse_press)
biomass <- lmer(mean_biomass_sqrt ~ mean_DOC + mean_TN + mean_PAR_log + (1|Lake), data=zoopl_pulse_press)
evenness <- lmer(mean_J ~ mean_TN_TP_ratio_100 + total_impact_log + (1|Lake), data=zoopl_pulse_press)
ENS <- lmer(mean_ENS_D ~ mean_TN_TP_ratio_100 + (1|Lake), data=zoopl_pulse_press)
richness <- lmer(mean_s ~ mean_TN + mean_ENS_D + mean_biomass_sqrt +
                  initial_stab_log + (1|Lake), data=zoopl_pulse_press)

zoopl_pulse_press_complete <- na.omit(zoopl_pulse_press)

modelList <- psem(
 resistance,
  AUC,
  evenness,
  ENS,
  richness,
  biomass,
  mean_ENS_D %~~% mean_J,
  total_impact_log %~~% initial_stab_log,
 mean_s  %~~% mean_J,
 mean_biomass_sqrt %~~% initial_stab_log,
  data=zoopl_pulse_press_complete
)

results <- summary(modelList)
# excluding phyto biovolume because of singular fit 

# Akaike’s information criterion
results$IC$AIC
# 93.454

results$dTable

# Fisher’s C 
results$Cstat
#      Fisher.C df P.Value
# 1   33.454 46   0.916

results$coefficients

rsquared(modelList)






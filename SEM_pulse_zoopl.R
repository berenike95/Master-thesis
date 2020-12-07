#---------------------------------------------------------------------------------------------------------#
#### SEM for pulse disturbance & zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# load packages

library(lmerTest)

library(MuMIn)

library(tidyverse)

library(dplyr)

library(piecewiseSEM)

library(faraway)

#---------------------------------------------------------------------------------------------------------#

# load dataset
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/master_dataset3.csv")

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for pulse disturbance only 
dataset_zoopl_pulse <- dataset_zoopl[which(dataset_zoopl$Treatment=='F'), ]

#---------------------------------------------------------------------------------------------------------#

# filtering dataset for phytoplankton data only 
dataset_phyto <- dataset_final[which(dataset_final$community=='phyto'), ]

# filtering data for pulse disturbance only 
dataset_phyto_pulse <- dataset_phyto[which(dataset_phyto$Treatment=='F'), ]

# renaming columns for phyto biovolume and chl a
names(dataset_phyto_pulse)[names(dataset_phyto_pulse)=="mean_biomass"] <- "phyto_biovolume"

dataset_phyto_pulse <- select(dataset_phyto_pulse, Lake, Experiment, Treatment, Enclosure, phyto_biovolume)

dataset_zooplankton_pulse <- merge(x=dataset_phyto_pulse, y=dataset_zoopl_pulse, by= c("Lake", "Experiment", 
                                    "Treatment", "Enclosure"))

#---------------------------------------------------------------------------------------------------------#
#### 0. normal distribution and scaling of variables ####
#---------------------------------------------------------------------------------------------------------#
# Variables:

# TN, TP, DOC, PAR, Temp, TN/TP ratio, DN/TP ratio


# checking normal distribution of TN
hist(dataset_zooplankton_pulse$mean_TN)
shapiro.test(dataset_zooplankton_pulse$mean_TN)
#  p-value = 1.406e-05


# checking normal distribution of TP
hist(dataset_zooplankton_pulse$mean_TP)
shapiro.test(dataset_zooplankton_pulse$mean_TP)
# p-value = 0.003156
dataset_zooplankton_pulse$mean_TP_log <- log(dataset_zooplankton_pulse$mean_TP)
hist(dataset_zooplankton_pulse$mean_TP_log)
shapiro.test(dataset_zooplankton_pulse$mean_TP_log)
# p-value = 0.1248


# checking normal distribution of DOC
hist(dataset_zooplankton_pulse$mean_DOC)
shapiro.test(dataset_zooplankton_pulse$mean_DOC)
#  p-value = 0.002989


# checking normal distribution of PAR
hist(dataset_zooplankton_pulse$mean_PAR)
shapiro.test(dataset_zooplankton_pulse$mean_PAR)
# p-value = 5.475e-07
dataset_zooplankton_pulse$mean_PAR_log <- log(dataset_zooplankton_pulse$mean_PAR)
hist(dataset_zooplankton_pulse$mean_PAR_log)
shapiro.test(dataset_zooplankton_pulse$mean_PAR_log)
# p-value = 0.1951


# checking normal distribution of Temp
hist(dataset_zooplankton_pulse$mean_Temp)
shapiro.test(dataset_zooplankton_pulse$mean_Temp)
# p-value = 3.626e-06


# checking normal distribution of TN/TP ratio 
hist(dataset_zooplankton_pulse$mean_TN_TP_ratio)
shapiro.test(dataset_zooplankton_pulse$mean_TN_TP_ratio)
# p-value = 0.003622
dataset_zooplankton_pulse$mean_TN_TP_ratio_100 <- dataset_zooplankton_pulse$mean_TN_TP_ratio*100
hist(dataset_zooplankton_pulse$mean_TN_TP_ratio_100)


# checking normal distribution of DN/TP ratio 
hist(dataset_zooplankton_pulse$mean_DN_TP_ratio)
shapiro.test(dataset_zooplankton_pulse$mean_DN_TP_ratio)
# p-value = 0.0005185
dataset_zooplankton_pulse$mean_DN_TP_ratio_100 <- dataset_zooplankton_pulse$mean_DN_TP_ratio*100
hist(dataset_zooplankton_pulse$mean_DN_TP_ratio_100)

#---------------------------------------------------------------------------------------------------------#
# Biomass, Evenness, ENS, Richness, Phytoplankton biovolume, Chl a 


# checking normal distribution of biomass
hist(dataset_zooplankton_pulse$mean_biomass)
shapiro.test(dataset_zooplankton_pulse$mean_biomass)
# p-value = 7.328e-06
dataset_zooplankton_pulse$mean_biomass_log <- log(dataset_zooplankton_pulse$mean_biomass)
hist(dataset_zooplankton_pulse$mean_biomass_log)
shapiro.test(dataset_zooplankton_pulse$mean_biomass_log)
# p-value = 0.2326

# checking normal distribution of evenness
hist(dataset_zooplankton_pulse$mean_J)
shapiro.test(dataset_zooplankton_pulse$mean_J)
# p-value = 0.001136


# checking normal distribution of ENS
hist(dataset_zooplankton_pulse$mean_ENS_D)
shapiro.test(dataset_zooplankton_pulse$mean_ENS_D)
# p-value = 0.956


# checking normal distribution of richness
hist(dataset_zooplankton_pulse$mean_s)
shapiro.test(dataset_zooplankton_pulse$mean_s)
# p-value = 0.04756


# checking normal distribution of phytoplankton biovolume
hist(dataset_zooplankton_pulse$phyto_biovolume)
shapiro.test(dataset_zooplankton_pulse$phyto_biovolume)
# p-value = 7.695e-12
dataset_zooplankton_pulse$phyto_biovolume_log <- log(dataset_zooplankton_pulse$phyto_biovolume)
hist(dataset_zooplankton_pulse$phyto_biovolume_log)
shapiro.test(dataset_zooplankton_pulse$phyto_biovolume_log)
# p-value = 0.1764


# checking normal distribution of Chl a 
hist(dataset_zooplankton_pulse$mean_Chla)
shapiro.test(dataset_zooplankton_pulse$mean_Chla)
# p-value = 8.299e-09
dataset_zooplankton_pulse$mean_Chla_log <- log(dataset_zooplankton_pulse$mean_Chla)
hist(dataset_zooplankton_pulse$mean_Chla_log)
shapiro.test(dataset_zooplankton_pulse$mean_Chla_log)
# p-value = 0.2532

#---------------------------------------------------------------------------------------------------------#
# Resistance, final reovery, resilience, area under the curve


# checking normal distribution of resistance
hist(dataset_zooplankton_pulse$initial_stab)
shapiro.test(dataset_zooplankton_pulse$initial_stab)
# p-value = 4.078e-05
dataset_zooplankton_pulse$initial_stab_log <- log(dataset_zooplankton_pulse$initial_stab*-1)
hist(dataset_zooplankton_pulse$initial_stab_log)
shapiro.test(dataset_zooplankton_pulse$initial_stab_log)
# p-value = 0.9424


# checking normal distribution of final recovery
hist(dataset_zooplankton_pulse$final_stab)
shapiro.test(dataset_zooplankton_pulse$final_stab)
# p-value = 5.371e-05
dataset_zooplankton_pulse$final_stab_sqrt <- sqrt(dataset_zooplankton_pulse$final_stab*-1)
hist(dataset_zooplankton_pulse$final_stab_sqrt)
shapiro.test(dataset_zooplankton_pulse$final_stab_sqrt)
#  p-value = 0.7624

# checking normal distribution of resilience
hist(dataset_zooplankton_pulse$rate_change_ort)
shapiro.test(dataset_zooplankton_pulse$rate_change_ort)
# p-value = 0.7825


# checking normal distribution of AUC
hist(dataset_zooplankton_pulse$total_impact)
shapiro.test(dataset_zooplankton_pulse$total_impact)
# p-value = 0.004421
dataset_zooplankton_pulse$total_impact_log <- log(dataset_zooplankton_pulse$total_impact)
hist(dataset_zooplankton_pulse$total_impact_log)
shapiro.test(dataset_zooplankton_pulse$total_impact_log)
# p-value = 0.09962



#---------------------------------------------------------------------------------------------------------#
#### 1. resistance for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resistance 
# explanatory = TP, TN, TN/TP, DN/TP, DOC, PAR, Temp, biomass, evenness, ens, richness, 
# phyto biovolume, chl a

# testing significance of all variables individually 
resistance <- lmer(initial_stab_log ~ mean_Chla_log + (1|Lake), data=dataset_zooplankton_pulse)

summary(resistance)
# TP p-value = 0.291
# TN p-value = 0.162
# TN/TP p-value = 0.843
# DN/TP p-value =  0.268
# DOC p-value = 0.0466 !
# PAR p-value = 0.685
# Temp p-value = 0.822 
# biomass p-value = 0.0271 !
# evenness p-value = 0.0275 !
# ENS p-value = 0.0346 !
# richness p-value = 0.00427 !
# phyto biovolume p-value = 0.758
# Chl a p-value = 0.726

# --> DOC, biomass, evenness, ens, richness

# testing collinearity:
# variance inflation factor. The variance inflation factor quantifies the effect of collinearity on the 
# variance of our regression estimates.
# In practice it is common to say that any VIF greater than 5 is cause for concern.

test <- lm(initial_stab_log ~ mean_DOC + mean_biomass_log + mean_J, data=dataset_zooplankton_pulse)

vif(test)
# mean_DOC_sqrt       mean_biomass_log           mean_J       mean_ENS_D           mean_s 
# 2.038301            2.745276                  6.431353         9.459548         4.521853 

# taking out mean_ENS_D 

# setting up the model fit 
resistance <- lmer(initial_stab_log ~ mean_biomass_log + mean_TN + (1|Lake), data=dataset_zooplankton_pulse)

summary(resistance)

r.squaredGLMM(resistance)
#    R2m       R2c
# [1,] 0.4556719 0.4796175

# So, the marginal R2 is the fixed effects variance, divided by the total variance (i.e. fixed + random + residual). 
# This value indicates how much of the "model variance" is explained by the fixed effects part only.
# The conditional R2 is the fixed+random effects variance divided by the total variance, and indicates 
# how much of the "model variance" is explained by your "complete" model.

#---------------------------------------------------------------------------------------------------------#
#### 2. final recovery for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = recovery 
# explanatory = TP, TN, TN/TP, DN/TP, DOC, PAR, Temp, biomass, evenness, ens, richness, 
# phyto biovolume, chl a

# testing significance of all variables individually 
recovery <- lmer(final_stab_sqrt ~ mean_Chla_log + (1|Lake), data=dataset_zooplankton_pulse)

summary(recovery)
# TP p-value = 0.000396 --> singular fit
# TN p-value = 0.00136 --> singular fit
# TN/TP p-value =  0.04781 !
# DN/TP p-value =   0.2745
# PAR p-value = 0.383
# Temp p-value = 0.000272  !
# biomass p-value = 0.151001 
# evenness p-value =  0.24114 
# ENS p-value = 0.32670
# richness p-value = 0.7824
# phyto biovolume p-value = 0.1162 
# CHl a p-value =  0.43737 

# --> TN/TP ratio, Temp

recovery <- lmer(final_stab_sqrt ~ mean_Temp + mean_DOC + (1|Lake), data=dataset_zooplankton_pulse)

summary(recovery)
# TN/TP ratio p-value = 0.0203 
# PAR p-value = 0.0455

r.squaredGLMM(recovery)
#  R2m       R2c
# [1,] 0.5231594 0.5602564

#---------------------------------------------------------------------------------------------------------#
#### 3. resilience for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience
# explanatory = TP, TN, TN/TP, DN/TP, DOC, PAR, Temp, biomass, evenness, ens, richness, 
# phyto biovolume, chl a

# testing significance of all variables individually 
resilience <- lmer(rate_change_ort ~ mean_Chla_log + (1|Lake), data=dataset_zooplankton_pulse)

summary(resilience)
# TP p-value = 0.712
# TN p-value = 0.742
# TN/TP p-value = 0.344
# DN/TP p-value = 0.869
# DOC p-value = 0.832
# PAR p-value = 0.432
# Temp p-value = 0.00845 !
# biomass p-value = 0.617
# evenness p-value = 0.201
# ENS p-value =  0.1331
# richness p-value = 0.1084
# phyto biovolume p-value = 0.151
# Chl a p-value = 0.95


resilience <- lmer(rate_change_ort ~ mean_Temp + (1|Lake), data=dataset_zooplankton_pulse)

summary(resilience)


r.squaredGLMM(resilience)
#      R2m       R2c
# [1,] 0.1888232 0.3755999

#---------------------------------------------------------------------------------------------------------#
#### 4. AUC for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = AUC
# explanatory = TP, TN, TN/TP, DN/TP, DOC, PAR, Temp, biomass, evenness, ens, richness, 
# phyto biovolume, chl a

# testing significance of all variables individually 
AUC <- lmer(total_impact_log ~ mean_Chla_log + (1|Lake), data=dataset_zooplankton_pulse)
summary(AUC)
# TP p-value =  0.0475 !
# TN p-value = 0.2742
# TN/TP p-value = 0.0323 !
# DN/TP p-value = 0.668 
# DOC p-value =  0.026 !
# PAR p-value = 0.35041 
# Temp p-value = 0.0117 !
# biomass p-value = 0.00605 !
# evenness p-value = 0.00438 !
# ENS p-value = 0.00731 !
# richness p-value = 0.0253 !
# phyto biovolume p-value = 0.23492  
# Chl a p-value = 0.595632 

# --> TP, TN/TP, DOC, Temp, biomass, evenness, ENS, richness

test <- lm(total_impact_log ~ mean_TP_log + mean_TN_TP_ratio_100 + mean_DOC + mean_Temp + 
               mean_biomass_log + mean_J + mean_ENS_D + mean_s, data=dataset_zooplankton_pulse)

vif(test)
#     mean_TP_log        mean_TN_TP_ratio_100          mean_DOC_10         mean_Temp_10     mean_biomass_log 
# 12.127242                     3.853224             7.634949             2.589897             5.290267 
# mean_J           mean_ENS_D               mean_s 
# 8.544746            12.939096            17.954677 

# taking out mean_TP_log, mean_ENS_D, mean_s
ggplot(dataset_zooplankton_pulse, aes(x=mean_TN_TP_ratio_100, y=mean_DOC_10))+
  geom_smooth() +
  geom_point()

# taking out TN/TP ratio, because it correlates with DOC 
AUC <- lmer(total_impact_log ~ mean_biomass_log + mean_TN + mean_Temp + (1|Lake), data=dataset_zooplankton_pulse)
# mean_DOC --> singular fit 
summary(AUC)

r.squaredGLMM(AUC)
#        R2m       R2c
# [1,] 0.4806491 0.5507878

#---------------------------------------------------------------------------------------------------------#
#### 5. Biomass for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = mean zoopl biomass
# explanatory = TP, TN, TN/TP, DN/TP, DOC, PAR, Temp, phyto biovolume, chl a


# testing significance of all variables individually 
biomass <- lmer(mean_biomass_log ~ mean_Chla_log + (1|Lake), data=dataset_zooplankton_pulse)
summary(biomass)
# TP p-value = 0.173
# TN p-value = 0.0712 
# TN/TP p-value = 0.9495 
# DN/TP p-value = 0.186 
# DOC p-value = 0.6320
# PAR p-value = 0.0371 !
# Temp p-value = 0.002217 !
# phyto biovolume p-value = 0.0667
# Chl a p-value = 0.131076

# --> PAR, Temp 

biomass <- lmer(mean_biomass_log ~ mean_Temp + mean_TN + (1|Lake), data=dataset_zooplankton_pulse)

summary(biomass)


r.squaredGLMM(biomass)
#  R2m       R2c
# [1,] 0.4169407 0.8901657

#---------------------------------------------------------------------------------------------------------#
#### 6. Evenness for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = TP, TN, TN/TP, DN/TP, DOC, PAR, Temp, phyto biovolume, chl a

# testing significance of all variables individually 
evenness <- lmer(mean_J ~ mean_Chla_log + (1|Lake), data=dataset_zooplankton_pulse)

summary(evenness)
# TP p-value = 0.069240
# TN p-value = 0.5618 
# TN/TP p-value = 0.0164 !
# DN/TP p-value = 0.271
# DOC p-value = 0.04962 !
# PAR p-value = 0.0623 
# Temp p-value = 0.00127 !
# phyto biovolume p-value = 0.6627 
# Chl a p-value = 0.616283 

# TN/TP ratio, DOC, Temperature 

evenness <- lmer(mean_J ~ mean_TN + mean_biomass_log + (1|Lake), data=dataset_zooplankton_pulse)

summary(evenness)


r.squaredGLMM(evenness)
# R2m      R2c
# [1,] 0.3680882 0.788355

#---------------------------------------------------------------------------------------------------------#
#### 7. ENS for zooplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = TP, TN, TN/TP, DN/TP, DOC, PAR, Temp, phyto biovolume, chl a

# testing significance of all variables individually 
ENS <- lmer(mean_ENS_D ~ mean_Chla_log + (1|Lake), data=dataset_zooplankton_pulse)

summary(ENS)
# TP p-value = 0.10445 
# TN p-value = 0.40502 
# TN/TP p-value = 0.1288 
# DN/TP p-valeu = 0.597849
# DOC p-value = 0.02891 !
# PAR p-value = 0.00887 !
# Temp p-value = 0.12932 
# phyto biovolume p-value = 0.242
# Chl a p-value = 0.445973 

# --> DOC, PAR 

ENS <- lmer(mean_ENS_D ~ mean_TN + mean_PAR_log + (1|Lake), data=dataset_zooplankton_pulse)

summary(ENS)

r.squaredGLMM(ENS)
# R2m       R2c
# [1,] 0.29634 0.6494586

#---------------------------------------------------------------------------------------------------------#
#### 8. SEM ####
#---------------------------------------------------------------------------------------------------------### 

## final individual models 
recovery <- lmer(final_stab_sqrt ~ mean_Temp + (1|Lake), data=dataset_zooplankton_pulse)
resilience <- lmer(rate_change_ort ~ mean_Temp + (1|Lake), data=dataset_zooplankton_pulse)
AUC <- lmer(total_impact_log ~ mean_biomass_log + mean_TN + (1|Lake), data=dataset_zooplankton_pulse)
biomass <- lmer(mean_biomass_log ~ mean_Temp + mean_TN + (1|Lake), data=dataset_zooplankton_pulse)
evenness <- lmer(mean_J ~ mean_biomass_log +  (1|Lake), data=dataset_zooplankton_pulse)


modelList <- psem(
 # resistance,
  recovery,
  resilience,
  AUC,
  biomass,
  evenness,
  rate_change_ort %~~% final_stab_sqrt,
  total_impact_log %~~% final_stab_sqrt,
  mean_J %~~% rate_change_ort,
  dataset_zooplankton_pulse
)

results <- summary(modelList)
# excluding richness and ENS from model 
# excluding DOC form model because of singular fit
# excluding resistance from model


# Akaike’s information criterion
results$IC$AIC
# 71.93

results$dTable

# Fisher’s C 
results$Cstat
#   Fisher.C df P.Value
# 1    27.93 20   0.111

results$coefficients

rsquared(modelList)



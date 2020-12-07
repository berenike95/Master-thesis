#---------------------------------------------------------------------------------------------------------#
#### SEM for pulse disturbance & phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# load packages

library(lmerTest)

library(MuMIn)

library(tidyverse)

library(dplyr)

library(faraway)

library(piecewiseSEM)

#---------------------------------------------------------------------------------------------------------#

# load dataset
dataset_final <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/master_dataset3.csv")

# filtering dataset for phytoplankton data only 
dataset_phyto <- dataset_final[which(dataset_final$community=='phyto'), ]

# filtering data for pulse disturbance only 
dataset_phyto_pulse <- dataset_phyto[which(dataset_phyto$Treatment=='F'), ]

dataset_phyto_pulse <- dataset_phyto_pulse[-16] 

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


# merging both datasets 
dataset_phyto_pulse <- merge(x=dataset_phyto_pulse, y=dataset_zoopl_pulse, by= c("Lake", "Experiment", 
                                                      "Treatment", "Enclosure"))

#---------------------------------------------------------------------------------------------------------#
#### 0. normal distribution and scaling of variables ####
#---------------------------------------------------------------------------------------------------------#
# Variables:

# TN, TP, DOC, PAR, Temp, TN/TP ratio, DN/TP ratio, Chl a

# checking normal distribution of TN
hist(dataset_phyto_pulse$mean_TN)
shapiro.test(dataset_phyto_pulse$mean_TN)
# p-value = 1.406e-05
# not possible to transform (2 peaks)

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
dataset_phyto_pulse$mean_DOC_sqrt <- sqrt(dataset_phyto_pulse$mean_DOC)
hist(dataset_phyto_pulse$mean_DOC_sqrt)
shapiro.test(dataset_phyto_pulse$mean_DOC_sqrt)
# p-value = 0.004453


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
# p-value = 3.626e-06
# not possible to transform

# checking normal distribution of TN/TP ratio
hist(dataset_phyto_pulse$mean_TN_TP_ratio)
shapiro.test(dataset_phyto_pulse$mean_TN_TP_ratio)
# p-value = 0.003622
dataset_phyto_pulse$mean_TN_TP_ratio_100 <- dataset_phyto_pulse$mean_TN_TP_ratio*100

# checking normal distribution of DN/TP ratio
hist(dataset_phyto_pulse$mean_DN_TP_ratio)
shapiro.test(dataset_phyto_pulse$mean_DN_TP_ratio)
#  p-value = 0.0005185
dataset_phyto_pulse$mean_DN_TP_ratio_100 <- dataset_phyto_pulse$mean_DN_TP_ratio*100

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

# checking normal distribution of biovolume
hist(dataset_phyto_pulse$mean_biomass)
shapiro.test(dataset_phyto_pulse$mean_biomass)
# p-value = 7.695e-12
# log-transformation
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
# p-value = 0.02638
dataset_phyto_pulse$mean_s_sqrt <- sqrt(dataset_phyto_pulse$mean_s)
hist(dataset_phyto_pulse$mean_s_sqrt)
shapiro.test(dataset_phyto_pulse$mean_s_sqrt)
# p-value = 0.02162

# checking normal distribution of mean zooplankton biomass
hist(dataset_phyto_pulse$mean_zoopl_biomass)
shapiro.test(dataset_phyto_pulse$mean_zoopl_biomass)
# p-value = 7.328e-06
dataset_phyto_pulse$mean_zoopl_biomass_log <- log(dataset_phyto_pulse$mean_zoopl_biomass)
hist(dataset_phyto_pulse$mean_zoopl_biomass_log)
shapiro.test(dataset_phyto_pulse$mean_zoopl_biomass_log)
#  p-value = 0.2326

# checking normal distribution of initial zooplankton body size
hist(dataset_phyto_pulse$initial_zoop_body_size)
shapiro.test(dataset_phyto_pulse$initial_zoop_body_size)
# p-value = 1.455e-05
dataset_phyto_pulse$initial_zoop_body_size_log <- log(dataset_phyto_pulse$initial_zoop_body_size)
hist(dataset_phyto_pulse$initial_zoop_body_size_log)
shapiro.test(dataset_phyto_pulse$initial_zoop_body_size_log)
# p-value = 0.002341

#---------------------------------------------------------------------------------------------------------#
# final recovery, Resistance, resilience, area under the curve

# checking normal distribution of final recovery
hist(dataset_phyto_pulse$final_stab)
shapiro.test(dataset_phyto_pulse$final_stab)
# p-value = 2.113e-05
dataset_phyto_pulse$final_stab_sqrt <- sqrt(dataset_phyto_pulse$final_stab*-1)
hist(dataset_phyto_pulse$final_stab_sqrt)
shapiro.test(dataset_phyto_pulse$final_stab_sqrt)
# p-value = 0.1379

# checking normal distribution of resistance
hist(dataset_phyto_pulse$initial_stab)
shapiro.test(dataset_phyto_pulse$initial_stab)
# p-value = 9.272e-05
dataset_phyto_pulse$initial_stab_sqrt <- sqrt(dataset_phyto_pulse$initial_stab*-1)
hist(dataset_phyto_pulse$initial_stab_sqrt)
shapiro.test(dataset_phyto_pulse$initial_stab_sqrt)
# p-value = 0.3998

# checking normal distribution of resilience
hist(dataset_phyto_pulse$rate_change_ort)
shapiro.test(dataset_phyto_pulse$rate_change_ort)
# p-value = 0.00785
# not possible to transform, because of negative and positive values

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
# explanatory = Chl a, TP, TN, TN/TP ratio, DN/TP ratio, DOC, PAR, Temp, biovolume, evenness, richness, ENS,
# zooplankton biomass, zooplankton body size 

# testing significance of all variables individually 
resistance <- lmer(initial_stab_sqrt ~ mean_PAR_log + (1|Lake), data=dataset_phyto_pulse)
summary(resistance)

# Chl a p-value = 0.33688 
# TP p-value= 0.204 
# TN p-value = 0.2385
# TN/TP p-value =  0.328
# DN/TP p-value =  0.9185 
# DOC p-value =  0.921 
# PAR p-value =  0.0486   !
# Temp p-value =  0.826 
# biovol p-value = 0.626
# evenness p-value = 0.851 
# richness p-value =  0.92649
# ENS p-value = 0.86867  
# zooplankton biomass p-value = 0.0202 !
# zooplankton body size p-value =  0.648

# --> PAR, zooplankton biomass

resistance <- lmer(initial_stab_sqrt ~ mean_zoopl_biomass_log + (1|Lake), data=dataset_phyto_pulse)
summary(resistance)

r.squaredGLMM(resistance)
#  R2m       R2c
# [1,] 0.1384513 0.2793343
#---------------------------------------------------------------------------------------------------------#
#### 2. final recovery for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = final recovery 
# explanatory = Chl a, TP, TN, TN/TP ratio, DN/TP ratio, DOC, PAR, Temp, biovolume, evenness, richness, ENS,
# zooplankton biomass, zooplankton body size 

# testing significance of all variables individually 
recovery <- lmer(final_stab_sqrt ~ mean_biovol + (1|Lake), data=dataset_phyto_pulse)
summary(recovery)
# Chl a p-value = 0.0405 --> singular fit
# TP p-value = 0.3810
# TN p-value =  0.4187  
# TN/TP ratio p-value = 0.619
# DN/TP ratio p-value = 0.78433
# DOC p-value = 0.7613
# PAR p-value = 0.245637
# Temp p-value = 0.006607 !
# biovolume p-value = 0.905
# evenness p-value = 0.3271 
# richness p-value = 0.1943  
# ENS p-value = 0.45943 
# zoopl biomass p-value = 0.00946 !
# initial zoopl body size p-value = 0.01893 !

# --> PAR, Temp, zoopl biomass, zoopl body size

recovery <- lmer(final_stab_sqrt ~ mean_PAR_log + mean_Temp + mean_zoopl_biomass_log + initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)
summary(recovery)
# Temperature and zooplankton biomass are not showing significant p-values anymore

test <- lm(final_stab_sqrt ~ mean_PAR_log + mean_Temp + mean_zoopl_biomass_log + initial_zoop_body_size_log, data=dataset_phyto_pulse)
vif(test)
#  mean_PAR_log                  mean_Temp     mean_zoopl_biomass_log         initial_zoop_body_size_log 
# 1.466818                   1.511910                   1.127864                   1.221103 

# setting up the model fit 
recovery <- lmer(final_stab_sqrt ~ mean_PAR_log + mean_zoopl_biomass_log + initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)

summary(recovery)

r.squaredGLMM(recovery)
# R2m     R2c
# [1,] 0.2843828 0.44603

#---------------------------------------------------------------------------------------------------------#
#### 3. resilience for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience
# explanatory = Chl a, TP, TN, TN/TP ratio, DN/TP ratio, DOC, PAR, Temp, biovolume, evenness, richness, ENS,
# zooplankton biomass, zooplankton body size 

# testing significance of all variables individually 
resilience <- lmer(rate_change_ort ~ mean_Temp + (1|Lake), data=dataset_phyto_pulse)
summary(resilience)
# Chl a p-value = 0.54
# TP p-value = 0.874 
# TN p-value = 0.781
# TN/TP p-value = 0.706
# DN/TP p-value = 0.903
# DOC p-value = 0.279
# PAR p-value = 0.00984 !
# Temp p-value = 0.00253 !
# biovol p-value = 0.948 
# evenness p-value = 0.267 
# richness p-value = 0.397
# ENS p-value =  0.67
# zoopl biomass p-value = 0.000102 !
# initial zoopl body size p-value = 0.030 !

# --> PAR, Temp, zoopl biomass, zoopl body size 
resilience <- lmer(rate_change_ort ~ mean_PAR_log + mean_zoopl_biomass_log + initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)
summary(resilience)
# singular fit 
# excluding mean_Temp

r.squaredGLMM(resilience)
#  R2m       R2c
# [1,] 0.5033821 0.5154882

#---------------------------------------------------------------------------------------------------------#
#### 4. AUC for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = AUC
# explanatory = Chl a, TP, TN, TN/TP ratio, DN/TP ratio, DOC, PAR, Temp, biovolume, evenness, richness, ENS,
# zooplankton biomass, zooplankton body size 

# testing significance of all variables individually 
AUC <- lmer(total_impact_log ~ mean_TP_log + (1|Lake), data=dataset_phyto_pulse)
summary(AUC)
# Chl a p-value = 0.0196 !
# TP p-value = 0.02392 !
# TN p-value = 0.028946 !
# TN/TP p-value =  0.0969
# DN/TP p-value =  0.5401
# DOC p-value = 0.7587 
# PAR p-value = 0.554079  
# Temp p-value =  0.0174 !
# biovol p-value = 0.8373 
# evenness p-value = 0.5199
# richness p-value =  0.15810
# ENS p-value = 0.429 
# zoopl biomass p-value =  0.361
# zoopl body size p-value = 0.0795

# --> Chl a, TP, TN, Temp
AUC <- lmer(total_impact_log ~ mean_Chla_log + mean_TP_log + mean_TN + mean_Temp + (1|Lake), data=dataset_phyto_pulse)
summary(AUC)
# singular fit

# setting up the model fit 
# excluding mean_TP_log and mean_TN (singular fit)
AUC <- lmer(total_impact_log ~ mean_Temp + mean_TN +(1|Lake), data=dataset_phyto_pulse)

summary(AUC)

r.squaredGLMM(AUC)
# R2m       R2c
# [1,] 0.4129992 0.4179996

#---------------------------------------------------------------------------------------------------------#
#### 5. Biomass for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = biovolume
# explanatory = Chl a, TP, TN, TN/TP ratio, DN/TP ratio, Temp, PAR, DOC, mean zoopl biomass, zooplankton body size

# testing significance of all variables individually 
biovol <- lmer(mean_biomass_log ~ initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)
summary(biovol)
# Chl a p-value = 0.0228 !
# TP p-value =  0.188649 
# TN p-value = 0.16555 
# TN/TP p-value = 0.255
# DN/TP p-value = 0.632  
# Temp p-value = 0.00953 !
# PAR p-value = 0.463 
# DOC p-value = 0.3681
# zoopl biomass p-value = 0.0372 !
# zoopl body size p-value = 0.0578

# --> Chl a, Temp, zoopl biomass
biovol <- lmer(mean_biomass_log ~ mean_Chla_log + mean_TN_TP_ratio_100 + mean_Temp + (1|Lake), data=dataset_phyto_pulse)
summary(biovol)

r.squaredGLMM(biovol)
# R2m       R2c
# [1,] 0.4925887 0.8544629

#---------------------------------------------------------------------------------------------------------#
# testing biomass proxy Chl a 

Chla <- lmer(mean_Chla_log ~ initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)
summary(Chla)
# TP p-value = 0.00284 !
# TN p-value = 0.00439 !
# TN/TP p-value = 0.0371 ! 
# DN/TP p-value = 1.76e-08 !
# Temp p-value= 0.313
# PAR p-value = 0.00283 !
# DOC p-value = 4.24e-05 !
# zoopl biomass p-value = 0.151 
# zoopl body size p-value = 0.592

Chla <- lmer(mean_Chla_log ~ mean_TP_log + mean_TN + mean_TN_TP_ratio_100 + mean_DN_TP_ratio_100 + 
               mean_PAR_log + mean_DOC_sqrt + (1|Lake), data=dataset_phyto_pulse)
summary(Chla)
  
test <- lm(mean_Chla_log ~ mean_TP_log + mean_TN + mean_TN_TP_ratio_100 + mean_DN_TP_ratio_100 + 
               mean_PAR_log + mean_DOC_sqrt, data=dataset_phyto_pulse)
vif(test)
#  mean_TP_log              mean_TN mean_TN_TP_ratio_100 mean_DN_TP_ratio_100         mean_PAR_log 
# 253.442088           130.479856            81.807394             6.137777             4.250958 
# mean_DOC_sqrt 
# 5.570900 

# excluding TN 

test <- lm(mean_Chla_log ~ mean_TP_log + mean_DN_TP_ratio_100 + 
             mean_PAR_log + mean_DOC_sqrt, data=dataset_phyto_pulse)
vif(test)

Chla <- lmer(mean_Chla_log ~ mean_TN + mean_PAR_log + mean_DOC_sqrt + (1|Lake), data=dataset_phyto_pulse)
summary(Chla)
r.squaredGLMM(Chla)
# R2m       R2c
# [1,] 0.2850456 0.9722623

#---------------------------------------------------------------------------------------------------------#
#### 6. Evenness for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = Chl a, TP, TN, TN/TP ratio, DN/TP ratio, Temp, PAR, DOC, mean zoopl biomass, zooplankton body size

# testing significance of all variables individually 
evenness <- lmer(mean_J ~ initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)
summary(evenness)
# Chl a p-value = 0.0295
# TP p-value = 0.70544 
# TN p-value =  0.672728
# TN/TP p-value = 0.953
# DN/TP p-value = 0.00966 !
# Temp p-value = 0.0319  !
# PAR p-value = 0.0517 
# DOC p-value = 0.28594 
# zoopl biomass p-value = 0.0168
# zoopl body size p-value = 0.324 

# --> DN/TP, Temp

# setting up the model fit 
evenness <- lmer(mean_J ~  mean_DN_TP_ratio_100 + mean_Temp + (1|Lake), data=dataset_phyto_pulse)
summary(evenness)

r.squaredGLMM(evenness)
#   R2m       R2c
# [1,] 0.2859262 0.5106154

#---------------------------------------------------------------------------------------------------------#
#### 7. ENS for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = Chl a, TP, TN, TN/TP ratio, DN/TP ratio, Temp, PAR, DOC, mean zoopl biomass, zooplankton body size

# testing significance of all variables individually 
ENS <- lmer(mean_ENS_D ~ initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)
summary(ENS)
# Chl a p-value =  0.00558 !
# TP p-value = 0.07516
# TN p-value = 0.07326
# TN/TP p-value = 0.408
# DN/TP p-value = 0.0015 !
# Temp p-value = 0.9815
# PAR p-value = 0.0134 !
# DOC p-value = 0.000107 !
# zoopl biomass p-value =  0.209784 
# zoopl body size p-value = 0.12706  

# --> Chl a, DN/TP, PAR, DOC

# setting up the model fit 
ENS <- lmer(mean_ENS_D ~  mean_PAR_log + mean_TN + (1|Lake), data=dataset_phyto_pulse)

summary(ENS)

r.squaredGLMM(ENS)
#  R2m       R2c
# [1,] 0.2617936 0.8055128

#---------------------------------------------------------------------------------------------------------#
#### 8. SEM  ####
#---------------------------------------------------------------------------------------------------------#

## final individual models 

ENS <- lmer(mean_ENS_D ~ mean_PAR_log + mean_TN + mean_DOC_sqrt + (1|Lake), data=dataset_phyto_pulse)
# resilience <- lmer(rate_change_ort ~ mean_PAR_log + mean_zoopl_biomass_log + (1|Lake), data=dataset_phyto_pulse)
resistance <- lmer(initial_stab_sqrt ~ mean_zoopl_biomass_log + (1|Lake), data=dataset_phyto_pulse)
recovery <- lmer(final_stab_sqrt ~ mean_PAR_log + initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)
# AUC <- lmer(total_impact_log ~ mean_Temp + mean_TN + (1|Lake), data=dataset_phyto_pulse)
Chla <- lmer(mean_Chla_log ~ mean_DOC_sqrt + mean_TN + mean_DOC_sqrt + mean_TN_TP_ratio_100 + mean_ENS_D + (1|Lake), data=dataset_phyto_pulse)
biovol <- lmer(mean_biomass_log ~ mean_TN + mean_Chla_log + mean_PAR_log + mean_Temp + (1|Lake), data=dataset_phyto_pulse)

# SEM with piecewiseSEM version 2.0
modelList <- psem(
  ENS,
  resistance,
  recovery,
  Chla,
  biovol,
  dataset_phyto_pulse
)
#  %~~% für ungerichtete Korrelationen

results <- summary(modelList)
# excluding resilience and AUC, because of singular fit!

results$IC$AIC
# 127.408
results$dTable
# Tests of directed separation Tabelle signifikante Beziehungen angezeigt bekommst, heißt das, dass diese 
# beiden Variablen nicht unabhängig voneinander sind, also wahrscheinlich etwas in unserer Modellstruktur nicht stimmt.
results$Cstat
# Fisher.C df P.Value
# 1   69.408 62   0.242
results$coefficients

rsq <- rsquared(modelList)


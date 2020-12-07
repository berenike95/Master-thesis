#---------------------------------------------------------------------------------------------------------#
#### SEM for press disturbance & phytoplankton####
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

# filtering data for press disturbance only 
dataset_phyto_press <- dataset_phyto[which(dataset_phyto$Treatment=='S'), ]

dataset_phyto_press = subset(dataset_phyto_press, select = -16)

#---------------------------------------------------------------------------------------------------------#

# filtering dataset for zooplankton data only 
dataset_zoopl <- dataset_final[which(dataset_final$community=='zoopl'), ]

# filtering data for press disturbance only 
dataset_zoopl_press <- dataset_zoopl[which(dataset_zoopl$Treatment=='S'), ]

# filtering for body size and biomass 
dataset_zoopl_press <- select(dataset_zoopl_press, Lake, Experiment, Treatment, Enclosure, 
                              initial_zoop_body_size, mean_biomass)

# renaming columns
names(dataset_zoopl_press)[names(dataset_zoopl_press)=="mean_biomass"] <- "zoopl_biomass"
names(dataset_zoopl_press)[names(dataset_zoopl_press)=="initial_zoop_body_size"] <- "zoopl_body_size"

# merging both datasets 
dataset_phyto_press <- merge(x=dataset_phyto_press, y=dataset_zoopl_press, by= c("Lake", "Experiment", 
                                                                        "Treatment", "Enclosure"))

#---------------------------------------------------------------------------------------------------------#
#### 0. normal distribution and scaling of variables ####
#---------------------------------------------------------------------------------------------------------#
# Variables:

# TN, TP, DOC, PAR, Temp, TN/TP ratio, DN/TP ratio, Chl a

# checking normal distribution of TP
hist(dataset_phyto_press$mean_TP)
shapiro.test(dataset_phyto_press$mean_TP)
# p-value = 0.005709

# checking normal distribution of Temp
hist(dataset_phyto_press$mean_Temp)
shapiro.test(dataset_phyto_press$mean_Temp)
# p-value = 3.777e-06

# checking normal distribution of DOC
hist(dataset_phyto_press$mean_DOC)
shapiro.test(dataset_phyto_press$mean_DOC)
# p-value = 0.0008227
dataset_phyto_press$mean_DOC_sqrt <- sqrt(dataset_phyto_press$mean_DOC)
hist(dataset_phyto_press$mean_DOC_sqrt)
shapiro.test(dataset_phyto_press$mean_DOC_sqrt)
#  p-value = 0.001198

# checking normal distribution of PAR
hist(dataset_phyto_press$mean_PAR)
shapiro.test(dataset_phyto_press$mean_PAR)
# p-value = 1.545e-06
dataset_phyto_press$mean_PAR_log <- log(dataset_phyto_press$mean_PAR)
hist(dataset_phyto_press$mean_PAR_log)
shapiro.test(dataset_phyto_press$mean_PAR_log)
# p-value = 0.00313


# checking normal distribution of TN  
hist(dataset_phyto_press$mean_TN)
shapiro.test(dataset_phyto_press$mean_TN)
# p-value = 5.19e-05

# checking normal distribution of Chl a 
hist(dataset_phyto_press$mean_Chla)
shapiro.test(dataset_phyto_press$mean_Chla)
# p-value = 1.67e-08
dataset_phyto_press$mean_Chla_log <- log(dataset_phyto_press$mean_Chla)
hist(dataset_phyto_press$mean_Chla_log)
shapiro.test(dataset_phyto_press$mean_Chla_log)
# p-value = 0.3784

# checking normal distribution of TN/TP ratio
hist(dataset_phyto_press$mean_TN_TP_ratio)
shapiro.test(dataset_phyto_press$mean_TN_TP)
# p-value = 0.004568
dataset_phyto_press$mean_TN_TP_ratio_100 <- dataset_phyto_press$mean_TN_TP_ratio*100


# checking normal distribution of DN/TP ratio 
hist(dataset_phyto_press$mean_DN_TP_ratio)
shapiro.test(dataset_phyto_press$mean_DN_TP_ratio)
# p-value = 0.0007845
dataset_phyto_press$mean_DN_TP_ratio_100 <- dataset_phyto_press$mean_DN_TP_ratio*100


#---------------------------------------------------------------------------------------------------------#
# Biovolume, Evenness, ENS, Richness, Zooplankton biomass, Zooplankton body size


# checking normal distribution of biomass
hist(dataset_phyto_press$mean_biomass)
shapiro.test(dataset_phyto_press$mean_biomass)
# p-value = 1.776e-12
dataset_phyto_press$mean_biomass_log <- log(dataset_phyto_press$mean_biomass)
hist(dataset_phyto_press$mean_biomass_log)
shapiro.test(dataset_phyto_press$mean_biomass_log)
# p-value = 0.1872

# checking normal distribution of zooplankton biomass
hist(dataset_phyto_press$zoopl_biomass)
shapiro.test(dataset_phyto_press$zoopl_biomass)
# p-value = 0.0007597
dataset_phyto_press$zoopl_biomass_log <- log(dataset_phyto_press$zoopl_biomass)
hist(dataset_phyto_press$zoopl_biomass_log)
shapiro.test(dataset_phyto_press$zoopl_biomass_log)
# p-value = 0.04232


# checking normal distribution of zooplankton body size
hist(dataset_phyto_press$zoopl_body_size)
shapiro.test(dataset_phyto_press$zoopl_body_size)
# p-value = 0.7595

#---------------------------------------------------------------------------------------------------------#
# Final reovery, resilience, area under the curve


# checking normal distribution of resilience
hist(dataset_phyto_press$rate_change_oti)
shapiro.test(dataset_phyto_press$rate_change_oti)
# p-value = 2.28e-06
dataset_phyto_press$rate_change_oti_trans <- sqrt(dataset_phyto_press$rate_change_oti*-1)
hist(dataset_phyto_press$rate_change_oti_trans)
shapiro.test(dataset_phyto_press$rate_change_oti_trans)
# p-value = 0.04547

# checking normal distribution of AUC
hist(dataset_phyto_press$total_impact)
shapiro.test(dataset_phyto_press$total_impact)
# p-value = 3.368e-06
dataset_phyto_press$total_impact_log <- log(dataset_phyto_press$total_impact)
hist(dataset_phyto_press$total_impact_log)
shapiro.test(dataset_phyto_press$total_impact_log)
# p-value = 0.5938

# checking normal distribution of final recovery
hist(dataset_phyto_press$final_stab)
shapiro.test(dataset_phyto_press$final_stab)
# p-value = 0.000172
dataset_phyto_press$final_stab_log <- log(dataset_phyto_press$final_stab*-1)
hist(dataset_phyto_press$final_stab_log)
shapiro.test(dataset_phyto_press$final_stab_log)
# p-value = 0.07287

#---------------------------------------------------------------------------------------------------------#
#### 1. final recovery for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# final recovery = response
# TP, TN, TN/TP ratio, DN/TP ratio, DOC, Temp, PAR, Chl a, evenness, ENS, richness, zoopl biomass,
# zoopl body size = explanatory

# testing significance of all variables individually 
final_recovery <- lmer(final_stab_log ~ mean_Temp + (1|Lake), data=dataset_phyto_press)

summary(final_recovery)
# TP p-value = 0.17809
# TN p-value = 0.20662
# TN/TP p-value = 0.250
# DN/TP p-value = 0.209632 
# DOC p-value = 0.491
# Temp p-value = 0.0472 !
# PAR p-value = 0.108327  
# Chl a p-value = 0.00849 ! but single fit
# evenness p-value = 0.0823
# ENS p-value = 0.2184
# richness p-value = 0.111462 
# zoopl biomass p-value = 0.4605
# zoopl body size p-value = 0.77354 

# --> Temp

# running linear mixed-effects model for final recovery
final_recovery <- lmer(final_stab_log ~ mean_J + mean_ENS_D + mean_Temp + (1|Lake), data=dataset_phyto_press)

# excluding  mean_TN because of singular fit 

summary(final_recovery)

# explained variance
r.squaredGLMM(final_recovery)
#            R2m       R2c
# [1,] 0.4721046 0.6096454

#---------------------------------------------------------------------------------------------------------#
#### 2. resilience for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience
# TP, TN, TN/TP ratio, DN/TP ratio, DOC, Temp, PAR, Chl a, biovolume, evenness, ENS, richness, zoopl biomass,
# zoopl body size = explanatory

# testing significance of all variables individually 
resilience <- lmer(rate_change_oti_trans ~ mean_biomass_log + (1|Lake),data=dataset_phyto_press)

summary(resilience)
# TP p-value = 0.20865 
# TN p-value = 0.7484
# TN/TP p-value = 0.0563
# DN/TP p-value = 0.5026  
# DOC p-value = 0.416
# Temp p-value = 0.002751 !
# PAR p-value = 0.19992  
# Chl a p-value = 0.0275 ! but singular fit
# biovolume p-value = 0.0558 
# evenness p-value = 0.4011 
# ENS p-value = 0.0842
# richness p-value =  0.20590  
# zoopl biomass p-value = 0.860
# zoopl body size p-value = 0.78876

# --> Temp

resilience <- lmer(rate_change_oti_trans ~ mean_Temp + (1|Lake),data=dataset_phyto_press)

summary(resilience)


r.squaredGLMM(resilience)
#           R2m       R2c
# [1,] 0.2071197 0.2368784

#---------------------------------------------------------------------------------------------------------#
#### 3. AUC for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = AUC
# TP, TN, TN/TP ratio, DN/TP ratio, DOC, Temp, PAR, Chl a, biovolume, evenness, ENS, richness, zoopl biomass,
# zoopl body size = explanatory

# testing significance of all variables individually
AUC <- lmer(total_impact_log ~ mean_biomass_log +(1|Lake), data=dataset_phyto_press)

summary(AUC)
# TP p-value = 0.148816
# TN p-value = 0.6761
# TN/TP p-value = 0.0993
# DN/TP p-value = 0.945904
# DOC p-value = 0.786
# Temp p-value = 0.012 !
# Chl a p-value = 0.0145 !
# biovolume p-value = 0.063265 
# evenness p-value = 0.119
# ENS p-value = 0.0289 !
# richness p-value =  0.535  
# zoopl biomass p-value = 0.03672 !
# zoopl body size p-value = 0.92 

# --> Temp, Chl a , ENS, zoopl biomass 

test <- lm(total_impact_log ~  mean_Temp + mean_Chla_log + mean_ENS_D + zoopl_biomass_log, data=dataset_phyto_press)
vif(test)
#  mean_Temp     mean_Chla_log        mean_ENS_D zoopl_biomass_log 
# 1.699506          9.021920          4.704285          3.760333 

# excluding Chl a 
AUC <- lmer(total_impact_log ~  mean_Temp + mean_ENS_D + (1|Lake), data=dataset_phyto_press)

summary(AUC)

r.squaredGLMM(AUC)
# R2m       R2c
# [1,] 0.3569754 0.5928481

#---------------------------------------------------------------------------------------------------------#
#### 4. Biovolume for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = biovolume
# TP, TN, TN/TP ratio, DN/TP ratio, DOC, Temp, PAR, Chl a, zoopl biomass,
# zoopl body size = explanatory

# testing significance of all variables individually
biovolume <- lmer(mean_biomass_log ~ mean_Temp + (1|Lake), data=dataset_phyto_press)

summary(biovolume)
# TP p-value = 0.0076 !
# TN p-value =  0.0456 !
# TN/TP p-value = 0.0941
# DN/TP p-value = 0.000832 !
# DOC p-value = 5.46e-05 !
# Temp p-value =  0.989
# PAR p-value = 0.213  
# Chl a p-value = 0.00575 !
# zoopl biomass p-value = 0.437  
# zoopl body size p-value = 0.589 

# --> TP, TN, DN/TP, DOC, Chl a 

test <- lm(mean_biomass_log ~  mean_TP + mean_DN_TP_ratio_100 + mean_TN + mean_DOC_sqrt + mean_Chla_log, data=dataset_phyto_press)
vif(test)
# mean_TP         mean_DN_TP_ratio_100              mean_TN        mean_DOC_sqrt        mean_Chla_log 
# 23.560442             6.089571            19.209531             5.770906             7.612967 


# excluding TP, TN, DOC, DN/TP 
biovolume <- lmer(mean_biomass_log ~ mean_Chla_log + (1|Lake), data=dataset_phyto_press)

summary(biovolume)


r.squaredGLMM(biovolume)
#      R2m       R2c
# [1,] 0.4882709 0.6171366
#---------------------------------------------------------------------------------------------------------#
# Chl a as biomass proxy
Chla <- lmer(mean_Chla_log ~ zoopl_body_size + (1|Lake), data=dataset_phyto_press)
summary(Chla)
# TP p-value = 4.34e-06 !
# TN p-value = 0.000265 !
# TN/TP p-value = 0.0837  !
# DN/TP p-value = 6.33e-06 !
# DOC p-value = 0.000407 !
# Temp p-value = 0.555 
# PAR p-value = 2.56e-09 !
# zoopl biomass = 0.0595 !
# zoopl body size = 0.985

Chla <- lmer(mean_Chla_log ~ mean_TP + mean_PAR_log + (1|Lake), data=dataset_phyto_press)

summary(Chla)

#---------------------------------------------------------------------------------------------------------#
#### 5. Evenness for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# TP, TN, TN/TP ratio, DN/TP ratio, DOC, Temp, PAR, Chl a, zoopl biomass,
# zoopl body size = explanatory

# testing significance of all variables individually
evenness <- lmer(mean_J ~  zoopl_body_size + (1|Lake), data=dataset_phyto_press)

summary(evenness)
# TP p-value = 0.0116  ! but singular fit
# TN p-value = 0.223537 
# TN/TP p-value = 0.426 
# DN/TP p-value =  0.117  
# DOC p-value = 0.985  
# Temp p-value = 0.118  
# PAR p-value = 0.0697 
# Chl a p-value = 0.00141 ! but singular fit
# zoopl biomass p-value = 0.91  
# zoopl body size pvalue =  0.0138 !

# --> zoopl body size 

evenness <- lmer(mean_J ~ zoopl_body_size + (1|Lake), data=dataset_phyto_press)

summary(evenness)

r.squaredGLMM(evenness)
#    R2m      R2c
# [1,] 0.1464127 0.179126

#---------------------------------------------------------------------------------------------------------#
#### 6. ENS for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# TP, TN, TN/TP ratio, DN/TP ratio, DOC, Temp, PAR, Chl a, zoopl biomass,
# zoopl body size = explanatory

# testing significance of all variables individually
ENS <- lmer(mean_ENS_D ~  zoopl_body_size + (1|Lake), data=dataset_phyto_press)

summary(ENS)
# TP p-value = 0.187 
# TN p-value = 0.878 
# TN/TP p-value = 0.7654 
# DN/TP p-value = 0.001680 !
# DOC p-value = 1.06e-08 !
# Temp p-value = 0.7458  
# PAR p-value = 0.011191 !
# Chl a p-value = 0.00149 !
# zoopl biomass p-value = 0.106060  
# zoopl body size p-value = 0.3362  

# --> DN/TP, DOC, PAR, Chl a 

test <- lm(mean_ENS_D ~  mean_DOC_sqrt + mean_TN_TP_ratio_100 + mean_PAR_log + mean_Chla_log, data=dataset_phyto_press)
vif(test)
# mean_DOC_sqrt     mean_TN_TP_ratio_100         mean_PAR_log        mean_Chla_log 
# 1.639339             1.649029             1.598686             1.284783 

# excluding mean_TN_TP_ratio_100 
ENS <- lmer(mean_ENS_D ~  mean_DOC_sqrt + mean_PAR_log + mean_Chla_log + (1|Lake), data=dataset_phyto_press)

summary(ENS)

r.squaredGLMM(ENS)
#     R2m       R2c
# [1,] 0.382522 0.9930729

#---------------------------------------------------------------------------------------------------------#
#### 7. SEM ####
#---------------------------------------------------------------------------------------------------------##  

## final individual models 


final_recovery <- lmer(final_stab_log ~ mean_J + mean_ENS_D + mean_Temp + (1|Lake), data=dataset_phyto_press)
# resilience <- lmer(rate_change_oti_trans ~ mean_Temp +(1|Lake),data=dataset_phyto_press)
AUC <- lmer(total_impact_log ~  mean_Temp + mean_ENS_D + (1|Lake), data=dataset_phyto_press)
biovolume <- lmer(mean_biomass_log ~ mean_Chla_log + (1|Lake), data=dataset_phyto_press)
ENS <- lmer(mean_ENS_D ~  mean_DOC_sqrt + mean_PAR_log + mean_Chla_log + (1|Lake), data=dataset_phyto_press)
Chla <- lmer(mean_Chla_log ~ mean_TP + mean_PAR_log + mean_Temp + mean_DOC_sqrt + (1|Lake), data=dataset_phyto_press)


dataset_phyto_press_complete <- na.omit(dataset_phyto_press)

# SEM
modelList <- psem(
  AUC,
  biovolume,
  ENS,  
  Chla,
  mean_biomass_log  %~~% mean_Temp,
  dataset_phyto_press_complete
  )


results <- summary(modelList)
# excluding resilience because of singular fit 

rsquared(modelList)

results$IC$AIC
# 72.141

results$dTable

results$Cstat
#  Fisher.C df P.Value
# 1   28.141 22   0.171

results$coefficients


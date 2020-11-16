#---------------------------------------------------------------------------------------------------------#
#### SEM for pulse disturbance & phytoplankton ####
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

# load packages

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
# explanatory = Chl a, TP, TN, TN/TP ratio, DN/TP ratio, DOC, PAR, Temp, biovolume, evenness, richness, ENS,
# zooplankton biomass, zooplankton body size 

# testing significance of all variables individually 
resistance <- lmer(initial_stab_log ~ initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)
summary(resistance)

# Chl a t-value = -0.935
# TP t-value= -1.107
# TN t-value = -1.493 
# TN/TP t-value = 0.416 
# DN/TP t-value =  -0.403
# DOC t-value =  -0.106  
# PAR t-value =  2.513 
# Temp t-value =  0.077  
# biovol t-value = -0.569
# evenness t-value = -0.114 
# richness t-value =  1.022
# ENS t-value = 0.061 
# zooplankton biomass t-value = -3.147 
# zooplankton body size t-value = 0.859 

# --> TP, TN, PAR, richness, zooplankton biomass

TP_TN_test <- lmer(mean_TP_log ~ mean_TN + (1|Lake), data = dataset_phyto_pulse)
summary(TP_TN_test)
# TP and TN are highly correlated

# setting up the model fit 
test <- lm(initial_stab_log ~ mean_TP_log + mean_TN + mean_PAR_log +
                     mean_s_sqrt + mean_zoopl_biomass_log, data=dataset_phyto_pulse)
summary(test)


# testing collinearity: 
# variance inflation factor. The variance inflation factor quantifies the effect of collinearity on the 
# variance of our regression estimates.
# In practice it is common to say that any VIF greater than 5 is cause for concern.

library(faraway)

vif(test)
# mean_TP_log                mean_TN           mean_PAR_log            mean_s_sqrt 
# 6.004513               5.719671               1.475181               1.527646 
# mean_zoopl_biomass_log 
# 1.166779 


# excluding TN from model 
resistance <- lmer(initial_stab_log ~ mean_TP_log + mean_PAR_log +
                   mean_s_sqrt + mean_zoopl_biomass_log + (1|Lake),data=dataset_phyto_pulse)
summary(resistance)
# PAR
# zooplankton biomass

r.squaredGLMM(resistance)
#   R2m      R2c
# [1,] 0.2396513 0.489559

# So, the marginal R2 is the fixed effects variance, divided by the total variance (i.e. fixed + random + residual). 
# This value indicates how much of the "model variance" is explained by the fixed effects part only.
# The conditional R2 is the fixed+random effects variance divided by the total variance, and indicates 
# how much of the "model variance" is explained by your "complete" model.

#---------------------------------------------------------------------------------------------------------#
#### 2. final recovery for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = final recovery 
# explanatory = Chl a, TP, TN, TN/TP ratio, DN/TP ratio, DOC, PAR, Temp, biovolume, evenness, richness, ENS,
# zooplankton biomass, zooplankton body size 

# testing significance of all variables individually 
test <- lmer(final_stab_log ~ initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)
summary(test)
# Chl a t-value = -1.870 --> singular fit
# TP t-value = -1.099 
# TN t-value =  -1.129  
# TN/TP ratio t-value = 0.813 
# DN/TP ratio t-value = 0.047 
# DOC t-value = -0.043
# PAR t-value = -1.398
# Temp t-value = -0.309
# biovolume t-value = -0.864
# evenness t-value = -0.815 
# richness t-value = -1.372 
# ENS t-value = -0.992 
# zoopl biomass t-value = 1.543 
# initial zoopl body size t-value = -1.668

# --> Chl a, TP, TN, PAR, richness, zoopl biomass, initial zoopl body size 

# setting up the model fit 
recovery <- lmer(final_stab_log ~ mean_TP_log + mean_PAR_log + mean_s_sqrt + 
                   mean_zoopl_biomass_log + initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)

summary(recovery)
# TP
# PAR
# richness
# mean zoopl biomass
# zoopl body size

r.squaredGLMM(recovery)
# R2m       R2c
# [1,] 0.2702587 0.2979298

#---------------------------------------------------------------------------------------------------------#
#### 3. resilience for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = resilience
# explanatory = Chl a, TP, TN, TN/TP ratio, DN/TP ratio, DOC, PAR, Temp, biovolume, evenness, richness, ENS,
# zooplankton biomass, zooplankton body size 

# testing significance of all variables individually 
test <- lmer(rate_change_ort_log ~ mean_PAR_log + (1|Lake), data=dataset_phyto_pulse)
summary(test)
# Chl a t-value = -0.396 --> singular fit
# TP t-value = 0.068 
# TN t-value = 0.070  
# TN/TP t-value = -0.125 
# DN/TP t-value = -0.690
# DOC t-value = 1.373  
# PAR t-value = -3.31 --> singular fit
# Temp t-value = -2.115 
# biovol t-value = 0.044
# evenness t-value = -0.414 
# richness t-value = -1.253
# ENS t-value = -0.143
# zoopl biomass t-value = 2.905
# initial zoopl body size t-value =  -1.861 

# --> DOC, PAR, Temp, zoopl biomass, zoopl body size 

# setting up the model fit 
resilience <- lmer(rate_change_ort_log ~ mean_DOC_log + mean_Temp_log + 
                     mean_zoopl_biomass_log + initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)

summary(resilience)
# DOC
# zoopl biomass
# zoopl body size 

r.squaredGLMM(resilience)
# R2m       R2c
# [1,] 0.3453048 0.4183187

#---------------------------------------------------------------------------------------------------------#
#### 4. AUC for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = AUC
# explanatory = Chl a, TP, TN, TN/TP ratio, DN/TP ratio, DOC, PAR, Temp, biovolume, evenness, richness, ENS,
# zooplankton biomass, zooplankton body size 

# testing significance of all variables individually 
test <- lmer(total_impact_log ~ initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)
summary(test)
# Chl a t-value = -3.656
# TP t-value = -3.283
# TN t-value = -3.56
# TN/TP t-value =  2.011
# DN/TP t-value =  0.565 
# DOC t-value = -0.497 
# PAR t-value = -0.599
# Temp t-value = -1.323 
# biovol t-value = -0.207
# evenness t-value = -0.650
# richness t-value = -1.499
# ENS t-value = -0.804
# zoopl biomass t-value =  0.925 
# zoopl body size = -1.807

# --> Chl a, TP, TN, TN/TP, Temp, richness, zoopl body size

# setting up the model fit 
AUC <- lmer(total_impact_log ~ mean_Chla_log + mean_TP_log + mean_Temp_log + mean_s_sqrt + 
              initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)

summary(AUC)
# TP
# zoopl body size 

r.squaredGLMM(AUC)
# R2m       R2c
# [1,] 0.4183433 0.4744889

#---------------------------------------------------------------------------------------------------------#
#### 5. Biomass for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = biomass
# explanatory = Chl a, TP, TN, TN/TP ratio, DN/TP ratio, Temp, PAR, DOC, mean zoopl biomass, zooplankton body size

# testing significance of all variables individually 
test <- lmer(mean_biomass_log ~  initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)
summary(test)
# Chl a t-value = 3.812
# TP t-value = 1.500
# TN t-value = 1.716
# TN/TP t-value = -1.134 --> singular fit
# DN/TP t-value = -0.465 --> singular fit
# Temp t-value = -0.563
# PAR t-value = 0.752
# DOC t-value = -0.964
# zoopl biomass t-value = 2.174
# zoopl body size = -1.961

# --> Chl a, TP, TN, TN/TP ratio, zoopl biomass, zoopl body size

# checking any collinearities
test <- lm(mean_biomass_log ~  mean_Chla_log + mean_TN + mean_zoopl_biomass_log + initial_zoop_body_size_log, data=dataset_phyto_pulse)
vif(test)
# no collinearities 

# setting up the model fit 
biomass <- lmer(mean_biomass_log ~  mean_Chla_log + mean_TP_log + mean_zoopl_biomass_log + initial_zoop_body_size_log + 
                  (1|Lake), data=dataset_phyto_pulse)

summary(biomass)
# Chl a 
# zoopl biomass (either significant or not depending on including TP or TN in the model)
# zoopl body size

r.squaredGLMM(biomass)
# R2m       R2c
# [1,] 0.368294 0.3904837

#---------------------------------------------------------------------------------------------------------#
#### 6. Evenness for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = evenness
# explanatory = Chl a, TP, TN, TN/TP ratio, DN/TP ratio, Temp, PAR, DOC, mean zoopl biomass, zooplankton body size

# testing significance of all variables individually 
test <- lmer(mean_J ~  initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)
summary(test)
# Chl a t-value =  3.142 
# TP t-value = -0.395
# TN t-value = -0.45
# TN/TP t-value = -0.438 
# DN/TP  t-value = -3.632
# Temp t-value = 0.821 
# PAR t-value = -2.021
# DOC t-value = -1.617 
# zoopl biomass t-value = -2.504
# zoopl body size t-value = 1.001 

# --> Chl a, PAR, DOC, zoopl biomass, zoopl body size

# setting up the model fit 
evenness <- lmer(mean_J ~  mean_Chla_log + mean_PAR_log + mean_DOC_log + initial_zoop_body_size_log + 
                   mean_zoopl_biomass_log + (1|Lake), data=dataset_phyto_pulse)
summary(evenness)
# Chl a 
# PAR
# DOC
# initial zoopl body size

r.squaredGLMM(evenness)
#   R2m       R2c
# [1,] 0.6270817 0.7481166

#---------------------------------------------------------------------------------------------------------#
#### 7. ENS for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
# response = ENS
# explanatory = Chl a, TP, TN, TN/TP ratio, DN/TP ratio, Temp, PAR, DOC, mean zoopl biomass, zooplankton body size

# testing significance of all variables individually 
test <- lmer(mean_ENS_D ~  initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)
summary(test)
# Chl a t-value =  3.139
# TP t-value = -1.839
# TN t-value = -1.898
# TN/TP t-value =  0.270 
# DN/TP t-value = -4.259
# Temp t-value = 0.482 
# PAR t-value = -2.596 
# DOC t-value = -5.166
# zoopl biomass t-value = -1.277
# zoopl body size t-value = 1.564 

# --> Chl a, TP, TN, DN/TP, PAR, DOC, zoopl biomass, zoopl body size 

# setting up the model fit 
ENS <- lmer(mean_ENS_D ~  mean_Chla_log + mean_TP + mean_PAR_log + mean_DOC_log + mean_zoopl_biomass_log + 
             initial_zoop_body_size_log + (1|Lake), data=dataset_phyto_pulse)

summary(ENS)

# Chl a 
# TP
# PAR
# DOC
# mean zoopl biomass

r.squaredGLMM(ENS)
# R2m       R2c
# [1,] 0.3833846 0.8945589

#---------------------------------------------------------------------------------------------------------#






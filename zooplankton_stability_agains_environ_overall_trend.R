#-------------------------------------------------------------------------------------------#

## Stability ~ environmental parameters correlation plots (Zooplankton) overall trend ####

#-------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------#

## 0. About ####

#-------------------------------------------------------------------------------------------#
# load packages
library(tidyverse)
library(scales)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(gdata)

# environmental parameters:
# TP
# TN 
# light (PAR I)
# DOC 
# temperature 
# TN/TP ratio
# DN/TP ratio

# stability measures:
# AUC (total_impact)
# recovery (final_stab) for pulse & press
# resistance (initial_stab) for pulse
# resilience (rate_change_ort, rate_change_oti) for pulse & press

#-------------------------------------------------------------------------------------------#

## 1. load and transform datasets ####

#-------------------------------------------------------------------------------------------#
# load stability dataset
stability1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Stability/stab_stand_yes_c.csv", sep=",")

# deleting first column
stability <- stability1[-1] 

# changing Lake names 
levels(stability$Lake)
#  "Bolmen"       "Erken"        "Erssj\xf6n"   "Feresj\xf6n"  "Stortj\xe4rn"

stability$Lake<- as.character(stability$Lake)

stability$Lake[stability$Lake == "Erssj\xf6n"] <- "Erssjoen"

stability$Lake[stability$Lake == "Feresj\xf6n"] <- "Feresjoen"

stability$Lake[stability$Lake == "Stortj\xe4rn"] <- "Stortjaern"

stability$Lake <- as.factor(stability$Lake)

levels(stability$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"


# filtering variable for phytoplankton only 
zoop_stability <- stability[which(stability$variable=='zoop_function'), ]

# only filtering the parameters we need ()
zoop_stability  <- select(zoop_stability , Lake, Experiment, Enclosure, 
                          Treatment, final_stab, initial_stab, 
                          rate_change_ort, rate_change_oti, total_impact)

#-------------------------------------------------------------------------------------------#
# load environmental parameter dataset 
env_parameters1 <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/physico_chemistry_fish.csv", sep=";")

#filtering 
env_parameters <- select(env_parameters1, Lake, Experiment, Sampling, Exp_day, Sampling_day, 
                         Sampling_month, Enclosure, Treatment, Replicate, TP, TN, DN, DOC, PAR_I, Temperature)

# changing names for column "Experiment"
env_parameters$Experiment <- as.integer(env_parameters$Experiment)

env_parameters$Experiment[env_parameters$Experiment == 1] <- "Spring"

env_parameters$Experiment[env_parameters$Experiment == 2] <- "Summer"

env_parameters$Experiment <- as.character(env_parameters$Experiment)


# changing names of column "Lake"
levels(env_parameters$Lake)
# "Asa"         "Bolmen"      "Erken"       "Skogaryd"    "Svartberget"

env_parameters$Lake<- as.character(env_parameters$Lake)

env_parameters$Lake[env_parameters$Lake == "Asa"] <- "Feresjoen"

env_parameters$Lake[env_parameters$Lake == "Skogaryd"] <- "Erssjoen"

env_parameters$Lake[env_parameters$Lake == "Svartberget"] <- "Stortjaern"

env_parameters$Lake <- as.factor(env_parameters$Lake)

levels(env_parameters$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

# set TN as numeric
env_parameters$TN <- as.character(env_parameters$TN)

env_parameters$TN <- as.numeric(env_parameters$TN)

# delete all replicate / treatment "lake" and "C" 
levels(env_parameters$Replicate)
env_parameters <- env_parameters[ ! (env_parameters$Replicate %in% c("C", "lake", "Lake")), ]

levels(env_parameters$Treatment)
env_parameters <- env_parameters[ ! (env_parameters$Treatment %in% c("C","Lake")), ]

# new column with TN/TP ratio
env_parameters <- mutate(env_parameters, "TN_TP_ratio" = TN / TP)

# new column with DN/TP ratio
env_parameters <- mutate(env_parameters, "DN_TP_ratio" = DN / TP)

# creating mean values for all environmental parameters
env_parameters_mean <- env_parameters %>%
  group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_TP = mean(TP, na.rm=T), mean_TN= mean(TN, na.rm=T), mean_DOC= mean(DOC, na.rm=T), 
            mean_PAR= mean(PAR_I, na.rm=T), mean_Temp = mean(Temperature, na.rm=T), 
            mean_TN_TP_ratio = mean(TN_TP_ratio, na.rm=T), mean_DN_TP_ratio=mean(DN_TP_ratio, na.rm=T))

#-------------------------------------------------------------------------------------------#
# merging both datsets
mean_zoop_stability_environ <- merge(x=env_parameters_mean, y=zoop_stability, 
                                     by =c("Lake", "Experiment", "Enclosure", "Treatment"), 
                                     all.x=TRUE)

#-------------------------------------------------------------------------------------------#

## 2. AUC ~ TP (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_AUC_TP <- ggplot(data=mean_zoop_stability_environ, aes(x=mean_TP, y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total phosphorus") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TP (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


# filtering only Treatment = F
mean_zoop_stability_environ_F <- mean_zoop_stability_environ[which(mean_zoop_stability_environ$Treatment=='F'), ]
# plotting all lakes, all experiments, treatment "F"
plot_zoop_AUC_TP_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_TP, 
                                                                    y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total phosphorus") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TP (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = S
mean_zoop_stability_environ_S <- mean_zoop_stability_environ[which(mean_zoop_stability_environ$Treatment=='S'), ]
# plotting all lakes, all experiments, treatment "S"
plot_zoop_AUC_TP_S<- ggplot(data=mean_zoop_stability_environ_S, aes(x=mean_TP, 
                                                                    y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total phosphorus") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TP (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# filtering only Treatment = FS
mean_zoop_stability_environ_FS <- mean_zoop_stability_environ[which(mean_zoop_stability_environ$Treatment=='FS'), ]
# plotting all lakes, all experiments, treatment "FS"
plot_zoop_AUC_TP_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_TP, 
                                                                      y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total phosphorus") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TP (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_AUC_TP, plot_zoop_AUC_TP_F, 
             plot_zoop_AUC_TP_S, plot_zoop_AUC_TP_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 3. AUC ~ TN (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_AUC_TN <- ggplot(data=mean_zoop_stability_environ, aes(x=mean_TN, 
                                                                 y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total nitrogen") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_AUC_TN_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_TN, 
                                                                    y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total nitrogen") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_AUC_TN_S<- ggplot(data=mean_zoop_stability_environ_S, aes(x=mean_TN, 
                                                                    y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total nitrogen") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_AUC_TN_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_TN, 
                                                                      y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total nitrogen") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_AUC_TN, plot_zoop_AUC_TN_F, 
             plot_zoop_AUC_TN_S, plot_zoop_AUC_TN_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 4. AUC ~ DOC (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_AUC_DOC <- ggplot(data=mean_zoop_stability_environ, aes(x=mean_DOC, 
                                                                  y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DOC") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DOC (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_AUC_DOC_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_DOC, 
                                                                     y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DOC") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DOC (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_AUC_DOC_S<- ggplot(data=mean_zoop_stability_environ_S, aes(x=mean_DOC, 
                                                                     y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DOC") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DOC (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_AUC_DOC_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_DOC, 
                                                                       y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DOC") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DOC (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_AUC_DOC, plot_zoop_AUC_DOC_F, 
             plot_zoop_AUC_DOC_S, plot_zoop_AUC_DOC_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 5. AUC ~ PAR (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_AUC_PAR <- ggplot(data=mean_zoop_stability_environ, aes(x=mean_PAR, 
                                                                  y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("PAR") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ PAR (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_AUC_PAR_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_PAR, 
                                                                     y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("PAR") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ PAR (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_AUC_PAR_S<- ggplot(data=mean_zoop_stability_environ_S, aes(x=mean_PAR, 
                                                                     y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("PAR") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ PAR (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_AUC_PAR_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_PAR, 
                                                                       y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("PAR") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ PAR (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_AUC_PAR, plot_zoop_AUC_PAR_F, 
             plot_zoop_AUC_PAR_S, plot_zoop_AUC_PAR_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 6. AUC ~ Temperature (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_AUC_Temp <- ggplot(data=mean_zoop_stability_environ, aes(x=mean_Temp, 
                                                                   y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Temperature") +
  ylab("Area under the curve") +
  xlim(11,20) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Temperature (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_AUC_Temp_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_Temp, 
                                                                      y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Temperature") +
  ylab("Area under the curve") +
  xlim(11,20) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Temperature (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_AUC_Temp_S<- ggplot(data=mean_zoop_stability_environ_S, aes(x=mean_Temp, 
                                                                      y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Temperature") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Temperature (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_AUC_Temp_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_Temp, 
                                                                        y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Temperature") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Temperature (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_AUC_Temp, plot_zoop_AUC_Temp_F, 
             plot_zoop_AUC_Temp_S, plot_zoop_AUC_Temp_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 7. AUC ~  TN/TP ratio (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_AUC_TN_TP <- ggplot(data=mean_zoop_stability_environ, aes(x=mean_TN_TP_ratio, 
                                                                    y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("TN/TP ratio") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN/TP ratio (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_AUC_TN_TP_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_TN_TP_ratio, 
                                                                       y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("TN/TP ratio") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN/TP ratio (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_AUC_TN_TP_S<- ggplot(data=mean_zoop_stability_environ_S, aes(x=mean_TN_TP_ratio, 
                                                                       y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("TN/TP ratio") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN/TP ratio (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_AUC_TN_TP_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_TN_TP_ratio, 
                                                                         y=total_impact)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("TN/TP ratio") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN/TP ratio (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_AUC_TN_TP, plot_zoop_AUC_TN_TP_F, 
             plot_zoop_AUC_TN_TP_S, plot_zoop_AUC_TN_TP_FS, ncol=2)


#-------------------------------------------------------------------------------------------#

## 8. AUC ~  DN/TP ratio (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_AUC_DN_TP <- ggplot(data=mean_zoop_stability_environ, aes(x=mean_DN_TP_ratio, 
                                                                    y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DN/TP ratio") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DN/TP ratio (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_AUC_DN_TP_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_DN_TP_ratio, 
                                                                       y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DN/TP ratio") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DN/TP ratio (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_AUC_DN_TP_S<- ggplot(data=mean_zoop_stability_environ_S, aes(x=mean_DN_TP_ratio, 
                                                                       y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DN/TP ratio") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DN/TP ratio (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_AUC_DN_TP_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_DN_TP_ratio, 
                                                                         y=total_impact)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DN/TP ratio") +
  ylab("Area under the curve") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DN/TP ratio (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_AUC_DN_TP, plot_zoop_AUC_DN_TP_F, 
             plot_zoop_AUC_DN_TP_S, plot_zoop_AUC_DN_TP_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 8. Recovery ~  TP (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_recovery_TP <- ggplot(data=mean_zoop_stability_environ, aes(x=mean_TP, 
                                                                      y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total phosphorus") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TP (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_recovery_TP_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_TP, 
                                                                         y=final_stab)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total phosphorus") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TP (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_recovery_TP_S<- ggplot(data=mean_zoop_stability_environ_S, aes(x=mean_TP, 
                                                                         y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total phosphorus") +
  ylab("Reovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TP (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_recovery_TP_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_TP, 
                                                                           y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total phosphorus") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TP (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_recovery_TP, plot_zoop_recovery_TP_F, 
             plot_zoop_recovery_TP_S, plot_zoop_recovery_TP_FS, ncol=2)
#-------------------------------------------------------------------------------------------#

## 9. Recovery ~  TN (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_recovery_TN <- ggplot(data=mean_zoop_stability_environ, aes(x=mean_TN, 
                                                                      y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total nitrogen") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_recovery_TN_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_TN, 
                                                                         y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total nitrogen") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_recovery_TN_S<- ggplot(data=mean_zoop_stability_environ_S, aes(x=mean_TN, 
                                                                         y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total nitrogen") +
  ylab("Reovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_recovery_TN_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_TN, 
                                                                           y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total nitrogen") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_recovery_TN, plot_zoop_recovery_TN_F, 
             plot_zoop_recovery_TN_S, plot_zoop_recovery_TN_FS, ncol=2)
#-------------------------------------------------------------------------------------------#

## 10. Recovery ~  DOC (Zooplankton) ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all treatments, all experiments
plot_zoop_recovery_DOC <- ggplot(data=mean_zoop_stability_environ, aes(x=mean_DOC, 
                                                                       y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DOC") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DOC (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_recovery_DOC_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_DOC, 
                                                                          y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DOC") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DOC (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_recovery_DOC_S<- ggplot(data=mean_zoop_stability_environ_S, aes(x=mean_DOC, 
                                                                          y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DOC") +
  ylab("Reovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DOC (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_recovery_DOC_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_DOC, 
                                                                            y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DOC") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DOC (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_recovery_DOC, plot_zoop_recovery_DOC_F, 
             plot_zoop_recovery_DOC_S, plot_zoop_recovery_DOC_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 11. Recovery ~  PAR (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_recovery_PAR <- ggplot(data=mean_zoop_stability_environ, aes(x=mean_PAR, 
                                                                       y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("PAR") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ PAR (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_recovery_PAR_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_PAR, 
                                                                          y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("PAR") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ PAR (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_recovery_PAR_S<- ggplot(data=mean_zoop_stability_environ_S, aes(x=mean_PAR, 
                                                                          y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("PAR") +
  ylab("Reovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ PAR (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_recovery_PAR_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_PAR, 
                                                                            y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("PAR") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ PAR (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_recovery_PAR, plot_zoop_recovery_PAR_F, 
             plot_zoop_recovery_PAR_S, plot_zoop_recovery_PAR_FS, ncol=2)
#-------------------------------------------------------------------------------------------#

## 12. Recovery ~  Temperature (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_recovery_Temp <- ggplot(data=mean_zoop_stability_environ, aes(x=mean_Temp, 
                                                                        y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Temperatur") +
  ylab("Recovery") +
  xlim(11,20)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Temperature (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_recovery_Temp_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_Temp, 
                                                                           y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Temperature") +
  ylab("Recovery") +
  xlim(11,20)+
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Temperature (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_recovery_Temp_S<- ggplot(data=mean_zoop_stability_environ_S, aes(x=mean_Temp, 
                                                                           y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Temperature") +
  ylab("Reovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Temperature (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_recovery_Temp_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_Temp, 
                                                                             y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Temperature") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Temperature (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_recovery_Temp, plot_zoop_recovery_Temp_F, 
             plot_zoop_recovery_Temp_S, plot_zoop_recovery_Temp_FS, ncol=2)
#-------------------------------------------------------------------------------------------#

## 13. Recovery ~  TN/TP ratio (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_recovery_TN_TP <- ggplot(data=mean_zoop_stability_environ, aes(x=mean_TN_TP_ratio, 
                                                                         y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("TN/TP ratio") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN/TP ratio (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_recovery_TN_TP_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_TN_TP_ratio, 
                                                                            y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("TN/TP ratio") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN/TP ratio (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_recovery_TN_TP_S<- ggplot(data=mean_zoop_stability_environ_S, aes(x=mean_TN_TP_ratio, 
                                                                            y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("TN/TP ratio") +
  ylab("Reovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN/TP ratio (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_recovery_TN_TP_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_TN_TP_ratio, 
                                                                              y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("TN/TP ratio") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN/TP ratio (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_recovery_TN_TP, plot_zoop_recovery_TN_TP_F, 
             plot_zoop_recovery_TN_TP_S, plot_zoop_recovery_TN_TP_FS, ncol=2)
#-------------------------------------------------------------------------------------------#

## 14. Recovery ~  DN/TP ratio (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all treatments, all experiments
plot_zoop_recovery_DN_TP <- ggplot(data=mean_zoop_stability_environ, aes(x=mean_DN_TP_ratio, 
                                                                         y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DN/TP ratio") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DN/TP ratio (all treatments)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "F"
plot_zoop_recovery_DN_TP_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_DN_TP_ratio, 
                                                                            y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DN/TP ratio") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DN/TP ratio (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_recovery_DN_TP_S<- ggplot(data=mean_zoop_stability_environ_S, aes(x=mean_DN_TP_ratio, 
                                                                            y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DN/TP ratio") +
  ylab("Reovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DN/TP ratio (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_recovery_DN_TP_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_DN_TP_ratio, 
                                                                              y=final_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DN/TP ratio") +
  ylab("Recovery") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DN/TP ratio (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_recovery_DN_TP, plot_zoop_recovery_DN_TP_F, 
             plot_zoop_recovery_DN_TP_S, plot_zoop_recovery_DN_TP_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 15. Resistance (for pulse) ~ TP (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resistance_TP_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_TP, 
                                                                           y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total phosphorus") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TP (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_resistance_TP_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_TP, 
                                                                             y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total phosphorus") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TP (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


#-------------------------------------------------------------------------------------------#

## 16. Resistance (for pulse) ~ TN (Zooplankton) ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all experiments, treatment "F"
plot_zoop_resistance_TN_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_TN, 
                                                                           y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total nitrogen") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_resistance_TN_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_TN, 
                                                                             y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Total nitrogen") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


grid.arrange(plot_zoop_resistance_TP_F, plot_zoop_resistance_TP_FS, 
             plot_zoop_resistance_TN_F, plot_zoop_resistance_TN_FS, ncol=2)

#-------------------------------------------------------------------------------------------#

## 17. Resistance (for pulse) ~ DOC (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resistance_DOC_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_DOC, 
                                                                            y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DOC") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DOC (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_resistance_DOC_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_DOC, 
                                                                              y=initial_stab)) +
  labs(color = "Lake") +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DOC") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DOC (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

#-------------------------------------------------------------------------------------------#

## 18. Resistance (for pulse) ~ PAR (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resistance_PAR_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_PAR, 
                                                                            y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("PAR") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ PAR (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_resistance_PAR_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_PAR, 
                                                                              y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("PAR") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ PAR (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


grid.arrange(plot_zoop_resistance_DOC_F, plot_zoop_resistance_DOC_FS, 
             plot_zoop_resistance_PAR_F, plot_zoop_resistance_PAR_FS, ncol=2)


#-------------------------------------------------------------------------------------------#

## 19. Resistance (for pulse) ~ Temp (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resistance_Temp_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_Temp, 
                                                                             y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Temperature") +
  ylab("Resistance") +
  xlim(11,20) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Temperature (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_resistance_Temp_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_Temp, 
                                                                               y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("Temperature") +
  ylab("Resistance") +
  xlim(11,20) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Temperature (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

#-------------------------------------------------------------------------------------------#

## 20. Resistance (for pulse) ~ TN/TP ratio (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resistance_TN_TP_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_TN_TP_ratio, 
                                                                              y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("TN/TP ratio") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN/TP ratio (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_resistance_TN_TP_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_TN_TP_ratio, 
                                                                                y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("TN/TP ratio") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN/TP ratio (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

#-------------------------------------------------------------------------------------------#

## 21. Resistance (for pulse) ~ DN/TP ratio (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resistance_DN_TP_F<- ggplot(data=mean_zoop_stability_environ_F, aes(x=mean_DN_TP_ratio, 
                                                                              y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DN/TP ratio") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DN/TP ratio (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "FS"
plot_zoop_resistance_DN_TP_FS<- ggplot(data=mean_zoop_stability_environ_FS, aes(x=mean_DN_TP_ratio, 
                                                                                y=initial_stab)) +
  geom_point(size=2) +
  geom_smooth(method = 'lm') +
  xlab("DN/TP ratio") +
  ylab("Resistance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DN/TP ratio (pulse & press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_resistance_Temp_F, plot_zoop_resistance_Temp_FS, 
             plot_zoop_resistance_TN_TP_F, plot_zoop_resistance_TN_TP_FS, 
             plot_zoop_resistance_DN_TP_F, plot_zoop_resistance_DN_TP_FS,ncol=2)

#-------------------------------------------------------------------------------------------#

## 22. Resilience ~ TP (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resilience_TP_F <- ggplot(data=mean_zoop_stability_environ_F,aes(x=mean_TP, y=rate_change_ort)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Total phosphorus") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TP (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_resilience_TP_S <- ggplot(data=mean_zoop_stability_environ_S,aes(x=mean_TP, y=rate_change_oti)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Total phosphorus") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TP (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

#-------------------------------------------------------------------------------------------#

## 23. Resilience ~ TN (Zooplankton) ####

#-------------------------------------------------------------------------------------------#

# plotting all lakes, all experiments, treatment "F"
plot_zoop_resilience_TN_F <- ggplot(data=mean_zoop_stability_environ_F,aes(x=mean_TN, y=rate_change_ort)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Total nitrogen") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_resilience_TN_S <- ggplot(data=mean_zoop_stability_environ_S,aes(x=mean_TN, y=rate_change_oti)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Total nitrogen") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_resilience_TP_F, plot_zoop_resilience_TP_S,
             plot_zoop_resilience_TN_F, plot_zoop_resilience_TN_S, ncol=2)

#-------------------------------------------------------------------------------------------#

## 24. Resilience ~ DOC (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resilience_DOC_F <- ggplot(data=mean_zoop_stability_environ_F,aes(x=mean_DOC, y=rate_change_ort)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("DOC") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DOC (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_resilience_DOC_S <- ggplot(data=mean_zoop_stability_environ_S,aes(x=mean_DOC, y=rate_change_oti)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("DOC") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DOC (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

#-------------------------------------------------------------------------------------------#

## 25. Resilience ~ PAR (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resilience_PAR_F <- ggplot(data=mean_zoop_stability_environ_F,aes(x=mean_PAR, y=rate_change_ort)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("PAR") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ PAR (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_resilience_PAR_S <- ggplot(data=mean_zoop_stability_environ_S,aes(x=mean_PAR, y=rate_change_oti)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("PAR") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ PAR (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_resilience_DOC_F, plot_zoop_resilience_DOC_S ,
             plot_zoop_resilience_PAR_F, plot_zoop_resilience_PAR_S, ncol=2)

#-------------------------------------------------------------------------------------------#

## 26. Resilience ~ Temp (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resilience_Temp_F <- ggplot(data=mean_zoop_stability_environ_F,aes(x=mean_Temp, y=rate_change_ort)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Temperature") +
  ylab("Resilience") +
  xlim(11,20) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Temperature (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_resilience_Temp_S <- ggplot(data=mean_zoop_stability_environ_S,aes(x=mean_Temp, y=rate_change_oti)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("Temperature") +
  ylab("Resilience") +
  xlim(11,20) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ Temperature (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

#-------------------------------------------------------------------------------------------#

## 27. Resilience ~ TN/TP ratio (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resilience_TN_TP_F <- ggplot(data=mean_zoop_stability_environ_F,aes(x=mean_TN_TP_ratio, y=rate_change_ort)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("TN/TP ratio") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN/TP ratio (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_resilience_TN_TP_S <- ggplot(data=mean_zoop_stability_environ_S,aes(x=mean_TN_TP_ratio, y=rate_change_oti)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("TN/TP ratio") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ TN/TP ratio (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

#-------------------------------------------------------------------------------------------#

## 28. Resilience ~ DN/TP ratio (Zooplankton) ####

#-------------------------------------------------------------------------------------------#
# plotting all lakes, all experiments, treatment "F"
plot_zoop_resilience_DN_TP_F <- ggplot(data=mean_zoop_stability_environ_F,aes(x=mean_DN_TP_ratio, y=rate_change_ort)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("DN/TP ratio") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DN/TP ratio (pulse)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# plotting all lakes, all experiments, treatment "S"
plot_zoop_resilience_DN_TP_S <- ggplot(data=mean_zoop_stability_environ_S,aes(x=mean_DN_TP_ratio, y=rate_change_oti)) +
  geom_point(size=2) +
  geom_smooth(method = lm) +
  xlab("DN/TP ratio") +
  ylab("Resilience") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Zooplankton stability ~ DN/TP ratio (press)")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

grid.arrange(plot_zoop_resilience_Temp_F, plot_zoop_resilience_Temp_S,
             plot_zoop_resilience_TN_TP_F, plot_zoop_resilience_TN_TP_S,
             plot_zoop_resilience_DN_TP_F, plot_zoop_resilience_DN_TP_S, ncol=2)


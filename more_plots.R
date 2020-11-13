#---------------------------------------------------------------------------------------------------------#
#### correlation plots for phytoplankton ####
#---------------------------------------------------------------------------------------------------------#
library(tidyverse)
library(scales)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(gdata)
library(readxl)

dataset_final <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/master_dataset.csv")

# TP ~ DOC

ggplot(data=dataset_final, aes(x=mean_DOC, y=mean_TP)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab("DOC (mean)") +
  ylab("Total phosphorus (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Nutrients")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

# light ~ DOC
ggplot(data=dataset_final, aes(x=mean_PAR, y=mean_DOC)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab("PAR (mean)") +
  ylab("DOC (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Light and DOC")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")



# zooplankton biomass ~ Chl a 
dataset_zooplankton <- dataset_final[which(dataset_final$community=='zoopl'), ]

ggplot(data=dataset_zooplankton, aes(x=mean_Chla, y=initial_zoop_body_size)) +
  scale_x_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab("Chl a  (log-transformed)") +
  ylab("Zooplankton biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chl a and Zooplankton biomass")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


# zooplankton initial biomass ~ Chl a 

ggplot(data=dataset_zooplankton, aes(x=mean_Chla, y=mean_initial_zoop_biomass)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab("Chl a  (mean)") +
  ylab("Zooplankton biomass (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chl a and Zooplankton biomass")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


# zooplankton initial body size 

ggplot(data=dataset_final, aes(x=mean_Chla, y=initial_zoop_body_size, colour=Lake, shape=Treatment)) +
  scale_x_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab("Chl a  (log-transformed)") +
  ylab("Zooplankton body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chl a and Zooplankton biomass")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


ggplot(data=dataset_phyto, aes(x=mean_ENS_D, y=mean_s)) +
  scale_x_continuous(trans = "log10", labels = comma) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab("Chl a  (log-transformed)") +
  ylab("Zooplankton body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chl a and Zooplankton biomass")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")



ggplot(data=dataset_final, aes(x=mean_TP, y=mean_TN)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab("Chl a  (log-transformed)") +
  ylab("Zooplankton body size (mean)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chl a and Zooplankton biomass")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

ggplot(data=dataset_final, aes(x=mean_PAR, y=mean_Chla, colour=Lake)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chl a and Zooplankton biomass")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


ggplot(data=dataset_phyto, aes(x=mean_biomass, y=initial_stab)) +
  scale_x_continuous(trans = "log10") +
  geom_point() +
  geom_smooth(method = lm) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chl a and Zooplankton biomass")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")

ggplot(data=dataset_phyto, aes(x=mean_TP, y=mean_TN, colour=Lake)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none") +
  ggtitle("Chl a and Zooplankton biomass")+
  theme(legend.title = element_blank()) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  scale_color_brewer(palette="Dark2")


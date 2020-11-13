# phytobiomass ~ zooplankton initial biomass 

phyto_biovol <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Mean_values/phytoplankton_dataset.csv")


#filtering 
phyto_biovol <- select(phyto_biovol, Lake, Experiment, Treatment, Enclosure, mean_biovolume)


zooplankton_biomass1 <- read_excel("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/Pablos Data/Zooplankton_SITES_AquaNet_2017.xlsx")

# delete columns 
zooplankton_biomass <- zooplankton_biomass1[-c(10:15)] 

# changing names of column "Lake"
zooplankton_biomass$Lake<- as.character(zooplankton_biomass$Lake)

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Asa"] <- "Feresjoen"

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Skogaryd"] <- "Erssjoen"

zooplankton_biomass$Lake[zooplankton_biomass$Lake == "Svartberget"] <- "Stortjaern"

zooplankton_biomass$Lake <- as.factor(zooplankton_biomass$Lake)

levels(zooplankton_biomass$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

# changing names for column "Experiment"
zooplankton_biomass$Experiment <- as.integer(zooplankton_biomass$Experiment)

zooplankton_biomass$Experiment[zooplankton_biomass$Experiment == 1] <- "Spring"

zooplankton_biomass$Experiment[zooplankton_biomass$Experiment == 2] <- "Summer"

zooplankton_biomass$Experiment <- as.character(zooplankton_biomass$Experiment)

# delete all replicate / treatment "lake"
levels(zooplankton_biomass$Replicate)
zooplankton_biomass <- zooplankton_biomass[ ! (zooplankton_biomass$Replicate %in% c("lake", "Lake")), ]

levels(zooplankton_biomass$Treatment)
zooplankton_biomass <- zooplankton_biomass[ ! (zooplankton_biomass$Treatment %in% c("Lake")), ]

zooplankton_biomass$Clean_Biomass<- as.numeric(zooplankton_biomass$Clean_Biomass)

# only initial body mass from Exp_day 1
zooplankton_biomass <- zooplankton_biomass[which(zooplankton_biomass$Exp_day==1), ]

# mean values
zooplankton_biomass_mean <- zooplankton_biomass %>%
  group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_biomass = mean(Clean_Biomass, na.rm=T))


# merge
zoopl_phyto_biomass <- merge(x=zooplankton_biomass_mean, y=phyto_biovol, 
                                    by =c("Lake", "Experiment", "Treatment", "Enclosure"), 
                                    all.x=TRUE)
library(ggplot2)



# plot
plot_zoopl_phyto_biomass <- ggplot(data=zoopl_phyto_biomass, aes(x=mean_biovolume, y=mean_biomass, 
                                            color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(x=mean_biovolume, y=mean_biomass, group = Lake)) +
  scale_x_continuous(trans = "log10", labels = comma) +
  xlab("Phytoplankton biovolume") +
  ylab("Zooplankton initial biomass") +
  guides(color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  ylim(0,150)


# filtering only Treatment = F
zoopl_phyto_biomass_F <- zoopl_phyto_biomass[which(zoopl_phyto_biomass$Treatment=='F'), ]

plot_zoopl_phyto_biomass_F <- ggplot(data=zoopl_phyto_biomass_F, aes(x=mean_biovolume, y=mean_biomass, 
                                                               color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  scale_x_continuous(trans = "log10", labels = comma) +
  xlab("Phytoplankton biovolume") +
  ylab("Zooplankton initial biomass") +
  ggtitle("Pulse")+
  guides(shape=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  ylim(0,150)

# filtering only Treatment = S
zoopl_phyto_biomass_S <- zoopl_phyto_biomass[which(zoopl_phyto_biomass$Treatment=='S'), ]

plot_zoopl_phyto_biomass_S <- ggplot(data=zoopl_phyto_biomass_S, aes(x=mean_biovolume, y=mean_biomass, 
                                                                   color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  scale_x_continuous(trans = "log10", labels = comma) +
  xlab("Phytoplankton biovolume") +
  ylab("Zooplankton initial biomass") +
  ggtitle("Press")+
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  ylim(0,150)


# filtering only Treatment = FS
zoopl_phyto_biomass_FS <- zoopl_phyto_biomass[which(zoopl_phyto_biomass$Treatment=='FS'), ]


plot_zoopl_phyto_biomass_FS <- ggplot(data=zoopl_phyto_biomass_FS, aes(x=mean_biovolume, y=mean_biomass, 
                                                                     color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  scale_x_continuous(trans = "log10", labels = comma) +
  xlab("Phytoplankton biovolume") +
  ylab("Zooplankton initial biomass") +
  ggtitle("Pulse & Press ")+
  guides(shape=FALSE, color=FALSE) +
  theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  ylim(0,150)

# filtering only Treatment = C
zoopl_phyto_biomass_C <- zoopl_phyto_biomass[which(zoopl_phyto_biomass$Treatment=='C'), ]

plot_zoopl_phyto_biomass_C <- ggplot(data=zoopl_phyto_biomass_C, aes(x=mean_biovolume, y=mean_biomass, 
                                                                      color = Lake, shape = Experiment)) +
  labs(color = "Lake") +
  geom_point() +
  geom_smooth(method = lm, aes(group = Lake)) +
  scale_x_continuous(trans = "log10", labels = comma) +
  xlab("Phytoplankton biovolume") +
  ylab("Zooplankton initial biomass") +
  ggtitle("Pulse & Press ")+
  guides(shape=FALSE, color=FALSE)+
theme(legend.position = "top",plot.title = element_text(hjust=0.5, face="bold")) +
  ylim(0,150)



grid.arrange(plot_zoopl_phyto_biomass, plot_zoopl_phyto_biomass_F, plot_zoopl_phyto_biomass_S, 
             plot_zoopl_phyto_biomass_FS, plot_zoopl_phyto_biomass_C, ncol=3)






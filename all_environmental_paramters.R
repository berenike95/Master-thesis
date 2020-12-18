#-------------------------------- Environmental paramters for all lakes -------------------------------------#

# environmental parameters: Chla, TP, TN, DN, DOC, PAR_I, Temperature

environment_ <- read.csv("/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/Pablos_Data/physico_chemistry_fish.csv", sep=";")

environment <- select(environment_, Lake, Experiment, Sampling, Exp_day, Sampling_day, 
       Sampling_month, Enclosure, Treatment, Replicate, Chla, TP, TN, DN, DOC, PAR_I, Temperature)

# set all factors as factors and all numeric variables as numeric

environment$TN <- as.character(environment$TN)

environment$TN <- as.numeric(environment$TN)

environment$PAR_I <- as.numeric(environment$PAR_I)


# changing names for column "Experiment"
environment$Experiment <- as.integer(environment$Experiment)

environment$Experiment[environment$Experiment == 1] <- "Spring"

environment$Experiment[environment$Experiment == 2] <- "Summer"

environment$Experiment <- as.character(environment$Experiment)

# changing names of column "Lake"
levels(environment$Lake)
# "Asa"         "Bolmen"      "Erken"       "Skogaryd"    "Svartberget"

environment$Lake<- as.character(environment$Lake)

environment$Lake[environment$Lake == "Asa"] <- "Feresjoen"

environment$Lake[environment$Lake == "Skogaryd"] <- "Erssjoen"

environment$Lake[environment$Lake == "Svartberget"] <- "Stortjaern"

environment$Lake <- as.factor(environment$Lake)

levels(environment$Lake)
# "Bolmen"     "Erken"      "Erssjoen"   "Feresjoen"  "Stortjaern"

# delete all replicate / treatment "lake"
levels(environment$Replicate)
environment<- environment[ ! (environment$Replicate %in% c("lake", "Lake")), ]

levels(environment$Treatment)
environment<- environment[ ! (environment$Treatment %in% c("Lake")), ]

# new column with TN/TP ratio
environment <- mutate(environment, "TN_TP_ratio" = TN / TP)

# new column with DN/TP ratio
environment <- mutate(environment, "DN_TP_ratio" = DN / TP)

# set Temperature from 165 to 16.5
environment$Temperature[environment$Temperature == 165.00] <- 16.5


# creating mean values for all environmental parameters
environment_mean <- environment %>%
  group_by(Lake, Experiment, Treatment, Enclosure) %>%
  summarize(mean_TP = mean(TP, na.rm=T), mean_TN= mean(TN, na.rm=T), mean_DOC= mean(DOC, na.rm=T), 
            mean_PAR= mean(PAR_I, na.rm=T), mean_Temp = mean(Temperature, na.rm=T), 
            mean_Chla = mean(Chla, nar.rm=T), mean_TN_TP_ratio = mean(TN_TP_ratio, na.rm=T), 
            mean_DN_TP_ratio=mean(DN_TP_ratio, na.rm=T))

write_csv(environment_mean, "/Users/berenikebick/Documents/Uni_Master/SoSe20/Masterarbeit/Experiment_Data/transformed_data/mean_values_environmental_parameters.csv")






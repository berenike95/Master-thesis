#-------------------------------- Environmental paramters for all lakes -------------------------------------#

# environmental parameters: TP, Chl a, DOC, Temperature, Chl a (Fluorometer), PAR I, Par II, Par III, TN 

data_tp <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/environmental_parameters.csv", header=T,sep=";")

library("readxl")

data_tp <- read_excel("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/environmental_parameters.xlsx")


# set all factors as factors and all numeric variables as numeric

data_tp$Experiment <- as.factor(data_tp$Experiment)

data_tp$Lake <- as.factor(data_tp$Lake)

data_tp$TP <- as.numeric(data_tp$TP)

data_tp$Chla <- as.numeric(data_tp$Chla)

data_tp$DOC <- as.numeric(data_tp$DOC)

data_tp$Temperature <- as.numeric(data_tp$Temperature)

data_tp$PAR_I <- as.numeric(data_tp$PAR_I)

data_tp$TN <- as.numeric(data_tp$TN)

# load package

library(psych)

head(data_tp)

# correlation for TP, Chl a, DOC, Temperature, Chl a (Fluorometer), PAR I, PAR II, PAR II, TN

pairs.panels(data_tp[,-c(1:2)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

?pairs.panels

# correlation plots for TP, Chl a, DOC, Temperature

pairs.panels(data_tp[,c(3:6)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

# correlation plots for Chl a (Fluorometer), PAR I, PAR II, PAR II

pairs.panels(data_tp[,c(7:10)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

# correlation for TP, Chl a, DOC, Temperature, Chl a (Fluorometer), PAR I, PAR II, PAR II, TN

library("graphics")


pairs(data_tp[,-c(1:2)], pch = 19,  cex = 0.5,
      col = data_tp$Lake, 
      lower.panel=NULL, oma = c(2, 2, 2, 2), cex.labels = 2)
par(xpd = NA)
legend("bottomleft",fill = unique(data_tp$Lake),legend= c(levels(data_tp$Lake)), cex = 1)


# bottom, left, top, right

# , oma=c(5,5,5,15)
# par(xpd = TRUE)
# legend("top", fill = unique(data_tp$Lake), legend = c(levels(data_tp$Lake)))





?pairs

# correlation plots for TP, Chl a, DOC, Temperature

pairs(data_tp[,c(3:6)], pch = 19,  cex = 0.5,
      col = data_tp$Lake,
      lower.panel=NULL)

# correlation plots for Chl a (Fluorometer), PAR I, PAR II, PAR II

pairs(data_tp[,c(7:10)], pch = 19,  cex = 0.5,
      col = data_tp$Lake,
      lower.panel=NULL)


# 

install.packages("GGally")
library("ggplot2")
library("GGally")                      

ggpairs(data)                          # Apply ggpairs function

#

# subset data at ggplot geom_point

data_tp <- read.csv("/Users/berenikebick/Documents/Uni Master/SoSe20/Masterarbeit/Experiment_Data/data_tp_all.csv", header=T,sep=";")

  
ggplot(data = data_tp, mapping = aes(x = Treatment, y = TP, colour = Lake, symbol = Experiment)) +
  geom_point(data = data_tp[data_tp$Treatment == c("C", "F", "FS"),]) 


?subset

### RScript: Unpacking terrorism threats manuscript ###
##  Rotem Dvir  ##
##  June 2020  ##


setwd("~/Dropbox/TAMU/Extra_Projects/Git_edits/Terror_types/Data")

#Packages
library(psych)  
library(foreign)
library(car)    
library(ggplot2)
library(gplots)
library(MASS)
library(Hmisc)
library(ggthemes)
library(plyr)
library(devtools)
library(dotwhisker)
library(broom)
library(dplyr)
library(coefplot)
library(reshape2)
set.seed(2020)

##  Load data file  ##
Data_type <- read.csv("~/Dropbox/TAMU/Extra_Projects/Git_edits/Terror_types/Data/TerrorTypes_EditNov19.csv", 
                      header=TRUE, sep = ",", strip.white = T, na.strings = "")
View(Data_type)

### In order to perform the analysis of types, we reshape the data into a long format 

## Reshape Data to long: 2 Terror Types  ##
data_long <- reshape(Data_type, varying = c(2:15), direction = "long", idvar = "CaseID", sep = "_", timevar = "order")
data_long <- 
  data_long[order(data_long$CaseID),] #order data by CaseID
View(data_long)

names(data_long) <- c("CaseID", "Partisanship", "Ideology", "Religion", "Age", "Age_group",
                      "Education", "Race", "Gender", "Income", "Region_4", "Region_9", "Internet", "Informed_Terror", 
                      "Econ_Roots", "Mus_Surv", "Force", "ScreenUS", "ScreenForg", "No_Immig", "No_Refugee", "Track_Phones",
                      "Issue_econ", "Issue_immig", "Issue_debt", "Issue_energy", "Issue_health", "Issue_climate", "Issue_terror", "Issue_envi",
                      "Issue_ineq", "Issue_women", "Inform_papers", "Inform_Fox", "Inform_CNN", "Inform_MSNBC",
                      "Inform_network", "Inform_online", "Inform_radio", "Inform_social",
                      "Fear_Terror", "Fear_Self", "Fear_Fam", "Fear_friend",
                      "Terror_Type", "Concern", "Severity", "PublicKnow", "ExpertKnow", "Casualties", "Likelihood", 
                      "Risk")  #rename vars in new data

##  Add binary variable for each terror type  
data_long$type_binary <- ifelse(data_long$Terror_Type == "bomb", 0, 1)

# Rescale terror atatributes  (0-1 scale): since attributes are measued on different scales
data_long$Likelihood2 <- ((data_long$Likelihood - 1)/3)
data_long$Casualties2 <- ((data_long$Casualties - 1)/4)
data_long$Concern2 <- ((data_long$Concern - 1)/3)

########################################################################
##################  Threat attributes by terror types  #################
########################################################################

## Create dataset with means, sd, se for all three attributes 

data_rawmeans <- data_long %>% group_by(Terror_Type)

dataplot <- data_rawmeans %>% 
  summarise(
    likely = mean(Likelihood2, na.rm = T),
    sd1 = sd(Likelihood2, na.rm = T),
    se1 = sd1 / sqrt(1730),
    se1a = se1 * 2,
    costnew = mean(Casualties2, na.rm = T),
    sd2 = sd(Casualties2, na.rm = T),
    se2 = sd2 / sqrt(1730),
    se2a = se2 * 2,
    conc2 = mean(Concern2, na.rm = T),
    sd3 = sd(Concern2, na.rm = T),
    se3 = sd3 / sqrt(1730),
    se3a = se3 * 2)


# Create Likelihood plot
plot1 <- ggplot(dataplot, aes(x=Terror_Type, y=likely)) +
  geom_errorbar(aes(ymin=likely-se1a, ymax=likely+se1a), colour="blue", width = .15, size = .95) +
  geom_line() +
  geom_point(colour = "blue") +
  theme_bw() +
  ylim(0.35, 0.8) +
  ggtitle("Threat Attribute: Likelihood") +
  xlab("") + ylab("Mean") +
  scale_x_discrete(name = "Terror Type", labels = c("bomb" = "Conventional","cyber" = "Cyber"))
plot1 

# Casualties plot
plot2 <- ggplot(dataplot, aes(x=Terror_Type, y=costnew)) +
  geom_errorbar(aes(ymin=costnew-se2a, ymax=costnew+se2a), colour="red", width = .15, size = .95) +
  geom_line() +
  geom_point(colour = "red") +
  theme_bw() +
  ylim(0.35, 0.8) +
  ggtitle("Threat Attribute: Casualties") +
  xlab("") + ylab("") +
  scale_x_discrete(name = "Terror Type", labels = c("bomb" = "Conventional","cyber" = "Cyber"))
plot2 

# Concern plot
plot3 <- ggplot(dataplot, aes(x=Terror_Type, y=conc2)) +
  geom_errorbar(aes(ymin=conc2-se3a, ymax=conc2+se3a), colour="black", width = .15, size = .95) +
  geom_line() +
  geom_point(colour = "black") +
  theme_bw() +
  ylim(0.35, 0.8) +
  ggtitle("Threat Attribute: Concern") +
  xlab("") + ylab("") +
  scale_x_discrete(name = "Terror Type", labels = c("bomb" = "Conventional","cyber" = "Cyber"))
plot3 

# Generate figure: Terror methods and attributes
library(ggpubr)

ggarrange(plot1, plot3, plot2, 
          ncol = 3, nrow = 1)










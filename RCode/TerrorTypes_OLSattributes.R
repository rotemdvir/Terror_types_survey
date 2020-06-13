### RScript: Unpacking terrorism threats manuscript ###
##  Rotem Dvir  ##
##  June 2020  ##


setwd("~/Dropbox/TAMU/Extra_Projects/Git_edits/Terror_types/Data")

#Packages
library(psych)  
library(foreign)
library(car)    
library(MASS)
library(Hmisc)
library(plyr)
library(devtools)
library(broom)
library(dplyr)
library(coefplot)
library(stargazer)
library(ggpubr)

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


######################################################
### Compare how terror types affect threat perceptions
######################################################

##  I use OLS regression models with clustered SEs for each of the attributes as DV.
##  In each model, conventional type (type_binary) is the reference category. 
##  Then, I generate a coefficient plot for each model

# Download package and import the function for clustering SEs.
library(RCurl)
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)

## Add knowledge Index (based on media items)
data_long$media_index <- data_long$Inform_papers + data_long$Inform_Fox + data_long$Inform_CNN + data_long$Inform_MSNBC +
  data_long$Inform_network + data_long$Inform_online + data_long$Inform_radio + data_long$Inform_social

# Model 1: Likelihood  
summary(m.like <- lm(Likelihood2 ~ type_binary + media_index + Partisanship +
                       Age + Education + Gender + Income, data = data_long))
summary(m.like, cluster = c("CaseID"))

coef1 <- coefplot(m.like, intercept=FALSE, title = "Likelihood", decreasing = T,
                  color="blue", ylab = "", xlab = "Coefficient Value", pointSize = 2, zeroLWD = 1,  
                  outerCI = 1.984, innerCI = 1.984, zeroType = 3, zeroColor = "black", sort="natural",
                  newNames=c(type_binary = "Method:Cyber", media_index = "Media Index")) 
coef1 <- coef1 + theme_pubr()
coef1

# Model 2: Casualties (costs)  
summary(m.cas <- lm(Casualties2 ~ type_binary + media_index + Partisanship +
                      Age + Education + Gender + Income, data = data_long))
summary(m.cas, cluster = c("CaseID"))

coef2 <- coefplot(m.cas, intercept=FALSE, title = "Casualties (Costs)", decreasing = T,
                  color="red", ylab = "", xlab = "Coefficient Value", pointSize = 2, zeroLWD = 1,
                  outerCI = 1.984, innerCI = 1.984, zeroType = 3, zeroColor = "black", sort="natural",
                  newNames=c(type_binary="Method:Cyber", media_index = "Media Index"))#
coef2 <- coef2 + theme_pubr()
coef2

# Combine plots 
ggarrange(coef1, coef2, 
          ncol = 2, nrow = 1)

# Regression table of all attributes (Latex code)
stargazer(m.like, m.cas, title="", align=T, no.space=T) 

### Predict: I compute the predicted values of each attribute 
## For each attribute, I compute the predicted value by threat type. 
## Then, I compute the difference between conventional and cyber threats and the percentage change in the attribute value 

# Compute means for data covariates
party <- mean(data_long$Partisanship)
age <- mean(data_long$Age)
ed <- mean(data_long$Education)
gen <- mean(data_long$Gender)
inc <- mean(data_long$Income)
media <- mean(data_long$media_index, na.rm = T)

# Likelihood: predicted values by terror type
conv.pred <- predict(m.like, data.frame(type_binary = 0,
                           Partisanship = party, Age = age, Education = ed, Gender = gen,
                           Income = inc, media_index = media))  ## Kinetic type

cyb.pred <- predict(m.like, data.frame(type_binary = 1,
                           Partisanship = party, Age = age, Education = ed, Gender = gen,
                           Income = inc, media_index = media))  ## Cyber type



# Casualties: predicted values by terror type
conv.pred2 <- predict(m.cas, data.frame(type_binary = 0,
                          Partisanship = party, Age = age, Education = ed, Gender = gen,
                          Income = inc, media_index = media))   ## Kinetic type

cyb.pred2 <- predict(m.cas, data.frame(type_binary = 1,
                          Partisanship = party, Age = age, Education = ed, Gender = gen,
                          Income = inc, media_index = media))   ## Cyber type

# Dataset with the computed change in prediction when threat change from convnetional to cyber

# A positive value indicates an increase in the level of the attribute
# A negative value indicates an increase in the level of the attribute

predict.deltas <- tibble(
  Method = c("Conv -> Cyber"),
  delta.likelihood = (conv.pred - cyb.pred) / conv.pred,
  delta.cyber = (conv.pred2 - cyb.pred2) / conv.pred2)
print(predict.deltas)



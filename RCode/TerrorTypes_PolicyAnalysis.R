### RScript: Unpacking terrorism threats manuscript ###
##  Rotem Dvir  ##
##  June 2020  ##

setwd("~/Dropbox/TAMU/Extra_Projects/Git_edits/Terror_types")

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
library(stargazer)
library(ggpubr)
set.seed(2020)

# Load data
Data_type2 <- read.csv("~/Dropbox/TAMU/Extra_Projects/Git_edits/Terror_types/Data/TerrorTypes_EditNov19.csv", 
                       header=TRUE, sep = ",", strip.white = T, na.strings = "")
View(Data_type2)

# Rescale policy items and attributes (0-1 scale)
Data_type2$newforce <- ((Data_type2$Pol_force - 1)/3)
Data_type2$newscreen <- ((Data_type2$Pol_screenUS - 1)/4)
Data_type2$surv <- ((Data_type2$Pol_survMusl - 1)/3)
Data_type2$phone <- ((Data_type2$Pol_trackPh - 1)/4)

Data_type2$conc_bomb <- ((Data_type2$concern_bomb - 1)/3)
Data_type2$conc_cyber <- ((Data_type2$concern_cyber - 1)/3)
Data_type2$like_bomb <- ((Data_type2$likely_bomb - 1)/3)
Data_type2$like_cyber <- ((Data_type2$likely_cyber - 1)/3)
Data_type2$ca_bomb <- ((Data_type2$cas_bomb - 1)/4)
Data_type2$ca_cyber <- ((Data_type2$cas_cyber - 1)/4)

## Add knowledge Index (based on media items)
Data_type2$media_index <- Data_type2$inform_papers + Data_type2$inform_Fox + Data_type2$inform_CNN + Data_type2$inform_MSNBC +
  Data_type2$inform_network + Data_type2$inform_online + Data_type2$inform_radio + Data_type2$inform_social

#change variables names
names(Data_type2)[names(Data_type2)=="KnowGen"] <- "Knowledge"
names(Data_type2)[names(Data_type2)=="DOV_XPARTY7"] <- "Party"
names(Data_type2)[names(Data_type2)=="PPAGE"] <- "Age"
names(Data_type2)[names(Data_type2)=="PPGENDER"] <- "Gender"

# I test the attributes of likelihood and costs as drivers of conern
# I run two OLS regression models, results in PDF file in this folder (code in Markdown file)

summary(m.concern1 <- lm(conc_bomb ~ like_bomb + ca_bomb + media_index + Party + Age + PPEDUCAT +
                    Gender + PPINCIMP, data = Data_type2))

summary(m.concern2 <- lm(conc_cyber ~ like_cyber + ca_cyber  + media_index + Party + Age + PPEDUCAT +
                    Gender + PPINCIMP, data = Data_type2))

# Regression table of both (Latex code)
stargazer(m.concern1, m.concern2, title="", align=T, no.space=T) 

## I compute the means and SEs to generate figure that compares support for both policy options

# 2 Policy options: compute means and SEs across types
mean(Data_type2$newforce, na.rm = T)
mean(Data_type2$newscreen, na.rm = T)
force_se<- sd(Data_type2$newforce, na.rm = T) / sqrt(length(Data_type2$newforce[!is.na(Data_type2$newforce)]))
screen_se<- sd(Data_type2$newscreen, na.rm = T) / sqrt(length(Data_type2$newscreen[!is.na(Data_type2$newscreen)]))

# Generate figure 

pol.data <- data.frame(
  pol.type = factor(c("Military Force", "Airport Screening")),
  pol.means = c(0.3859649, 0.6467262),
  pol.ses = c(0.007755211, 0.00716887),
  pol.SEs = c(0.007755211 * 2, 0.00716887 *2))

pol.plot <- ggplot(pol.data, aes(x=pol.type, y=pol.means)) +
  geom_col(fill = "grey", width = 0.65) +
  geom_errorbar(aes(ymin=pol.means-pol.SEs, ymax=pol.means+pol.SEs), colour="black", width = .2) +
#  geom_line() +
  geom_point() +
  ylim(0, 0.8) +
  ggtitle("Counter-terrorism Policies") +
  xlab("") + ylab("Mean Support")

pol.plot <- pol.plot + theme_pubr() + coord_flip()
pol.plot

### Compare the effects of terror type on public policy preferences

##  I run two OLS regression models with each policy option as DV. 
##  In each model, the attributes by type are used as IVs.
##  Then, I generate a coefficient plot that combines both models for comparison

# Military & Screening: OLS models and combine results using tidy datasets 
summary(m.force1 <- lm(newforce ~ like_bomb + ca_bomb + like_cyber + ca_cyber + media_index + Party + Age + PPEDUCAT +
                         Gender + PPINCIMP, data = Data_type2))

m.force1a <- tidy(m.force1) %>%
  mutate(model = "Military Force")

summary(m.screen1 <- lm(newscreen ~ like_bomb + ca_bomb + like_cyber + ca_cyber + media_index + Party + Age + PPEDUCAT +
                          Gender + PPINCIMP, data = Data_type2))

m.screen1a <- tidy(m.screen1) %>%
  mutate(model = "Airport Security")

m.polcomb <- bind_rows(m.force1a, m.screen1a)
m.polcomb <- rbind(m.force1a, m.screen1a)
m.polcomb <-  m.polcomb %>% group_by(model)
m.polcomb <- m.polcomb %>%
  relabel_predictors(c('like_bomb' = "Conv_Likelihood",
                       'ca_bomb' = "Conv_Costs",
                       'like_cyber' = "Cyber_Likelihood",
                       'ca_cyber' = "Cyber_Costs",
                       'media_index' = "Media",
                       'Party' = "Partisanship",
                       'Age' = "Age",
                       'PPEDUCAT' = "Education",
                       'Gender' = "Gender",
                       'PPINCIMP' = "Income"))


## Generate dot-whisker plot to compare results

# Grouping predictors
brackets <- list(c("Attributes", "Conv_Likelihood", "Cyber_Costs"),
                 c("Covariates", "Media", "Income"))

comp.plot <- {dwplot(m.polcomb) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = 2) +
  theme_bw() + 
  xlab("Coefficient Estimate") + ylab("") + 
  ggtitle("CT Policies & Threat Attributes") +
  theme(legend.position = 'right',
        legend.justification = 'right',
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5) +
  scale_colour_grey(start = .3, end = .7,
                    name = "Policy Option",
                    breaks = c("Airport Security", "Military Force"),
                    labels = c("Airport Security", "Military Force"))} %>%
  add_brackets(brackets)




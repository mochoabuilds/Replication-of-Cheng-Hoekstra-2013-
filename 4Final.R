#########################################################################

# Does Strengthening Self-Defense Law Deter Crime or Escalate Violence? 

# A Replication of Cheng & Hoekstra (2013)     
 
# by Michael Valentino Ochoa           

#########################################################################

# load libraries
library(haven)
library(fixest)
library(tidyverse)
library(modelsummary)
library(ggthemes)
library(dplyr)
library(car)

# clear all objects
rm(list=ls())

# load data (macbook)
castle <- readRDS('/Users/mvo/Desktop/castle.RDS')

# peek at data
head(castle)

######################

# Homicide in Florida 

######################

# data prep Florida
florida <- castle  %>% filter(state == "Florida" | treated == 0)
florida <- florida %>% mutate(treat = ifelse(state == "Florida", "Florida", "Control Group"))
plot1   <- florida %>% group_by(treat, year) %>% summarize(Homicide = mean(l_homicide))

# plot Florida 
ggplot() +
  geom_line(data = plot1, aes(x = year, y = Homicide, col = treat), linewidth = 2.5) +
  scale_color_fivethirtyeight("Treatment Status") +
  theme_fivethirtyeight() +
  theme_minimal() +
  labs(x = "Year", y = "Homicide (in Log)") +
  geom_vline(xintercept = 2005, linetype = "dashed") +
  scale_x_continuous(breaks = c(2000, 2003, 2005, 2008, 2010)) +
  theme(axis.text  = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"))

# effect of CDL on homicide rates in Florida
florida_plot = feols(l_homicide ~ i(year, treated, ref = 2005) | 
                       state + year, cluster = ~state, data = florida)

# plot Florida measure
iplot(florida_plot, main = "Effect of CDL on Homicide Rates in Florida")


######################

# Homicide in Georgia 

######################

# data prep Georgia
georgia   <- castle  %>% filter(state == "Georgia" | treated == 0)
georgia   <- georgia %>% mutate(treat = ifelse(state == "Georgia", "Georgia", "Control Group"))
plot2     <- georgia %>% group_by(treat, year) %>% summarize(Homicide = mean(l_homicide))

# plot Georgia 
ggplot() +
  geom_line(data = plot2, aes(x = year, y = Homicide, col = treat), linewidth = 2.5) +
  scale_color_fivethirtyeight("Treatment Status") +
  theme_fivethirtyeight() +
  theme_minimal() +
  labs(x = "Year", y = "Homicide (in Log)") +
  geom_vline(xintercept = 2005, linetype = "dashed") +
  scale_x_continuous(breaks = c(2000, 2003, 2005, 2008, 2010)) +
  theme(axirs.text  = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"))

# effect of CDL on homicide rates in Georgia
georgia_plot = feols(l_homicide ~ i(year, treated, ref = 2005) | 
                       state + year, cluster= ~state, data = georgia)

# plot Georgia measure
iplot(georgia_plot, main = "Effect of CDL on Homicide Rates in Georiga")


#################################################################

# Multistate Homicide Regression WITHOUT Weighting by Population 

#################################################################

# TWFE without weighting
reg_homic1 <- feols(l_homicide ~ cdl | state + year, cluster = ~state, data = castle)
  
# TWFE adding Region-by-Year FE
reg_homic2 <- feols(l_homicide ~ cdl | state + year + region^year, cluster = ~state, data = castle)

# TWFE with Region-by-Year FE, additional controls 
reg_homic3 <- feols(l_homicide ~ cdl + blackm_15_24 + whitem_15_24 + blackm_25_44 +
                    whitem_25_44 + l_exp_subsidy + l_exp_pubwelfare + l_police +
                    unemployrt+poverty + l_income + l_prisoner + l_lagprisoner + state:year
                    | state + year + region^year, cluster = ~state, data = castle)

# generate table 
etable(reg_homic1, reg_homic2, reg_homic3, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
       keep = c("cdl","blackm_15_24","whitem_15_24","blackm_25_44",
              "whitem_25_44","l_exp_subsidy","l_exp_pubwelfare","l_police",
              "unemployrt","poverty","l_income","l_prisoner","l_lagprisoner"))


##############################################################

# Multistate Homicide Regression WITH Weighting by Population 

##############################################################

# TWFE with weighting
reg_homic1 <- feols(l_homicide ~ cdl | state + year, cluster = ~state, weights = castle$popwt, data = castle)

# TWFE adding Region-by-Year FE
reg_homic2 <- feols(l_homicide ~ cdl | state + year + region^year, weights = castle$popwt, cluster = ~state, data = castle)

# TWFE with Region-by-Year FE, additional controls
reg_homic3 <- feols(l_homicide ~ cdl + blackm_15_24 + whitem_15_24 + blackm_25_44 +
                    whitem_25_44 + l_exp_subsidy + l_exp_pubwelfare + l_police +
                    unemployrt + poverty + l_income + l_prisoner + l_lagprisoner + state:year
                   | state + year + region^year, weights = castle$popwt, cluster = ~state, data = castle)

# generate table 
etable(reg_homic1, reg_homic2, reg_homic3, signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
       keep=c("cdl","blackm_15_24","whitem_15_24","blackm_25_44",
              "whitem_25_44","l_exp_subsidy","l_exp_pubwelfare","l_police",
              "unemployrt","poverty","l_income","l_prisoner","l_lagprisoner"))

# generate professional looking table
dat <- castle

models1 <- list(
  "reg_homic1" = feols(l_homicide ~ cdl | state + year, cluster = ~state, weights = castle$popwt, data = castle),
  "reg_homic2" = feols(l_homicide ~ cdl + blackm_15_24 + whitem_15_24 + blackm_25_44 +
                         whitem_25_44 + l_exp_subsidy + l_exp_pubwelfare + l_police +
                         unemployrt + poverty + l_income + l_prisoner + l_lagprisoner + state:year
                       | state + year + region^year, weights = castle$popwt, cluster = ~state, data = castle),
  "reg_homic3" = feols(l_homicide ~ cdl + blackm_15_24 + whitem_15_24 + blackm_25_44 +
                                       whitem_25_44 + l_exp_subsidy + l_exp_pubwelfare + l_police +
                                       unemployrt + poverty + l_income + l_prisoner + l_lagprisoner + state:year
                                     | state + year + region^year, weights = castle$popwt, cluster = ~state, data = castle))

modelsummary(models1)


###########################################

# Falsification Tests: Motor Vehicle Theft 

###########################################

# TWFE 
reg_motor1 <- feols(l_motor ~ cdl | state + year, cluster = ~state, data = castle)

# TWFE with Region-by-Year FE
reg_motor2 <- feols(l_motor ~ cdl | state + year + region^year, cluster = ~state, data = castle)

# TWFE with Region-by-Year FE, additional controls and state-specific linear time trends 
reg_motor3 <- feols(l_motor ~ cdl + blackm_15_24 + whitem_15_24 + blackm_25_44 +
                    whitem_25_44 + l_exp_subsidy + l_exp_pubwelfare + l_police +
                    unemployrt + poverty + l_income + l_prisoner + l_lagprisoner + state:year
                    | state + year + region^year, cluster = ~state, data = castle)

# generate table for motor vehicle theft
etable(reg_motor1, reg_motor2, reg_motor3, signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),  
       keep=c("cdl","blackm_15_24","whitem_15_24","blackm_25_44",
              "whitem_25_44","l_exp_subsidy","l_exp_pubwelfare","l_police",
              "unemployrt","poverty","l_income","l_prisoner","l_lagprisoner"))

# generate professional looking table
dat <- castle

models2 <- list(
  "reg_motor1" = feols(l_motor ~ cdl | state + year, cluster = ~state, data = castle),
  "reg_motor2" = feols(l_motor ~ cdl | state + year + region^year, cluster = ~state, data = castle),
  "reg_motor3" = feols(l_motor ~ cdl + blackm_15_24 + whitem_15_24 + blackm_25_44 +
                          whitem_25_44 + l_exp_subsidy + l_exp_pubwelfare + l_police +
                          unemployrt + poverty + l_income + l_prisoner + l_lagprisoner + state:year
                        | state + year + region^year, cluster = ~state, data = castle))

modelsummary(models2)


###############################

# Falsification Tests: Larceny 

###############################

# TWFE
reg_lar1 <- feols(l_larceny ~ cdl | state + year, cluster = ~state, data = castle)

# TWFE with Region-by-Year FE
reg_lar2 <- feols(l_larceny ~ cdl | state + year + region^year, cluster = ~state, data = castle)

# TWFE with Region-by-Year FE, additional controls and state-specific linear time trends
reg_lar3 <- feols(l_larceny ~ cdl + blackm_15_24 + whitem_15_24 + blackm_25_44 +
                  whitem_25_44 + l_exp_subsidy + l_exp_pubwelfare + l_police +
                  unemployrt + poverty + l_income + l_prisoner + l_lagprisoner + state:year
                  | state + year + region^year, cluster = ~state, data = castle)

# generate table for larceny
etable(reg_lar1, reg_lar2, reg_lar3, signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10),   
       keep = c("cdl","blackm_15_24","whitem_15_24","blackm_25_44",
       "whitem_25_44","l_exp_subsidy","l_exp_pubwelfare","l_police",
       "unemployrt","poverty","l_income","l_prisoner","l_lagprisoner"))

# generate professional looking table
dat <- castle

models3 <- list(
  "reg_lar1" = feols(l_larceny ~ cdl | state + year, cluster = ~state, data = castle),
  "reg_lar2" = feols(l_larceny ~ cdl | state + year + region^year, cluster = ~state, data = castle),
  "reg_lar3" = feols(l_larceny ~ cdl + blackm_15_24 + whitem_15_24 + blackm_25_44 +
                       whitem_25_44 + l_exp_subsidy + l_exp_pubwelfare + l_police +
                       unemployrt + poverty + l_income + l_prisoner + l_lagprisoner + state:year
                     | state + year + region^year, cluster = ~state, data = castle))

modelsummary(models3)


##############################

# Deterrence Effects: Robbery 

##############################

# TWFE 
reg_rob1 <- feols(l_robbery ~ cdl | state + year, cluster = ~state, data = castle)

# TWFE with Region-by-Year FE
reg_rob2 <- feols(l_robbery ~ cdl | state + year + region^year, cluster = ~state, data = castle)

# TWFE with Region-by-Year FE, additional controls and State-Specific Linear Time trends
reg_rob3 <- feols(l_robbery ~ cdl + blackm_15_24 + whitem_15_24 + blackm_25_44 +
                  whitem_25_44 + l_exp_subsidy + l_exp_pubwelfare + l_police +
                  unemployrt + poverty + l_income + l_prisoner + l_lagprisoner + state:year
                  | state + year + region^year, cluster = ~state, data = castle)

etable(reg_rob1, reg_rob2, reg_rob3, signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10),   
       keep = c("cdl","blackm_15_24","whitem_15_24","blackm_25_44",
       "whitem_25_44","l_exp_subsidy","l_exp_pubwelfare","l_police",
       "unemployrt","poverty","l_income","l_prisoner","l_lagprisoner"))

# generate professional looking table
dat <- castle

models4 <- list(
  "reg_rob1" = feols(l_robbery ~ cdl | state + year, cluster = ~state, data = castle),
  "reg_rob2" = feols(l_robbery ~ cdl | state + year + region^year, cluster = ~state, data = castle),
  "reg_rob3" = feols(l_robbery ~ cdl + blackm_15_24 + whitem_15_24 + blackm_25_44 +
                       whitem_25_44 + l_exp_subsidy + l_exp_pubwelfare + l_police +
                       unemployrt + poverty + l_income + l_prisoner + l_lagprisoner + state:year
                     | state + year + region^year, cluster = ~state, data = castle))

modelsummary(models4)


##########################################

#  Deterrence Effects: Aggravated Assault  

##########################################

# TWFE
reg_aslt1 <- feols(l_assault ~ cdl | state + year, cluster = ~state, data = castle)

# TWFE with Region-by-Year FE
reg_aslt2 <- feols(l_assault ~ cdl | state + year + region^year, cluster = ~state, data = castle)

# TWFE with Region-by-Year FE, additional controls 
reg_aslt3 <- feols(l_assault ~ cdl + blackm_15_24 + whitem_15_24 + blackm_25_44 +
                  whitem_25_44 + l_exp_subsidy + l_exp_pubwelfare + l_police +
                  unemployrt + poverty + l_income + l_prisoner + l_lagprisoner + state:year
                  | state + year + region^year, cluster = ~state, data = castle)

etable(reg_aslt1, reg_aslt2, reg_aslt3, signifCode = c("***" = 0.01, "**" = 0.05, "*" = 0.10),   
       keep = c("cdl","blackm_15_24","whitem_15_24","blackm_25_44",
       "whitem_25_44","l_exp_subsidy","l_exp_pubwelfare","l_police",
       "unemployrt","poverty","l_income","l_prisoner","l_lagprisoner"))

# generate professional looking table
dat <- castle

models5 <- list(
  "reg_aslt1" = feols(l_assault ~ cdl | state + year, cluster = ~state, data = castle),
  "reg_aslt1" = feols(l_assault ~ cdl | state + year + region^year, cluster = ~state, data = castle),
  "reg_aslt1" = feols(l_assault ~ cdl + blackm_15_24 + whitem_15_24 + blackm_25_44 +
                        whitem_25_44 + l_exp_subsidy + l_exp_pubwelfare + l_police +
                        unemployrt + poverty + l_income + l_prisoner + l_lagprisoner + state:year
                      | state + year + region^year, cluster = ~state, data = castle))

modelsummary(models5)


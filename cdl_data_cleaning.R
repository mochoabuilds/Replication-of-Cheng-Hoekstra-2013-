############################################################################

# Does Strengthening Self-Defense Law Deter Crime or Escalate Violence? 

# A Replication of Cheng & Hoekstra (2013)

# by Michael Valentino Ochoa 

# data: scripts for going from raw data to cleaned dataset used in analysis 
      
############################################################################

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


######################

# Homicide in Georgia 

######################

# data prep Georgia
georgia   <- castle  %>% filter(state == "Georgia" | treated == 0)
georgia   <- georgia %>% mutate(treat = ifelse(state == "Georgia", "Georgia", "Control Group"))
plot2     <- georgia %>% group_by(treat, year) %>% summarize(Homicide = mean(l_homicide))




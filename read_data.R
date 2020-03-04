setwd("~/Stuff/IPSDS/Project Consulting/Project/Downloads")
#setwd("C:\\Users\\jose\\Desktop\\Pessoal\\Docs Pessoais\\IPSDS\\Disciplinas\\Project Consulting\\Digital Traces\\IPSDS\\")

library(tidyverse)
#library (feather)
#library(e1071)

#df.survey <- feather::read_feather("df.survey.feather")
#df.socdem <- feather::read_feather("df.socdem.feather")
#df.url1 <- feather::read_feather("df.url1.feather")
#df.visits <- feather::read_feather("df.visits.feather")

df.url1 <- readRDS("./URL1.rds")
df.visits <- readRDS("./visits.rds")
df.survey <- readRDS("./dat_surv.rds")
df.socdem <- readRDS("./sociodemo.rds")

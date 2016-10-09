setwd("/Users/whitesi/Documents/Programming/Python/DataAnalystND/UD651")


##IMPORT PACKAGES
library(data.table)
library(dplyr)
library(dtplyr)
library(tidyr)
library(ggplot2)
source("dlin.R")


##READ IN DATA
player_data=read.csv("player_data.csv")
event_data=read.csv("event_data.csv")

player_data=as.data.table(player_data) 
player_data[ ,X := NULL]
setnames(player_data,old=c('X3FGA','X3FGM'),new=c('3FGA','3FGM'))

#CALCULATE FANDUEL POINTS
player_data[,fd:=3*`3FGM`+2*(FGM-`3FGM`)+1*FTM+1.2*rebounds+1.5*assists+2*blocks+2*steals-1*turnovers]


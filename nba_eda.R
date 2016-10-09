setwd("/Users/whitesi/Documents/Programming/Python/DataAnalystND/UD651")

##IMPORT PACKAGES
library(data.table)
library(dplyr)
library(dtplyr)
library(tidyr)
library(ggplot2)
source("dlin.R")
library(RColorBrewer)
library(reshape2)


##display.brewer.all() ##view all palettes with this
palette <- brewer.pal("YlGnBu", n=9)

#######################################
####### DATA IMPORT & CLEANING ########
#######################################

##READ IN DATA
player_data=read.csv("player_data.csv")
event_data=read.csv("event_data.csv")

event_data=as.data.table(event_data)
player_data=as.data.table(player_data) 
player_data[ ,X := NULL]
setnames(player_data,old=c('X3FGA','X3FGM'),new=c('3FGA','3FGM'))

#get rid of dupe rows in event_data
dim(event_data)
setkey(event_data,gameID)
event_data=unique(event_data)
dim(event_data)

##CALCULATE FANDUEL POINTS
player_data[,fd:=3*`3FGM`+2*(FGM-`3FGM`)+1*FTM+1.2*rebounds+1.5*assists+2*blocks+2*steals-1*turnovers]

##CREATE A SEASON VARIABLE
player_data[,date:=as.Date(date)] ##conver string date to actual date
player_data[,season_code:=20122013]
player_data[date >= '2013-10-28' & date <= '2014-06-16', season_code:=20132014]
player_data[date >= '2014-10-28' & date <= '2015-06-16', season_code:=20142015]
player_data[date >= '2015-10-27' & date <= '2016-06-19', season_code:=20152016]


##JOIN TABLES
setkey(player_data,gameID)
setkey(event_data,gameID)
players=player_data[event_data,nomatch=0]

##figure out why its size increased --get rid of dupes in event_data
View(players[,player_count:=.N,by=.(gameID,player,position)][player_count>1])

##filter out players who didnt play
player_data=filter(player_data,minutes>0)


##############################
##### UNIVARIATE PLOTS #######
##############################

ggplot(player_data,aes(fd)) + geom_histogram(binwidth = 1) + theme_dlin() +
  labs(title = 'NBA Fanduel Points Histogram')
summary(player_data$fd)
###comments - you can get -ive points. the distribution is positively skewed

ggplot(player_data,aes(position)) + geom_bar() + theme_dlin() +
  labs(title = 'NBA Positions Histogram')

#upon further inspection, there are a small
dim(player_data[position %in% c('F','G')])[1]
player_data[position=='F'][,.N,by=(player)] ##get player_names

##get count of games per season
player_data[,.N,by=season_code]


##multiple variable distribution plots
##code from http://stackoverflow.com/questions/13035834/plot-every-column-in-a-data-frame-as-a-histogram-on-one-page-using-ggplotcoz
cols=colnames(player_data)
cols=cols[!(cols %in% c('date','team','season_code','gameID','player','sport','position'))]

d <- melt(player_data[, cols, with=FALSE])
ggplot(d,aes(x = value)) + theme_dlin() + facet_wrap(~variable,scales = "free_x") + geom_histogram()


##############################
##### UNIVARIATE ANALYSIS ####
##############################

# What is the structure of your dataset?
# What is/are the main feature(s) of interest in your dataset?
# What other features in the dataset do you think will help support your investigation into your feature(s) of interest?
# Did you create any new variables from existing variables in the dataset?
# Of the features you investigated, were there any unusual distributions? Did you perform any operations on the data to tidy, adjust, or change the form of the data? If so, why did you do this?


##############################
###### BIVARIATE PLOTS #######
##############################

##minutes
ggplot(player_data,aes(x = minutes, y = fd)) + geom_point(colour=palette[5]) + theme_dlin() +
  labs(title = 'NBA Fanduel Points versus minutes', x = 'minutes', y = 'fanduel points') +
  geom_smooth(method = "lm", se = FALSE,colour='black')





##USEFUL Funcs
summary(player_data)
summary(player_data)
dim(player_data) ##dim[1]=nrows, dim[2]=ncolumns

###FEATURES TO ADD
##home/away
##rolling variables
##starter categorical


##############################
##############################
###### REQUIRED SECTIONS #####
##############################
##############################

# 1) INTRO
# 2) UNIVARIATE PLOTS
# 3) UNIVARIATE ANALYSIS
# 4) BIVARIATE PLOTS SECTION
# 5) BIVARIATE ANALYSIS
# 6) MULTIVARIATE PLOTS SECTION
# 7) MULTIVARIATE ANALYSIS
# 8) FINAL PLOTS AND SUMMARY
# 9) REFLECTION

rm(list=ls())
setwd("/Users/whitesi/Documents/Programming/Python/data_analyst_nd/nba-dfs")
setwd("/Users/cmt606/Documents/Ian/ME/nba-dfs")

##IMPORT PACKAGES
library(data.table)
library(dplyr)
library(dtplyr)
library(tidyr)
library(ggplot2)
source("dlin.R")
library(RColorBrewer)
library(reshape2)
library(corrplot)
source("roll_variable.R")
library(RcppRoll)
library(rms)
source("multiplot.R")
library(plyr)

##display.brewer.all() ##view all palettes with this
palette <- brewer.pal("YlGnBu", n=9)

#######################################
####### DATA IMPORT & CLEANING ########
#######################################

##read in data
player_data=read.csv("data/player_data.csv")
event_data=read.csv("data/event_data.csv")

##convert to data.tables
event_data=as.data.table(event_data)
player_data=as.data.table(player_data)

#rename and drop unnecessary columns
player_data[ ,c("X","sport") := NULL]
event_data[,X:=NULL]
setnames(player_data,old=c('X3FGA','X3FGM'),new=c('3FGA','3FGM'))

#get rid of duplicate rows in event_data
setkey(event_data,gameID)
event_data=unique(event_data)

#convert positions coded as 'F' to 'SF', 'G' to 'SG'
player_data[position=='F',position:='SF']
player_data[position=='G',position:='SG']

####################################
####### FEATURE ENGINEERING ########
####################################

##calculate fanduel points
player_data[,fd:=points*1+1.2*rebounds+1.5*assists+2*blocks+2*steals-1*turnovers]

##create a seaosn variable that can be used to distinguish between different seasons
player_data[,date:=as.Date(date)] ##convert string date to actual date
player_data[,date_num:=as.numeric(date)]
player_data[,season_code:=20122013]
player_data[date >= '2013-10-28' & date <= '2014-06-16', season_code:=20132014]
player_data[date >= '2014-10-28' & date <= '2015-06-16', season_code:=20142015]
player_data[date >= '2015-10-27' & date <= '2016-06-19', season_code:=20152016]

event_data = event_data %>% join(player_data[,.N,by=.(gameID,date)][,.(gameID,date)])

event_data[,date:=as.Date(date)] ##convert string date to actual date
event_data[,season_code:=20122013]
event_data[date >= '2013-10-28' & date <= '2014-06-16', season_code:=20132014]
event_data[date >= '2014-10-28' & date <= '2015-06-16', season_code:=20142015]
event_data[date >= '2015-10-27' & date <= '2016-06-19', season_code:=20152016]

# ##JOIN TABLES - FOR FUTURE USE OF EVENT_DATA
# setkey(player_data,gameID)
# setkey(event_data,gameID)
# players=player_data[event_data,nomatch=0]
# ##figure out why its size increased --get rid of dupes in event_data
# players[,player_count:=.N,by=.(gameID,player,position)][player_count>1]


##get team info
team_data=event_data[,.(gameID,home_team,away_team)]
setkey(player_data,gameID)
setkey(team_data,gameID)
player_data=player_data[team_data,nomatch=0]

##create home/away variable
player_data[,homeaway:= 1]
player_data[team == away_team,homeaway:= 0]

##add rolling variables
window_size = seq(5,55,10)
player_data = player_data %>% group_by(player) %>% arrange(date) %>% roll_variable_mean(., 'fd', window_size)
player_data = player_data %>% group_by(player) %>% arrange(date) %>% roll_variable_mean(., 'minutes', window_size)
player_data = player_data %>% group_by(player) %>% arrange(date) %>% roll_variable_mean(., 'FGA', window_size)
player_data = player_data %>% group_by(player) %>% arrange(date) %>% roll_variable_mean(., 'FTA', window_size)

window_size = seq(1,3,1)
player_data = player_data %>% group_by(player) %>% arrange(date) %>% roll_variable_mean(., 'fd', window_size)
player_data = player_data %>% group_by(player) %>% arrange(date) %>% roll_variable_mean(., 'minutes', window_size)
player_data = player_data %>% group_by(player) %>% arrange(date) %>% roll_variable_mean(., 'FGA', window_size)
player_data = player_data %>% group_by(player) %>% arrange(date) %>% roll_variable_mean(., 'FTA', window_size)


##NORMALIZE FD POINTS per 48 min
player_data[, fdpm := fd/minutes]
# ggplot(player_data,aes(fdpm)) + geom_histogram(binwidth=0.025) + theme_dlin() 

## cap & floor
player_data[fdpm<=0, fdpm := 0.75]
player_data[fdpm>=2.5, fdpm := 0.75]
player_data[minutes==0, fdpm := 0.75]

window_size = seq(5,55,10)
player_data = player_data %>% group_by(player) %>% arrange(date) %>% roll_variable_mean(., 'fdpm', window_size)
window_size = seq(1,3,1)
player_data = player_data %>% group_by(player) %>% arrange(date) %>% roll_variable_mean(., 'fdpm', window_size)


##BACK TO BACK GAME VARIABLE
days_rest=player_data[,.N,by=.(team,date_num)][,.(team,date_num)][order(team,date_num)]
days_rest$days_rest=ave(days_rest$date_num, days_rest$team, FUN=function(x) c(10, diff(x)))
days_rest$b2b=ifelse(days_rest$days_rest==1,1,0)

##add opponent field
player_data[,opponent:=home_team]
player_data[team==home_team,opponent:=away_team]

##join b2b,opp_b2b, days_rest, opp_days_rest
setkey(player_data,team,date_num)
setkey(days_rest,team,date_num)
player_data=player_data[days_rest[,.(team,date_num,b2b,days_rest)],nomatch=0]

days_rest[, `:=` (opp_b2b=b2b, opp_days_rest=days_rest)][,`:=` (b2b=NULL,days_rest=NULL)]
setkey(days_rest,team,date_num)
setkey(player_data,opponent,date_num)
player_data=player_data[days_rest[,.(team,date_num,opp_b2b,opp_days_rest)],nomatch=0] ##not working

player_data[days_rest>6, days_rest := 6]

##one-encoding for position
player_data[, `:=` (pg = 0,sg = 0,sf = 0,pf = 0,c = 0)]
player_data[position == 'PG', pg := 1]
player_data[position == 'SG', sg := 1]
player_data[position == 'SF', sf := 1]
player_data[position == 'PF', pf := 1]
player_data[position == 'C', c := 1]

###TEAM BASED DATA

##calculate final scores for each game
event_data[,home_score:=sum(home_Q1,home_Q2,home_Q3,home_Q4,home_Q5,home_Q6,home_Q7,home_Q8,na.rm=TRUE),
           by=1:NROW(event_data)]
event_data[,away_score:=sum(away_Q1,away_Q2,away_Q3,away_Q4,away_Q5,away_Q6,away_Q7,away_Q8,na.rm=TRUE),
           by=1:NROW(event_data)]

##to calculate team based statistics, a data frame/table with 2 records per game -
##[cont'd] one for each team, is ideal
##the code below splits the event_data table into two tables, one for each team,
##[cont'd] standardizes the variable names, and then joins the two tables back together
team_variables= c('3FGA','3FGM','FGM','FGA','FTA','FTM',
                  'Q1','Q2','Q3','Q4','Q5','Q6','Q7',
                  'Q8','assists','blocks','fouls',
                  'points','rebounds','steals','turnovers')

away_variables= paste0('away_',team_variables)
home_variables= paste0('home_',team_variables)

##data table for the home team of each game
event_data_1=event_data[,team:=home_team][,setdiff(colnames(event_data),away_variables),with=FALSE]
##data table for hte away team of each game
event_data_2=event_data[,team:=away_team][,setdiff(colnames(event_data),home_variables),with=FALSE]

##change the column names to generic names (i.e. away_FGM --> FGM,home_FTA --> FTA)
setnames(event_data_1,old=home_variables,new=team_variables)
setnames(event_data_2,old=away_variables,new=team_variables)

##join the two tables back together
team_data=rbind(event_data_1,event_data_2)

##add date to team_data
setkey(team_data,gameID)
setkey(player_data,gameID)
team_data=team_data[player_data[,.N,by=.(gameID,date_num)][,.(gameID,date_num)],nomatch=0][order(date_num)]

##define opponent in team_data table
team_data[,opponent:=home_team]
team_data[team==home_team,opponent:=away_team]

###team total FD points
team_tot_fd=player_data[,.(team_fd=sum(fd)),by=.(gameID,team)]
setkey(team_tot_fd,gameID,team)
setkey(player_data,gameID,team)
setkey(team_data,gameID,team)
player_data=player_data[team_tot_fd,nomatch=0]
team_data=team_data[team_tot_fd,nomatch=0]
  
###opponent total FD points
team_tot_fd[,`:=` (opponent = team, team = NULL,opp_fd=team_fd,team_fd=NULL)]
setkey(team_tot_fd,gameID,opponent)
setkey(player_data,gameID,opponent)
setkey(team_data,gameID,opponent)
player_data=player_data[team_tot_fd,nomatch=0]
team_data=team_data[team_tot_fd,nomatch=0]

###opponent rebounds and turnovers
opp_stats = team_data[,.(team,rebounds,turnovers)]
opp_stats[,`:=` (opponent = team, team = NULL)]

##position points summary
##calculate the total FD points for each team by position
##in some cases, teams played w/o a center. for those, use the post statistic (p_fd)
posn_sum=player_data[,.(posn_points=sum(fd)),by=.(gameID,team,position)]
posn_sum=posn_sum[,.(pg_fd=sum(ifelse(position=='PG',posn_points,0)),
                     sg_fd=sum(ifelse(position=='SG',posn_points,0)),
                     sf_fd=sum(ifelse(position=='SF',posn_points,0)),
                     pf_fd=sum(ifelse(position=='PF',posn_points,0)),
                     c_fd=sum(ifelse(position=='C',posn_points,0)),
                     g_fd=sum(ifelse(position %in% c('PG','SG','SF'),posn_points,0)),
                     p_fd=sum(ifelse(position %in% c('PF','C'),posn_points,0))),
                     by=.(gameID,team)]

posn_sum_n=player_data[,.(posn_points=sum(fd),num_players = .N),by=.(gameID,team,position)]
posn_sum_n=posn_sum_n[,.(pg_fd_n=sum(ifelse(position=='PG',posn_points,0))/sum(ifelse(position=='PG',num_players,0)),
                         sg_fd_n=sum(ifelse(position=='SG',posn_points,0))/sum(ifelse(position=='SG',num_players,0)),
                         sf_fd_n=sum(ifelse(position=='SF',posn_points,0))/sum(ifelse(position=='SF',num_players,0)),
                         pf_fd_n=sum(ifelse(position=='PF',posn_points,0))/sum(ifelse(position=='PF',num_players,0)),
                         c_fd_n=sum(ifelse(position=='C',posn_points,0))/sum(ifelse(position=='C',num_players,0)),
                         g_fd_n=sum(ifelse(position %in% c('PG','SG','SF'),posn_points,0))/sum(ifelse(position %in% c('PG','SG','SF'),num_players,0)),
                         p_fd_n=sum(ifelse(position %in% c('PF','C'),posn_points,0))/sum(ifelse(position %in% c('PF','C'),num_players,0))),
                      by=.(gameID,team)]


##merge positional points with team_data
setkey(posn_sum,gameID,team)
setkey(team_data,gameID,team)
team_data=team_data[posn_sum,nomatch=0]

setkey(posn_sum_n,gameID,team)
setkey(team_data,gameID,team)
team_data=team_data[posn_sum_n,nomatch=0]

###IAN - LEFT OFF HERE!!!

##get team opponent data
team_data_opp=team_data[,.(gameID,team,pg_fd,sg_fd,sf_fd,pf_fd,c_fd,g_fd,p_fd,team_fd)]
setnames(team_data_opp,old=c("team","pg_fd","sg_fd","sf_fd","pf_fd","c_fd","g_fd","p_fd","team_fd"),
         new=c("opponent","opp_pg_fd","opp_sg_fd","opp_sf_fd","opp_pf_fd","opp_c_fd","opp_g_fd","opp_p_fd","opp_fd"))
setkey(team_data,gameID,opponent)
setkey(team_data_opp,gameID,opponent)
team_data=team_data[team_data_opp,nomatch=0]

##rolling team statistics
##the following stat describes how many fantasy points a team has been giving up to opposing teams,
##...[cont'd] expressed as rolling averages over the last X games
window_size = seq(5,55,10)
team_data=team_data %>% group_by(team) %>% arrange(date_num) %>% roll_variable_mean(., 'opp_pg_fd', window_size)
team_data=team_data %>% group_by(team) %>% arrange(date_num) %>% roll_variable_mean(., 'opp_sg_fd', window_size)
team_data=team_data %>% group_by(team) %>% arrange(date_num) %>% roll_variable_mean(., 'opp_sf_fd', window_size)
team_data=team_data %>% group_by(team) %>% arrange(date_num) %>% roll_variable_mean(., 'opp_pf_fd', window_size)
team_data=team_data %>% group_by(team) %>% arrange(date_num) %>% roll_variable_mean(., 'opp_c_fd', window_size)

window_size = seq(1,3,1)
team_data=team_data %>% group_by(team) %>% arrange(date_num) %>% roll_variable_mean(., 'opp_pg_fd', window_size)
team_data=team_data %>% group_by(team) %>% arrange(date_num) %>% roll_variable_mean(., 'opp_sg_fd', window_size)
team_data=team_data %>% group_by(team) %>% arrange(date_num) %>% roll_variable_mean(., 'opp_sf_fd', window_size)
team_data=team_data %>% group_by(team) %>% arrange(date_num) %>% roll_variable_mean(., 'opp_pf_fd', window_size)
team_data=team_data %>% group_by(team) %>% arrange(date_num) %>% roll_variable_mean(., 'opp_c_fd', window_size)


## IAN - NORMALIZE THESE EITHER BY NUMBER OF PLAYERS AT THAT POSITION OR STARTER VS BENCH sep features

##join these features to player_data
##join team_data "team" on player_data "opponent"
rolling_team_variables= colnames(team_data)[grepl('fd_\\w*\\d', colnames(team_data))]
player_data=merge(player_data, team_data[,append(rolling_team_variables,c("gameID","team")),with=FALSE], 
                  by.x=c('gameID','opponent'), by.y=c('gameID','team'), all=FALSE)


player_data[position == 'PG', `:=` (opp_fd_1 = opp_pg_fd_1, opp_fd_2 = opp_pg_fd_2, opp_fd_3 = opp_pg_fd_3, 
                                    opp_fd_5 = opp_pg_fd_5, opp_fd_15 = opp_pg_fd_15, opp_fd_25 = opp_pg_fd_25, 
                                    opp_fd_35 = opp_pg_fd_35, opp_fd_45 = opp_pg_fd_45, opp_fd_55 = opp_pg_fd_55)]

player_data[position == 'SG', `:=` (opp_fd_1 = opp_sg_fd_1, opp_fd_2 = opp_sg_fd_2, opp_fd_3 = opp_sg_fd_3, 
                                    opp_fd_5 = opp_sg_fd_5, opp_fd_15 = opp_sg_fd_15, opp_fd_25 = opp_sg_fd_25, 
                                    opp_fd_35 = opp_sg_fd_35, opp_fd_45 = opp_sg_fd_45, opp_fd_55 = opp_sg_fd_55)]

player_data[position == 'SF', `:=` (opp_fd_1 = opp_sf_fd_1, opp_fd_2 = opp_sf_fd_2, opp_fd_3 = opp_sf_fd_3, 
                                    opp_fd_5 = opp_sf_fd_5, opp_fd_15 = opp_sf_fd_15, opp_fd_25 = opp_sf_fd_25, 
                                    opp_fd_35 = opp_sf_fd_35, opp_fd_45 = opp_sf_fd_45, opp_fd_55 = opp_sf_fd_55)]

player_data[position == 'PF', `:=` (opp_fd_1 = opp_pf_fd_1, opp_fd_2 = opp_pf_fd_2, opp_fd_3 = opp_pf_fd_3, 
                                    opp_fd_5 = opp_pf_fd_5, opp_fd_15 = opp_pf_fd_15, opp_fd_25 = opp_pf_fd_25, 
                                    opp_fd_35 = opp_pf_fd_35, opp_fd_45 = opp_pf_fd_45, opp_fd_55 = opp_pf_fd_55)]

player_data[position == 'C', `:=` (opp_fd_1 = opp_c_fd_1, opp_fd_2 = opp_c_fd_2, opp_fd_3 = opp_c_fd_3, 
                                   opp_fd_5 = opp_c_fd_5, opp_fd_15 = opp_c_fd_15, opp_fd_25 = opp_c_fd_25, 
                                   opp_fd_35 = opp_c_fd_35, opp_fd_45 = opp_c_fd_45, opp_fd_55 = opp_c_fd_55)]

## positional depth
team_depth = player_data[, .(pos_depth=.N - 1) ,by=.(gameID,team,position)]
player_data = player_data %>% inner_join(team_depth)


drop_cols = c("opp_fd","team_fd")


##############################
######## Modelling ###########
##############################

min_variables = colnames(player_data)[grepl('minutes_\\w*\\d', colnames(player_data))]
fd_variables = colnames(player_data)[grepl('fd_\\w*\\d', colnames(player_data))]
fta_variables = colnames(player_data)[grepl('FTA_\\w*\\d', colnames(player_data))]
fga_variables = colnames(player_data)[grepl('FGA_\\w*\\d', colnames(player_data))]
opp_variables = colnames(player_data)[grepl('opp_fd_\\w*\\d', colnames(player_data))]
fdpm_variables = colnames(player_data)[grepl('fdpm_\\w*\\d', colnames(player_data))]

pos_feature = c('pg','sg','sf','pf')
  
base_variables = c("fd_1","fd_2","fd_3","fd_5","fd_15","fd_25","fd_35","fd_45","fd_55")
all_variables = c(base_variables,min_variables,fta_variables,fga_variables,opp_variables,
                    'b2b','opp_b2b','homeaway','starter','pos_depth',pos_feature,fdpm_variables)

##filter training on players with fd>0???
training = player_data[season_code != 20142015]
test = player_data[season_code == 20142015]
ytest = select(test, gameID, player, fd)
all = select(player_data,gameID,player,fd)

#########################
######  BASE MODEL ######
#########################

fmla = reformulate(base_variables, response='fd')

#get model stats
mdl=lm(fmla, data = training)
mdl %>% summary

#test model on test data
ytest$mdl = predict(mdl, test)

lm(fd ~ mdl, data = ytest) %>% summary


######################
######  MODEL 1 ######
######################

fmla1 = reformulate(all_variables, response='fd')

#get model stats
mdl1=lm(fmla1, data = training)
mdl1 %>% summary

#test model on test data
ytest$mdl1 = predict(mdl1, test)
lm(fd ~ mdl1, data = ytest) %>% summary


vars_kept = fastbw(mdl1, k.aic = 1.5)$names.kept

mdl15 = lm(reformulate(vars_kept, response = 'fd'), data = training)

md15 %>% summary



######################
######  MODEL 2 ######
######################

mdl2 = Glm(fmla1,data = training)

vars_kept = fastbw(mdl2, k.aic = 1.5)$names.kept

mdl2 = lm(reformulate(vars_kept, response = 'fd'), data = training)

mdl2 %>% summary

#test model on test data
ytest$mdl2 = predict(mdl2, test)
lm(fd ~ mdl2, data = ytest) %>% summary

all$mdl2 = predict(mdl2, player_data)

########################
### FEATURES TO ADD ###
########################

## pace
## usage rate
## points per minute
## weak rebounding teams? (especially important for centers..opp_rebounds allowed feature)
## teams with lots of turnovers??
## starter is resting/injured.
## trailing points (actual basketball points)

#################
### MODELLING ###
#################

## separate model for each position 
## redo days rest at the player level??
## days rest at team level (fix this...)
## normalize opp_fd variables...

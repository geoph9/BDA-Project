library("rstan")
library(knitr)
library(dplyr)

#read the match performance of each team in group stage
#have make the data to look like factory data
data <- read.csv(file = './data/match_performance_group_region.csv')

sm <- rstan::stan_model(file = './model/Separate.stan')
stan_data <- list(y = data,
                  N = nrow(data),
                  J = ncol(data))
model_sm <- rstan::sampling(sm, data = stan_data)
model_sm
draws_sm <- as.data.frame(model_sm)

#return the win_rate of team_num_1
predict_result_sm <- function(team_num_1, team_num_2) {
  ypred_1 <- draws_sm[, 36 + team_num_1]
  ypred_2 <- draws_sm[, 36 + team_num_2]
  win_rate_1 = {
  }
  for (i in 1:length(ypred_1)) {
    win_rate_1[i] = ypred_1[i] / (ypred_1[i] + ypred_2[i])
  }
  
  mean_rate_1 <- (mean(win_rate_1))
  
  return(mean_rate_1)
}

#return the win team
compare <- function(team_name_1,team_name_2){
  team_num_1 <- which(colnames(data) == team_name_1)
  team_num_2 <- which(colnames(data) == team_name_2)
  predict <- predict_result_sm(team_num_1,team_num_2)
  if(predict>=0.5)
    return(team_name_1)
  else 
    return(team_name_2)
}

#return the win team
compare_rate <- function(team_name_1,team_name_2){
  team_num_1 <- which(colnames(data) == team_name_1)
  team_num_2 <- which(colnames(data) == team_name_2)
  predict <- predict_result_sm(team_num_1,team_num_2)
  return(predict)
}

#Calculate the result of main event stage
compare("PSG.LGD","VP")
compare("VG","TNC")
compare("OG","Newbee")
compare("Secret","EG")
compare("Alliance","RNG")
compare("Fnatic","Liquid")
compare("Infamous","KG")
compare("Mineski","Na.Vi")
compare("VP","RNG")
compare("TNC","Liquid")
compare("Newbee","Infamous")
compare("Secret","Mineski")
compare("PSG.LGD","VG")
compare("OG","EG")
compare("RNG","Liquid")
compare("Infamous","Secret")
compare("EG","Liquid")
compare("VG","Secret")
compare("Liquid","Secret")
compare("PSG.LGD","OG")
compare("PSG.LGD","Liquid")
compare("OG","Liquid")

#output actually win rate
compare_rate("PSG.LGD","VP")
compare_rate("VG","TNC")
compare_rate("OG","Newbee")
compare_rate("Secret","EG")
compare_rate("Alliance","RNG")
compare_rate("Fnatic","Liquid")
compare_rate("Infamous","KG")
compare_rate("Mineski","Na.Vi")
compare_rate("VP","RNG")
compare_rate("TNC","Liquid")
compare_rate("Newbee","Infamous")
compare_rate("Secret","Mineski")
compare_rate("PSG.LGD","VG")
compare_rate("OG","EG")
compare_rate("RNG","Liquid")
compare_rate("Infamous","Secret")
compare_rate("EG","Liquid")
compare_rate("VG","Secret")
compare_rate("Liquid","Secret")
compare_rate("PSG.LGD","OG")
predict_result_sm("PSG.LGD","Liquid")
compare_rate("OG","Liquid")

hm <- rstan::stan_model(file = './model/Region-Hierarchical.stan')
Europe_data <- list(y = data[,1:6],
                  N = nrow(data[,1:6]),
                  J = ncol(data[,1:6]))
model_Europe_hm <- rstan::sampling(hm, data = Europe_data)
model_Europe_hm
draws_Europe_hm <- as.data.frame(model_Europe_hm)

China_data <- list(y = data[,7:10],
                    N = nrow(data[,7:10]),
                    J = ncol(data[,7:10]))
model_China_hm <- rstan::sampling(hm, data = China_data)
model_China_hm
draws_China_hm <- as.data.frame(model_China_hm)

CIS_data <- list(y = data[,11:12],
                   N = nrow(data[,11:12]),
                   J = ncol(data[,11:12]))
model_CIS_hm <- rstan::sampling(hm, data = CIS_data)
model_CIS_hm
draws_CIS_hm <- as.data.frame(model_CIS_hm)


SAsia_data <- list(y = data[,13:15],
                 N = nrow(data[,13:15]),
                 J = ncol(data[,13:15]))
model_SAsia_hm <- rstan::sampling(hm, data = SAsia_data)
model_SAsia_hm
draws_SAsia_hm <- as.data.frame(model_SAsia_hm)

NA_data <- list(y = data[,16:17],
                N = nrow(data[,16:17]),
                J = ncol(data[,16:17]))
model_NA_hm <- rstan::sampling(hm, data = NA_data)
model_NA_hm
draws_NA_hm <- as.data.frame(model_NA_hm)

#only one south America team, use the separate pred to represent
draws_SA_hm <- draws_sm$`ypred[18]`

draws_hm <- cbind(draws_Europe_hm[,10:15],draws_China_hm[,8:11],
                  draws_CIS_hm[,6:7],draws_SAsia_hm[,7:9],draws_NA_hm[,6:7],draws_SA_hm)

#return the win_rate of team_num_1
predict_result_hm <- function(team_num_1, team_num_2) {
  ypred_1 <- draws_hm[, team_num_1]
  ypred_2 <- draws_hm[, team_num_2]
  win_rate_1 = {
  }
  for (i in 1:length(ypred_1)) {
    win_rate_1[i] = ypred_1[i] / (ypred_1[i] + ypred_2[i])
  }
  
  mean_rate_1 <- (mean(win_rate_1))
  
  return(mean_rate_1)
}

#output the win rate of different teams
compare_hm_rate<- function(team_name_1,team_name_2){
  team_num_1 <- which(colnames(data) == team_name_1)
  team_num_2 <- which(colnames(data) == team_name_2)
  win_rate_1 <- predict_result_hm(team_num_1,team_num_2)
  if(win_rate_1 >= 0.5)
    return(team_name_1)
  else 
    return(team_name_2)
}

#return the win team
compare_hm <- function(team_name_1,team_name_2){
  team_num_1 <- which(colnames(data) == team_name_1)
  team_num_2 <- which(colnames(data) == team_name_2)
  win_rate_1 <- predict_result_hm(team_num_1,team_num_2)
  if(win_rate_1 >= 0.5)
    return(team_name_1)
  else 
    return(team_name_2)
}

#Calculate the result of main event stage
compare_hm("PSG.LGD","VP")
compare_hm("VG","TNC")
compare_hm("OG","Newbee")
compare_hm("Secret","EG")
compare_hm("Alliance","RNG")
compare_hm("Fnatic","Liquid")
compare_hm("Infamous","KG")
compare_hm("Mineski","Na.Vi")
compare_hm("VP","RNG")
compare_hm("TNC","Liquid")
compare_hm("Newbee","Infamous")
compare_hm("Secret","Mineski")
compare_hm("PSG.LGD","VG")
compare_hm("OG","EG")
compare_hm("RNG","Liquid")
compare_hm("Infamous","Secret")
compare_hm("EG","Liquid")
compare_hm("VG","Secret")
compare_hm("Liquid","Secret")
compare_hm("PSG.LGD","OG")
compare_hm("PSG.LGD","Liquid")
compare_hm("OG","Liquid")


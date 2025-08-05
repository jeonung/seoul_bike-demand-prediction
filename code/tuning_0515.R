setwd("C:/Users/s_sjw0513/Desktop/데이터마이닝/project")
rm(list=ls())

library(dplyr)
library(randomForest)
library(xgboost)
library(ggplot2)
library(cowplot)
library(gridExtra)
source("./code/make_result_train_val_mse_0511.R")
source("./code/tune_hyperparameter.R")
source("./code/graph_function_0515.R")

filter.fun = function(data){
  data = data %>% select(-c(timestamp, spot_number))
  data = data %>% rename("y"=names(data)[1])
  return(data)
}

set.seed(0504)
# Setting
rf_params = expand.grid(ntree = c(100, 150, 200, 250, 300),
                        mtry = c(40, 50))

# elnet_params = expand.grid(alpha = seq(0, 1, by=0.1),
#                            lambda = 10^seq(-3, 3, by=0.1))

# xg_params = expand.grid(nrounds = c(800, 1300, 1800),
#                         max_depth = c(10, 40, 70),
#                         eta = c(0.08, 0.1, 0.12))

n_cv_tt = 1  # number of cv for train / test
n_cv_tv = 3  # number of cv for train / validation


file_list = list.files("./data/return/analysis_data", pattern="*.csv")
file_list = file_list[1:4]  # 웅
# file_list = file_list[5:9]  # 준희
# file_list = file_list[10:13]  # 다은

for (file in file_list){
  print(paste("-------------", file, "-------------"))
  data = read.csv(paste0("./data/return/analysis_data/", file))
  data = filter.fun(data)
  rf_result = tune_hyperparameter(model='rf', params=rf_params,
                                  data=data, n_cv_tt, n_cv_tv)
  # elnet_result = tune_hyperparameter(model='elnet', params=elnet_params,
  #                                    data=data, n_cv_tt, n_cv_tv)
  # xg_result = tune_hyperparameter(model='xg', params=xg_params,
  #                                 data=data, n_cv_tt, n_cv_tv)
  # 
  rf_train_result = rf_result$train_mse
  rf_val_result = rf_result$val_mse
  # elnet_train_result = elnet_result$train_mse
  # elnet_val_result = elnet_result$val_mse
  # xg_train_result = xg_result$train_mse
  # xg_val_result = xg_result$val_mse
  #  
  # 기존 graph
  rf_train_df = read.csv(paste0('./result/return/tuning/rbind_rf_train_result_', file))
  rf_val_df = read.csv(paste0('./result/return/tuning/rbind_rf_val_result_', file))
  # 
  # xg_train_df = read.csv(paste0('./result/return/tuning/rbind_xg_train_result_', file))
  # xg_val_df = read.csv(paste0('./result/return/tuning/rbind_xg_val_result_', file))
  # 
  rf_train_result = rbind(rf_train_df, rf_train_result)
  rf_val_result = rbind(rf_val_df, rf_val_result)

  # xg_train_result = rbind(xg_train_df, xg_train_result)
  # xg_val_result = rbind(xg_val_df, xg_val_result)
  # 
  write.csv(rf_train_result, paste0("./result/return/tuning/rbind_rf_train_result_", file), row.names=F)
  write.csv(rf_val_result, paste0("./result/return/tuning/rbind_rf_val_result_", file), row.names=F)
  
  # write.csv(xg_train_result, paste0("./result/return/tuning/rbind_xg_train_result_", file), row.names=F)
  # write.csv(xg_val_result, paste0("./result/return/tuning/rbind_xg_val_result_", file), row.names=F)

  graph.fun(mod='RF', file_list=file)
  # graph.fun(mod='XG', file_list=file_list)
  

  # write.csv(elnet_train_result, paste0("elnet_train_result_", file), row.names=F)
  # write.csv(elnet_val_result, paste0("elnet_val_result_", file), row.names=F)
  
}

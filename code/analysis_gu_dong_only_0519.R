setwd("C:/Users/s_sjw0513/Desktop/데이터마이닝/project")
rm(list=ls())

y_name = "rent"
start_time = Sys.time()

# --------------------------------------------------- #
#
# 0. Load libraries and functions
#
# --------------------------------------------------- #

library(dplyr)
source("./code/make_result_train_val_mse_0511.R")

find_best_combination = function(val_result){
    # This function is to find the best combination of parameters minimizing validation error
    val_medians = aggregate(val_mse ~ ., val_result, median)
    best_com = val_medians[which.min(val_medians$val_mse), ]
    
    return(best_com)
}

filter.fun = function(data){
    data = data %>% select(-c(timestamp, spot_number))
    data = data %>% rename("y"=names(data)[1])
    return(data)
}

# --------------------------------------------------- #
#
# 1. Read data
#
# --------------------------------------------------- #

data_590 = read.csv(paste0("./data/변수추가o/", y_name, "/analysis_data/건국대학교 (입학정보관)_590_",
                           y_name, "_data.csv"))
data_591 = read.csv(paste0("./data/변수추가o/", y_name, "/analysis_data/건국대학교 (행정관)_591_",
                           y_name, "_data.csv"))
data_3523 = read.csv(paste0("./data/변수추가o/", y_name, "/analysis_data/건국대학교 과학관(이과대) 앞_3523_",
                            y_name, "_data.csv"))
data_3860 = read.csv(paste0("./data/변수추가o/", y_name, "/analysis_data/건국대학교 정문 앞_3860_",
                            y_name, "_data.csv"))
data_592 = read.csv(paste0("./data/변수추가o/", y_name, "/analysis_data/건국대학교 학생회관_592_",
                           y_name, "_data.csv"))
data_3569 = read.csv(paste0("./data/변수추가o/", y_name, "/analysis_data/건대병원후문_3569_",
                            y_name, "_data.csv"))
data_3579 = read.csv(paste0("./data/변수추가o/", y_name, "/analysis_data/광진 캠퍼스시티_3579_",
                            y_name, "_data.csv"))
data_500 = read.csv(paste0("./data/변수추가o/", y_name, "/analysis_data/어린이대공원역 3번출구 앞_500_",
                           y_name, "_data.csv"))
data_3571 = read.csv(paste0("./data/변수추가o/", y_name, "/analysis_data/화양 APT(횡단보도 옆)_3571_",
                            y_name, "_data.csv"))
data_3582 = read.csv(paste0("./data/변수추가o/", y_name, "/analysis_data/화양동 우체국_3582_",
                            y_name, "_data.csv"))
data_3508 = read.csv(paste0("./data/변수추가o/", y_name, "/analysis_data/건국대학교 (입학정보관)_590_",
                            y_name, "_data.csv"))
gu = read.csv(paste0("./data/변수추가o/", y_name, "/analysis_data/광진구_unif_",
                     y_name, "_data.csv"))
dong = read.csv(paste0("./data/변수추가o/", y_name, "/analysis_data/화양동_unif_",
                       y_name, "_data.csv"))

# Delete timestamp, spot_number
data_590 = filter.fun(data_590)
data_591 = filter.fun(data_591)
data_3523 = filter.fun(data_3523)
data_3860 = filter.fun(data_3860)
data_592 = filter.fun(data_592)
data_3569 = filter.fun(data_3569)
data_3579 = filter.fun(data_3579)
data_500 = filter.fun(data_500)
data_3571 = filter.fun(data_3571)
data_3582 = filter.fun(data_3582)
data_3508 = filter.fun(data_3508)
gu = filter.fun(gu)
dong = filter.fun(dong)

# --------------------------------------------------- #
#
# 2. Setting for analysis
#
# --------------------------------------------------- #

# -------- Setting hyper parameters -------- #

########################## gu ##########################
rf_params_gu = expand.grid(ntree = c(100, 150, 200),
                           mtry = c(10, 20, 30, 40))

elnet_params_gu = expand.grid(alpha = seq(0, 1, by=0.1),
                              lambda = 10^seq(-3, 3, by=0.1))

xg_params_gu = expand.grid(nrounds = c(800, 1000, 1200),
                           max_depth = c(6, 9, 12),
                           eta = c(0.08, 0.1, 0.12))

########################## dong ##########################
rf_params_dong = expand.grid(ntree = c(100, 150, 200),
                             mtry = c(10, 20, 30, 40))

elnet_params_dong = expand.grid(alpha = seq(0, 1, by=0.1),
                                lambda = 10^seq(-3, 3, by=0.1))

xg_params_dong = expand.grid(nrounds = c(800, 1000, 1200),
                             max_depth = c(6, 9, 12),
                             eta = c(0.08, 0.1, 0.12))

# ---------------- Setting to make result  ------------------ #
n_cv_tt = 1  # number of cv for train / test
n_cv_tv = 5  # number of cv for train / validation

# train, validation result (2 X n_cv_tv)
# best result (2 X n_cv_tt)
rf_train_result = rf_val_result = rf_best_result = NULL
elnet_train_result = elnet_val_result = elnet_best_result = NULL
xg_train_result = xg_val_result = xg_best_result = NULL

# test result (11 X n_cv_tt) (gu, dong)
rf_test_result_gu = elnet_test_result_gu = xg_test_result_gu = NULL
rf_test_result_dong = elnet_test_result_dong = xg_test_result_dong = NULL

# --------------------------------------------------- #
#
# 3. Run model
#
# --------------------------------------------------- #

# to split data
n = 365*24
idx = 1:n

for (iter in 1:n_cv_tt){
    print(paste("#", iter, "cv train/test"))
    # split data --> train / test with 80:20
    train_val_idx = sample(idx, n*0.8)
    test_idx = setdiff(idx, train_val_idx)
    
    # make test set
    test_3508 = data_3508[test_idx, ]
    test_3523 = data_3523[test_idx, ]
    test_3569 = data_3569[test_idx, ]
    test_3571 = data_3571[test_idx, ]
    test_3579 = data_3579[test_idx, ]
    test_3582 = data_3582[test_idx, ]
    test_3860 = data_3860[test_idx, ]
    test_500 = data_500[test_idx, ]
    test_590 = data_590[test_idx, ]
    test_591 = data_591[test_idx, ]
    test_592 = data_592[test_idx, ]
    
    for (iiter in 1:n_cv_tv){
        print(paste("#", iiter, "cv train/val"))
        # split train --> train / validation with 40:40
        train_idx = sample(train_val_idx, size=n*0.4)
        val_idx = setdiff(train_val_idx, train_idx)
        
        # make train set
        train_gu = gu[train_idx, ]
        train_dong = dong[train_idx, ]
        
        # make validation set
        val_gu = gu[val_idx, ]
        val_dong = dong[val_idx, ]
        
        # ----------- Calculate train and validation error  ------------- #
        # to find the best hyper parameters
        
        ########################## random forest ##########################
        print(paste("#", iiter, "cv train/val", "random forest"))
        # train validation mse of random forest
        rf_tv_mse_gu = make_result_train_val_mse(model="rf", params=rf_params_gu, 
                                               train=train_gu, val=val_gu)
        rf_tv_mse_dong = make_result_train_val_mse(model="rf", params=rf_params_dong, 
                                               train=train_dong, val=val_dong)
        
        # combine train result
        rf_train_result = rbind(rf_train_result, rf_tv_mse_gu$train_mse_mat)
        rf_train_result = rbind(rf_train_result, rf_tv_mse_dong$train_mse_mat)
        
        # combine validation result
        rf_val_result = rbind(rf_val_result, rf_tv_mse_gu$val_mse_mat)
        rf_val_result = rbind(rf_val_result, rf_tv_mse_dong$val_mse_mat)
        
        ########################## elastic net ##########################
        print(paste("#", iiter, "cv train/val", "elastic net"))
        # train validation mse of elastic net
        elnet_tv_mse_gu = make_result_train_val_mse(model="elnet", params=elnet_params_gu, 
                                                train=train_gu, val=val_gu)
        elnet_tv_mse_dong = make_result_train_val_mse(model="elnet", params=elnet_params_dong, 
                                                  train=train_dong, val=val_dong)
        
        # combine train result
        elnet_train_result = rbind(elnet_train_result, elnet_tv_mse_gu$train_mse_mat)
        elnet_train_result = rbind(elnet_train_result, elnet_tv_mse_dong$train_mse_mat)
        
        # combine validation result
        elnet_val_result = rbind(elnet_val_result, elnet_tv_mse_gu$val_mse_mat)
        elnet_val_result = rbind(elnet_val_result, elnet_tv_mse_dong$val_mse_mat)
        
        ########################## xgboost ##########################
        print(paste("#", iiter, "cv train/val", "xgboost"))
        # train validation mse of xgboost
        xg_tv_mse_gu = make_result_train_val_mse(model="xg", params=xg_params_gu, 
                                                train=train_gu, val=val_gu)
        xg_tv_mse_dong = make_result_train_val_mse(model="xg", params=xg_params_dong, 
                                                  train=train_dong, val=val_dong)
        
        # combine train result
        xg_train_result = rbind(xg_train_result, xg_tv_mse_gu$train_mse_mat)
        xg_train_result = rbind(xg_train_result, xg_tv_mse_dong$train_mse_mat)
        
        # combine validation result
        xg_val_result = rbind(xg_val_result, xg_tv_mse_gu$val_mse_mat)
        xg_val_result = rbind(xg_val_result, xg_tv_mse_dong$val_mse_mat)
        
    }
    
    # ------------------- Find the best hyper parameters ------------------ #
    
    ########################## random forest ##########################
    rf_best_com_gu = find_best_combination(rf_tv_mse_gu$val_mse_mat)
    rf_best_com_dong = find_best_combination(rf_tv_mse_dong$val_mse_mat)
    
    ########################## elastic net ##########################
    elnet_best_com_gu = find_best_combination(elnet_tv_mse_gu$val_mse_mat)
    elnet_best_com_dong = find_best_combination(elnet_tv_mse_dong$val_mse_mat)
    
    ########################## xgboost ##########################
    xg_best_com_gu = find_best_combination(xg_tv_mse_gu$val_mse_mat)
    xg_best_com_dong = find_best_combination(xg_tv_mse_dong$val_mse_mat)
    
    # --------------- Make model with the best hyper parameters --------------- #
    
    # Make the best model with (train + val) data
    train_val_gu = gu[-test_idx, ]
    train_val_dong = dong[-test_idx, ]
    
    # Fitting with the best hyper parameters
    
    ########################## random forest ##########################
    print(paste("#", iter, "cv train/test", "random forest"))
    # Fitting
    rf_best_fit_gu = randomForest(train_val_gu$y ~., data=train_val_gu, 
                                    ntree=rf_best_com_gu$ntree, mtry=rf_best_com_gu$mtry)
    rf_best_fit_dong = randomForest(train_val_dong$y ~., data=train_val_dong, 
                                    ntree=rf_best_com_dong$ntree, mtry=rf_best_com_dong$mtry)
    
    # calculate mse with the best model
    rf_best_mse_gu = calculate_mse(model='rf', fit=rf_best_fit_gu, train_val_gu)
    rf_best_mse_dong = calculate_mse(model='rf', fit=rf_best_fit_dong, train_val_dong)
    
    # combine best mse
    rf_best_result = rbind(rf_best_result, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_best_mse_gu))
    rf_best_result = rbind(rf_best_result, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_best_mse_dong))
    
    # Calculate the test mse for gu
    rf_test_mse_gu_3508 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_3508)
    rf_test_mse_gu_3523 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_3523)
    rf_test_mse_gu_3569 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_3569)
    rf_test_mse_gu_3571 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_3571)
    rf_test_mse_gu_3579 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_3579)
    rf_test_mse_gu_3582 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_3582)
    rf_test_mse_gu_3860 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_3860)
    rf_test_mse_gu_500 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_500)
    rf_test_mse_gu_590 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_590)
    rf_test_mse_gu_591 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_591)
    rf_test_mse_gu_592 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_592)
    
    # Calculate the test mse for dong
    rf_test_mse_dong_3508 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_3508)
    rf_test_mse_dong_3523 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_3523)
    rf_test_mse_dong_3569 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_3569)
    rf_test_mse_dong_3571 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_3571)
    rf_test_mse_dong_3579 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_3579)
    rf_test_mse_dong_3582 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_3582)
    rf_test_mse_dong_3860 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_3860)
    rf_test_mse_dong_500 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_500)
    rf_test_mse_dong_590 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_590)
    rf_test_mse_dong_591 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_591)
    rf_test_mse_dong_592 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_592)
    
    # combine the test mse result for gu
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3508))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3523))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3569))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3571))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3579))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3582))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3860))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_500))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_590))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_591))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_592))
    
    # combine the test mse result for dong
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3508))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3523))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3569))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3571))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3579))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3582))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3860))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_500))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_590))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_591))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_592))
    
    
    
    ########################## elastic net ##########################
    print(paste("#", iter, "cv train/test", "elastic net"))
    elnet_best_fit_gu = glmnet(x=train_val_gu[, -1], y=train_val_gu[, 1], 
                                 alpha=elnet_best_com_gu$alpha, lambda=elnet_best_com_gu$alpha)
    elnet_best_fit_dong = glmnet(x=train_val_dong[, -1], y=train_val_dong[, 1], 
                                 alpha=elnet_best_com_dong$alpha, lambda=elnet_best_com_dong$alpha)
    
    # calculate mse with the best model
    elnet_best_mse_gu = calculate_mse(model='elnet', fit=elnet_best_fit_gu, train_val_gu)
    elnet_best_mse_dong = calculate_mse(model='elnet', fit=elnet_best_fit_dong, train_val_dong)
    
    # combine best mse result
    elnet_best_result = rbind(elnet_best_result, c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_best_mse_gu))
    elnet_best_result = rbind(elnet_best_result, c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_best_mse_dong))
    
    # Calculate the test mse for gu
    elnet_test_mse_gu_3508 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_3508)
    elnet_test_mse_gu_3523 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_3523)
    elnet_test_mse_gu_3569 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_3569)
    elnet_test_mse_gu_3571 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_3571)
    elnet_test_mse_gu_3579 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_3579)
    elnet_test_mse_gu_3582 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_3582)
    elnet_test_mse_gu_3860 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_3860)
    elnet_test_mse_gu_500 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_500)
    elnet_test_mse_gu_590 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_590)
    elnet_test_mse_gu_591 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_591)
    elnet_test_mse_gu_592 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_592)
    
    # Calculate the test mse for dong
    elnet_test_mse_dong_3508 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_3508)
    elnet_test_mse_dong_3523 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_3523)
    elnet_test_mse_dong_3569 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_3569)
    elnet_test_mse_dong_3571 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_3571)
    elnet_test_mse_dong_3579 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_3579)
    elnet_test_mse_dong_3582 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_3582)
    elnet_test_mse_dong_3860 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_3860)
    elnet_test_mse_dong_500 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_500)
    elnet_test_mse_dong_590 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_590)
    elnet_test_mse_dong_591 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_591)
    elnet_test_mse_dong_592 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_592)
    
    # combine the test mse result for gu
    elnet_test_result_gu = rbind(elnet_test_result_gu, 
                                 c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3508))
    elnet_test_result_gu = rbind(elnet_test_result_gu, 
                                 c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3523))
    elnet_test_result_gu = rbind(elnet_test_result_gu, 
                                 c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3569))
    elnet_test_result_gu = rbind(elnet_test_result_gu, 
                                 c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3571))
    elnet_test_result_gu = rbind(elnet_test_result_gu, 
                                 c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3579))
    elnet_test_result_gu = rbind(elnet_test_result_gu, 
                                 c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3582))
    elnet_test_result_gu = rbind(elnet_test_result_gu, 
                                 c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3860))
    elnet_test_result_gu = rbind(elnet_test_result_gu, 
                                 c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_500))
    elnet_test_result_gu = rbind(elnet_test_result_gu, 
                                 c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_590))
    elnet_test_result_gu = rbind(elnet_test_result_gu, 
                                 c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_591))
    elnet_test_result_gu = rbind(elnet_test_result_gu, 
                                 c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_592))
    
    # combine the test mse result for dong
    elnet_test_result_dong = rbind(elnet_test_result_dong, 
                                 c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3508))
    elnet_test_result_dong = rbind(elnet_test_result_dong, 
                                 c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3523))
    elnet_test_result_dong = rbind(elnet_test_result_dong, 
                                 c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3569))
    elnet_test_result_dong = rbind(elnet_test_result_dong, 
                                 c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3571))
    elnet_test_result_dong = rbind(elnet_test_result_dong, 
                                 c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3579))
    elnet_test_result_dong = rbind(elnet_test_result_dong, 
                                 c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3582))
    elnet_test_result_dong = rbind(elnet_test_result_dong, 
                                 c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3860))
    elnet_test_result_dong = rbind(elnet_test_result_dong, 
                                 c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_500))
    elnet_test_result_dong = rbind(elnet_test_result_dong, 
                                 c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_590))
    elnet_test_result_dong = rbind(elnet_test_result_dong, 
                                 c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_591))
    elnet_test_result_dong = rbind(elnet_test_result_dong, 
                                 c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_592))
    
    ########################## xgboost ##########################
    print(paste("#", iter, "cv train/test", "xgboost"))
    # Fitting
    xg_best_fit_gu = xgboost(data=as.matrix(train_val_gu[, -1]), label=train_val_gu[, 1], verbose=0,
                             nrounds=xg_best_com_gu$nrounds, 
                             max_depth=xg_best_com_gu$max_depth, 
                             eta=xg_best_com_gu$eta)
    xg_best_fit_dong = xgboost(data=as.matrix(train_val_dong[, -1]), label=train_val_dong[, 1], verbose=0,
                               nrounds=xg_best_com_dong$nrounds, 
                               max_depth=xg_best_com_dong$max_depth, 
                               eta=xg_best_com_dong$eta)
    
    # calculate mse with the best model
    xg_best_mse_gu = calculate_mse(model='xg', fit=xg_best_fit_gu, train_val_gu)
    xg_best_mse_dong = calculate_mse(model='xg', fit=xg_best_fit_dong, train_val_dong)
    
    # combine best mse result
    xg_best_result = rbind(xg_best_result, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                             xg_best_com_gu$eta, xg_best_mse_gu))
    xg_best_result = rbind(xg_best_result, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                             xg_best_com_dong$eta, xg_best_mse_dong))
    
    # Calculate the test mse for gu
    xg_test_mse_gu_3508 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_3508)
    xg_test_mse_gu_3523 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_3523)
    xg_test_mse_gu_3569 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_3569)
    xg_test_mse_gu_3571 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_3571)
    xg_test_mse_gu_3579 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_3579)
    xg_test_mse_gu_3582 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_3582)
    xg_test_mse_gu_3860 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_3860)
    xg_test_mse_gu_500 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_500)
    xg_test_mse_gu_590 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_590)
    xg_test_mse_gu_591 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_591)
    xg_test_mse_gu_592 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_592)
    
    # Calculate the test mse for dong
    xg_test_mse_dong_3508 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_3508)
    xg_test_mse_dong_3523 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_3523)
    xg_test_mse_dong_3569 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_3569)
    xg_test_mse_dong_3571 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_3571)
    xg_test_mse_dong_3579 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_3579)
    xg_test_mse_dong_3582 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_3582)
    xg_test_mse_dong_3860 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_3860)
    xg_test_mse_dong_500 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_500)
    xg_test_mse_dong_590 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_590)
    xg_test_mse_dong_591 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_591)
    xg_test_mse_dong_592 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_592)
    
    # combine the test mse result for gu
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_3508))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_3523))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_3569))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_3571))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_3579))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_3582))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_3860))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_500))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_590))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_591))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_592))
    
    # combine the test mse result for dong
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                   xg_best_com_dong$eta, xg_test_mse_dong_3508))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                   xg_best_com_dong$eta, xg_test_mse_dong_3523))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                   xg_best_com_dong$eta, xg_test_mse_dong_3569))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                   xg_best_com_dong$eta, xg_test_mse_dong_3571))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                   xg_best_com_dong$eta, xg_test_mse_dong_3579))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                   xg_best_com_dong$eta, xg_test_mse_dong_3582))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                   xg_best_com_dong$eta, xg_test_mse_dong_3860))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                   xg_best_com_dong$eta, xg_test_mse_dong_500))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                   xg_best_com_dong$eta, xg_test_mse_dong_590))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                   xg_best_com_dong$eta, xg_test_mse_dong_591))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                   xg_best_com_dong$eta, xg_test_mse_dong_592))
    
}


# --------------------------------------------------- #
#
# 4. Save results
#
# --------------------------------------------------- #

# convert matrix to data frame 
rf_train_result = as.data.frame(rf_train_result)
rf_val_result = as.data.frame(rf_val_result)
rf_best_result = as.data.frame(rf_best_result)
rf_test_result_gu = as.data.frame(rf_test_result_gu)
rf_test_result_dong = as.data.frame(rf_test_result_dong)

elnet_train_result = as.data.frame(elnet_train_result)
elnet_val_result = as.data.frame(elnet_val_result)
elnet_best_result = as.data.frame(elnet_best_result)
elnet_test_result_gu = as.data.frame(elnet_test_result_gu)
elnet_test_result_dong = as.data.frame(elnet_test_result_dong)

xg_train_result = as.data.frame(xg_train_result)
xg_val_result = as.data.frame(xg_val_result)
xg_best_result = as.data.frame(xg_best_result)
xg_test_result_gu = as.data.frame(xg_test_result_gu)
xg_test_result_dong = as.data.frame(xg_test_result_dong)

# naming columns
rf_colnames = c('ntree', 'mtry', 'mse')
elnet_colnames = c('alpha', 'lambda', 'mse')
xg_colnames = c('nrounds', 'max_depth', 'eta', 'mse')

colnames(rf_best_result) = rf_colnames
colnames(rf_test_result_gu) = rf_colnames
colnames(rf_test_result_dong) = rf_colnames

colnames(elnet_best_result) = elnet_colnames
colnames(elnet_test_result_gu) = elnet_colnames
colnames(elnet_test_result_dong) = elnet_colnames

colnames(xg_best_result) = xg_colnames
colnames(xg_test_result_gu) = xg_colnames
colnames(xg_test_result_dong) = xg_colnames

# make station list for separating each station
test_station_list = c(3508, 3523, 3569, 3571, 3579, 3582,
                      3860, 500, 590, 591, 592)

rf_test_result_gu$station = rep(test_station_list, n_cv_tt)
elnet_test_result_gu$station = rep(test_station_list, n_cv_tt)
xg_test_result_gu$station = rep(test_station_list, n_cv_tt)

rf_test_result_dong$station = rep(test_station_list, n_cv_tt)
elnet_test_result_dong$station = rep(test_station_list, n_cv_tt)
xg_test_result_dong$station = rep(test_station_list, n_cv_tt)

# make station list for separating each station
station_list = c('gu', 'dong')

rf_best_result$station = rep(station_list, n_cv_tt)
elnet_best_result$station = rep(station_list, n_cv_tt)
xg_best_result$station = rep(station_list, n_cv_tt)

rf_train_result$station = rep(station_list, each=nrow(rf_tv_mse_gu$train_mse_mat))
rf_val_result$station = rep(station_list, each=nrow(rf_tv_mse_gu$val_mse_mat))

elnet_train_result$station = rep(station_list, each=nrow(elnet_tv_mse_gu$train_mse_mat))
elnet_val_result$station = rep(station_list, each=nrow(elnet_tv_mse_gu$val_mse_mat))

xg_train_result$station = rep(station_list, each=nrow(xg_tv_mse_gu$train_mse_mat))
xg_val_result$station = rep(station_list, each=nrow(xg_tv_mse_gu$val_mse_mat))

# Integrate the test results
final_result = cbind(rf_test_result_gu$mse, rf_test_result_dong$mse, 
                     elnet_test_result_gu$mse, elnet_test_result_dong$mse, 
                     xg_test_result_gu$mse, xg_test_result_dong$mse, 
                     xg_test_result_gu$station)

colnames(final_result) = c("RF_GU", "RF_DONG",
                           "ELNET_GU", "ELNET_DONG", 
                           "XG_GU", "XG_DONG", "station")


write.csv(final_result, paste0("./result/변수추가o/", y_name, "/test/", y_name, "_unif_final_result.csv"), row.names=F)

# save results
# write.csv(rf_train_result, "./result/변수추가o/rent/test/rent_rf_train_result.csv", row.names=F)
# write.csv(rf_val_result, "./result/변수추가o/rent/test/rent_rf_val_result.csv", row.names=F)
# write.csv(rf_best_result, "./result/변수추가o/rent/test/rent_rf_best_result.csv", row.names=F)
# write.csv(rf_test_result_gu, "./result/변수추가o/rent/test/rent_rf_test_result_gu.csv", row.names=F)
# write.csv(rf_test_result_dong, "./result/변수추가o/rent/test/rent_rf_test_result_dong.csv", row.names=F)
# 
# write.csv(elnet_train_result, "./result/변수추가o/rent/test/rent_elnet_train_result.csv", row.names=F)
# write.csv(elnet_val_result, "./result/변수추가o/rent/test/rent_elnet_val_result.csv", row.names=F)
# write.csv(elnet_best_result, "./result/변수추가o/rent/test/rent_elnet_best_result.csv", row.names=F)
# write.csv(elnet_test_result_gu, "./result/변수추가o/rent/test/rent_elnet_test_result_gu.csv", row.names=F)
# write.csv(elnet_test_result_dong, "./result/변수추가o/rent/test/rent_elnet_test_result_dong.csv", row.names=F)
# 
# write.csv(xg_train_result, "./result/변수추가o/rent/test/rent_xg_train_result.csv", row.names=F)
# write.csv(xg_val_result, "./result/변수추가o/rent/test/rent_xg_val_result.csv", row.names=F)
# write.csv(xg_best_result, "./result/변수추가o/rent/test/rent_xg_best_result.csv", row.names=F)
# write.csv(xg_test_result_gu, "./result/변수추가o/rent/test/rent_xg_test_result_gu.csv", row.names=F)
# write.csv(xg_test_result_dong, "./result/변수추가o/rent/test/rent_xg_test_result_dong.csv", row.names=F)

end_time = Sys.time()
running_time = end_time - start_time
print(running_time)

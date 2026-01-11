library(ggplot2)
library(readr)
library(bsts)
library(Metrics)
library(dplyr)
library(forecast)
library(fpp3)
library(tsibble)
library(fable)
library(readxl)
library(patchwork)

setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/Data')
cpu_data <- read_xlsx('GCPU_Data.xlsx')[1:282,1:2]
cpu_data$date <- as.Date(cpu_data$date)
cpu_data$date <- ymd(cpu_data$date)
cpu <- unlist(cpu_data[,2])

sum(is.na(cpu_data))
colSums(is.na(cpu_data))
cpu_data[is.na(cpu_data)] <- 0
sum(is.na(cpu_data))

covariates_reduced <- read_xlsx('CPU_Data_Reduced.xlsx') %>% select(-cpu_index)  
covariates_reduced <- covariates_reduced[154:nrow(covariates_reduced),]

##################################################### Evaluation Function #####################################################


evaluate <- function(test,pred,model){
  MAPE <- mape(test,pred)*100
  SMAPE <- smape(test,pred)
  MAE <- mae(test,pred)
  MASE <- mase(test,pred)
  RMSE <- rmse(test,pred)
  
  return(tibble('MODEL' = model,
                'MAPE' = MAPE,
                'SMAPE' = SMAPE,
                'MAE' = MAE,
                'MASE' = MASE,
                'RMSE' = RMSE))
}


##################################################### Horizon 3 #####################################################

n = 3
set.seed(100)

train_3 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_3 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_3 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_3 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_3 <- tibble()  
predict_3 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_3) + 1:length(test_3)]))  


# ARFIMA
arfima_3 <- arfima(train_3$CPU_US, xreg = train_reg_reduced_3[,c(1:66,68:69,71,73:74,76:77,88)])  # variables dropped to avoid multicollinearity
arfima_3_pred <- forecast(arfima_3, h = n, xreg = test_reg_reduced_3[,c(1:66,68:69,71,73:74,76:77,88)])
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, arfima_3_pred$mean, model = 'ARFIMA'))
predict_3 <- predict_3 %>% mutate('ARFIMA' = arfima_3_pred$mean)


# ARIMA
arima_3 <- auto.arima(train_3$CPU_US, xreg = train_reg_reduced_3[,c(1:66,68:69,71,73:74,76:77,79:80,86,88)])  # variables dropped to avoid multicollinearity
arima_3_pred <- forecast(arima_3, h = n, xreg = test_reg_reduced_3[,c(1:66,68:69,71,73:74,76:77,79:80,86,88)])
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, arima_3_pred$mean,  model = 'ARIMA'))
predict_3 <- predict_3 %>% mutate('ARIMA' = arima_3_pred$mean)


# ARNN
arnn_3 <- nnetar(train_3$CPU_US, xreg = train_reg_reduced_3, MaxNWts = 5500)
arnn_3_pred <- forecast(arnn_3, h = n, xreg = test_reg_reduced_3)
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, arnn_3_pred$mean,  model = 'ARNN'))
predict_3 <- predict_3 %>% mutate('ARNN' = arnn_3_pred$mean)

# BSTS
ss <- AddLocalLinearTrend(list(), train_3$CPU_US)
bsts_3 <- bsts(cpu_index ~ ., 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_3$CPU_US, train_reg_reduced_3)) %>% rename('cpu_index' = V1))
bsts_3_pred <- predict(bsts_3, horizon = n, burn = SuggestBurn(.1, bsts_3), newdata = test_reg_reduced_3)
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, bsts_3_pred$mean, model = 'BSTS'))
predict_3 <- predict_3 %>% mutate('BSTS' = bsts_3_pred$mean)


setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/Dataset_Deep_Learning_Models_Forecasts/Models - Macroeconomic + Google/US Alternate')


# NBEATS
cpu_3_NBEATS <- unlist(read_csv('NBEATS_3.csv')[2])  
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, cpu_3_NBEATS, model = 'NBEATS'))

# NHiTS
cpu_3_NHiTS <- unlist(read_csv('NHiTS_3.csv')[2])
model_evaluate_1 <- rbind(model_evaluate_3, evaluate(test_3, cpu_3_NHiTS, model = 'NHiTS'))

# DLinear
cpu_3_Dlinear <- unlist(read_csv('DLinear_3.csv')[2]) 
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, cpu_3_Dlinear, model = 'DLinear'))

# NLinear
cpu_3_Nlinear <- unlist(read_csv('NLinear_3.csv')[2]) 
model_evaluate_3 <- rbind(model_evaluate_3, evaluate(test_3, cpu_3_Nlinear, model = 'NLinear'))

write.csv(model_evaluate_3, 'Horizon 3.csv', row.names = FALSE)
write.csv(predict_3, 'Forecast 3.csv', row.names = FALSE)

##################################################### Horizon 6 #####################################################

n = 6
set.seed(100)

train_6 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_6 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_6 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_6 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_6 <- tibble()  
predict_6 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_6) + 1:length(test_6)]))  


# ARFIMA
arfima_6 <- arfima(train_6$CPU_US, xreg = train_reg_reduced_6[,c(1:66,68:69,71,73:74,76:77,88)])
arfima_6_pred <- forecast(arfima_6, h = n, xreg = test_reg_reduced_6[,c(1:66,68:69,71,73:74,76:77,88)])
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, arfima_6_pred$mean, model = 'ARFIMA'))
predict_6 <- predict_6 %>% mutate('ARFIMA' = arfima_6_pred$mean)


# ARIMA
arima_6 <- auto.arima(train_6$CPU_US, xreg = train_reg_reduced_6[,c(1:66,68:69,71,73:74,76:77,79:80,86,88)])
arima_6_pred <- forecast(arima_6, h = n, xreg = test_reg_reduced_6[,c(1:66,68:69,71,73:74,76:77,79:80,86,88)])
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, arima_6_pred$mean,  model = 'ARIMA'))
predict_6 <- predict_6 %>% mutate('ARIMA' = arima_6_pred$mean)


# ARNN
arnn_6 <- nnetar(train_6$CPU_US, xreg = train_reg_reduced_6, MaxNWts = 5700)
arnn_6_pred <- forecast(arnn_6, h = n, xreg = test_reg_reduced_6)
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, arnn_6_pred$mean,  model = 'ARNN'))
predict_6 <- predict_6 %>% mutate('ARNN' = arnn_6_pred$mean)


# BSTS
ss <- AddLocalLinearTrend(list(), train_6$CPU_US)
ss <- AddSeasonal(ss, train_6$CPU_US, nseasons = 12)
bsts_6 <- bsts(cpu_index ~ .,
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100,
               data = as.data.frame(cbind(train_6$CPU_US, train_reg_reduced_6)) %>% rename('cpu_index' = V1))
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6)
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, bsts_6_pred$mean, model = 'BSTS'))
predict_6 <- predict_6 %>% mutate('BSTS' = bsts_6_pred$mean)


setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/Dataset_Deep_Learning_Models_Forecasts/Models - Macroeconomic + Google/US Alternate')


# NBEATS
cpu_6_NBEATS <- unlist(read_csv('NBEATS_6.csv')[2])  
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, cpu_6_NBEATS, model = 'NBEATS'))

# NHiTS
cpu_6_NHiTS <- unlist(read_csv('NHiTS_6.csv')[2])
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, cpu_6_NHiTS, model = 'NHiTS'))

# DLinear
cpu_6_Dlinear <- unlist(read_csv('DLinear_6.csv')[2]) 
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, cpu_6_Dlinear, model = 'DLinear'))

# NLinear
cpu_6_Nlinear <- unlist(read_csv('NLinear_6.csv')[2]) 
model_evaluate_6 <- rbind(model_evaluate_6, evaluate(test_6, cpu_6_Nlinear, model = 'NLinear'))

write.csv(model_evaluate_6, 'Horizon 6.csv', row.names = FALSE)
write.csv(predict_6, 'Forecast 6.csv', row.names = FALSE)

##################################################### Horizon 12 #####################################################

n = 12
set.seed(100)

train_12 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_12 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_12 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_12 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_12 <- tibble()  
predict_12 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_12) + 1:length(test_12)]))  


# ARFIMA
arfima_12 <- arfima(train_12$CPU_US, xreg = train_reg_reduced_12[,c(1:66,68:69,71,73:74,76:77,88)])
arfima_12_pred <- forecast(arfima_12, h = n, xreg = test_reg_reduced_12[,c(1:66,68:69,71,73:74,76:77,88)])
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, arfima_12_pred$mean, model = 'ARFIMA'))
predict_12 <- predict_12 %>% mutate('ARFIMA' = arfima_12_pred$mean)


# ARIMA
arima_12 <- auto.arima(train_12$CPU_US, xreg = train_reg_reduced_12[,c(1:66,68:69,71,73:74,76:77,79:80,86,88)])
arima_12_pred <- forecast(arima_12, h = n, xreg = test_reg_reduced_12[,c(1:66,68:69,71,73:74,76:77,79:80,86,88)])
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, arima_12_pred$mean,  model = 'ARIMA'))
predict_12 <- predict_12 %>% mutate('ARIMA' = arima_12_pred$mean)


# ARNN
arnn_12 <- nnetar(train_12$CPU_US, xreg = train_reg_reduced_12, MaxNWts = 5600)
arnn_12_pred <- forecast(arnn_12, h = n, xreg = test_reg_reduced_12)
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, arnn_12_pred$mean,  model = 'ARNN'))
predict_12 <- predict_12 %>% mutate('ARNN' = arnn_12_pred$mean)


# BSTS 
ss <- AddLocalLinearTrend(list(), train_12$CPU_US)

bsts_12 <- bsts(cpu_index ~ .,
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                data = as.data.frame(cbind(train_12$CPU_US, train_reg_reduced_12)) %>% rename('cpu_index' = V1))
plot(bsts_12,'coefficients')
bsts_12_pred <- predict(bsts_12, horizon = n, burn = SuggestBurn(.1, bsts_12), newdata = test_reg_reduced_12)
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, bsts_12_pred$mean, model = 'BSTS'))
predict_12 <- predict_12 %>% mutate('BSTS' = bsts_12_pred$mean)


setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/Dataset_Deep_Learning_Models_Forecasts/Models - Macroeconomic + Google/US Alternate')


# NBEATS
cpu_12_NBEATS <- unlist(read_csv('NBEATS_12.csv')[2])  
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, cpu_12_NBEATS, model = 'NBEATS'))

# NHiTS
cpu_12_NHiTS <- unlist(read_csv('NHiTS_12.csv')[2])
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, cpu_12_NHiTS, model = 'NHiTS'))

# DLinear
cpu_12_Dlinear <- unlist(read_csv('DLinear_12.csv')[2]) 
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, cpu_12_Dlinear, model = 'DLinear'))

# NLinear
cpu_12_Nlinear <- unlist(read_csv('NLinear_12.csv')[2]) 
model_evaluate_12 <- rbind(model_evaluate_12, evaluate(test_12, cpu_12_Nlinear, model = 'NLinear'))

write.csv(model_evaluate_12, 'Horizon 12.csv', row.names = FALSE)
write.csv(predict_12, 'Forecast 12.csv', row.names = FALSE)

##################################################### Horizon 24 #####################################################

n = 24
set.seed(100)

train_24 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_24 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_24 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_24 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate_24 <- tibble()  
predict_24 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_24) + 1:length(test_24)]))  


# ARFIMA
arfima_24 <- arfima(train_24$CPU_US, xreg = train_reg_reduced_24[,c(1:66,68:69,71,73:74,76:77,88)])
arfima_24_pred <- forecast(arfima_24, h = n, xreg = test_reg_reduced_24[,c(1:66,68:69,71,73:74,76:77,88)])
arfima_24 <- arfima(train_24$CPU_US)
arfima_24_pred <- forecast(arfima_24, h = n)
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, arfima_24_pred$mean, model = 'ARFIMA'))
predict_24 <- predict_24 %>% mutate('ARFIMA' = arfima_24_pred$mean)


# ARIMA
arima_24 <- auto.arima(train_24$CPU_US, xreg = train_reg_reduced_24[,c(1:66,68:69,71,73:74,76:77,79:80,86,88)])
arima_24_pred <- forecast(arima_24, h = n, xreg = test_reg_reduced_24[,c(1:66,68:69,71,73:74,76:77,79:80,86,88)])
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, arima_24_pred$mean,  model = 'ARIMA'))
predict_24 <- predict_24 %>% mutate('ARIMA' = arima_24_pred$mean)


# ARNN
arnn_24 <- nnetar(train_24$CPU_US, xreg = train_reg_reduced_24[,1:90], MaxNWts = 5500)
arnn_24_pred <- forecast(arnn_24, h = n, xreg = test_reg_reduced_24[,1:90])
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, arnn_24_pred$mean,  model = 'ARNN'))
predict_24 <- predict_24 %>% mutate('ARNN' = arnn_24_pred$mean)


# BSTS
set.seed(100)
ss <- AddLocalLevel(list(), train_24$CPU_US)
ss <- AddAutoAr(ss, train_24$CPU_US)
ss <- AddSeasonal(ss, train_24$CPU_US, nseasons = 12)
bsts_24 <- bsts(cpu_index ~ .,
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100,
                data = as.data.frame(cbind(train_24$CPU_US, train_reg_reduced_24)) %>% rename('cpu_index' = V1))
bsts_24_pred <- predict(bsts_24, horizon = n, burn = SuggestBurn(.1, bsts_24), newdata = test_reg_reduced_24)
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, bsts_24_pred$mean, model = 'BSTS'))
predict_24 <- predict_24 %>% mutate('BSTS' = bsts_24_pred$mean)


setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/Dataset_Deep_Learning_Models_Forecasts/Models - Macroeconomic + Google/US Alternate')

# NBEATS
cpu_24_NBEATS <- unlist(read_csv('NBEATS_24.csv')[2])  
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, cpu_24_NBEATS, model = 'NBEATS'))

# NHiTS
cpu_24_NHiTS <- unlist(read_csv('NHiTS_24.csv')[2])
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, cpu_24_NHiTS, model = 'NHiTS'))

# DLinear
cpu_24_Dlinear <- unlist(read_csv('DLinear_24.csv')[2]) 
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, cpu_24_Dlinear, model = 'DLinear'))

# NLinear
cpu_24_Nlinear <- unlist(read_csv('NLinear_24.csv')[2]) 
model_evaluate_24 <- rbind(model_evaluate_24, evaluate(test_24, cpu_24_Nlinear, model = 'NLinear'))

write.csv(model_evaluate_24, 'Horizon 24.csv', row.names = FALSE)
write.csv(predict_24, 'Forecast 24.csv', row.names = FALSE)

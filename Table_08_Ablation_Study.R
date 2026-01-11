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

##################################################### Primary US #####################################################

setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/Data')
cpu_data <- read_xlsx('CPU_Data.xlsx')
cpu_data$Date <- as.Date(cpu_data$Date)
cpu_data$Date <- ydm(cpu_data$Date)
covariates_reduced <- read_xlsx('CPU_Data_Reduced.xlsx') %>% select(-cpu_index)  


sum(is.na(cpu_data))
colSums(is.na(cpu_data))
cpu_data[is.na(cpu_data)] <- 0
sum(is.na(cpu_data))

cpu <- cpu_data[,139]
cpu <- unlist(cpu)


# h = 3
n = 3
set.seed(100)

train_3 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_3 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_3 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_3 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate <- tibble()  
predict_3 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_3) + 1:length(test_3)]))  


# BSTS
ss <- AddLocalLevel(list(), train_3$cpu_index)
ss <- AddAutoAr(ss, train_3$cpu_index)
ss <- AddSeasonal(ss, train_3$cpu_index, nseasons = 12)
ss <- AddDynamicRegression(ss, train_3$cpu_index ~ train_reg_reduced_3)
bsts_3 <- bsts(train_3$cpu_index, 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100)
bsts_3_pred <- predict(bsts_3, horizon = n, burn = SuggestBurn(.1, bsts_3), newdata = test_reg_reduced_3)
model_evaluate <- rbind(model_evaluate, evaluate(test_3, bsts_3_pred$mean, model = 'BSTS - US - Primary - 3'))
predict_3 <- predict_3 %>% mutate('BSTS - US - Primary' = bsts_3_pred$mean)
write.csv(predict_3, 'Forecast 3.csv', row.names = FALSE)


# h = 6
n = 6
set.seed(100)

train_6 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_6 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_6 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_6 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

predict_6 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_6) + 1:length(test_6)]))  


# BSTS
ss <- AddLocalLinearTrend(list(), train_6$cpu_index)
ss <- AddAutoAr(ss, train_6$cpu_index)
ss <- AddSeasonal(ss, train_6$cpu_index, nseasons = 12)
ss <- AddDynamicRegression(ss, train_6$cpu_index ~ train_reg_reduced_6)
bsts_6 <- bsts(train_6$cpu_index, 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100)
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6)
model_evaluate <- rbind(model_evaluate, evaluate(test_6, bsts_6_pred$mean, model = 'BSTS - US - Primary - 6'))
predict_6 <- predict_6 %>% mutate('BSTS - US - Primary' = bsts_6_pred$mean)
write.csv(predict_6, 'Forecast 6.csv', row.names = FALSE)


# h = 12
n = 12
set.seed(100)

train_12 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_12 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_12 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_12 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

predict_12 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_12) + 1:length(test_12)]))  


# BSTS 
ss <- AddLocalLinearTrend(list(), train_12$cpu_index)
ss <- AddDynamicRegression(ss, train_12$cpu_index ~ train_reg_reduced_12)
bsts_12 <- bsts(train_12$cpu_index, 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100)
bsts_12_pred <- predict(bsts_12, horizon = n, burn = SuggestBurn(.1, bsts_12), newdata = test_reg_reduced_12)
model_evaluate <- rbind(model_evaluate, evaluate(test_12, bsts_12_pred$mean, model = 'BSTS - US - Primary - 12'))
predict_12 <- predict_12 %>% mutate('BSTS - US - Primary' = bsts_12_pred$mean)
write.csv(predict_12, 'Forecast 12.csv', row.names = FALSE)


# h = 24
n = 24
set.seed(100)

train_24 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_24 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_24 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_24 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

predict_24 <- tibble(Date = as.Date(cpu_data$Date[nrow(train_24) + 1:length(test_24)]))  


# BSTS
ss <- AddLocalLevel(list(), train_24$cpu_index)
ss <- AddLocalLinearTrend(ss, train_24$cpu_index)
ss <- AddAutoAr(ss, train_24$cpu_index)
ss <- AddSeasonal(ss, train_24$cpu_index, nseasons = 12)
ss <- AddDynamicRegression(ss, train_24$cpu_index ~ train_reg_reduced_24)
bsts_24 <- bsts(train_24$cpu_index, 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100)
bsts_24_pred <- predict(bsts_24, horizon = n, burn = SuggestBurn(.1, bsts_24), newdata = test_reg_reduced_24)
model_evaluate <- rbind(model_evaluate, evaluate(test_24, bsts_24_pred$mean, model = 'BSTS - US - Primary - 24'))
predict_24 <- predict_24 %>% mutate('BSTS - US - Primary' = bsts_24_pred$mean)
write.csv(predict_24, 'Forecast 24.csv', row.names = FALSE)

write.csv(model_evaluate, 'US Primary.csv', row.names = FALSE)


##################################################### Alternate US #####################################################

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


# h = 3
n = 3
set.seed(100)

train_3 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_3 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_3 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_3 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate <- tibble()  
predict_3 <- tibble(Date = as.Date(cpu_data$date[nrow(train_3) + 1:length(test_3)]))  


# BSTS
ss <- AddLocalLinearTrend(list(), train_3$CPU_US)
ss <- AddDynamicRegression(ss, train_3$CPU_US ~ train_reg_reduced_3)
bsts_3 <- bsts(train_3$CPU_US, 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100)
bsts_3_pred <- predict(bsts_3, horizon = n, burn = SuggestBurn(.1, bsts_3), newdata = test_reg_reduced_3)
model_evaluate <- rbind(model_evaluate, evaluate(test_3, bsts_3_pred$mean, model = 'BSTS - US - Alterante - 3'))
predict_3 <- predict_3 %>% mutate('BSTS - US - Alterante' = bsts_3_pred$mean)
write.csv(predict_3, 'Forecast 3.csv', row.names = FALSE)


# h = 6
n = 6
set.seed(100)

train_6 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_6 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_6 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_6 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

predict_6 <- tibble(Date = as.Date(cpu_data$date[nrow(train_6) + 1:length(test_6)]))  


# BSTS
ss <- AddLocalLinearTrend(list(), train_6$CPU_US)
ss <- AddSeasonal(ss, train_6$CPU_US, nseasons = 12)
ss <- AddDynamicRegression(ss, train_6$CPU_US ~ train_reg_reduced_6)
bsts_6 <- bsts(train_6$CPU_US,
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100)
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6)
model_evaluate <- rbind(model_evaluate, evaluate(test_6, bsts_6_pred$mean, model = 'BSTS - US - Alterante - 6'))
predict_6 <- predict_6 %>% mutate('BSTS - US - Alterante - 6' = bsts_6_pred$mean)
write.csv(predict_6, 'Forecast 6.csv', row.names = FALSE)


# h = 12
n = 12
set.seed(100)

train_12 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_12 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_12 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_12 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

predict_12 <- tibble(Date = as.Date(cpu_data$date[nrow(train_12) + 1:length(test_12)]))  

# BSTS 
ss <- AddLocalLinearTrend(list(), train_12$CPU_US)
ss <- AddDynamicRegression(ss, train_12$CPU_US ~ train_reg_reduced_12)
bsts_12 <- bsts(train_12$CPU_US, 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100)
bsts_12_pred <- predict(bsts_12, horizon = n, burn = SuggestBurn(.1, bsts_12), newdata = test_reg_reduced_12)
model_evaluate <- rbind(model_evaluate, evaluate(test_12, bsts_12_pred$mean, model = 'BSTS - US - Alterante - 12'))
predict_12 <- predict_12 %>% mutate('BSTS - US - Alterante' = bsts_12_pred$mean)
write.csv(predict_12, 'Forecast 12.csv', row.names = FALSE)


# h = 24
n = 24
set.seed(100)

train_24 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_24 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_24 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_24 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

predict_24 <- tibble(Date = as.Date(cpu_data$date[nrow(train_24) + 1:length(test_24)]))  


# BSTS
set.seed(100)
ss <- AddLocalLevel(list(), train_24$CPU_US)
ss <- AddAutoAr(ss, train_24$CPU_US)
ss <- AddSeasonal(ss, train_24$CPU_US, nseasons = 12)
ss <- AddDynamicRegression(ss, train_24$CPU_US ~ train_reg_reduced_24)
bsts_24 <- bsts(train_24$CPU_US, 
                state.specification = ss,
                niter = 1000,
                ping = 100,
                seed = 100)
bsts_24_pred <- predict(bsts_24, horizon = n, burn = SuggestBurn(.1, bsts_24), newdata = test_reg_reduced_24)
model_evaluate <- rbind(model_evaluate, evaluate(test_24, bsts_24_pred$mean, model = 'BSTS - US - Alterante - 24'))
predict_24 <- predict_24 %>% mutate('BSTS - US - Alterante' = bsts_24_pred$mean)
write.csv(predict_24, 'Forecast 24.csv', row.names = FALSE)

write.csv(model_evaluate, 'US Alternate.csv', row.names = FALSE)


##################################################### Global #####################################################

setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/Data')
cpu_data <- read_xlsx('GCPU_Data.xlsx')[1:282,c(1,4)]
cpu_data$date <- as.Date(cpu_data$date)
cpu_data$date <- ymd(cpu_data$date)
cpu <- unlist(cpu_data[,2])

sum(is.na(cpu_data))
colSums(is.na(cpu_data))
cpu_data[is.na(cpu_data)] <- 0
sum(is.na(cpu_data))

covariates_reduced <- read_xlsx('CPU_Data_Reduced.xlsx') %>% select(-cpu_index)  
covariates_reduced <- covariates_reduced[154:nrow(covariates_reduced),]


# h = 3
n = 3
set.seed(100)

train_3 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_3 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_3 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_3 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

model_evaluate <- tibble()  
predict_3 <- tibble(Date = as.Date(cpu_data$date[nrow(train_3) + 1:length(test_3)]))  


# BSTS
ss <- AddAutoAr(list(), train_3$`GCPU(PPP-adjusted GDP)`)
ss <- AddDynamicRegression(ss, train_3$`GCPU(PPP-adjusted GDP)` ~ train_reg_reduced_3)
bsts_3 <- bsts(train_3$`GCPU(PPP-adjusted GDP)`, 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100)
bsts_3_pred <- predict(bsts_3, horizon = n, burn = SuggestBurn(.1, bsts_3), newdata = test_reg_reduced_3)
model_evaluate <- rbind(model_evaluate, evaluate(test_3, bsts_3_pred$mean, model = 'BSTS - Global - 3'))
predict_3 <- predict_3 %>% mutate('BSTS - Global - 3' = bsts_3_pred$mean)
write.csv(predict_3, 'Forecast 3.csv', row.names = FALSE)


# h = 6
n = 6
set.seed(100)

train_6 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_6 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_6 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_6 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

predict_6 <- tibble(Date = as.Date(cpu_data$date[nrow(train_6) + 1:length(test_6)])) 


# BSTS
ss <- AddLocalLevel(list(), train_6$`GCPU(PPP-adjusted GDP)`)
ss <- AddAutoAr(ss, train_6$`GCPU(PPP-adjusted GDP)`)
ss <- AddDynamicRegression(ss, train_6$`GCPU(PPP-adjusted GDP)` ~ train_reg_reduced_6)
bsts_6 <- bsts(train_6$`GCPU(PPP-adjusted GDP)`, 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100)
bsts_6_pred <- predict(bsts_6, horizon = n, burn = SuggestBurn(.1, bsts_6), newdata = test_reg_reduced_6)
model_evaluate <- rbind(model_evaluate, evaluate(test_6, bsts_6_pred$mean, model = 'BSTS - Global - 6'))
predict_6 <- predict_6 %>% mutate('BSTS - Global - 6' = bsts_6_pred$mean)
write.csv(predict_6, 'Forecast 6.csv', row.names = FALSE)


# h = 12
n = 12
set.seed(100)

train_12 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_12 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_12 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_12 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

predict_12 <- tibble(Date = as.Date(cpu_data$date[nrow(train_12) + 1:length(test_12)]))  


# BSTS 
ss <- AddLocalLevel(list(), train_12$`GCPU(PPP-adjusted GDP)`)
ss <- AddAutoAr(ss, train_12$`GCPU(PPP-adjusted GDP)`)
ss <- AddDynamicRegression(ss, train_12$`GCPU(PPP-adjusted GDP)` ~ train_reg_reduced_12)
bsts_12 <- bsts(train_12$`GCPU(PPP-adjusted GDP)`, 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100)
bsts_12_pred <- predict(bsts_12, horizon = n, burn = SuggestBurn(.1, bsts_12), newdata = test_reg_reduced_12)
model_evaluate <- rbind(model_evaluate, evaluate(test_12, bsts_12_pred$mean, model = 'BSTS - Global - 12'))
predict_12 <- predict_12 %>% mutate('BSTS - Global - 12' = bsts_12_pred$mean)
write.csv(predict_12, 'Forecast 12.csv', row.names = FALSE)


# h = 24
n = 24
set.seed(100)

train_24 <- cpu_data[1:(nrow(cpu_data) - n), 2:ncol(cpu_data)]
train_reg_reduced_24 <- as.matrix(covariates_reduced[1:(nrow(cpu_data) - n), ])

test_24 <- cpu[(length(cpu) - n + 1):length(cpu)]
test_reg_reduced_24 <- as.matrix(covariates_reduced[(nrow(covariates_reduced) - n + 1):nrow(covariates_reduced),])

predict_24 <- tibble(Date = as.Date(cpu_data$date[nrow(train_24) + 1:length(test_24)]))  


# BSTS
ss <- AddLocalLinearTrend(list(), train_24$`GCPU(PPP-adjusted GDP)`)
ss <- AddAutoAr(ss, train_24$`GCPU(PPP-adjusted GDP)`)
ss <- AddSeasonal(ss, train_24$`GCPU(PPP-adjusted GDP)`, nseasons = 12)
ss <- AddDynamicRegression(ss, train_24$`GCPU(PPP-adjusted GDP)` ~ train_reg_reduced_24)
bsts_24 <- bsts(train_24$`GCPU(PPP-adjusted GDP)`, 
               state.specification = ss,
               niter = 1000,
               ping = 100,
               seed = 100)
bsts_24_pred <- predict(bsts_24, horizon = n, burn = SuggestBurn(.1, bsts_24), newdata = test_reg_reduced_24)
model_evaluate <- rbind(model_evaluate, evaluate(test_24, bsts_24_pred$mean, model = 'BSTS - Global - 24'))
predict_24 <- predict_24 %>% mutate('BSTS - Global - 24' = bsts_24_pred$mean)
write.csv(predict_24, 'Forecast 24.csv', row.names = FALSE)
write.csv(model_evaluate, 'Global.csv', row.names = FALSE)

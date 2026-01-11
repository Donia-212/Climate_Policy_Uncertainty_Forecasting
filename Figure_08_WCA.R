library(ggplot2)
library(readr)
library(dplyr)
library(tsibble)
library(fable)
library(readxl)
library(stats)
library(patchwork)
library(gridExtra)
library(biwavelet)
library(reshape2)

##################################################### Wavelet Coherence #####################################################

setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/Data')
setwd('C:/Users/d2o0n/OneDrive - Sorbonne University Abu Dhabi/Documents/Course Materials/Intership/Bayesian Neural Network')
cpu_data <- read_xlsx('CPU_Data.xlsx')
cpu_data$Date <- as.Date(cpu_data$Date)
cpu_data$Date <- ydm(cpu_data$Date)
covariates_reduced <- read_xlsx('CPU_Data_Reduced.xlsx') %>% select(-cpu_index)  

sum(is.na(cpu_data))
colSums(is.na(cpu_data))
cpu_data[is.na(cpu_data)] <- 0
sum(is.na(cpu_data))

par(mfrow = c(1,1), mfcol = c(1,1))
par(mgp = c(3, 1, 0))
par(mar = c(5.1, 4.1, 4.1, 2.1))


cpu <- data$cpu_index
train_cpu <- cpu[1:(length(cpu) - n)]
train_cpu <- cbind(Date = 1:length(train_cpu), cpu = train_cpu)

wc <- wtc(cbind(Date = 1:nrow(train_24),train_24[,2]), cbind(Date = 1:nrow(train_24),train_reg_reduced_24[,"capr"]), nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "OG US CPU vs CAP/Rent")

wc <- wtc(cbind(Date = 1:nrow(train_24),train_24[,2]), cbind(Date = 1:nrow(train_24),train_reg_reduced_24[,"prfi_gdp"]), nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "OG US CPU vs PRFI_GDP")

wc <- wtc(cbind(Date = 1:nrow(train_24),train_24[,2]), cbind(Date = 1:nrow(train_24),train_reg_reduced_24[,"BCI"]), nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "OG US CPU vs BCI")

wc <- wtc(cbind(Date = 1:nrow(train_24),train_24[,2]), cbind(Date = 1:nrow(train_24),train_reg_reduced_24[,"climate policy: (Worldwide)"]), nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "OG US CPU vs Climate Policy")

wc <- wtc(cbind(Date = 1:nrow(train_24),train_24[,2]), cbind(Date = 1:nrow(train_24),train_reg_reduced_24[,"carbon emissions: (Worldwide)"]), nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "OG US CPU vs Carbon Emissions")

wc <- wtc(cbind(Date = 1:nrow(train_24),train_24[,2]), cbind(Date = 1:nrow(train_24),train_reg_reduced_24[,"climate action: (Worldwide)"]), nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "OG US CPU vs Climate Action")

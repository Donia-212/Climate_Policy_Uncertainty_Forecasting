library(tidyverse)
library(readxl)
library(RTransferEntropy)
library(NlinTS)
library(biwavelet)
library(reshape2)

par(mfrow = c(1,1), mfcol = c(1,1))
par(mgp = c(3, 1, 0))
par(mar = c(5.1, 4.1, 4.1, 2.1))

setwd('Climate_Policy_Uncertainty_Forecasting/Dataset/Data')
data <- read_excel('CPU_Data_Reduced.xlsx')   

n = 24
set.seed(100)

cpu <- data$cpu_index
train_cpu <- cpu[1:(length(cpu) - n)]
train_cpu <- cbind(Date = 1:length(train_cpu), cpu = train_cpu)

# Carbon Credits
carbon_credits <- data$`carbon credits: (Worldwide)`
train_carbon_credits <- carbon_credits[1:(length(carbon_credits) - n)]
train_carbon_credits <- cbind(Date = 1:length(train_carbon_credits), carbon_credits = train_carbon_credits)
# Transfer Entropy
calc_te(train_carbon_credits[,2],train_cpu[,2],1,1)
transfer_entropy(train_carbon_credits[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_carbon_credits[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_carbon_credits[,2], main = 'Carbon Credits')
# Wavelet
wc <- wtc(train_cpu, train_carbon_credits, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Carbon Credits")


# Carbon Emissions
carbon_emissions <- data$`carbon emissions: (Worldwide)`
train_carbon_emissions <- carbon_emissions[1:(length(carbon_emissions) - n)]
train_carbon_emissions <- cbind(Date = 1:length(train_carbon_emissions), carbon_emissions = train_carbon_emissions)
# Transfer Entropy
calc_te(train_carbon_emissions[,2],train_cpu[,2],1,1)
transfer_entropy(train_carbon_emissions[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_carbon_emissions[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_carbon_emissions[,2], main = 'Carbon Emissions')
# Wavelet
wc <- wtc(train_cpu, train_carbon_emissions, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Carbon Emissions")


# Carbon Footprint
carbon_footprint <- data$`carbon footprint: (Worldwide)`
train_carbon_footprint <- carbon_footprint[1:(length(carbon_footprint) - n)]
train_carbon_footprint <- cbind(Date = 1:length(train_carbon_footprint), carbon_footprint = train_carbon_footprint)
# Transfer Entropy
calc_te(train_carbon_footprint[,2],train_cpu[,2],1,1)
transfer_entropy(train_carbon_footprint[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_carbon_footprint[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_carbon_footprint[,2], main = 'Carbon Footprint')
# Wavelet
wc <- wtc(train_cpu, train_carbon_footprint, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Carbon Footprint")


# Carbon Tax
carbon_tax <- data$`carbon tax: (Worldwide)`
train_carbon_tax <- carbon_tax[1:(length(carbon_tax) - n)]
train_carbon_tax <- cbind(Date = 1:length(train_carbon_tax), carbon_tax = train_carbon_tax)
# Transfer Entropy
calc_te(train_carbon_tax[,2],train_cpu[,2],1,1)
transfer_entropy(train_carbon_tax[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_carbon_tax[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_carbon_tax[,2], main = 'Carbon Tax')
# Wavelet
wc <- wtc(train_cpu, train_carbon_tax, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Carbon Tax")


# Clean Energy
clean_energy <- data$`clean energy: (Worldwide)`
train_clean_energy <- clean_energy[1:(length(clean_energy) - n)]
train_clean_energy <- cbind(Date = 1:length(train_clean_energy), clean_energy = train_clean_energy)
# Transfer Entropy
calc_te(train_clean_energy[,2],train_cpu[,2],1,1)
transfer_entropy(train_clean_energy[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_clean_energy[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_clean_energy[,2], main = 'Clean Energy')
# Wavelet
wc <- wtc(train_cpu, train_clean_energy, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Clean Energy")


# Climate Action
climate_action <- data$`climate action: (Worldwide)`
train_climate_action <- climate_action[1:(length(climate_action) - n)]
train_climate_action <- cbind(Date = 1:length(train_climate_action), climate_action = train_climate_action)
# Transfer Entropy
calc_te(train_climate_action[,2],train_cpu[,2],1,1)
transfer_entropy(train_climate_action[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_climate_action[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_climate_action[,2], main = 'Climate Action')
# Wavelet
wc <- wtc(train_cpu, train_climate_action, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Climate Action")


# Climate News
climate_news <- data$`climate news: (Worldwide)`
train_climate_news <- climate_news[1:(length(climate_news) - n)]
train_climate_news <- cbind(Date = 1:length(train_climate_news), climate_news = train_climate_news)
# Transfer Entropy
calc_te(train_climate_news[,2],train_cpu[,2],1,1)
transfer_entropy(train_climate_news[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_climate_news[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_climate_news[,2], main = 'Climate News')
# Wavelet
wc <- wtc(train_cpu, train_climate_news, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Climate News")


# Climate Policy
climate_policy <- data$`climate policy: (Worldwide)`
train_climate_policy <- climate_policy[1:(length(climate_policy) - n)]
train_climate_policy <- cbind(Date = 1:length(train_climate_policy), climate_policy = train_climate_policy)
# Transfer Entropy
calc_te(train_climate_policy[,2],train_cpu[,2],1,1)
transfer_entropy(train_climate_policy[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_climate_policy[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_climate_policy[,2], main = 'Climate Policy')
# Wavelet
wc <- wtc(train_cpu, train_climate_policy, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Climate Policy")


# Climate Technology
climate_tech <- data$`climate technology: (Worldwide)`
train_climate_tech <- climate_tech[1:(length(climate_tech) - n)]
train_climate_tech <- cbind(Date = 1:length(train_climate_tech), climate_tech = train_climate_tech)
# Transfer Entropy
calc_te(train_climate_tech[,2],train_cpu[,2],1,1)
transfer_entropy(train_climate_tech[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_climate_tech[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_climate_tech[,2], main = 'Climate Technology')
# Wavelet
wc <- wtc(train_cpu, train_climate_tech, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Climate Technology")


# Electric Vehicle
electric_vehicle <- data$`electric vehicle: (Worldwide)`
train_electric_vehicle <- electric_vehicle[1:(length(electric_vehicle) - n)]
train_electric_vehicle <- cbind(Date = 1:length(train_electric_vehicle), electric_vehicle = train_electric_vehicle)
# Transfer Entropy
calc_te(train_electric_vehicle[,2],train_cpu[,2],1,1)
transfer_entropy(train_electric_vehicle[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_electric_vehicle[,2], lag = 2, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_electric_vehicle[,2], main = 'Electric Vehicle')
# Wavelet
wc <- wtc(train_cpu, train_electric_vehicle, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Electric Vehicle")


# Energy Efficiency
energy_efficiency <- data$`energy efficiency: (Worldwide)`
train_energy_efficiency <- energy_efficiency[1:(length(energy_efficiency) - n)]
train_energy_efficiency <- cbind(Date = 1:length(train_energy_efficiency), energy_efficiency = train_energy_efficiency)
# Transfer Entropy
calc_te(train_energy_efficiency[,2],train_cpu[,2],1,1)
transfer_entropy(train_energy_efficiency[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_energy_efficiency[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_energy_efficiency[,2], main = 'Energy Efficiency')
# Wavelet
wc <- wtc(train_cpu, train_energy_efficiency, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Energy Efficiency")


# Energy Policy
energy_policy <- data$`energy policy: (Worldwide)`
train_energy_policy <- energy_policy[1:(length(energy_policy) - n)]
train_energy_policy <- cbind(Date = 1:length(train_energy_policy), energy_policy = train_energy_policy)
# Transfer Entropy
calc_te(train_energy_policy[,2],train_cpu[,2],1,1)
transfer_entropy(train_energy_policy[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_energy_policy[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_energy_policy[,2], main = 'Energy Policy')
# Wavelet
wc <- wtc(train_cpu, train_energy_policy, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Energy Policy")


# Energy Transition
energy_transition <- data$`energy transition: (Worldwide)`
train_energy_transition <- energy_transition[1:(length(energy_transition) - n)]
train_energy_transition <- cbind(Date = 1:length(train_energy_transition), energy_transition = train_energy_transition)
# Transfer Entropy
calc_te(train_energy_transition[,2],train_cpu[,2],1,1)
transfer_entropy(train_energy_transition[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_energy_transition[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_energy_transition[,2], main = 'Energy Transition')
# Wavelet
wc <- wtc(train_cpu, train_energy_transition, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Energy Transition")


# Environmental Policy
environmental_policy <- data$`environmental policy: (Worldwide)`
train_environmental_policy <- environmental_policy[1:(length(environmental_policy) - n)]
train_environmental_policy <- cbind(Date = 1:length(train_environmental_policy), environmental_policy = train_environmental_policy)
# Transfer Entropy
calc_te(train_environmental_policy[,2],train_cpu[,2],1,1)
transfer_entropy(train_environmental_policy[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_environmental_policy[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_environmental_policy[,2], main = 'Environmental Policy')
# Wavelet
wc <- wtc(train_cpu, train_environmental_policy, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Environmental Policy")


# Environmental Tax
environmental_tax <- data$`environmental tax: (Worldwide)`
train_environmental_tax <- environmental_tax[1:(length(environmental_tax) - n)]
train_environmental_tax <- cbind(Date = 1:length(train_environmental_tax), environmental_tax = train_environmental_tax)
# Transfer Entropy
calc_te(train_environmental_tax[,2],train_cpu[,2],1,1)
transfer_entropy(train_environmental_tax[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_environmental_tax[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_environmental_tax[,2], main = 'Environmental Tax')
# Wavelet
wc <- wtc(train_cpu, train_environmental_tax, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Environmental Tax")


# Global Warming Policy
global_warming_policy <- data$`global warming policy: (Worldwide)`
train_global_warming_policy <- global_warming_policy[1:(length(global_warming_policy) - n)]
train_global_warming_policy <- cbind(Date = 1:length(train_global_warming_policy), global_warming_policy = train_global_warming_policy)
# Transfer Entropy
calc_te(train_global_warming_policy[,2],train_cpu[,2],1,1)
transfer_entropy(train_global_warming_policy[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_global_warming_policy[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_global_warming_policy[,2], main = 'Global Warming Policy')
# Wavelet
wc <- wtc(train_cpu, train_global_warming_policy, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Global Warming Policy")


# Green Finance
green_finance <- data$`green finance: (Worldwide)`
train_green_finance <- green_finance[1:(length(green_finance) - n)]
train_green_finance <- cbind(Date = 1:length(train_green_finance), green_finance = train_green_finance)
# Transfer Entropy
calc_te(train_green_finance[,2],train_cpu[,2],1,1)
transfer_entropy(train_green_finance[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_green_finance[,2], lag = 4, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_green_finance[,2], main = 'Green Finance')
# Wavelet
wc <- wtc(train_cpu, train_green_finance, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Green Finance")


# Green Infrastructure
green_infrastructure <- data$`green infrastructure: (Worldwide)`
train_green_infrastructure <- green_infrastructure[1:(length(green_infrastructure) - n)]
train_green_infrastructure <- cbind(Date = 1:length(train_green_infrastructure), green_infrastructure = train_green_infrastructure)
# Transfer Entropy
calc_te(train_green_infrastructure[,2],train_cpu[,2],1,1)
transfer_entropy(train_green_infrastructure[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_green_infrastructure[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_green_infrastructure[,2], main = 'Green Infrastructure')
# Wavelet
wc <- wtc(train_cpu, train_green_infrastructure, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Green Infrastructure")


# Green Jobs
green_jobs <- data$`green jobs: (Worldwide)`
train_green_jobs <- green_jobs[1:(length(green_jobs) - n)]
train_green_jobs <- cbind(Date = 1:length(train_green_jobs), green_jobs = train_green_jobs)
# Transfer Entropy
calc_te(train_green_jobs[,2],train_cpu[,2],1,1)
transfer_entropy(train_green_jobs[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_green_jobs[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_green_jobs[,2], main = 'Green Jobs')
# Wavelet
wc <- wtc(train_cpu, train_green_jobs, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Green Jobs")


# Green Technology
green_tech <- data$`green technology: (Worldwide)`
train_green_tech <- green_tech[1:(length(green_tech) - n)]
train_green_tech <- cbind(Date = 1:length(train_green_tech), green_tech = train_green_tech)
# Transfer Entropy
calc_te(train_green_tech[,2],train_cpu[,2],1,1)
transfer_entropy(train_green_tech[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_green_tech[,2], lag = 2, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_green_tech[,2], main = 'Green Technology')
# Wavelet
wc <- wtc(train_cpu, train_green_tech, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Green Technology")


# Greenhouse Gas Emissions
ghg_emissions <- data$`greenhouse gas emissions: (Worldwide)`
train_ghg_emissions <- ghg_emissions[1:(length(ghg_emissions) - n)]
train_ghg_emissions <- cbind(Date = 1:length(train_ghg_emissions), ghg_emissions = train_ghg_emissions)
# Transfer Entropy
calc_te(train_ghg_emissions[,2],train_cpu[,2],1,1)
transfer_entropy(train_ghg_emissions[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_ghg_emissions[,2], lag = 2, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_ghg_emissions[,2], main = 'Greenhouse Gas Emissions')
# Wavelet
wc <- wtc(train_cpu, train_ghg_emissions, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Greenhouse Gas Emissions")


# Greenwashing
greenwashing <- data$`greenwashing: (Worldwide)`
train_greenwashing <- greenwashing[1:(length(greenwashing) - n)]
train_greenwashing <- cbind(Date = 1:length(train_greenwashing), greenwashing = train_greenwashing)
# Transfer Entropy
calc_te(train_greenwashing[,2],train_cpu[,2],1,1)
transfer_entropy(train_greenwashing[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_greenwashing[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_greenwashing[,2], main = 'Greenwashing')
# Wavelet
wc <- wtc(train_cpu, train_greenwashing, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Greenwashing")


# Renewable Energy
renewable_energy <- data$`renewable energy: (Worldwide)`
train_renewable_energy <- renewable_energy[1:(length(renewable_energy) - n)]
train_renewable_energy <- cbind(Date = 1:length(train_renewable_energy), renewable_energy = train_renewable_energy)
# Transfer Entropy
calc_te(train_renewable_energy[,2],train_cpu[,2],1,1)
transfer_entropy(train_renewable_energy[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_renewable_energy[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_renewable_energy[,2], main = 'Renewable Energy')
# Wavelet
wc <- wtc(train_cpu, train_renewable_energy, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Renewable Energy")


# Sustainability
sustainability <- data$`sustainability: (Worldwide)`
train_sustainability <- sustainability[1:(length(sustainability) - n)]
train_sustainability <- cbind(Date = 1:length(train_sustainability), sustainability = train_sustainability)
# Transfer Entropy
calc_te(train_sustainability[,2],train_cpu[,2],1,1)
transfer_entropy(train_sustainability[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_sustainability[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_sustainability[,2], main = 'Sustainability')
# Wavelet
wc <- wtc(train_cpu, train_sustainability, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Sustainability")


# Sustainable Development
sd <- data$`sustainable development: (Worldwide)`
train_sd <- sd[1:(length(sd) - n)]
train_sd <- cbind(Date = 1:length(train_sd), sd = train_sd)
# Transfer Entropy
calc_te(train_sd[,2],train_cpu[,2],1,1)
transfer_entropy(train_sd[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_sd[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_sd[,2], main = 'Sustainable Development')
# Wavelet
wc <- wtc(train_cpu, train_sd, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Sustainable Development")


# Sustainable Development Goals
sdg <- data$`sustainable development goals: (Worldwide)`
train_sdg <- sdg[1:(length(sdg) - n)]
train_sdg <- cbind(Date = 1:length(train_sdg), sdg = train_sdg)
# Transfer Entropy
calc_te(train_sdg[,2],train_cpu[,2],1,1)
transfer_entropy(train_sdg[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_sdg[,2], lag = 3, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_sdg[,2], main = 'Sustainable Development Goals')
# Wavelet
wc <- wtc(train_cpu, train_sdg, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Sustainable Development Goals")


# Sustainable Living
sustainable_living <- data$`sustainable living: (Worldwide)`
train_sustainable_living <- sustainable_living[1:(length(sustainable_living) - n)]
train_sustainable_living <- cbind(Date = 1:length(train_sustainable_living), sustainable_living = train_sustainable_living)
# Transfer Entropy
calc_te(train_sustainable_living[,2],train_cpu[,2],1,1)
transfer_entropy(train_sustainable_living[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_sustainable_living[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_sustainable_living[,2], main = 'Sustainable Living')
# Wavelet
wc <- wtc(train_cpu, train_sustainable_living, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Sustainable Living")


# UN Climate Conference
un_cc <- data$`UN climate conference: (Worldwide)`
train_un_cc <- un_cc[1:(length(un_cc) - n)]
train_un_cc <- cbind(Date = 1:length(train_un_cc), un_cc = train_un_cc)
# Transfer Entropy
calc_te(train_un_cc[,2],train_cpu[,2],1,1)
transfer_entropy(train_un_cc[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_un_cc[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_un_cc[,2], main = 'UN Climate Conference')
# Wavelet
wc <- wtc(train_cpu, train_un_cc, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs UN Climate Conference")


# Zero Emissions
zero_emissions <- data$`zero emissions: (Worldwide)`
train_zero_emissions <- zero_emissions[1:(length(zero_emissions) - n)]
train_zero_emissions <- cbind(Date = 1:length(train_zero_emissions), zero_emissions = train_zero_emissions)
# Transfer Entropy
calc_te(train_zero_emissions[,2],train_cpu[,2],1,1)
transfer_entropy(train_zero_emissions[,2],train_cpu[,2],1,1)
# Granger Test
set.seed(100)
causality.test(train_cpu[,2], train_zero_emissions[,2], lag = 1, diff = TRUE)$summary()
# Cross Correlation
ccf(train_cpu[,2],train_zero_emissions[,2], main = 'Zero Emissions')
# Wavelet
wc <- wtc(train_cpu, train_zero_emissions, nrands = 100)
par(mfrow = c(1,1), oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wc, xlab = "Period", ylab = 'Scale', plot.phase = TRUE, plot.cb = TRUE, main = "CPU vs Zero Emissions")


setwd("~/Desktop/Data Mining/data-mining-labs/Project4")

data <- read.csv(file="./Data/flatMembers.csv")
# members <- read.csv(file="./Data/discretized_data.csv")
##################################################################################################################
library(arules)
library(caret)
library(arulesViz)
##################################################################################################################
data[,grep('specialty_*', colnames(data))] <- data[,grep('specialty_*', colnames(data))] > 0
data[,grep('pcg_*', colnames(data))] <- data[,grep('pcg_*', colnames(data))] > 0
data[,grep('placesvc_*', colnames(data))] <- data[,grep('placesvc_*', colnames(data))] > 0
data[,grep('sex_*', colnames(data))] <- data[,grep('sex_*', colnames(data))] > 0
data$age_at_first_claim <- as.factor(data$age_at_first_claim)
data$charlson_max <- as.factor(data$charlson_max)
data$drugs_max <- discretize(data$drugs_max, method='fixed', breaks=c(0, 1, 2, 5, 7))
data$labs_max <- discretize(data$labs_max, method='fixed', breaks=c(0, 1, 2, 7, 10))
##################################################################################################################
data$member_id <- NULL
trans <- as(data, "transactions")
summary(trans)

rules <- apriori(trans, parameter = list(supp = 0.001, confidence = 0.1))
inspect(head(rules, 10, by = "lift"))
itemFrequencyPlot(trans, topN = 20)

rules.sub = subset(rules, subset = (rhs %pin% "days_in_hospital=5+ days"))
inspect(head(rules.sub, 10, by = "lift"))

# downsample
data_balanced = downSample(data, data$days_in_hospital)
data_balanced$Class <- NULL
trans <- as(data_balanced, "transactions")
summary(trans)

rules <- apriori(trans, parameter = list(supp = 0.1, confidence = 0.1))
rules.sub = subset(rules, subset = (rhs %pin% "days_in_hospital"))
inspect(head(rules.sub, 100, by = "lift"))

inspectDT(rules.sub)
head(quality(rules))
##################################################################################################################
itemFrequencyPlot(trans, topN = 20)
plot(rules.sub)

subrules2 <- head(rules, n = 10, by = "lift")
plot(subrules2, method = "graph")

plot(subrules2, method = "paracoord")

# scatterplot", "two-key plot", "matrix", "matrix3D", "mosaic", "doubledecker", "graph", "paracoord" or "grouped", "iplots" 

plot(rules.sub, method="iplots")
##################################################################################################################
drop <- c("member_id", "age_at_first_claim", "charlson_max", "specialty_anesthesiology", "specialty_diagnostic_imaging", "specialty_emergency", "specialty_general_practice", "specialty_internal", "specialty_laboratory", "specialty_obstetrics_and_gynecology", "specialty_other", "specialty_pathology", "specialty_pediatrics", "specialty_rehabilitation", "specialty_surgery", "placesvc_ambulance", "placesvc_home", "placesvc_independent_lab", "placesvc_inpatient_hospital", "placesvc_office", "placesvc_other", "placesvc_outpatient_hospital", "placesvc_urgent_care", "drugs_max", "labs_max", "sex_m", "sex_f")
condition_data = data[,!(names(data) %in% drop)]

trans <- as(condition_data, "transactions")

rules <- apriori(trans, parameter = list(supp = 0.001, confidence = 0.1))
rules.sub = subset(rules, subset = (rhs %pin% "days_in_hospital"))
inspect(head(rules, 40, by = "lift"))

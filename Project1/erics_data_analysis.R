library(easyGgplot2)

# load data
# make sure to do Session > Set Working Directory > To Source File Location
setwd("~/Desktop/Data Mining/data-mining-labs/Project1")
claims <- read.csv(file="Data/hospital/Claims_Y1.csv")
members <- read.csv(file="Data/hospital/Members_Y1.csv")
days_in_hospital <- read.csv(file="Data/hospital/DayInHospital_Y2.csv")
print("Data loaded successfully.")

summary(claims)

# barplots
# format percentage floats
format_percentage <- function(percentage) {
  sprintf("%0.2f%%", percentage*100)
}


# Distrbution of Age
png("Graphs/age.png", width = 3.25, height = 3.25, units = "in", res = 1200, pointsize = 4)
par(cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  # set label sizes
ylim <- c(0, max(summary(members$AgeAtFirstClaim)) + 1000)
b <- barplot(summary(members$AgeAtFirstClaim), xlab="Age Group", ylab="Count", ylim=ylim, main="Distribution of Age")  
percents <- lapply(prop.table(table(members$AgeAtFirstClaim)), format_percentage)
text(x = b, y = summary(members$AgeAtFirstClaim), 
     label = percents, pos = 3, cex = 1.5, col = "red")
dev.off()

# Distrbution of Sex
png("Graphs/sex.png", width = 3.25, height = 3.25, units = "in", res = 1200, pointsize = 4)
par(cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  # set label sizes
ylim <- c(0, max(summary(members$sex)) + 4000)
b <- barplot(summary(members$sex), xlab="Sex", ylab="Count", ylim=ylim, main = "Distrbution of Sex")  # M = 45%, F = 55%
percents <- lapply(prop.table(table(members$sex)), format_percentage)
text(x = b, y = summary(members$sex), 
     label = percents, pos = 3, cex = 2, col = "red")
dev.off()


# Distribution of Specialty
png("Graphs/specialty2.png", width = 3.25, height = 3.25, units = "in", res = 1200, pointsize = 4)
par(cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  # set label sizes
par(mar=c(18, 6.1, 4.1, 2.1)) # adjust dimensions
percents <- lapply(prop.table(table(claims$specialty)), format_percentage)
ylim <- c(0, max(summary(claims$specialty)) + 40000)
b <- barplot(summary(claims$specialty), ylim=ylim, main="Distribution of Specialty", las=2)  
text(x = b, y = summary(claims$specialty), 
     label = percents, pos = 3, cex = 1.2, col = "red")
dev.off()

# Distribution of place of service
png("Graphs/service.png", width = 3.25, height = 3.25, units = "in", res = 1200, pointsize = 4)
par(cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  # set label sizes
par(mar=c(15, 6.1, 4.1, 2.1)) # adjust dimensions
options(scipen=999) # remove scientific notation
percents <- lapply(prop.table(table(claims$placesvc)), format_percentage)
ylim <- c(0, max(summary(claims$placesvc)) + 100000)
b <- barplot(summary(claims$placesvc), ylim=ylim, main="Place of Service", las=2)  
text(x = b, y = summary(claims$placesvc), 
     label = percents, pos = 3, cex = 1.5, col = "red")
dev.off()


# see http://michael.hahsler.net/SMU/EMIS7332/slides/Essential_Visualizations_Cheat_sheet.pdf

#######################################################################################
# DATA QUALITY, group-wise averages
# compare y2 days in hospital vs members age


claims_and_days_in_hospital = merge(claims[,c("MemberID", "CharlsonIndex")], 
                                    days_in_hospital[,c("memberid", "DaysInHospital_Y2")],
                                    by.x="MemberID", by.y="memberid")

claims_and_days_in_hospital = unique(claims_and_days_in_hospital)  # remove duplicate rows

#
# if a member ID is duplicated, keep the one that was the highest charlson index
# 
# sort by member id, then charlson
claims_and_days_in_hospital <- claims_and_days_in_hospital[
  order( claims_and_days_in_hospital[,1], claims_and_days_in_hospital[,2] ),
]
claims_and_days_in_hospital <- claims_and_days_in_hospital[!rev(duplicated(rev(claims_and_days_in_hospital$MemberID))),]  # keep last instances of duplicates

# find the mean days_in_hospital for each CharlsonIndex
myFun <- function(x) {
  c(min = min(x), max = max(x), 
    mean = mean(x), median = median(x), 
    std = sd(x))
}
claims_and_days_in_hospital_mean <- tapply(claims_and_days_in_hospital$DaysInHospital_Y2, claims_and_days_in_hospital$CharlsonIndex, myFun)
claims_and_days_in_hospital_mean <- cbind(CharlsonIndex = unique(claims_and_days_in_hospital$CharlsonIndex), 
      do.call(rbind, tapply(claims_and_days_in_hospital$DaysInHospital_Y2, claims_and_days_in_hospital$CharlsonIndex, myFun)))

means = c(claims_and_days_in_hospital_mean[1, 4],
          claims_and_days_in_hospital_mean[2, 4],
          claims_and_days_in_hospital_mean[3, 4],
          claims_and_days_in_hospital_mean[4, 4])

#means = c(claims_and_days_in_hospital_mean[["0"]][["mean"]], 
#          claims_and_days_in_hospital_mean[["1-2"]][["mean"]],
#          claims_and_days_in_hospital_mean[["3-4"]][["mean"]],
#          claims_and_days_in_hospital_mean[["5+"]][["mean"]])

png("Graphs/charlson.png", width = 3.25, height = 3.25, units = "in", res = 1200, pointsize = 4)
par(mar=c(9, 4.1, 4.1, 2.1)) # adjust dimensions
par(cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5)  # set label sizes
ylim <- c(0, max(means) + 1)
b <- barplot(means,names.arg=c(0, "1-2", "3-4", "5+"), xlab="Highest Charlson Index per Patient", ylim=ylim, ylab="Average Days in Hospital", main="Charlson Index vs Days in Hospital")  
means <- rapply(as.list(means), sprintf, fmt = "%0.2f", how = "replace")
text(x = b, y = means, 
     label = means, pos = 3, cex = 1.5, col = "red")
dev.off()

#######################################################################################
paydelay_and_days_in_hospital = merge(claims[,c("MemberID", "paydelay")], 
                                    days_in_hospital[,c("memberid", "DaysInHospital_Y2")],
                                    by.x="MemberID", by.y="memberid")
paydelay_and_days_in_hospital = unique(paydelay_and_days_in_hospital)  # remove duplicate rows
paydelay_and_days_in_hospital <- paydelay_and_days_in_hospital[complete.cases(paydelay_and_days_in_hospital), ] # remove NAs
# group by member id then pay delay
paydelay_and_days_in_hospital$paydelay <- as.numeric(as.character(paydelay_and_days_in_hospital$paydelay))
paydelay_and_days_in_hospital <- paydelay_and_days_in_hospital[
  order( paydelay_and_days_in_hospital[,1], paydelay_and_days_in_hospital[,2] ),
]

paydelay_and_days_in_hospital <- paydelay_and_days_in_hospital[!rev(duplicated(rev(paydelay_and_days_in_hospital$MemberID))),] # keep highest pay delay only
cor(paydelay_and_days_in_hospital$paydelay, paydelay_and_days_in_hospital$DaysInHospital_Y2) # 0.097


png("Graphs/paydelay.png", width = 3.25, height = 3.25, units = "in", res = 1200, pointsize = 4)
par(mar=c(6, 5.1, 4.1, 2.1)) # adjust dimensions
par(cex.lab=1.5, cex.axis=1.2, cex.main=2, cex.sub=1.5)  # set label sizes
ylim <- c(0, max(means) + 1)
# b <- barplot(means,names.arg=c(0, "1-2", "3-4", "5+"), xlab="Highest Charlson Index per Patient", ylim=ylim, ylab="Average Days in Hospital", main="Charlson Index vs Days in Hospital")  
b <- boxplot(paydelay_and_days_in_hospital$paydelay~paydelay_and_days_in_hospital$DaysInHospital_Y2, paydelay_and_days_in_hospital, xlab="Days in Hospital", ylab="Pay Delay in Days", main="Longest Pay Delay vs Days in Hospital")
dev.off()
#######################################################################################
# num claims vs days in hospital
library(plyr)
count(claims, "MemberID")

num_clams_v_hospital = merge(count(claims, "MemberID"), 
      days_in_hospital[,c("memberid", "DaysInHospital_Y2")],
      by.x="MemberID", by.y="memberid")

num_clams_v_hospital_no_zeroes = num_clams_v_hospital[num_clams_v_hospital$DaysInHospital_Y2 > 0, ]
cor(num_clams_v_hospital_no_zeroes$freq, num_clams_v_hospital_no_zeroes$DaysInHospital_Y2) # 0.2128609

#######################################################################################################
# violin plot by sex: num claims v day in hospital
sex_num_clams_v_hospital = merge(num_clams_v_hospital, 
                             members[,c("MemberID", "sex")],
                             by="MemberID")

# source: https://stackoverflow.com/a/45614547/8853372
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, draw_group = function(self, data, ..., draw_quantiles = NULL){
  data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
  grp <- data[1,'group']
  newdata <- plyr::arrange(transform(data, x = if(grp%%2==1) xminv else xmaxv), if(grp%%2==1) y else -y)
  newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
  newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
  if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
    stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 
                                              1))
    quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
    aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
    aesthetics$alpha <- rep(1, nrow(quantiles))
    both <- cbind(quantiles, aesthetics)
    quantile_grob <- GeomPath$draw_panel(both, ...)
    ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
  }
  else {
    ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
  }
})

geom_split_violin <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

# sex and days in hospital
sex_v_hospital <- tapply(sex_num_clams_v_hospital$DaysInHospital_Y2, sex_num_clams_v_hospital$sex, myFun)
# sex_num_clams_v_hospital$
means <- c(sex_v_hospital$F["mean"], sex_v_hospital$M["mean"])
b <- barplot(means)
#b <- boxplot(sex_num_clams_v_hospital$DaysInHospital_Y2~sex_num_clams_v_hospital$sex, sex_num_clams_v_hospital, xlab="Days in Hospital", ylab="Pay Delay in Days", main="Longest Pay Delay vs Days in Hospital")

png("Graphs/sex_days.png", width = 3.25, height = 3.25, units = "in", res = 1200, pointsize = 4)
#par(mar=c(9, 4.1, 4.1, 2.1)) # adjust dimensions
par(cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.0)  # set label sizes
ylim <- c(0, max(means) + 1)
b <- barplot(means,names.arg=c("F", "M"), xlab="Gender", ylim=ylim, ylab="Average Days in Hospital")  
means <- rapply(as.list(means), sprintf, fmt = "%0.2f", how = "replace")
text(x = b, y = means, 
     label = means, pos = 3, cex = 1.5, col = "red")
dev.off()

sex_num_clams_v_hospital$sex <- as.factor(sex_num_clams_v_hospital$sex)
sex_num_clams_v_hospital$DaysInHospital_Y2 <- as.factor(sex_num_clams_v_hospital$DaysInHospital_Y2)

png("Graphs/num_claims.png", width = 7, height = 3.25, units = "in", res = 1200, pointsize = 4)
p <- ggplot(sex_num_clams_v_hospital, aes(x=DaysInHospital_Y2, y=freq, fill=sex)) + geom_split_violin()
p + labs(title="Number of Claims vs Days in Hospital", y="Number of Claims", x="Days in Hospital Y2")
dev.off()

#######################################################################################################

# 2. Age vs days in hospital and condition grouping 
age_vs_days <- merge(members[,c("MemberID", "AgeAtFirstClaim")], days_in_hospital[,c("memberid", "DaysInHospital_Y2")], by.x="MemberID", by.y="memberid")
age_vs_days$AgeAtFirstClaim <- mapvalues(age_vs_days$AgeAtFirstClaim, c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"), c(1,2,3,4,5,6,7,8,9))
age_vs_days_nonzero <- age_vs_days[age_vs_days$DaysInHospital_Y2 > 0, ]
cor(as.numeric(age_vs_days$AgeAtFirstClaim), age_vs_days$DaysInHospital_Y2) # 0.1589626
cor(as.numeric(age_vs_days_nonzero$AgeAtFirstClaim), age_vs_days_nonzero$DaysInHospital_Y2) # 0.2633964

# age_days_condition = merge(age_vs_days, claims[,c("MemberID", "PrimaryConditionGroup")], on="MemberID")
# age_vs_days = transform(age_days_condition, PrimaryConditionGroup = colsplit(PrimaryConditionGroup, split = "\\|", names = c('PrimaryConditionGroup', 'b')))
# plot(x=num_clams_v_hospital_no_zeroes$freq, x=num_clams_v_hospital_no_zeroes$DaysInHospital_Y2)

#######################################################################################################
# 3. another additional attribute: group primary condition groups
likely <- c("RENAL1","RESPR4","HEART4","CANCRA","COPD","RENAL2","METAB1","GIOBSENT","GYNECA","ODaBNCA","LIVERDZ","MISCHRT","HEART2","METAB3","RENAL3","CANCRM","PERVALV","SEIZURE","SKNAUT","STROKE")
'%!in%' <- function(x,y)!('%in%'(x,y))
claims_w_likely <- claims
claims_w_likely$likely <- NA
claims_w_likely$likely[claims_w_likely$PrimaryConditionGroup %in% likely] <- TRUE
claims_w_likely$likely[claims_w_likely$PrimaryConditionGroup %!in% likely] <- FALSE

members_claims_w_likely <- merge(claims_w_likely, days_in_hospital[,c("memberid", "DaysInHospital_Y2")], by.x="MemberID", by.y="memberid")

x <- members_claims_w_likely[members_claims_w_likely$likely == TRUE, c("MemberID", "DaysInHospital_Y2")]
summary(x)
y <- members_claims_w_likely[members_claims_w_likely$likely == FALSE, c("MemberID", "DaysInHospital_Y2")]
summary(y)
#######################################################################################################


# load data

claims <- read.csv(file="Data/hospital/Claims_Y1.csv")
members <- read.csv(file="Data/hospital/Members_Y1.csv")
days_in_hospital <- read.csv(file="Data/hospital/DayInHospital_Y2.csv")
print("Data loaded successfully.")

summary(claims)

# barplots

# Distrbution of Age
ylim <- c(0, max(summary(members$AgeAtFirstClaim)) + 1000)
b <- barplot(summary(members$AgeAtFirstClaim), xlab="Age Group", ylab="Count", ylim=ylim, main="Distribution of Age")  
text(x = b, y = summary(members$AgeAtFirstClaim), 
     label = summary(members$AgeAtFirstClaim), pos = 3, cex = 0.8, col = "red")

# Distrbution of Sex
ylim <- c(0, max(summary(members$sex)) + 4000)
b <- barplot(summary(members$sex), xlab="Sex", ylab="Count", ylim=ylim, main = "Distrbution of Sex")  # M = 45%, F = 55%
text(x = b, y = summary(members$sex), 
     label = summary(members$sex), pos = 3, cex = 0.8, col = "red")

# format percentage floats
format_percentage <- function(percentage) {
  sprintf("%0.2f%%", percentage*100)
}

par(mar=c(12, 4.1, 4.1, 2.1)) # adjust dimensions

# Distribution of Specialty
percents <- lapply(prop.table(table(claims$specialty)), format_percentage)
ylim <- c(0, max(summary(claims$specialty)) + 40000)
b <- barplot(summary(claims$specialty), ylim=ylim, main="Distribution of Specialty", las=2)  
text(x = b, y = summary(claims$specialty), 
     label = percents, pos = 3, cex = 0.8, col = "red")

percents <- lapply(prop.table(table(claims$placesvc)), format_percentage)
ylim <- c(0, max(summary(claims$placesvc)))
b <- barplot(summary(claims$placesvc), ylim=ylim, main="Place of Service", las=2)  
text(x = b, y = summary(claims$placesvc), 
     label = percents, pos = 3, cex = 0.8, col = "red")

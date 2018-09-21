# import data
claims  <- read.csv(file="./data/hospital/Claims_Y1.csv")
members <- read.csv(file="./data/hospital/Members_Y1.csv")
dih     <- read.csv(file="./data/hospital/DayInHospital_Y2.csv")

# merge data
claims_members <- merge(claims, members)


# looking at important attributes


# look at most common pcgs
pcg_counts <- sort(table(claims_members$PrimaryConditionGroup), decreasing=TRUE)

# plot top 10 pcgs bar plot
#pcg_labels <- read.csv(file="./data/hospital/Lookup PrimaryConditionGroup.csv")
par(mar=c(7,4,4,2)) # adjust margins
barplot(pcg_counts[1:10], las=2)

# charlson index
summary(claims_members$CharlsonIndex)

# dih plots
hist(dih$DaysInHospital_Y2, xlab='Days in Hospital (Year 2)', main='Histogram of Days in Hospital (Year 2)')

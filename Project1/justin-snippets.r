# disable scientific notation
options(scipen = 999)

# import data
claims  <- read.csv(file="./data/hospital/Claims_Y1.csv")
members <- read.csv(file="./data/hospital/Members_Y1.csv")
dih     <- read.csv(file="./data/hospital/DayInHospital_Y2.csv")
# merge data
claims_members <- merge(claims, members)

# convert to factors
claims_members$MemberID <- as.factor(claims_members$MemberID)
claims_members$ProviderID <- as.factor(claims_members$ProviderID)
claims_members$vendor <- as.factor(claims_members$vendor)
claims_members$pcp <- as.factor(claims_members$pcp)

# important attributes plots

# pcg plot
pcg_counts <- sort(table(claims_members$PrimaryConditionGroup), decreasing=TRUE)
pcg_labels <- read.csv(file="./data/hospital/Lookup PrimaryConditionGroup.csv")
get_pcg_description <- function(s) {
    as.character(pcg_labels[pcg_labels$PrimaryConditionGroup == s,][['Description']])
}
pcg_plot_labels <- sapply(names(pcg_counts),
                          get_pcg_description,
                          USE.NAMES=FALSE)
par(mar=c(7,14,4,2))
barplot(pcg_counts, horiz=TRUE,
        las=2, cex.names=.8, names.arg=pcg_plot_labels,
        main='Frequency of Primary Condition Groups')
dev.print(png, './plots/pcg.png', width=800)


# charlson index plot
barplot(summary(claims_members$CharlsonIndex),
        xlab='Charlson Index',
        ylab='Frequency',
        main='Distribution of Charlson Index')
dev.print(png, './plots/charlson-index.png', width=800)


# dih plot
hist(dih$DaysInHospital_Y2,
     xlab='Days in Hospital (Year 2)',
     main='Histogram of Days in Hospital (Year 2)')



# relationships

# pcg/index contingency/heat map
pcg_index_tab <- round(prop.table(ftable(index ~ pcg), 1) * 100)
m <- as.matrix(pcg_index_tab)
m <- t(m[nrow(m):1, ]) # transpose and flip row order
levelplot(m, aspect='fill',
          main='Relative Frequencies of Charlson Index against Primary Condition Group',
          ylab='Primary Condition Group', xlab='Charlson Index',
          col.regions=rev(gray(0:100/100)))
dev.print(png, './plots/pcg-index-heat.png', width=800)

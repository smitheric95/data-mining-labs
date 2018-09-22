# disable scientific notation
options(scipen = 999)

# import data
claims  <- read.csv(file="./data/hospital/Claims_Y1.csv")
members <- read.csv(file="./data/hospital/Members_Y1.csv")
dih     <- read.csv(file="./data/hospital/DayInHospital_Y2.csv")
# merge data
claims_members <- merge(claims, members)
cmdih <- merge(claims_members, dih, by.x='MemberID', by.y='memberid')

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

cmdih_first_only <- cmdih[!duplicated(cmdih['MemberID']),]


# age/dih
t <- round(prop.table(ftable(age ~ dih_), 2) * 100)
barplot(t, main='Relative Frequencies of Days in Hospital in Age Groups',
        xlab='Age Group', ylab='Relative Frequency of Days In Hospital',
        beside=TRUE)
dev.print(png, './plots/age-dih.png', width=800)


# pcg transactions
library('arules')

write.csv(cmdih, './tmp/cmdih.csv')

pcg_trans <- read.transactions(file='./tmp/cmdih.csv',
                                 format='single',
                                 sep = ',',
                                 cols=c('MemberID', 'PrimaryConditionGroup'))
summary(pcg_trans)

# distribution of number of conditions per member
pcg_count_dist <- c(15306, 14516, 12562, 10180, 7675, 5749, 4059,
                    2907, 1883, 1204, 652, 336, 151, 72, 31, 2, 4)
barplot(pcg_count_dist, names.arg=1:17,
        main='Frequency of Primary Conditions Groups/Year Per Member',
        ylab='Frequency', xlab='Number of Primary Condition Groups/Year')
dev.print(png, './plots/pcg-num.png', width=800)


# TODO add number of conditions to member row


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
# merge with dih
cmdih <- merge(claims_members, dih, by.x='MemberID', by.y='memberid')

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

png('./plots/pcg.png', width=3.25, height=3.25, units='in', res=1200, pointsize=4)
par(mar=c(7,20,4,2), cex.lab=1.25, cex.axis=1, cex.main=1.5, cex.sub=1.5)
barplot(pcg_counts, horiz=TRUE,
        las=2, cex.names=.8, names.arg=pcg_plot_labels,
        main='Frequency of Primary Condition Groups')
dev.off()


# charlson index plot
png('./plots/charlson.png', width=3.25, height=3.25, units='in', res=1200, pointsize=4)
par(cex.lab=1.25, cex.axis=1, cex.main=1.5, cex.sub=1.5)
barplot(summary(claims_members$CharlsonIndex),
        xlab='Charlson Index',
        ylab='Frequency',
        main='Distribution of Charlson Index')
dev.off()


# dih plot
png('./plots/dih.png', width=3.25, height=3.25, units='in', res=1200, pointsize=4)
par(cex.lab=1.25, cex.axis=1, cex.main=1.5, cex.sub=1.5)
hist(dih$DaysInHospital_Y2,
     xlab='Days in Hospital (Year 2)',
     main='')
dev.off()



# relationships

# pcg/index contingency/heat map
index <- claims_members$CharlsonIndex
pcg <- claims_members$PrimaryConditionGroup
pcg_index_tab <- round(prop.table(ftable(index ~ pcg), 1) * 100)
m <- as.matrix(pcg_index_tab)
m <- t(m[nrow(m):1, ]) # transpose and flip row order

png('./plots/pcg-index.png', width=3.25, height=5.25, units='in', res=1200, pointsize=4)
library('lattice')
par(cex.lab=1.25, cex.axis=1, cex.main=.1, cex.sub=1.5)
levelplot(m, aspect='fill', xaxt='n',
          scales=list(x=list(cex=.8), y=list(cex=.5)),
          ylab=list(label='Primary Condition Group', cex=.8),
          xlab=list(label='Charlson Index', cex=.8),
          col.regions=rev(gray(0:100/100)))
dev.off()

cmdih_first_only <- cmdih[!duplicated(cmdih['MemberID']),]


# age/dih
t <- round(prop.table(ftable(members$AgeAtFirstClaim ~ dih$DaysInHospital_Y2), 2) * 100)
png('./plots/age-dih.png', width=3.25, height=3.25, units='in', res=1200, pointsize=4)
par(cex.lab=1.25, cex.axis=1, cex.main=1.5, cex.sub=1.5)
barplot(t, main='',
        xlab='Age Group', ylab='Relative Frequency of Days In Hospital',
        beside=TRUE, names.arg=levels(members$AgeAtFirstClaim))
dev.off()


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

png('./plots/pcg-num.png', width=3.25, height=3.25, units='in', res=1200, pointsize=4)
par(cex.lab=1.25, cex.axis=1, cex.main=1.5, cex.sub=1.5)
barplot(pcg_count_dist, names.arg=1:17,
        main='',
        ylab='Frequency', xlab='Number of Primary Condition Groups/Year')
dev.off()


# add number of conditions as an attribute

# this will take a while
find_num_pcg_per_member <- function (data) {
    memberIDs <- unique(data$MemberID)
    num_rows <- as.integer(length(memberIDs))
    counts <- data.frame(MemberID=memberIDs, NumPCGYear=rep(NA, num_rows))
    for (memberID in memberIDs) {
        member_rows <- data[data$MemberID == memberID,]
        pcg_count <- nrow(member_rows[!duplicated(member_rows['PrimaryConditionGroup']),])
        counts[counts$MemberID == memberID,]$NumPCGYear <- pcg_count
    }
    counts
}
num_pcg <- find_num_pcg_per_member(claims_members)

num_pcg <- read.csv('./data/num_pcg.csv')
num_pcg_dih = merge(num_pcg, dih, by.x='MemberID', by.y='memberid')

png('./plots/pcg-num-box.png', width=3.25, height=3.25, units='in', res=1200, pointsize=4)
par(cex.lab=1.25, cex.axis=1, cex.main=1.5, cex.sub=1.5)
boxplot(num_pcg$NumPCGYear ~ num_pcg_dih$DaysInHospital_Y2, notch=T,
        ylab='Number of Conditions/Year', xlab='Days in Hospital',
        main='')
dev.off()



# pca
flat <- read.csv('./data/flattened_members.csv')
flat$pay_delay_mean <- NULL
flat$pay_delay_min <- NULL
flat$pay_delay_max <- NULL
flat$MemberID <- NULL
pca <- prcomp(flat, scale. = T)

# plot how much each component contributes
pr_var <- pca$sdev^2
prop_var <- pr_var/sum(pr_var)

png('./plots/pc-var.png', width=3.25, height=3.25, units='in', res=1200, pointsize=4)
par(cex.lab=1.25, cex.axis=1, cex.main=2, cex.sub=1.5)
plot(prop_var, type='b', main='',
     xlab='Principal Component', ylab='Proportion of variance explained')
dev.off()

# plot importance of features in PC1
png('./plots/pc1.png', width=3.25, height=5.25, units='in', res=1200, pointsize=4)
par(mar=c(7,18,4,2), cex.lab=1.25, cex.axis=1, cex.main=2, cex.sub=1.5)
barplot(sort(abs(pca$rotation[,1])), las=2, horiz=T,
        main='', xaxt='n',
        xlab='Magnitude of feature loading')
axis(1, cex.axis=1)
dev.off()




# look at some counts
si <- flat$specialty_Internal
si <- cut(si, breaks=c(-Inf, 5, 10, 15, 20, Inf), labels=c('0-5', '6-10', '11-15', '16-20', '20+'))

si_tab <- round(prop.table(ftable(si ~ dih$DaysInHospital_Y2), 2) * 100)


png('./plots/si-num.png', width=3.25, height=3.25, units='in', res=1200, pointsize=4)
par(cex.lab=1.25, cex.axis=1, cex.main=1.5, cex.sub=1.5)
barplot(si_tab, beside=T, names.arg=c('0-5', '6-10', '11-15', '16-20', '20+'),
        main='Days in Hospital by Number of Internal Specialty Claims',
        xlab='Number of Internal Specialty Claims', ylab='Relative Frequency of Days in Hospital')
dev.off()


si_dih <- data.frame(NumInternal=si, dih=dih$DaysInHospital_Y2)
si_dih_means <- c(mean(si_dih[si_dih$NumInternal == '0-5',]$dih),
                  mean(si_dih[si_dih$NumInternal == '6-10',]$dih),
                  mean(si_dih[si_dih$NumInternal == '11-15',]$dih),
                  mean(si_dih[si_dih$NumInternal == '16-20',]$dih),
                  mean(si_dih[si_dih$NumInternal == '20+',]$dih))
si_dih_vars  <- c(var(si_dih[si_dih$NumInternal == '0-5',]$dih),
                  var(si_dih[si_dih$NumInternal == '6-10',]$dih),
                  var(si_dih[si_dih$NumInternal == '11-15',]$dih),
                  var(si_dih[si_dih$NumInternal == '16-20',]$dih),
                  var(si_dih[si_dih$NumInternal == '20+',]$dih))

png('./plots/si-dih-means.png', width=3.25, height=3.25, units='in', res=1200, pointsize=4)
par(cex.lab=1.25, cex.axis=1, cex.main=1.75, cex.sub=1.5)
xx <- barplot(si_dih_means, beside=T, names.arg=c('0-5', '6-10', '11-15', '16-20', '20+'),
        main='', ylim=c(0.0, 3.5),
        xlab='Number of Internal Specialty Claims', ylab='Mean Days In Hospital')
text(xx, si_dih_means, label=round(si_dih_means*100)/100, pos=3, col='red')
dev.off()




preg <- flat$pcg_PRGNCY
preg <- cut(preg, breaks=c(-Inf, 0, 5, 10, 15, 20, Inf),
            labels=c('0', '1-5', '6-10', '11-15', '16-20', '20+'))
preg_tab <- round(prop.table(ftable(preg ~ dih$DaysInHospital_Y2), 2) * 100)
preg_dih <- data.frame(NumInternal=preg, dih=dih$DaysInHospital_Y2)
preg_dih_means <- c(mean(preg_dih[preg_dih$NumInternal == '0',]$dih),
                  mean(preg_dih[preg_dih$NumInternal == '1-5',]$dih),
                  mean(preg_dih[preg_dih$NumInternal == '6-10',]$dih),
                  mean(preg_dih[preg_dih$NumInternal == '11-15',]$dih),
                  mean(preg_dih[preg_dih$NumInternal == '16-20',]$dih),
                  mean(preg_dih[preg_dih$NumInternal == '20+',]$dih))
preg_dih_vars  <- c(var(preg_dih[preg_dih$NumInternal == '0',]$dih),
                  var(preg_dih[preg_dih$NumInternal == '1-5',]$dih),
                  var(preg_dih[preg_dih$NumInternal == '6-10',]$dih),
                  var(preg_dih[preg_dih$NumInternal == '11-15',]$dih),
                  var(preg_dih[preg_dih$NumInternal == '16-20',]$dih),
                  var(preg_dih[preg_dih$NumInternal == '20+',]$dih))


png('./plots/preg-means.png', width=3.25, height=3.25, units='in', res=1200, pointsize=4)
par(cex.lab=1.25, cex.axis=1, cex.main=1.5, cex.sub=1.5)
xx <- barplot(preg_dih_means, beside=T, ylim=c(0.0, 2.5),
              main='',
              xlab='Number of Pregnancy Conditions Claims', ylab='Mean Days In Hospital',
              names.arg=c('0', '1-5', '6-10', '11-15', '16-20', '20+'))
text(xx, preg_dih_means, label=round(preg_dih_means*100)/100, pos=3, cex=0.8, col='red')
dev.off()


# PROJECT 2

# load data
# make sure to do Session > Set Working Directory > To Source File Location
setwd("~/Desktop/Data Mining/data-mining-labs/Project2")
Claims <- read.csv(file="Data/hospital3/Claims.csv")
DaysInHospital_Y2 <- read.csv(file="Data/hospital3/DaysInHospital_Y2.csv")
DaysInHospital_Y3 <- read.csv(file="Data/hospital3/DaysInHospital_Y3.csv")
DrugCount <- read.csv(file="Data/hospital3/DrugCount.csv")
LabCount <- read.csv(file="Data/hospital3/LabCount.csv")
PrimaryConditionGroup <- read.csv(file="Data/hospital3/Lookup PrimaryConditionGroup.csv")
ProcedureGroup <- read.csv(file="Data/hospital3/Lookup ProcedureGroup.csv")
Members <- read.csv(file="Data/hospital3/Members.csv")
Target <- read.csv(file="Data/hospital3/Target.csv")
print("Data loaded successfully.")

###################################################################################################################
members_list <- c("Y1", "Y2", "Y3")

if(!file.exists("Data/membersY1.rds")) {
		claims <- read.csv(file="Data/hospital3/Claims.csv")
		members <- read.csv(file="Data/hospital3/Members.csv")
		drugs <- read.csv(file="Data/hospital3/DrugCount.csv")
		labs <- read.csv(file="Data/hospital3/LabCount.csv")
		
    for (member in members_list) {
      # filter Y1 data
      claimsY1 <- claims[claims$Year == member,]
      drugsY1 <- drugs[drugs$Year == member,]
      labsY1 <- labs[labs$Year == member,]
      membersY1 <- members
      
      # add number of claims
      n_claims <- table(claimsY1$MemberID)
      
      membersY1 <- merge(membersY1, data.frame(MemberID=names(n_claims),
                                               claims=as.numeric(n_claims)))
      
      # add median paydelay
      levels(claimsY1$PayDelay)[levels(claimsY1$PayDelay)=="162+"] <- 162
      claimsY1$PayDelay <- as.numeric(as.character(claimsY1$PayDelay))
      membersY1 <- merge(membersY1, aggregate(PayDelay~MemberID,
                                              data=claimsY1, FUN=median))
      
      # add highest Charlson index
      membersY1 <- merge(membersY1, aggregate(CharlsonIndex~MemberID, data=claimsY1,
                                              FUN=function(x) levels(x)[which.max(table(x))]))
      membersY1$CharlsonIndex <- as.factor(membersY1$CharlsonIndex)
      
      # add highest LabCount
      membersY1 <- merge(membersY1, aggregate(LabCount~MemberID, data=labsY1,
      																				FUN=function(x) levels(x)[which.max(table(x))]))
      membersY1$LabCount <- as.factor(membersY1$LabCount)
      levels(membersY1$LabCount) <- c(1,2,3,4,5,6,7,8,9,10) # translate
      membersY1$LabCount <- as.numeric(as.character(membersY1$LabCount))
      
      
      # add highest drug count
      membersY1 <- merge(membersY1, aggregate(DrugCount~MemberID, data=drugsY1,
      																				FUN=function(x) levels(x)[which.max(table(x))]))
      membersY1$DrugCount <- as.factor(membersY1$DrugCount)
      levels(membersY1$DrugCount) <- c(1,2,3,4,5,6,7) # translate
      membersY1$DrugCount <- as.numeric(as.character(membersY1$DrugCount))
      
      
      membersY1$MemberID <- factor(membersY1$MemberID)
      
      summary(membersY1)
      
      # translate age
      levels(membersY1$AgeAtFirstClaim) 
      age <- gsub("(\\d+).*", "\\1", levels(membersY1$AgeAtFirstClaim))
      age
      
      levels(membersY1$AgeAtFirstClaim) <- age
      membersY1$AgeAtFirstClaim <- as.numeric(as.character(membersY1$AgeAtFirstClaim))
      
      # translate Charlson Index
      levels(membersY1$CharlsonIndex)
      levels(membersY1$CharlsonIndex) <- c(0, 1.5, 3.5, 5)
      membersY1$CharlsonIndex <- as.numeric(as.character(membersY1$CharlsonIndex))
      
      # translate sex
      levels(membersY1$Sex) <- c(0, 1, 2) # none, Female, Male
      membersY1$Sex <- as.numeric(as.character(membersY1$Sex))
      
      summary(membersY1)
      
      # save data for later
      saveRDS(membersY1, file = paste("Data/members", member, ".rds", sep=''))
      
    }
}
membersY1 <- readRDS("Data/membersY1.rds")
membersY2 <- readRDS("Data/membersY2.rds")
membersY3 <- readRDS("Data/membersY3.rds")
###################################################################################################################

# select some features for clustering
membersCluster <- membersY1[, c("AgeAtFirstClaim", "claims", 
                                "PayDelay", "CharlsonIndex")]
membersCluster <- membersCluster[complete.cases(membersCluster),]
membersCluster <- scale(membersCluster)
summary(membersCluster)

km <- kmeans(membersCluster, centers = 2) # k-means where k = 2
library(cluster)
clusplot(membersCluster, km$cluster, color = TRUE, col.p = c("blue", "black"))

####### elbow method ######
library(tidyverse)  # data manipulation
set.seed(123)


elbow <- function(clusterVar) {
  # Compute and plot wss for k = 1 to k = 15
  k.values <- 1:15

  # function to compute total within-cluster sum of square   
  wss <- function(k) {
    kmeans(clusterVar, k, nstart = 10 )$tot.withinss
  }
  
  # extract wss for 2-15 clusters
  wss_values <- map_dbl(k.values, wss)
  
  plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
  
}
###############################################
# claims and ageatfirst claim (Year 1)
# ideal num clusters = 3
claims_age_at_first_Y1 <- membersY3[, c("claims", "AgeAtFirstClaim")]
claims_age_at_first_Y1 <- claims_age_at_first_Y1[complete.cases(claims_age_at_first_Y1),]
claims_age_at_first_Y1 <- scale(claims_age_at_first_Y1)
summary(claims_age_at_first_Y1)

km <- kmeans(claims_age_at_first_Y1, centers = 3) # k-means where k = 3
plot(claims_age_at_first_Y1, col=km$cluster)
points(km$centers, pch=3, cex=2) # this adds the centroids
text(km$centers, labels=1:4, pos=2) # this adds the cluster ID
###############################################
# drugs and claims (Year 1)

# translate drug count
levels(drugsY1$DrugCount) 
drugs <- gsub("(\\d+).*", "\\1", levels(drugsY1$DrugCount))

levels(drugsY1$DrugCount) <- drugs
drugsY1$DrugCount <- as.numeric(as.character(drugsY1$DrugCount))



# clustering
# drugs_and_claims = merge(membersY1[,c("MemberID", "claims")], drugsY1[,c("MemberID", "DrugCount")], by="MemberID")


drugsCluster <- membersY3[, c("DrugCount", "claims")]
drugsCluster <- drugsCluster[complete.cases(drugsCluster),]
drugsCluster <- scale(drugsCluster)
summary(drugsCluster)

elbow(drugsCluster) # k = 3

km <- kmeans(drugsCluster, centers = 3) # k-means where k = 3
plot(drugsCluster, col=km$cluster)
points(km$centers, pch=3, cex=2) # this adds the centroids
text(km$centers, labels=1:4, pos=2) # this adds the cluster ID



###############################################
drugsY2 <- drugs[drugs$Year == "Y2",]

# translate drug count
d <- gsub("(\\d+).*", "\\1", levels(drugsY2$DrugCount))
levels(drugsY2$DrugCount) <- d
drugsY2$DrugCount <- as.numeric(as.character(drugsY2$DrugCount))

drugs_and_days_y2 = merge(drugsY2[,c("MemberID", "DrugCount")], DaysInHospital_Y2[,c("MemberID", "DaysInHospital")], by="MemberID")
drugsCluster <- drugs_and_days_y2[, c("DrugCount", "DaysInHospital")]
drugsCluster <- drugsCluster[complete.cases(drugsCluster),]
drugsCluster <- scale(drugsCluster)
summary(drugsCluster)

elbow(drugsCluster) # k = 3

km <- kmeans(drugsCluster, centers = 4) # k-means where k = 4
plot(drugsCluster, col=km$cluster)
points(km$centers, pch=3, cex=2) # this adds the centroids
text(km$centers, labels=1:4, pos=2) # this adds the cluster ID
###############################################
drugsY3 <- drugs[drugs$Year == "Y3",]

# translate drug count
d <- gsub("(\\d+).*", "\\1", levels(drugsY3$DrugCount))
levels(drugsY3$DrugCount) <- d
drugsY3$DrugCount <- as.numeric(as.character(drugsY3$DrugCount))

drugs_and_days_Y3 = merge(drugsY3[,c("MemberID", "DrugCount")], DaysInHospital_Y3[,c("MemberID", "DaysInHospital")], by="MemberID")
drugsCluster <- drugs_and_days_Y3[, c("DrugCount", "DaysInHospital")]
drugsCluster <- drugsCluster[complete.cases(drugsCluster),]
drugsCluster <- scale(drugsCluster)
summary(drugsCluster)

elbow(drugsCluster) # k = 3

km <- kmeans(drugsCluster, centers = 3) # k-means where k = 3
plot(drugsCluster, col=km$cluster)
points(km$centers, pch=3, cex=2) # this adds the centroids
text(km$centers, labels=1:4, pos=2) # this adds the cluster ID



###############################################
# do clusters with 4 and 5, visualize by two  !
# plot3d 

# hierarchical w 4 or 5, look at splits (analyze diff k's)
# within sum of squares
# plot avg for each value within each cluster
###############################################
# select some features for clustering
membersCluster <- membersY1[, c("AgeAtFirstClaim", "claims", 
																"PayDelay", "CharlsonIndex")]


membersCluster <- membersCluster[complete.cases(membersCluster),]
membersCluster <- scale(membersCluster)
summary(membersCluster)

elbow(membersCluster) # k = 3

km <- kmeans(membersCluster, centers = 3) # k-means where k = 3
clusplot(membersCluster, km$cluster, color = TRUE, col.p = c("blue", "black"))
# plot(membersCluster, col=km$cluster)
# points(km$centers, pch=3, cex=2) # this adds the centroids
text(km$centers, labels=1:4, pos=2) # this adds the cluster ID

###############################################
# select some features for clustering
membersCluster <- membersY1[, c("AgeAtFirstClaim","Sex", "claims", 
																"PayDelay", "CharlsonIndex", "LabCount", "DrugCount")]
membersCluster <- membersCluster[complete.cases(membersCluster),]
membersCluster <- scale(membersCluster)
# summary(membersCluster)

# elbow(membersCluster) # k = 2

km <- kmeans(membersCluster, centers = 2) # k-means where k = 2

def.par <- par(no.readonly = TRUE, mar = c(8.1, 4.1, 4.1, 2.1)) # save default, for resetting...
layout(t(1:2)) # 4 plots in one
for(i in 1:2) barplot(km$centers[i,], ylim=c(-3,3), main=paste("Cluster", i), las = 2)

layout(1)
clusplot(membersCluster, km$cluster, color = TRUE, col.p = c("blue", "black"))

library(rgl)
plot3d(membersCluster, col=km$cluster)
points(km$centers, pch=3, cex=2) # this adds the centroids
text(km$centers, labels=1:4, pos=2) # this adds the cluster ID
###############################################



# internal validation code: cluster quality
mC <- membersCluster[2000:4000]
# mC <- mC[complete.cases(mC),]
mC <- scale(mC)
km <- kmeans(mC, centers = 2) # k-means where k = 2

fpc::cluster.stats(d, km$cluster)

d <- dist(membersCluster)
# hc <- cutree(hclust(d, method="complete"), k=10)
hc <- hclust(d, method="complete")


#hc2 <- cutree(hclust(d, method="complete"), k=10)
hc2 <- cut(as.dendrogram(hc), h = 6)$upper
plot(hc2, ylim=c(6,10), leaflab="none", ylab = "Height")
rect.hclust(hc, k=2)

fpc::cluster.stats(d, hc)

dface <- dist(face)
complete3 <- cutree(hclust(dface),3)
cluster.stats(dface,complete3,
							alt.clustering=as.integer(attr(face,"grouping")))

layout(t(1:1)) # 4 plots in one
plot(hc)
rect.hclust(hc, k=2)
# means within each cluster "for(i in 1:4)" graph


# heirarchical and chop off bottom
# "difficult to find clustering"
# upload to folder
# clustering tendency
# log out and log in to do plot3d
library("ggplot2")
library("scales")
library("plyr")
# library("corrplot")
# library("gbm")

#Load the data
setwd("C:/Users/SILVIA TOSHIBA/Documents/MASTER/Data mining techniques/Assignment 2/Data")
#real_train <- read.csv("train.csv", header = TRUE)
#real_test <- read.csv("test.csv", header = TRUE)

#LOAD A SUBSET OF THE DATA FOR MY OWN COMPUTER, NOT ABLE TO LOAD THE WHOLE DATA
real_train <- read.csv("train.csv",header=TRUE, nrows = 100000)
#real_test <- read.csv("train.csv",header=TRUE, nrows = 20)

#First split of the data
dt1 <- sort(sample(nrow(real_train), nrow(real_train)*.85))
whole_train <- real_train[dt1,]
test <- real_train[-dt1,]
#Deletion of the variable (not enogh space)
rm(real_train)

#Second split of the data
dt2 <- sort(sample(nrow(whole_train), nrow(whole_train)*.82353)) #to have 15% of the real_train.
train <- whole_train[dt2,]
validation <- whole_train[-dt2,]
#Deletin of the not useful variables
rm(whole_train)
rm(dt1)
rm(dt2)

# Convert Factors into numeric variables
factor_cols <- unlist(lapply(train, is.factor))
factor_cols["date_time"] <- FALSE

#train[factor_cols] <- as.numeric(as.character(train[factor_cols]))

for (name in colnames(train[factor_cols])){
  train[name] <- as.numeric(sub('NULL',NA,as.character(train[name][,1])))
}

head(train)

## Initial data exploration
# Plot missing values
# Get NA value booleans
nas <- is.na(train)

len.nas <- length(nas[,1])

# Initialize NA vector
na.vec <- vector(mode="integer",length=length(colnames(train)))

# Sum the NA values and fill NA vector
for (i in 1:length(na.vec)){
  na.vec[i] <- sum(nas[,i]) / len.nas
}

# Create bar plot Data Frame
na.df <- data.frame(colnames(train),na.vec)
colnames(na.df) <- c("attribute","missing_values")

# Sort by number of missing values
position <- arrange(na.df,missing_values)["attribute"][,1]

# Plot missing values bar plot
ggplot(data=na.df,aes(x=factor(attribute),y=missing_values)) + 
  geom_bar(stat="identity",width=0.7) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(limits=position) + scale_y_continuous(labels=percent) +
  labs(title="Missing values per attribute", x="Attribute", y="Missing values (%)")

# Data summaries
summary(train)

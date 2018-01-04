### This is scratch pad file to play with data.

library(data.table)

salesDF <- read.csv("../data/SalesKaggle3.csv")


### Col 1 - SoldFlag
salesDF$SoldFlag <- as.factor(salesDF$SoldFlag)
summary(salesDF$SoldFlag) 
# Total rows - 198917
# 0 - 63000  = 31.6%
# 1 - 12996  = 06.5%
# NA- 122921 = 61.8%

# Handle NA's for further analysis
#salesDF$SoldFlag <- ifelse(is.na(salesDF$SoldFlag),-1,salesDF$SoldFlag)

### Col 2 - SoldCount
summary(salesDF$SoldCount)
# Total rows - 198917
# NA- 122921 = 61.8%
summary(salesDF[salesDF$SoldFlag==0,]$SoldCount)
# SoldFlag 0 - No data
summary(salesDF[salesDF$SoldFlag==1,]$SoldCount)
# SoldFlag 1 - 1 to 73
hist(salesDF[salesDF$SoldFlag==1,]$SoldCount)
# Max SoldCount are small

### Col 3- MarketingType
summary(salesDF$MarketingType)
# D - 97971
# S - 100946
summary(salesDF[salesDF$SoldFlag==0,]$MarketingType)
summary(salesDF[salesDF$SoldFlag==1,]$MarketingType)

### Col 4 - ReleaseNumber
summary(salesDF$ReleaseNumber)
summary(salesDF[salesDF$SoldFlag==0,]$ReleaseNumber)
summary(salesDF[salesDF$SoldFlag==1,]$ReleaseNumber)
hist(salesDF[salesDF$SoldFlag==1,]$ReleaseNumber)

### Col 5 - New_Release_Flag
summary(salesDF$New_Release_Flag)
summary(salesDF[salesDF$SoldFlag==0,]$New_Release_Flag)
summary(salesDF[salesDF$SoldFlag==1,]$New_Release_Flag)
unique(salesDF[salesDF$SoldFlag==0,]$New_Release_Flag)
unique(salesDF[salesDF$SoldFlag==1,]$New_Release_Flag)
hist(salesDF[salesDF$SoldFlag==1,]$New_Release_Flag)
hist(salesDF[salesDF$SoldFlag==0,]$New_Release_Flag)

### Col 6 - StrengthFactor
summary(salesDF$StrengthFactor)
summary(salesDF[salesDF$SoldFlag==0,]$StrengthFactor)
boxplot(salesDF[salesDF$SoldFlag==0,]$StrengthFactor)
summary(salesDF[salesDF$SoldFlag==1,]$StrengthFactor)
boxplot(salesDF[salesDF$SoldFlag==1,]$StrengthFactor)
## Lot of outliers

### Col 7 - 
summary(salesDF[salesDF$File_Type=="Active",]$SoldFlag)

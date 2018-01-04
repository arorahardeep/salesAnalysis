library(data.table)
library(corrplot)
library(dplyr)

df <- fread("../data/SalesKaggle3.csv")

historicSales <- df[df$File_Type=="Historical",]
activeInv <- df[df$File_Type=="Active",]

### This is scratch pad file to play with data.

### Look at sales data first
summary(historicSales)

length(unique(historicSales$SKU_number))
length(unique(activeInv$SKU_number))

setdiff(activeInv$SKU_number,historicSales$SKU_number)
invSalesRecord <- activeInv[activeInv$SKU_number %in% historicSales$SKU_number,]

#historicSales$StrByYear <- historicSales$StrengthFactor / historicSales$ReleaseYear
historicSales[, `:=`(mic=mean(ItemCount)), by=ReleaseYear]

cols = c("File_Type","MarketingType")
head(scale(historicSales[,!cols,with=FALSE]))

M <- cor(scale(historicSales[,!cols,with=FALSE]))

corrplot(M, method="number")


x <- df[, `:=`(CNT=.N), by=SKU_number]

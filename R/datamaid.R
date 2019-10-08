#' 25 August
#' Data Maid Script, reads data and outputs .Rdata and CSV files
#' Mantele Senoge
#' 
#' 


#--------------------------------------------------------
# Libraries ---------------------------------------------
library(tidyverse)
library(lubridate)
library(xlsx)
library(readxl)
library(timeSeries)
library(xts)


#---------------------------------------------------------
# Read in Raw daily data -----------------------------------------

set.seed(2019)
sheets = c('ask', 'bid', 'vol', 'close1dayago', 'Dailyopen', 'JSEallshare')
columnDataTypes = c('date', rep('numeric', 482)) # coloumn data types

sheet1 = as.data.frame(read_excel("data/raw/daily - jse.xlsx", sheet = sheets[1], col_types = columnDataTypes))
sheet2 = as.data.frame(read_excel("data/raw/daily - jse.xlsx", sheet = sheets[2], col_types = columnDataTypes))
sheet3 = as.data.frame(read_excel("data/raw/daily - jse.xlsx", sheet = sheets[3], col_types = columnDataTypes))
sheet4 = as.data.frame(read_excel("data/raw/daily - jse.xlsx", sheet = sheets[4], col_types = columnDataTypes))
sheet5 = as.data.frame(read_excel("data/raw/daily - jse.xlsx", sheet = sheets[5], col_types = columnDataTypes))
sheet6 = as.data.frame(read_excel("data/raw/daily - jse.xlsx", sheet = sheets[6], col_types = c('date','numeric')))
#sheet7 = as.data.frame(read_excel("~/Desktop/Data_management/daily - jse.xlsx", sheet = sheets[4], col_types = columnDataTypes))

sheet1 = xts(x = sheet1[,-1],order.by = as.Date(sheet1[,1]))
sheet2 = xts(x = sheet2[,-1],order.by = as.Date(sheet2[,1]))
sheet3 = xts(x = sheet3[,-1],order.by = as.Date(sheet3[,1]))
sheet4 = xts(x = sheet4[,-1],order.by = as.Date(sheet4[,1]))
sheet5 = xts(x = sheet5[,-1],order.by = as.Date(sheet5[,1]))
sheet6 = xts(x = sheet6[,-1],order.by = as.Date(sheet6[,1]))


dailyBook = list('ask'=sheet1, 'bid'=sheet2, 'vol'=sheet3, 'close1dayago'= sheet4, 'Dailyopen'=sheet5, 'JSEallshare' =sheet6)
sampleColNames = colnames(dailyBook$vol[,sample(20)])


#---------------------------------------------------------
# Read in Raw quarterly data -----------------------------

set.seed(2019)
sheets = c('ask', 'bid', 'marketcap','vol', 'shares outstanding')
columnDataTypes = c('date', rep('numeric', 481)) # coloumn data types
columnDataTypes1 = c('date', rep('numeric', 482)) # coloumn data types

sheet1 = as.data.frame(read_excel("data/raw/quarterly - jse.xlsx", sheet = sheets[1], col_types = columnDataTypes))
sheet2 = as.data.frame(read_excel("data/raw/quarterly - jse.xlsx", sheet = sheets[2], col_types = columnDataTypes))
sheet3 = as.data.frame(read_excel("data/raw/quarterly - jse.xlsx", sheet = sheets[3], col_types = columnDataTypes))
sheet4 = as.data.frame(read_excel("data/raw/quarterly - jse.xlsx", sheet = sheets[4], col_types = columnDataTypes))
sheet5 = as.data.frame(read_excel("data/raw/quarterly - jse.xlsx", sheet = sheets[5], col_types = columnDataTypes1))


sheet1 = xts(x = sheet1[,-1],order.by = as.Date(sheet1[,1]))
sheet2 = xts(x = sheet2[,-1],order.by = as.Date(sheet2[,1]))
sheet3 = xts(x = sheet3[,-1],order.by = as.Date(sheet3[,1]))
sheet4 = xts(x = sheet4[,-1],order.by = as.Date(sheet4[,1]))
sheet5 = xts(x = sheet5[,-1],order.by = as.Date(sheet5[,1]))


quarterlyBook = list('ask'=sheet1, 'bid'=sheet2, 'marketcap'=sheet3, 'vol'= sheet4, 'shares outstanding'=sheet5)
sampleColNames = colnames(quarterlyBook$vol[,sample(20)])


columnDataTypes = c('date', rep('numeric', 400)) # coloumn data types
sheet1 = as.data.frame(read_excel("data/raw/quarterly - jsemarketcap.xlsx", sheet = "marketcap", col_types = columnDataTypes))
sheet1 = xts(x = sheet1[,-1],order.by = as.Date(sheet1[,1]))
quarterlymarketcap = sheet1

save.image(file = "data/processed/JSE_env1.RData")




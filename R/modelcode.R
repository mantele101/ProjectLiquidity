load("data/processed/JSE_env1.RData")

#-------------------------------------------
# NB variables -----------------------------

dates = time(quarterlyBook$ask)
datesv2 = as.character(as.Date(dates, "%m/%d/%Y"), "%Y%m%d")


#-------------------------------------------
# Dependencies -----------------------------


library(readxl)
library(timeSeries)
library(xts)
library(fmdates)
library(lubridate)
library(assertthat)
library(bizdays)
library(timeDate)
library(dplyr)
set.seed(2019)

#-------------------------------------------
# useful functions -------------------------
lagging  = function(x){
  return(x$acf[2])
}

#-------------------------------------------
# Random sample -----------------------------

set.seed(2019)
equitynames = names(dailyBook$ask)
sampleNames = sample(equitynames,50,replace = F)
saveRDS(sampleNames, file = "data/processed/samplenames.rds")

#-------------------------------------------
# useful variables -------------------------

dailymktreturn = diff(log(dailyBook$JSEallshare)) # remember this is where we added na omit. If any na errors persist, start here!!!
dailyreturn = diff(log(dailyBook$close1dayago))

dailyreturn.sub1 = list()
dailyreturn.sub2 = list()
dailyreturn.sub3 = list()

for (i in 1:length(sampleNames)){
  dailyreturn.sub1[[i]] = dailyreturn[dailymktreturn<0, sampleNames[i]]
  dailyreturn.sub1[[i]] = dailyreturn.sub1[[i]][dailyreturn.sub1[[i]]!=0]  
  
  dailyreturn.sub2[[i]] = dailyreturn[dailymktreturn>0, sampleNames[i]]
  dailyreturn.sub2[[i]] = dailyreturn.sub2[[i]][dailyreturn.sub2[[i]]!=0]
  
  dailyreturn.sub3[[i]] = dailyreturn[,sampleNames[i]][dailyreturn[,sampleNames[i]]==0]
}



#-------------------------------------------
# Quoted spread ----------------------------

minusspread = quarterlyBook$ask[,sampleNames]/100 - quarterlyBook$bid[,sampleNames]/100
plusspread = quarterlyBook$ask[,sampleNames]/100 + quarterlyBook$bid[,sampleNames]/100

term1 = (minusspread[2:(nrow(minusspread)),])/(plusspread[2:(nrow(minusspread)),]/2)

term2 = minusspread[1:(nrow(minusspread)-1),]/(plusspread[1:(nrow(minusspread)-1),]/2)

outputterm1 = which(term1<0, arr.ind = T)
outputterm2 = which(term2<0, arr.ind = T)

term1[outputterm1[,1], outputterm1[,2]] = NA
term2[outputterm2[,1], outputterm2[,2]] = NA

quotedspreadq = 0.5*(as.matrix(term1)+as.matrix(term2))
quotedspreadmeanq = rowMeans(quotedspreadq, na.rm = TRUE)

#--------------------------------------
# Turnover ----------------------------

#assuming they are using just trading days

Turnover = 1/66 *quarterlyBook$vol[,sampleNames]/(quarterlyBook$`shares outstanding`[,sampleNames] *1000000)
turnovermeanq = rowMeans(Turnover, na.rm = T)

saveRDS(Turnover, file = "data/processed/turnover.rds")

#--------------------------------------
# Amihud's measure --------------------

dates = time(quarterlyBook$ask)
saveRDS(dates, file = "data/processed/dates.rds")
datesv2 = as.character(as.Date(dates, "%m/%d/%Y"), "%Y%m%d")

dailyreturnperq = list()
amihudmeas = list()

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  prices.q = dailyBook$close1dayago[paste(p, "/", q, sep=""),sampleNames]
  volume.q = dailyBook$vol[paste(p, "/", q, sep=""),sampleNames]
  returns.q = abs(diff(log(prices.q)))
  output1 = which(returns.q>0.5, arr.ind = T)
  returns.q[output1[,1], output1[,2]] = NA
  output2 = which(returns.q<(-0.5), arr.ind = T)
  returns.q[output2[,1], output2[,2]] = NA
  ami = abs(returns.q) / (prices.q*volume.q)
  output3 = which(ami == Inf, arr.ind = T)
  returns.q[output3[,1], output3[,2]] = NA
  amihudmeas[[i]] = 1e+06 * 1/66 * colSums( ( abs(returns.q) / (prices.q*volume.q)) , na.rm = T   )
  dailyreturnperq[[i]] = diff(log(dailyBook$close1dayago[paste(p, "/", q, sep=""),sampleNames]))
}

amihudmeasmeanq = lapply(amihudmeas,mean, trim= 0.01)
amihudmeasmeanqv = unlist(amihudmeasmeanq)

saveRDS(amihudmeas, file = "data/processed/amihud.rds")

#--------------------------------------
# Roll's measure ----------------------

autocov = list()
roll = list()
rollmean = list()

for (i in 1:39){
  autocov1 = apply(dailyreturnperq[[i]], 2,  acf, lag.max = 1, type = "covariance", plot = F, na.action = na.pass)
  autocov[[i]] = unlist(lapply(autocov1, lagging))
  roll[[i]] = 2*sqrt(abs(autocov[[i]])) #we get rid of the lag 0, the titles as well for numerical manipulation to be possible
  rollmean[[i]] = mean(roll[[i]], na.rm = TRUE)
}

rollmeanv = unlist(rollmean)
rollmeanv

saveRDS(roll, file = "data/processed/roll.rds")

#--------------------------------------
# FHT measure -------------------------

zeroes.matrix = matrix(nrow = 40, ncol = 50)
fht.matrix = matrix(nrow = 40, ncol = 50)
variance.matrix = matrix(nrow = 40, ncol = 50)

for (j in 1:length(sampleNames)){
  for (i in 1:(length(datesv2)-1)){
    p = as.numeric(datesv2[i])
    q = as.numeric(datesv2[i+1])
    zeroret.q = dailyreturn.sub3[[j]][paste(p, "/", q, sep=""),]
    return.q = dailyreturn[paste(p, "/", q, sep=""),sampleNames[j]]
    variance.q = var(return.q, na.rm = TRUE)
    variance.matrix[i,j] = variance.q
    zeroes.matrix[i,j] = length(zeroret.q)/91
    fht.matrix[i,j] = 2*sqrt(variance.q)*qnorm( (1+zeroes.matrix[i,j]) /2)
  }
}

fht.r = rowMeans(fht.matrix, na.rm = TRUE)
saveRDS(fht.matrix, file = "data/processed/fhtmatrix.rds")

#--------------------------------------
# marketcap ---------------------------

sampleNamesv1%in%names(quarterlymarketcap)
sampleNamesv1 = c(sampleNames[1:7], sampleNames[9:10], sampleNames[12:15], sampleNames[17:25], sampleNames[27:32], sampleNames[34:36], sampleNames[42], sampleNames[44:45], sampleNames[47:48], sampleNames[50])

quarterlymktcapmean.q = rowMeans(quarterlymarketcap[,sampleNamesv1], na.rm = TRUE)*1000000

#--------------------------------------
# Price -------------------------------

averageprice = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  prices.q = dailyBook$close1dayago[paste(p, "/", q, sep=""),sampleNames]
  averageprice[[i]] = mean(colMeans(prices.q, na.rm = TRUE), na.rm = TRUE)
}

averagepricev = unlist(averageprice)

#--------------------------------------
# Volume ------------------------------

averagevolume = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  volume.q = dailyBook$vol[paste(p, "/", q, sep=""),sampleNames]
  averagevolume[[i]] = mean(colMeans(volume.q, na.rm = TRUE) ,na.rm=TRUE)
}

averagevolumev = unlist(averagevolume)

#--------------------------------------
# Variance ----------------------------

averagevariance = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  returns.q = dailyreturn[paste(p, "/", q, sep=""),sampleNames]
  averagevariance[[i]] = mean( apply(returns.q, MARGIN = 2, FUN = sd, na.rm=T)^2 , na.rm = TRUE)
}

averagevariancev = unlist(averagevariance)

#--------------------------------------
# list of measures --------------------

measures = list('Quoted spread'=quotedspreadmeanq, 'Turnover'=turnovermeanq, 'Amihud' = amihudmeasmeanqv, 'Roll'=rollmeanv, 'fht' = fht.r)
saveRDS(measures, file = "data/processed/Computedmeasures.rds")

---------------------------------------
## list of stolls variables -----------

stollvariables = list('Marketcap' = quarterlymktcapmean.q, 'Price'=averagepricev, 'Volume' = averagevolumev, 'Variance' = averagevariancev)
saveRDS(stollvariables, file = "data/processed/StollVariables.rds")

#--------------------------------------
# summary statistics  -----------------

summaryturnover = summary(turnovermeanq)
summaryamihud = summary(amihudmeasmeanqv)
summaryroll = summary(rollmeanv)
summaryfht = summary(fht.r)

sdturnover = sd(turnovermeanq)
sdamihud = sd(amihudmeasmeanqv)
sdroll = sd(rollmeanv)
sdfht = sd(fht.r, na.rm = T)






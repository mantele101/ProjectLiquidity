measures = readRDS(file='data/processed/Computedmeasures.rds')
stollvar = readRDS(file='data/processed/StollVariables.rds')
sampleNames = readRDS(file = "data/processed/samplenames.rds")
turnover = readRDS(file = "data/processed/turnover.rds")
amihudmeas = readRDS(file = "data/processed/amihud.rds")
roll = readRDS(file = "data/processed/roll.rds")
fht.matrix = readRDS(file = "data/processed/fhtmatrix.rds")

load("data/processed/JSE_env1.RData")

#-------------------------------------------
# NB variables -----------------------------
sampleNamesv1 = c(sampleNames[1:7], sampleNames[9:10], sampleNames[12:15], sampleNames[17:25], sampleNames[27:32], sampleNames[34:36], sampleNames[42], sampleNames[44:45], sampleNames[47:48], sampleNames[50])

#-------------------------------------------
# Correllation analysis --------------------

#using proportional spread 
spreadvsturnover = cor(unlist(measures[1]), unlist(measures[2])[-1], method = c("pearson"))
spreadvsamihud = cor(unlist(measures[1]), unlist(measures[3]), method = c("pearson"))
spreadvsroll = cor(unlist(measures[1]), unlist(measures[4]), method = c("pearson"))
spreadvsfht = cor(unlist(measures[1]), unlist(measures[5])[-40], method = c("pearson"))

#using quarterly bid ask spread 

quarterlybidask = as.numeric(rowMeans(quarterlyBook$ask[,sampleNames] - quarterlyBook$bid[,sampleNames], na.rm = TRUE))

spreadvsturnover.1 = cor(quarterlybidask[-1], unlist(measures[2])[-1], method = c("pearson"))
spreadvsamihud.1 = cor(quarterlybidask[-1], unlist(measures[3]), method = c("pearson"))
spreadvsroll.1 = cor(quarterlybidask[-1], unlist(measures[4]), method = c("pearson"))
spreadvsfht.1 = cor(quarterlybidask[-1], unlist(measures[5])[-40], method = c("pearson"))

#-------------------------------------------
# Regression analysis ----------------------

#using proportional spread 
lm.liquidityproxies = lm(unlist(measures[1]) ~ log(unlist(stollvar[2])) + log(unlist(stollvar[3])) + unlist(stollvar[4]) + log(unlist(stollvar[1])[2:40]))
lm.varvsroll = lm(unlist(measures[1]) ~ log(unlist(stollvar[2])) + log(unlist(stollvar[3])) + unlist(stollvar[4]) + log(unlist(stollvar[1])[2:40]) + unlist(measures[4]))
lm.varvsamihud = lm(unlist(measures[1]) ~ log(unlist(stollvar[2])) + log(unlist(stollvar[3])) + unlist(stollvar[4]) + log(unlist(stollvar[1])[2:40]) + unlist(measures[3]))
lm.varvsturnover = lm(unlist(measures[1]) ~ log(unlist(stollvar[2])) + log(unlist(stollvar[3])) + unlist(stollvar[4]) + log(unlist(stollvar[1])[2:40]) + unlist(measures[2])[-1])
lm.varvsfht = lm(unlist(measures[1]) ~ log(unlist(stollvar[2])) + log(unlist(stollvar[3])) + unlist(stollvar[4]) + log(unlist(stollvar[1])[2:40]) + unlist(measures[5])[-40])

#-------------------------------------------
# the decile portfolios --------------------
existant = c(sampleNames[1:7], sampleNames[9:10], sampleNames[12:15], sampleNames[17:25], sampleNames[27:32], sampleNames[34:36], sampleNames[42], sampleNames[44:45], sampleNames[47:48], sampleNames[50])
existantdata = quarterlymarketcap[41, existant]

write.xlsx(existantdata, file ="data/processed/marketcap.xlsx" , sheetName = "marketcap", append = FALSE)
columnDataTypes2 = c('text', 'numeric', 'numeric') # coloumn data types
marketcapsheet = as.data.frame(read_excel("data/processed/marketcap.xlsx", sheet = "marketcap", col_types = columnDataTypes2))

group1 = marketcapsheet[1:10, 1]
group2 = marketcapsheet[11:20, 1]
group3 = marketcapsheet[21:30, 1]
group4 = marketcapsheet[31:40, 1]
group5 = marketcapsheet[41:50, 1]

#---------------------------------------------------------------------------------------------------------
# group1 -------------------------------------------------------------------------------------------------

#quoted spread 

quotedspread.1 = quotedspreadq[,group1]
quotedspread.1mean = rowMeans(quotedspread.1, na.rm = TRUE)

#turnover 

turnover.1 = turnover[,group1]
turnover.1mean = rowMeans(turnover.1, na.rm = T)

#amihud 

amihudmeas.1 = list()

for (i in (1:39)){
  amihudmeas.1[[i]] = amihudmeas[[i]][group1]
}

amihudmeasmean.1mean = lapply(amihudmeas.1, mean)

#roll

roll.1 = list()
rollmean.1 = list()

for (i in 1:39){
  roll.1[[i]] = roll[[i]][group1]
  rollmean.1[[i]] = mean(roll.1[[i]], na.rm = TRUE)
}

#fht 

fht.1 = fht.matrix[,which(sampleNames %in% group1)]
fht.1.mean = rowMeans(fht.1, na.rm = TRUE)

#price
averageprice.g1 = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  prices.q = dailyBook$close1dayago[paste(p, "/", q, sep=""),group1]
  averageprice.g1[[i]] = mean(colMeans(prices.q, na.rm = TRUE), na.rm = TRUE)
}

averagepricev.g1 = unlist(averageprice.g1)

# Volume 

averagevolume.g1 = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  volume.q = dailyBook$vol[paste(p, "/", q, sep=""),group1]
  averagevolume.g1[[i]] = mean(colMeans(volume.q, na.rm = TRUE) ,na.rm=TRUE)
}

averagevolumev.g1 = unlist(averagevolume.g1)

# Variance 

averagevariance.g1 = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  returns.q = dailyreturn[paste(p, "/", q, sep=""),group1]
  averagevariance.g1[[i]] = mean( apply(returns.q, MARGIN = 2, FUN = sd, na.rm=T)^2 , na.rm = TRUE)
}

averagevariancev.g1 = unlist(averagevariance.g1)

#marketcap 
group1%in%sampleNamesv1
quarterlymktcapmean.q.g1 = rowMeans(quarterlymarketcap[,group1[3:9]], na.rm = TRUE)*1000000

#correllation
spreadvsturnover.g1 = cor(quotedspread.1mean, turnover.1mean[-1], method = c("pearson"))
spreadvsamihud.g1 = cor(quotedspread.1mean, unlist(amihudmeasmean.1mean), method = c("pearson"))
spreadvsroll.g1 = cor(quotedspread.1mean, unlist(rollmean.1), method = c("pearson"))
spreadvsfht.g1 = cor(quotedspread.1mean, fht.1.mean[-40], method = c("pearson"))

#regression

lm.liquiditypoxies.g1 = lm(quotedspread.1mean ~ log(averagepricev.g1) + log(averagevolumev.g1) + averagevariancev.g1 + log(quarterlymktcapmean.q.g1)[2:40])
lm.varvsfht.g1 = lm(quotedspread.1mean ~ log(averagepricev.g1) + log(averagevolumev.g1) + averagevariancev.g1 + log(quarterlymktcapmean.q.g1)[2:40] + fht.1.mean[-40])
lm.varvsroll.g1 = lm(quotedspread.1mean ~ log(averagepricev.g1) + log(averagevolumev.g1) + averagevariancev.g1 + log(quarterlymktcapmean.q.g1)[2:40] + unlist(rollmean.1))
lm.varvsamihud.g1 = lm(quotedspread.1mean ~ log(averagepricev.g1) + log(averagevolumev.g1) + averagevariancev.g1 + log(quarterlymktcapmean.q.g1)[2:40] + unlist(amihudmeasmean.1mean))
lm.varvsturnover.g1 = lm(quotedspread.1mean ~ log(averagepricev.g1) + log(averagevolumev.g1) + averagevariancev.g1 + log(quarterlymktcapmean.q.g1)[2:40] + turnover.1mean[-1])


#-------------------------------------------
# group2 -----------------------------------

#quoted spread 

quotedspread.2 = quotedspreadq[,group2]
quotedspread.2mean = rowMeans(quotedspread.2, na.rm = TRUE)

#turnover 

turnover.2 = turnover[,group2]
turnover.2mean = rowMeans(turnover.2, na.rm = T)

#amihud 

amihudmeas.2 = list()

for (i in (1:39)){
  amihudmeas.2[[i]] = amihudmeas[[i]][group2]
}

amihudmeasmean.2mean = lapply(amihudmeas.2, mean)

#roll

roll.2 = list()
rollmean.2 = list()

for (i in 1:39){
  roll.2[[i]] = roll[[i]][group2]
  rollmean.2[[i]] = mean(roll.2[[i]], na.rm = TRUE)
}

#fht 

fht.2 = fht.matrix[,which(sampleNames %in% group2)]
fht.2.mean = rowMeans(fht.2, na.rm = TRUE)

#price
averageprice.g2 = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  prices.q = dailyBook$close1dayago[paste(p, "/", q, sep=""),group2]
  averageprice.g2[[i]] = mean(colMeans(prices.q, na.rm = TRUE), na.rm = TRUE)
}

averagepricev.g2 = unlist(averageprice.g2)

# Volume 

averagevolume.g2 = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  volume.q = dailyBook$vol[paste(p, "/", q, sep=""),group2]
  averagevolume.g2[[i]] = mean(colMeans(volume.q, na.rm = TRUE) ,na.rm=TRUE)
}

averagevolumev.g2 = unlist(averagevolume.g2)

# Variance 

averagevariance.g2 = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  returns.q = dailyreturn[paste(p, "/", q, sep=""), group2]
  averagevariance.g2[[i]] = mean( apply(returns.q, MARGIN = 2, FUN = sd, na.rm=T)^2 , na.rm = TRUE)
}

averagevariancev.g2 = unlist(averagevariance.g2)

#marketcap 

group2%in%sampleNamesv1
g2names = c(group2[1:3], group2[5:8], group2[10])
quarterlymktcapmean.q.g2 = rowMeans(quarterlymarketcap[,g2names], na.rm = TRUE)*1000000

#correlalation 
spreadvsturnover.g2 = cor(quotedspread.2mean, turnover.2mean[-1], method = c("pearson"))
spreadvsamihud.g2 = cor(quotedspread.2mean, unlist(amihudmeasmean.2mean), method = c("pearson"))
spreadvsroll.g2 = cor(quotedspread.2mean, unlist(rollmean.2), method = c("pearson"))
spreadvsfht.g2 = cor(quotedspread.2mean, fht.2.mean[-40], method = c("pearson"))

#regression

lm.liquiditypoxies.g2 = lm(quotedspread.2mean ~ log(averagepricev.g2) + log(averagevolumev.g2) + averagevariancev.g2 + log(quarterlymktcapmean.q.g2)[2:40])
lm.varvsfht.g2 = lm(quotedspread.2mean ~ log(averagepricev.g2) + log(averagevolumev.g2) + averagevariancev.g2 + log(quarterlymktcapmean.q.g2)[2:40] + fht.2.mean[-40])
lm.varvsroll.g2 = lm(quotedspread.2mean ~ log(averagepricev.g2) + log(averagevolumev.g2) + averagevariancev.g2 + log(quarterlymktcapmean.q.g2)[2:40] + unlist(rollmean.2))
lm.varvsamihud.g2 = lm(quotedspread.2mean ~ log(averagepricev.g2) + log(averagevolumev.g2) + averagevariancev.g2 + log(quarterlymktcapmean.q.g2)[2:40] + unlist(amihudmeasmean.2mean))
lm.varvsturnover.g2 = lm(quotedspread.2mean ~ log(averagepricev.g2) + log(averagevolumev.g2) + averagevariancev.g2 + log(quarterlymktcapmean.q.g2)[2:40] + turnover.2mean[-1])


#-------------------------------------------
# group3 -----------------------------------

#quoted spread 

quotedspread.3 = quotedspreadq[,group3]
quotedspread.3mean = rowMeans(quotedspread.3, na.rm = TRUE)

#turnover 

turnover.3 = turnover[,group3]
turnover.3mean = rowMeans(turnover.3, na.rm = T)

#amihud 

amihudmeas.3 = list()

for (i in (1:39)){
  amihudmeas.3[[i]] = amihudmeas[[i]][group3]
}

amihudmeasmean.3mean = lapply(amihudmeas.3, mean)

#roll

roll.3 = list()
rollmean.3 = list()

for (i in 1:39){
  roll.3[[i]] = roll[[i]][group3]
  rollmean.3[[i]] = mean(roll.3[[i]], na.rm = TRUE)
}

#fht 

fht.3 = fht.matrix[,which(sampleNames %in% group3)]
fht.3.mean = rowMeans(fht.3, na.rm = TRUE)

#price
averageprice.g3 = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  prices.q = dailyBook$close1dayago[paste(p, "/", q, sep=""),group3]
  averageprice.g3[[i]] = mean(colMeans(prices.q, na.rm = TRUE), na.rm = TRUE)
}

averagepricev.g3 = unlist(averageprice.g3)

# Volume 

averagevolume.g3 = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  volume.q = dailyBook$vol[paste(p, "/", q, sep=""),group3]
  averagevolume.g3[[i]] = mean(colMeans(volume.q, na.rm = TRUE) ,na.rm=TRUE)
}

averagevolumev.g3 = unlist(averagevolume.g3)

# Variance 

averagevariance.g3 = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  returns.q = dailyreturn[paste(p, "/", q, sep=""),group3]
  averagevariance.g3[[i]] = mean( apply(returns.q, MARGIN = 2, FUN = sd, na.rm=T)^2 , na.rm = TRUE)
}

averagevariancev.g3 = unlist(averagevariance.g3)

#marketcap
group3%in%sampleNamesv1

g3names = c(group3[1], group3[3:4], group3[7], group3[9:10])

quarterlymktcapmean.q.g3 = rowMeans(quarterlymarketcap[,g3names], na.rm = TRUE)*1000000

# correllation 
spreadvsturnover.g3 = cor(quotedspread.3mean, turnover.3mean[-1], method = c("pearson"))
spreadvsamihud.g3 = cor(quotedspread.3mean, unlist(amihudmeasmean.3mean), method = c("pearson"))
spreadvsroll.g3 = cor(quotedspread.3mean, unlist(rollmean.3), method = c("pearson"))
spreadvsfht.g3 = cor(quotedspread.3mean, fht.3.mean[-40], method = c("pearson"))

#regression

lm.liquiditypoxies.g3 = lm(quotedspread.3mean ~ log(averagepricev.g3) + log(averagevolumev.g3) + averagevariancev.g3 + log(quarterlymktcapmean.q.g3)[2:40])
lm.varvsfht.g3 = lm(quotedspread.3mean ~ log(averagepricev.g3) + log(averagevolumev.g3) + averagevariancev.g3 + log(quarterlymktcapmean.q.g3)[2:40] + fht.3.mean[-40])
lm.varvsroll.g3 = lm(quotedspread.3mean ~ log(averagepricev.g3) + log(averagevolumev.g3) + averagevariancev.g3 + log(quarterlymktcapmean.q.g3)[2:40] + unlist(rollmean.3))
lm.varvsamihud.g3 = lm(quotedspread.3mean ~ log(averagepricev.g3) + log(averagevolumev.g3) + averagevariancev.g3 + log(quarterlymktcapmean.q.g3)[2:40] + unlist(amihudmeasmean.3mean))
lm.varvsturnover.g3 = lm(quotedspread.3mean ~ log(averagepricev.g3) + log(averagevolumev.g3) + averagevariancev.g3 + log(quarterlymktcapmean.q.g3)[2:40] + turnover.3mean[-1])


#-------------------------------------------
# group4 -----------------------------------

#quoted spread 

quotedspread.4 = quotedspreadq[,group4]
quotedspread.4mean = rowMeans(quotedspread.4, na.rm = TRUE)

#turnover 

turnover.4 = turnover[,group4]
turnover.4mean = rowMeans(turnover.4, na.rm = T)

#amihud 

amihudmeas.4 = list()

for (i in (1:39)){
  amihudmeas.4[[i]] = amihudmeas[[i]][group4]
}

amihudmeasmean.4mean = lapply(amihudmeas.4, mean)

#roll

roll.4 = list()
rollmean.4 = list()

for (i in 1:39){
  roll.4[[i]] = roll[[i]][group4]
  rollmean.4[[i]] = mean(roll.4[[i]], na.rm = TRUE)
}

#fht 

fht.4 = fht.matrix[,which(sampleNames %in% group4)]
fht.4.mean = rowMeans(fht.4, na.rm = TRUE)

#price
averageprice.g4 = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  prices.q = dailyBook$close1dayago[paste(p, "/", q, sep=""),group4]
  averageprice.g4[[i]] = mean(colMeans(prices.q, na.rm = TRUE), na.rm = TRUE)
}

averagepricev.g4 = unlist(averageprice.g4)

# Volume 

averagevolume.g4 = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  volume.q = dailyBook$vol[paste(p, "/", q, sep=""),group4]
  averagevolume.g4[[i]] = mean(colMeans(volume.q, na.rm = TRUE) ,na.rm=TRUE)
}

averagevolumev.g4 = unlist(averagevolume.g4)

# Variance 

averagevariance.g4 = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  returns.q = dailyreturn[paste(p, "/", q, sep=""), group4]
  averagevariance.g4[[i]] = mean( apply(returns.q, MARGIN = 2, FUN = sd, na.rm=T)^2 , na.rm = TRUE)
}

averagevariancev.g4 = unlist(averagevariance.g4)

#marketcap
group4%in%sampleNamesv1

quarterlymktcapmean.q.g4 = rowMeans(quarterlymarketcap[,group4[-10]], na.rm = TRUE)*1000000

#correllation 
spreadvsturnover.g4 = cor(quotedspread.4mean, turnover.4mean[-1], method = c("pearson"))
spreadvsamihud.g4 = cor(quotedspread.4mean, unlist(amihudmeasmean.4mean), method = c("pearson"))
spreadvsroll.g4 = cor(quotedspread.4mean, unlist(rollmean.4), method = c("pearson"))
spreadvsfht.g4 = cor(quotedspread.4mean, fht.4.mean[-40], method = c("pearson"))

#regression
lm.liquiditypoxies.g4 = lm(quotedspread.4mean ~ log(averagepricev.g4) + log(averagevolumev.g4) + averagevariancev.g4 + log(quarterlymktcapmean.q.g4)[2:40])
lm.varvsfht.g4 = lm(quotedspread.4mean ~ log(averagepricev.g4) + log(averagevolumev.g4) + averagevariancev.g4 + log(quarterlymktcapmean.q.g4)[2:40] + fht.4.mean[-40])
lm.varvsroll.g4 = lm(quotedspread.4mean ~ log(averagepricev.g4) + log(averagevolumev.g4) + averagevariancev.g4 + log(quarterlymktcapmean.q.g4)[2:40] + unlist(rollmean.4))
lm.varvsamihud.g4 = lm(quotedspread.4mean ~ log(averagepricev.g4) + log(averagevolumev.g4) + averagevariancev.g4 + log(quarterlymktcapmean.q.g4)[2:40] + unlist(amihudmeasmean.4mean))
lm.varvsturnover.g4 = lm(quotedspread.4mean ~ log(averagepricev.g4) + log(averagevolumev.g4) + averagevariancev.g4 + log(quarterlymktcapmean.q.g4)[2:40] + turnover.4mean[-1])


#-------------------------------------------
# group5 -----------------------------------

#quoted spread 

quotedspread.5 = quotedspreadq[,group5]
quotedspread.5mean = rowMeans(quotedspread.5, na.rm = TRUE)

#turnover 

turnover.5 = turnover[,group5]
turnover.5mean = rowMeans(turnover.5, na.rm = T)

#amihud 

amihudmeas.5 = list()

for (i in (1:39)){
  amihudmeas.5[[i]] = amihudmeas[[i]][group5]
}

amihudmeasmean.5mean = lapply(amihudmeas.5, mean)

#roll

roll.5 = list()
rollmean.5 = list()

for (i in 1:39){
  roll.5[[i]] = roll[[i]][group5]
  rollmean.5[[i]] = mean(roll.5[[i]], na.rm = TRUE)
}

#fht 

fht.5 = fht.matrix[,which(sampleNames %in% group5)]
fht.5.mean = rowMeans(fht.5, na.rm = TRUE)

#price
averageprice.g5 = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  prices.q = dailyBook$close1dayago[paste(p, "/", q, sep=""),group5]
  averageprice.g5[[i]] = mean(colMeans(prices.q, na.rm = TRUE), na.rm = TRUE)
}

averagepricev.g5 = unlist(averageprice.g5)

# Volume 

averagevolume.g5 = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  volume.q = dailyBook$vol[paste(p, "/", q, sep=""),group5]
  averagevolume.g5[[i]] = mean(colMeans(volume.q, na.rm = TRUE) ,na.rm=TRUE)
}

averagevolumev.g5 = unlist(averagevolume.g5)

# Variance 

averagevariance.g5 = list() 

for (i in 1:(length(datesv2)-1)){
  p = as.numeric(datesv2[i])
  q = as.numeric(datesv2[i+1])
  returns.q = dailyreturn[paste(p, "/", q, sep=""),group5]
  averagevariance.g5[[i]] = mean( apply(returns.q, MARGIN = 2, FUN = sd, na.rm=T)^2 , na.rm = TRUE)
}

averagevariancev.g5 = unlist(averagevariance.g5)

#marketcap
group5%in%sampleNamesv1

g5names = c(group5[1], group5[3:7], group5[9])

quarterlymktcapmean.q.g5 = rowMeans(quarterlymarketcap[,g5names], na.rm = TRUE)*1000000

#correllation
spreadvsturnover.g5 = cor(quotedspread.5mean, turnover.5mean[-1], method = c("pearson"))
spreadvsamihud.g5 = cor(quotedspread.5mean, unlist(amihudmeasmean.5mean), method = c("pearson"))
spreadvsroll.g5 = cor(quotedspread.5mean, unlist(rollmean.5), method = c("pearson"))
spreadvsfht.g5 = cor(quotedspread.5mean, fht.5.mean[-40], method = c("pearson"))

#regression 

lm.liquiditypoxies.g5 = lm(quotedspread.5mean ~ log(averagepricev.g5) + log(averagevolumev.g5) + averagevariancev.g5 + log(quarterlymktcapmean.q.g5)[2:40])
lm.varvsfht.g5 = lm(quotedspread.5mean ~ log(averagepricev.g5) + log(averagevolumev.g5) + averagevariancev.g5 + log(quarterlymktcapmean.q.g5)[2:40] + fht.5.mean[-40])
lm.varvsroll.g5 = lm(quotedspread.5mean ~ log(averagepricev.g5) + log(averagevolumev.g5) + averagevariancev.g5 + log(quarterlymktcapmean.q.g5)[2:40] + unlist(rollmean.5))
lm.varvsamihud.g5 = lm(quotedspread.5mean ~ log(averagepricev.g5) + log(averagevolumev.g5) + averagevariancev.g5 + log(quarterlymktcapmean.q.g5)[2:40] + unlist(amihudmeasmean.5mean))
lm.varvsturnover.g5 = lm(quotedspread.5mean ~ log(averagepricev.g5) + log(averagevolumev.g5) + averagevariancev.g5 + log(quarterlymktcapmean.q.g5)[2:40] + turnover.5mean[-1])



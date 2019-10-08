library(reshape2)
library(ggplot2)

measures = readRDS(file='data/processed/Computedmeasures.rds')
stollvar = readRDS(file='data/processed/StollVariables.rds')
sampleNames = readRDS(file = "data/processed/samplenames.rds")
turnover = readRDS(file = "data/processed/turnover.rds")
amihudmeas = readRDS(file = "data/processed/amihud.rds")
roll = readRDS(file = "data/processed/roll.rds")
fht.matrix = readRDS(file = "data/processed/fhtmatrix.rds")
dates = readRDS(file = "data/processed/dates.rds")
load("data/processed/JSE_env1.RData")

#-------------------------------------------
# heat map ---------------------------------

x= quarterlyBook$ask[,sampleNames]
x = as.data.frame(x)
dimnames(x) = list('dates' = rownames(x), 'stocks' = colnames(x))
inx = which(is.na(x), arr.ind = T)
x[inx[,1],inx[,2]] = 0

x =(cbind(rownames(x),x))
colnames(x)[1] = "date"

#-------------------------------------------
# heat map ---------------------------------

MeltedOOS = melt(x, id =1)
MeltedOOS  = MeltedOOS %>% mutate( ifelse(is.na(value), 0, 1))
colnames(MeltedOOS)[4] = 'yesNo'
head(MeltedOOS)
plt = ggplot(MeltedOOS, aes(x=date, y=variable, fill=yesNo))
plt + geom_tile() + 
  scale_fill_gradient2(low = "blue", high = '#CE6C47', limit = c(0,1), space = "Lab", 
                       name="indicator") +
  labs(title='Suvivorship bias', y='Stock', x = 'Date') + 
  theme(axis.text.x = element_text(face="bold", 
                                   size=8, angle=90), 
        axis.text.y = element_text(face="bold", 
                                   size=7, angle=0), 
        legend.position="none")

x$`OMU SJ Equity`

#-------------------------------------------
# Graphs for each measure ------------------

#_____turnover
turnoverplot = data.frame(turnovermeas = measures[2], dates1 = dates)
turnoverxts = xts(turnoverplot$Turnover, order.by = as.Date(turnoverplot$dates1))
plot(turnoverxts, main = NULL)

turnoverplot$Turnover
?xts

#______amihud 

amihudplot = data.frame(amihudmeas = measures[3], dates1 = dates[-1])
amihudxts = xts(amihudplot$Amihud, order.by = as.Date(amihudplot$dates1))
plot(amihudxts)

#______roll

rollplot = data.frame(rollmeas = measures[4], dates1 = dates[-1])
rollxts = xts(rollplot$Roll, order.by = as.Date(rollplot$dates1))
plot(rollxts)

#_______fht

fhtplot = data.frame(fhtmeas = measures[5], dates1 = dates)
fhtxts = xts(fhtplot$fht, order.by = as.Date(fhtplot$dates1))
plot(fhtxts)

#_______quoted spread

spreadplot = data.frame(spreadmeas = measures[1], dates1 = dates[-1])
spreadxts = xts(spreadplot$Quoted.spread, order.by = as.Date(spreadplot$dates1))
plot(spreadxts)

glenreturns = dailyreturn[, "GLN SJ Equity"]
?qqnorm(glenreturns, plot.it=T)

plot(qqnorm(glenreturns, plot.it=T))
lines(qqline(glenreturns), )

acf(glenreturns, na.action = na.omit)

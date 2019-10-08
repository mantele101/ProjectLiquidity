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
  labs(title='Suvivorship bias', y='Stock', x = 'Date')

x$`OMU SJ Equity`

#-------------------------------------------
# Graphs for each measure ------------------










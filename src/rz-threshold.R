# rz-threshold.R
# 
#   This code extends Ramey and Zubairy (2017) to the model that slack states are estimated from the data,
#  i.e. the threshold parameter will be estimated
#
# Updates:
#   2019-01-10  Original code
#


rm(list=ls())

library('ggplot2')
library('DataCombine')

# ------------------------------------------------------------------------------------
# Set the environment variables
#-------------------------------------------------------------------------------------
# Choose the data length
# 'short': 1920:1 - 2015:4
# 'long' : 1889:1 - 2015:4
# - Notice that the 'tbill' series starts from 1920:1 and you should choose 'short' for using tbill as a factor
dat.length = 'long' # 'short' or 'long'

# Minimum and maximum bounds for the propotion of each regime
tau.lower.bound = 0.05
tau.upper.bound = 0.95
tau.lower.bound = 0
tau.upper.bound = 1

# Number of lags in the regressor part, x
n.lags=4                                    

# Number of time horizon for the impulse-response function
h=20                                    


# ------------------------------------------------------------------------------------
# Read and Clean Data
# ------------------------------------------------------------------------------------
raw.ts=read.csv(file='../data/dat_rz.csv', header=TRUE)
if (dat.length == 'short'){
  my.ts=ts(raw.ts[-c(1:124),], start=c(1920,1), end=c(2015,4), frequency=4)
  dat.year =  time(my.ts)
  my.ts=my.ts[,c('y', 'g', 'newsy', 'unemp', 'tbill')]
}
if (dat.length == 'long'){
  my.ts=data.frame(ts(raw.ts, start=c(1889,1), end=c(2015,4), frequency=4))
  # Generate lag variables
  for (i.lag in (1:n.lags)){
    my.ts=slide(data=my.ts, Var='y', NewVar=paste('L',i.lag,'.y',sep=''), slideBy=-i.lag)
    my.ts=slide(data=my.ts, Var='g', NewVar=paste('L',i.lag,'.g',sep=''), slideBy=-i.lag)
    my.ts=slide(data=my.ts, Var='newsy', NewVar=paste('L',i.lag,'.newsy',sep=''), slideBy=-i.lag)
  }
  dat.year =  time(as.matrix(my.ts))[-(1:4)]
  dat.year =  time(as.matrix(my.ts))
}

y = my.ts[,'y']
x = my.ts[,c('newsy',paste('L',(1:4),'.newsy',sep=''),paste('L',(1:4),'.y',sep=''),paste('L',(1:4),'.g',sep=''))]
x = as.matrix(cbind(1,x))
colnames(x)[1] = 'const'
q = my.ts[,'unemp']
q = as.matrix(slide(data=data.frame(q), Var='q', NewVar='L.q', slideBy=-1))
tau.range = sort(q[,2])
tau.bound = quantile(q,prob=c(tau.lower.bound,tau.upper.bound),na.rm=TRUE)
tau.range = subset(tau.range, (tau.range>tau.bound[1]&tau.range<tau.bound[2]))
n.tau = length(tau.range)
r.sq = rep(NA,n.tau)

for ( i.tau in c(1:n.tau)) {
  ind.regime = (q[,'L.q'] > tau.range[i.tau])
  reg = cbind(x[,-1], ind.regime*x)
  i.model = lm(y~reg)
  r.sq[i.tau] = summary(i.model)$r.squared
  print(summary(i.model))
}
cat('tau range: [', tau.bound[1],'(',tau.lower.bound,'), ', tau.bound[2], '(',tau.upper.bound,')] \n')
tau.hat = tau.range[which.max(r.sq)]
cat('opt.tau = ',tau.hat,'\n')


# Generate new data set for STATA - IV reg
dat.temp=read.csv(file='../data/rz_dat_original.csv', header=T)
fstate = as.integer(dat.temp[,'unemp'] > tau.hat)
dat.temp = cbind(dat.temp,fstate)
write.csv(dat.temp, file='../data/rz_dat_updated.csv', na='')

# Draw recession periods on the output series
fstate.graph = (q[,'q'] > tau.hat)
fstate.graph = fstate.graph[-(1:4)]
my.ts.graph = my.ts[-(1:4),]
dat.year =   as.numeric(time(ts(raw.ts, start=c(1890,1), end=c(2015,4), frequency=4)))


pdf(file="../output/recessions-gdp.pdf")
recess.gdp = ggplot(data.frame(my.ts.graph, dat.year))+
  geom_line(mapping=aes_string(x="dat.year", y="y"))+
  annotate("rect", fill = "blue", alpha = 0.2, 
           xmin = dat.year[fstate.graph], xmax = dat.year[fstate.graph]+0.25,
           ymin = -Inf, ymax = Inf) +
  xlab('Year') +
  ylab('GDP') +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=20))
print(recess.gdp)
dev.off()

pdf(file="../output/recessions-unemp.pdf")
recess.unemp = ggplot(data.frame(my.ts.graph, dat.year))+
  geom_line(mapping=aes_string(x="dat.year", y="unemp"))+
  annotate("rect", fill = "blue", alpha = 0.2, 
           xmin = dat.year[fstate.graph], xmax = dat.year[fstate.graph]+0.25,
           ymin = -Inf, ymax = Inf) +
  xlab('Year') +
  ylab('Unemployment Rate') +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=20))
recess.unemp = recess.unemp  + geom_hline(yintercept=tau.hat, linetype="dashed", color = "red")
print(recess.unemp)
dev.off()

obj.val = 1 - r.sq
tau.bound05 = quantile(q,prob=c(0.05,0.95),na.rm=TRUE)
tau.bound10 = quantile(q,prob=c(0.1,0.9),na.rm=TRUE)
tau.bound15 = quantile(q,prob=c(0.15,0.85),na.rm=TRUE)

pdf(file="../output/obj-values.pdf")
obj.val.fig = ggplot(data.frame(tau.range, obj.val), aes(tau.range,obj.val))+
  geom_line(col='red', linetype = 1) +
  xlab('Unemployment Rate') +
  ylab('1 - R-squared') +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=20))
obj.val.fig = obj.val.fig + geom_vline(xintercept=tau.bound05, linetype=5, col='blue')
obj.val.fig = obj.val.fig + geom_vline(xintercept=tau.bound10, linetype='dashed', col='blue')
obj.val.fig = obj.val.fig + geom_vline(xintercept=tau.bound15, linetype='dotted', col='blue')
print(obj.val.fig)
dev.off()




save.image('../output/rz-base.RData')

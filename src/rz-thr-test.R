rm(list=ls())

# Call libraries
library('ggplot2')
library('DataCombine')

# Import threshold test and estimation codes written by Hansen
source('thr_het.R')
source('thr_est.R')

# ------------------------------------------------------------------------------------
# Set the environment variables
#-------------------------------------------------------------------------------------
# Set seed
set.seed(816402)

# data trimming parameter
trim.ratio = 0.05

# Number of lags in the regressor part, x
n.lags=4                                    

# Output file name
output.name = '../output/rz-thr-test.out'

# ------------------------------------------------------------------------------------
# Read and Clean Data
# ------------------------------------------------------------------------------------
raw.ts=read.csv(file='../data/dat_rz.csv', header=TRUE)
my.ts = data.frame(ts(raw.ts, start=c(1889,1), end=c(2015,4), frequency=4))
my.ts = my.ts[,c('quarter','y','g', 'newsy','unemp')]

# Generate lag variables
my.ts=slide(data=my.ts, Var='unemp', NewVar=paste('L',1,'.unemp',sep=''), slideBy=-1)
for (i.lag in (1:n.lags)){
  my.ts=slide(data=my.ts, Var='y', NewVar=paste('L',i.lag,'.y',sep=''), slideBy=-i.lag)
  my.ts=slide(data=my.ts, Var='g', NewVar=paste('L',i.lag,'.g',sep=''), slideBy=-i.lag)
  my.ts=slide(data=my.ts, Var='newsy', NewVar=paste('L',i.lag,'.newsy',sep=''), slideBy=-i.lag)
}


# Final data: T=500 (1891Q1 - 2014Q4)
dat = subset(my.ts, quarter>=1891)
dat = as.matrix(dat[,-1])

# Program switches #                         

rep <- 2000
dum <- c(3,6:17)

sink(file=output.name)

cat ("Testing for a First Sample Split", "\n")
cat ("\n")
out <- thr_het(dat,1,dum,5,trim.ratio,rep)
cat ("\n")

# Estimate First Sample Split#
h = 1 # Heteroskedasticity  
var.name = colnames(dat)
qhat1 = thr_est(dat,var.name,1,dum,5,h)

  
# Second Level #
k = ncol(dat)
q = dat[,'L1.unemp']
indx = as.matrix((q < qhat1)%*%matrix(c(1),1,k))
dati = as.matrix(dat[indx>0])
dati = matrix(dati,nrow=nrow(dati)/k,ncol=k)
cat ("\n")
cat ("\n")
cat ("Sub-Sample, Unemployment below 11.97", "\n")
cat ("Testing for a Second Sample Split", "\n")
cat ("\n")
out = thr_het(dati,1,dum,5,trim.ratio,rep)
cat ("\n")

sink()
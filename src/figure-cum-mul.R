library('ggplot2')
junkmultse.original = data.frame(read.csv(file='../output/junkmultse-original.csv', header=T))
junkmultse.original = junkmultse.original[1:20,]
junkmultse.original[,'h'] = c(1:20)

junkmultse = data.frame(read.csv(file='../output/junkmultse-newsy.csv', header=T))
junkmultse = junkmultse[1:20,]
junkmultse[,'h'] = c(1:20)

#---------------------------------
# Draw Graphs
#---------------------------------

pdf(file="../output/fig-mul-original.pdf", width=12, height=7.5)
f1 = ggplot(data=junkmultse.original)
f1 = f1 + geom_ribbon(aes(x=h, ymin=multrec1-1.96*seyrec,ymax=multrec1+1.96*seyrec), fill = 'grey70')
f1 = f1 + geom_line(aes(x=h,y=multexp1), linetype='F1',color='red') + geom_line(aes(x=h,y=multrec1), color='blue') 
f1 = f1 + geom_line(aes(x=h,y=multexp1+1.96*seyexp), linetype='longdash', color='red') + geom_line(aes(x=h,y=multexp1-1.96*seyexp), linetype='longdash', color='red')
f1 = f1 + geom_line(aes(x=h,y=1), linetype='dotdash')
f1 = f1 + labs(x='Quarters', y='Cumulative Multiplier')
f1 = f1 + theme(axis.text=element_text(size=16),axis.title=element_text(size=20)) 
f1 = f1 + coord_cartesian(xlim=c(1.9, 19.5))
f1 = f1 + scale_x_continuous(breaks=seq(2,20,2), limits=c(1,20))
print(f1)  
dev.off()


pdf(file="../output/fig-mul-llss-newsy.pdf", width=12, height=7.5)
f2 = ggplot(data=junkmultse)
f2 = f2 + geom_ribbon(aes(x=h, ymin=multrec1-1.96*seyrec,ymax=multrec1+1.96*seyrec), fill = 'grey70') + xlim(1,20)
f2 = f2 + geom_line(aes(x=h,y=multexp1), linetype='f2',color='red') + geom_line(aes(x=h,y=multrec1), color='blue') 
f2 = f2 + geom_line(aes(x=h,y=multexp1+1.96*seyexp), linetype='longdash', color='red') + geom_line(aes(x=h,y=multexp1-1.96*seyexp), linetype='longdash', color='red')
f2 = f2 + geom_line(aes(x=h,y=1), linetype='dotdash')
f2 = f2 + labs(x='Quarters', y='Cumulative Multiplier')
f2 = f2 + theme(axis.text=element_text(size=16),axis.title=element_text(size=20))
f2 = f2 + coord_cartesian(xlim=c(1.9, 19.5))
f2 = f2 + scale_x_continuous(breaks=seq(2,20,2), limits=c(1,20))
print(f2)  
dev.off()

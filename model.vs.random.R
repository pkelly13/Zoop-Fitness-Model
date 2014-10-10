#script that makes vectors of random strategies,and then compares it to the actual strategy to get estimated percent correct for just random data
#12 September 2014 Patrick Kelly

#load data
setwd('~/Zoop-Fitness-Model')
source('model.empirical.comparison.R')

percent.correct<-c()
n.normal<-c()
n.reverse<-c()
n.noDVM<-c()
n.bottom<-c()
for(k in 1:1000){ #run 1000 times
guesses<-round(runif(nrow(dvm.emp),1,4),digits=0) #make vector of random numbers from 1 through 4
strat.guess<-c()
for(j in 1:length(guesses)){ #give each number a strategy
	if(guesses[j]==1){
		strat.guess[j]='normal'
	}
	if(guesses[j]==2){
		strat.guess[j]='reverse'
	}
	if(guesses[j]==3){
		strat.guess[j]='noDVM'
	}
	else if(guesses[j]==4){
		strat.guess[j]='bottom'
	}
}
n.correct<-c()
for(i in 1:nrow(dvm.emp)){ #compare to data and find out how many of the random 
	if(dvm.emp$strategy[i]==strat.guess[i]){
		n.correct[i]=1
	}
	else{
		n.correct[i]=0
	}
}
percent.correct[k]<-sum(n.correct)/length(n.correct)

n.normal[k]<-length(strat.guess[strat.guess=='normal'])
n.reverse[k]<-length(strat.guess[strat.guess=='reverse'])
n.noDVM[k]<-length(strat.guess[strat.guess=='noDVM'])
n.bottom[k]<-length(strat.guess[strat.guess=='bottom'])
}

length(percent.correct[percent.correct]>0.48) #0

#figure out probabilities for each strategy
normal.prob<-nrow(dvm.emp[dvm.emp$strategy=='normal',])/nrow(dvm.emp)
reverse.prob<-nrow(dvm.emp[dvm.emp$strategy=='reverse',])/nrow(dvm.emp)
noDVM.prob<-nrow(dvm.emp[dvm.emp$strategy=='noDVM',])/nrow(dvm.emp)
bottom.prob<-nrow(dvm.emp[dvm.emp$strategy=='bottom',])/nrow(dvm.emp)

pval<-c()
for(l in 1:500){
percent.correct<-c()
for(i in 1:1000){
	guesses<-sample(c('normal','reverse','noDVM','bottom'),nrow(dvm.emp),replace=T,prob=c(normal.prob,reverse.prob,noDVM.prob,bottom.prob))

n.correct<-c()
for(k in 1:nrow(dvm.emp)){ #compare to data and find out how many of the random 
	if(dvm.emp$strategy[k]==guesses[k]){
		n.correct[k]=1
	}
	else{
		n.correct[k]=0
	}
}
percent.correct[i]<-sum(n.correct)/length(n.correct)
}
pval[l]<-length(percent.correct[percent.correct>0.48])/1000
print(l)
}

mean(pval) #p=0.056558
median(pval) #p=0.057
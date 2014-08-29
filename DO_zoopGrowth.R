#DO_zoopGrowth.R - use the paper Homer and Waller 1982 that looks and Daphnia under different diss. oxygen concentrations to determine impact of DO on zooplankton growth rates

num.young<-c(61.2,117,127,124.9)  #number of offspring total at the end of the experiment
n<-c(17,18,18,18)  #number of Daphnia in each experiment
days<-26  #number of days for the experiment

r<-(num.young/n)/days  #determine population growth rate in days

rel.r<-r/max(r)  #percent of growth rate from the maximum

rel.r<-c(0,rel.r)  #add 0 to the beginning at DO of 1.1 mg/L (Koh et al)

DO<-c(0.2,1.8,2.7,3.7,7.6)  #DO concentrations from the experiments

plot(DO,rel.r)

#make ML function to determine parameters for this relationship
monoMolec<-function(p,x,y){
	a=p[1]
	b=p[2]
	sigma=exp(p[3])
	
	yhat<-a*(1-exp(-b*x))
	-sum(dnorm(y,yhat,sigma,log=T))
}
guess<-c(1,1,1)

fit<-optim(par=guess,fn=monoMolec,x=DO,y=rel.r)
#parameters: a=1, b=0.511
#Zooplankton DVM fitness model - objective is to determine the optimal strategy for a given lake
#Need to include cost of swimming, and change predation rates in fitness cost of predation, i.e. probability of predation and opporunity costs involved in probability of predation imoacting future reproduction - oxygen survival ratio = model from Koh et al 1997 in Ecological modeling
#PTK 14 April 2014


#Calculate u/g for each situation for each lake
#+develop a function that will calculate u/g given a set of parameters - develop u from Jensen et al 2006 using reaction distances, swimming speeds, and predator density - use the submodel for growth rates - alter growth rates by costs (O2, DO, etc.)

#source zooplankton growth submodel - can use this function to determine growth rates in different environments
setwd('~/Zoop-Fitness-Model')
source('zoopGrowthSubmodel.R')

#run growth model on each row of model parameters
growth<-c()
for(i in 1:nrow(modelParms)){
if(!is.na(modelParms$mgC_L[i]) & !is.na(modelParms$PC[i]) & !is.na(modelParms$EPA_mgC[i]) & !is.na(modelParms$DHA_mgC[i])){	
parms<-c(FQphyt=0.5,PHYT=modelParms$mgC_L[i],FQdet=0.1,DETc=0.1,aC1=0.9,aC2=0.03,lambda=0.6,wPhyt=1,wDet=0,mu=0.035,PCphyt=modelParms$PC[i],fEPA=modelParms$EPA_mgC[i],fDHA=modelParms$DHA_mgC[i],sPC=0.00001,sEPA=0.00001,sDHA=0.00001,tP=0.1,tEPA=0.1,tDHA=0.1,tM=0.05,m=0.05,v=0.5,e=0.05,Pmin=0.009,Popt=0.05,EPAmin=0.0007,EPAopt=0.0082,DHAmin=0.0001,DHAopt=0.0014,X=0.25,hEPA=0.8,hDHA=0.8,r=0.9,p=0.5,thresh=0.05,Jpufa=0.0118)

times=seq(1,500,by=1)

n=c(Pint=0.5,EPAint=0.5,DHAint=0.5)

test<-ode(y=n,times=times,func=timestep,parms=parms)

Pmin=0.009
Popt=0.05
EPAmin=0.0007
EPAopt=0.0082
DHAmin=0.0001
DHAopt=0.0014
r=0.9

#calculate growth rate
glimP<-(test[41,2]-Pmin)/(Popt-Pmin)
glimEPA<-(test[41,3]-EPAmin)/(EPAopt-EPAmin)
glimDHA<-(test[41,4]-DHAmin)/(DHAopt-DHAmin)

r=0.9
growth[i]<-r*min(c(glimP,glimEPA,glimDHA))
}
if(is.na(modelParms$mgC_L[i]) | is.na(modelParms$PC[i]) | is.na(modelParms$EPA_mgC[i]) | is.na(modelParms$DHA_mgC[i])){
	growth[i]=NA
	}
}





#Epilimnion
Topt<-20 #optimal temperature for zoop growth
Temp<-20 #temperature of environment ###USER INPUT <------
growth<-growth*exp(-0.015*abs(Topt-Temp)^2)
#include cost of low O2 (Koh et al 1997) - as survivorship probability
#if DO > 1.1 --> s=1; if 0.2<DO<1.1 --> s=sqrt(1.111*(DO-0.2)); if DO<0.2 --> s=0
DO=5 #USER INPUT <------
if(DO>=1.1){
	s=1
}
if(DO<1.1 & DO>=0.2){
	s=sqrt(1.111*(DO-0.2))
}
if(DO<0.2){
	s=0
}

#calculate mortality rates
Nm<-0.015 #natural mortality rate
chaobDens<-400 #<---- USER INPUT	
Pchaob<-0.1095+(0.0001302*chaobDens) #calculate predation rate for Daphnia from chaoborus
Pchaob<-Pchaob/1000

chaob<-1 #turn on/off chaob predation.  If present, make 1.  If absent, make 0 <-----USER INPUT
Pchaob<-Pchaob*chaob

Bass<-0 #different fish predation rates if bass are present vs if bass are not present.  Different feeding behaviors of bluegill
NoBass<-1 #Make Bass = 1 when bass are present, 0 when absent.  NoBass should be 1 when no bass are present, and 0 when bass are present

Pfish<-(0.15*Bass)+(0.02*NoBass) #Still need to get this from Brian's data

pvores<-1 #can turn on/off planktivores.  If present make 1, if absent, make 0 <----- USER INPUT
Pfish<-Pfish*pvores

Ptot<-Pfish+Pchaob+Nm
Ps<-1-Ptot

Ps<-Ps*s

#Calculate growth rate for epilimnion specifically
#establish parameters
parms<-c(FQphyt=0.7,PHYT=0.4,FQdet=0.1,DETc=0.1,aC1=0.9,aC2=0.03,lambda=0.6,wPhyt=0.5,wDet=0.5,mu=0.035,PCphyt=0.015,fEPA=0.01,fDHA=0.0003,sPC=0.00001,sEPA=0.00001,sDHA=0.00001,tP=0.1,tEPA=0.1,tDHA=0.1,tM=0.05,m=0.05,v=0.5,e=0.05,Pmin=0.009,Popt=0.05,EPAmin=0.0007,EPAopt=0.0082,DHAmin=0.0001,DHAopt=0.0014,X=0.25,hEPA=0.8,hDHA=0.8,r=0.9)

times=seq(1,500,by=0.1)

n=c(Pint=0.5,EPAint=0.5,DHAint=0.5)

test<-ode(y=n,times=times,func=timestep,parms=parms)

Pmin=0.009
Popt=0.05
EPAmin=0.0007
EPAopt=0.0082
DHAmin=0.0001
DHAopt=0.0014
r=0.9

#calculate growth rate
glimP<-(test[4991,2]-Pmin)/(Popt-Pmin)
glimEPA<-(test[4991,3]-EPAmin)/(EPAopt-EPAmin)
glimDHA<-(test[4991,4]-DHAmin)/(DHAopt-DHAmin)

growth<-r*min(c(glimP,glimEPA,glimDHA))


#Now proportion of time spent in the epilimnion
pEpi<-0.4 #<------USER INPUT

#multiply rates by proportion
Ps.epi<-Ps*pEpi
growth.epi<-growth*pEpi

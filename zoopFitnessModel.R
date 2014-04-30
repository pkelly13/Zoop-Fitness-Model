#Zooplankton DVM fitness model - objective is to determine the optimal strategy for a given lake
#Need to include cost of swimming, and change predation rates in fitness cost of predation, i.e. probability of predation and opporunity costs involved in probability of predation imoacting future reproduction - oxygen survival ratio = model from Koh et al 1997 in Ecological modeling
#PTK 14 April 2014


#Calculate u/g for each situation for each lake
#+develop a function that will calculate u/g given a set of parameters - develop u from Jensen et al 2006 using reaction distances, swimming speeds, and predator density - use the submodel for growth rates

#source zooplankton growth submodel - can use this function to determine growth rates in different environments
setwd('~/Documents/Notre Dame/UNDERC 2013/zoop growth model')
source('zoopGrowthSubmodel_11Mar2014.R')

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


#Calculate growth rate
#List of parameters - only need to change a few for differences in resource quality
FQphyt=0.7 #food quality index for phytoplankton - dimless 
PHYT=0.4 #food concentration of phytoplankton - mg C? <-----user input
FQdet=0.1 #food quality of detritus - dimless
DETc=0.1 #concentration of detritus - mg C?
aC1=0.9 #zooplankton carbon assimilation efficiency - dimless
aC2=0.03 #half sat constant for zoop growth efficiency - (mg C L-1)^1/2
lambda=0.6 #maximum zooplankton grazing rate -  day^-1
wPhyt=0.5 #phytoplankton food preference - dimless
wDet=0.5 #detritus food preference - dimless
mu=0.035 #zooplankton grazing half sat constant
PCphyt=0.015 #phytoplankton P:C - mg P mg C^-1 <----User input
fEPA=0.01 #phytoplankton EFA:C - mg EPA mg C^-1 <----User input
fDHA=0.0003 #phytoplankton DHA:C - mg DHA mg C^-1
sPC=0.001 #detritus P:C - mg P mg C^-1
sEPA=0.00001 #detritus EPA:C - mg EPA mg C^-1
sDHA=0.00001 #detritus DHA:C - mg DHA mg C^-1
tP=0.1 #zooplankton P biomass turnover rate - day ^-1
tEPA=0.1 #zooplankton biomass EPA turnover rate - day^-1
tDHA=0.1 #zooplankton biomass DHA turnover rate - day ^-1
tM=0.05 #zooplankton moult P turnover rate - day^-1
m=0.05 #moult as a fraction of zoop biomass - dimless
v=0.5 #conversion efficiency of EPA to DHA - mg DHA mg EPA^-1
e=0.05 #fraction of EPA to DHA via conversion - dimless
Pmin=0.009 #minimum zooplankton somatic P - mg P mg C^-1
Popt=0.05 #optimal zooplankton somatic P - mg P mg C^-1
EPAmin=0.0007 #minimum zooplankton somatic EPA - mg EPA mg C-1
EPAopt=0.0082 #Optimum zooplankton somatic EPA - mg EPA mg C-1
DHAmin=0.0001 #minimum zooplankton somatic DHA - mg DHA mg C-1
DHAopt=0.0014 #Optimum zooplankton somatic DHA - mg DHA mg C^-1
X=0.25 #Excretion rate - day^-1
hEPA=0.8 #proxy reproduction paramter
hDHA=0.8 #proxy reproduction parameter
r=0.9 # growth rate

parms<-c(FQphyt=FQphyt,PHYT=PHYT,FQdet=FQdet,DETc=DETc,aC1=aC1,aC2=aC2,lambda=lambda,wPhyt=wPhyt,wDet=wDet,mu=mu,PCphyt=PCphyt,fEPA=fEPA,fDHA=fDHA,sPC=sPC,sEPA=sEPA,sDHA=sDHA,tP=tP,tEPA=tEPA,tDHA=tDHA,tM=tM,m=m,v=v,e=e,Pmin=Pmin,Popt=Popt,EPAmin=EPAmin,EPAopt=EPAopt,DHAmin=DHAmin,DHAopt=DHAopt,X=X,hEPA=hEPA,hDHA=hDHA,r=r)


#User input includes parameters for growth model, temperature, DO concentration, Chaoborus density, Bass/NoBass, and pvores (1 or 0), and proportion spent in each environment

#Now, include ways of altering growth and death rates per environmental factors
#need to calculate for both Epi and Hypo, and determine how much time spent in each habitat. i.e. for standard migration, calculate all parameters for night and day, then multiply by proportion of time spent in each habitat during the night and day, respectively.  Then average these together for use in the life table model

#function will calculate u/g for one environment

fit(rmax=0.9,times=seq(1,500,by=1),n=c(Pint=0.5,EPAint=0.5,DHAint=0.5),parms=parms,temp=16,DO=5.6,chaobDens=300,Bass=0,NoBass=1,pvores=1)

fit<-function(rmax,times,n,parms,temp,DO,chaobDens,Bass,NoBass,pvores){
	growth=ode(y=n,times=times,func=timestep,parm=parms)
	glimP=growth[length(times)-2,2]
	glimEPA=growth[length(times)-2,3]
	glimDHA=growth[length(times)-2,4]
	growth=rmax*min(glimP,glimEPA,glimDHA)
	
	growth=growth*exp(-0.015*abs(20-temp)^2)
	
	if(DO>=1.1){
		s=1
	}
	if(DO<1.1 & DO>=0.2){
		s=sqrt(1.111*(DO-0.2))
	}
	if(DO<0.2){
		s=0
	}
	
	Nm=0.015
	Pchaob=(0.1095+(0.0001302*chaobDens))/1000
	
	Pfish=(0.15*NoBass)+(0.02*Bass)
	Pfish=Pfish*pores
	
	mu=Pfish+Pchaob+Nm
	
	mu.g=mu/growth
	return(mu.g)
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

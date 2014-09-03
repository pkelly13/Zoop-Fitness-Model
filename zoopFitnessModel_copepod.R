#Zooplankton DVM fitness model - objective is to determine the optimal strategy for a given lake
#Need to include cost of swimming, and change predation rates in fitness cost of predation, i.e. probability of predation and opporunity costs involved in probability of predation imoacting future reproduction 
#PTK 14 April 2014


#Calculate u/g for each situation for each lake - use modelParms data frame to calculate u/g for each row of the data frame - will be for each PML Day/Night and Hypo Day/Night, then can compare all situations

library(deSolve)
#source zooplankton growth submodel - can use this function to determine growth rates in different environments
setwd('~/Zoop-Fitness-Model')
source('zoopGrowthSubmodel.R')

#open data frame of model parameters
modelParms<-read.csv('fitnessModel_parameters.csv')

#convert PAR to lx = 1ueinstein m-2 s-1 = 54 lx
modelParms$lx<-modelParms$PAR*54

#need to fix RS data
modelParms[modelParms$lakeID=='RS' & modelParms$depthClass=='Hypo',15]=mean(modelParms$PC[modelParms$lakeID=='RS'],na.rm=T)

#calculate reaction distances for fish - convert to m 
modelParms$reactionDist<-(4.3*log(modelParms$lx,10)*(0.1+log(modelParms$lx,10))^-1)/10
#change NaNs to 0
modelParms$reactionDist[is.na(modelParms$reactionDist)]=0

#calculate encounter rates (foraging rate potential) from reaction distance and swimming speeds.  Daphnia swimming speed = 2.26 mm s-1, fish swimming speed = 4.67 cm s-1, chaoborus = nearly 0.  reaction distance of Chaoborus = 2 mm.  Convert to m min-1 -> Daphnia = 0.1356 m min-1, fish = 2.802 m min-1, copepod = 0.42 m min-1 (Yen 1988)
vj=2.802 #fish swimming speed
vi=0.42 #Copepod swimming speed
Pcont=0.333 #Copepod strike efficiency
Peat=1 #copepod strike efficiency
Pfish<-((((pi*modelParms$reactionDist^2)/3)*((3*vj^2+vi^2)/vj)*modelParms$fish_m3)/modelParms$zoops.allDepths)*720

vj=0.0001 #Chaoborus swimming speed - essentially 0
Pchaob<-((((pi*0.002^2)/3)*((3*vj^2+vi^2)/vj)*modelParms$ind_m3)/modelParms$Daphnia)*720
Pchaob<-(Pchaob*Pcont)*Peat

modelParms$mu=Pfish+Pchaob

#run growth model on each row of model parameters
growth<-c()
for(i in 1:nrow(modelParms)){
if(!is.na(modelParms$mgC_L[i]) & !is.na(modelParms$PC[i]) & !is.na(modelParms$EPA_mgC[i]) & !is.na(modelParms$DHA_mgC[i])){	
parms<-c(FQphyt=0.5,PHYT=modelParms$mgC_L[i],FQdet=0.1,DETc=0.1,aC1=0.9,aC2=0.03,lambda=0.4,wPhyt=1,wDet=0,mu=0.035,PCphyt=modelParms$PC[i],fEPA=modelParms$EPA_mgC[i],fDHA=modelParms$DHA_mgC[i],sPC=0.00001,sEPA=0.00001,sDHA=0.00001,tP=0.3,tEPA=0.1,tDHA=0.0184,tM=0.05,m=0.05,v=0.9,e=0.9,Pmin=0.003,Popt=0.03,EPAmin=0.0008,EPAopt=0.008,DHAmin=0.0017,DHAopt=0.0166,X=0.25,hEPA=0.4,hDHA=0.4,r=0.6,p=0.5,thresh=0.05,Jpufa=0.0118)

times=seq(1,500,by=1)

n=c(Pint=0.5,EPAint=0.5,DHAint=0.5)

test<-ode(y=n,times=times,func=timestep,parms=parms) #use parameters from the model parameters above and the zoop growth submodel to determine growth rate based on resource quality data

Pmin=0.003
Popt=0.03
EPAmin=0.0008
EPAopt=0.008
DHAmin=0.0017
DHAopt=0.0166
r=0.6

#calculate growth rate
glimP<-(test[41,2]-Pmin)/(Popt-Pmin)
glimEPA<-(test[41,3]-EPAmin)/(EPAopt-EPAmin)
glimDHA<-(test[41,4]-DHAmin)/(DHAopt-DHAmin)

r=0.6
growthi<-r*min(c(glimP,glimEPA,glimDHA)) #maximum growth rate modified by most limiting nutrient from the growth submodel
growthi<-growthi*exp(-0.015*abs(20-modelParms$temp[i]))  #modify growth rate again by temperature

#influence of dissolved oxygen on zooplankton growth rate - take from Homer and Waller 1982 - see script "DO_zoopGrowth.R" for the determination of the function
if(modelParms$DOmgL[i]>=1.1){
	s=1*(1-exp(-0.511*modelParms$DOmgL[i]))
}
if(modelParms$DOmgL[i]<0.2){
	s=0
}
growth[i]=growthi*s
if(is.na(modelParms$mgC_L[i]) | is.na(modelParms$PC[i]) | is.na(modelParms$EPA_mgC[i]) | is.na(modelParms$DHA_mgC[i])){
	growth[i]=NA
	}
}
}

modelParms$growth=growth

#write data to Zoop-Fitness-Model folder
setwd('~/Zoop-Fitness-Model')
write.csv(modelParms,'fitnessModel_parametersFINAL_copepods.csv')

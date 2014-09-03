#Sensitivity analysis for zooplankton optimal patch use - change fish densities from 0 to something outrageous to see how much fish density changes the optimal strategy by zooplankton
#Prediction is that chaoborus predation, and not fish predation is the significant influence on zooplankton decision making - therefore, changing fish densities shouldn't change the optimal strategy much
#Patrick Kelly 15 July 2014

#borrow some code from zoopFitnessModel.R
#Calculate u/g for each situation for each lake - use modelParms data frame to calculate u/g for each row of the data frame - will be for each PML Day/Night and Hypo Day/Night, then can compare all situations

library(deSolve)
#source zooplankton growth submodel - can use this function to determine growth rates in different environments
setwd('~/Zoop-Fitness-Model')
source('zoopGrowthSubmodel.R')

#open data frame of model parameters
setwd('~/Documents/Notre Dame/UNDERC 2013/zoop growth model/R files')
modelParms<-read.csv('modelParameters_forFitnessModel.csv')

#convert PAR to lx = 1ueinstein m-2 s-1 = 54 lx
modelParms$lx<-modelParms$PAR*54

#calculate reaction distances for fish - convert to m 
modelParms$reactionDist<-(4.3*log(modelParms$lx,10)*(0.1+log(modelParms$lx,10))^-1)/10
#change NaNs to 0
modelParms$reactionDist[is.na(modelParms$reactionDist)]=0

#calculate encounter rates (foraging rate potential) from reaction distance and swimming speeds.  Daphnia swimming speed = 2.26 mm s-1, fish swimming speed = 4.67 cm s-1, chaoborus = nearly 0.  reaction distance of Chaoborus = 2 mm.  Convert to m min-1 -> Daphnia = 0.1356 m min-1, fish = 2.802 m min-1
vj=2.802 #fish swimming speed
vi=0.1356 #Daphnia swimming speed

#make Pfish column for varying fish density (0-5 fish/m3)
PfishEst<-((((pi*modelParms$reactionDist^2)/3)*((3*vj^2+vi^2)/vj)*modelParms$fish_m3)/modelParms$zoops.allDepths)*720
Pfish0<-((((pi*modelParms$reactionDist^2)/3)*((3*vj^2+vi^2)/vj)*0)/modelParms$zoops.allDepths)*720
Pfish0.01<-((((pi*modelParms$reactionDist^2)/3)*((3*vj^2+vi^2)/vj)*0.01)/modelParms$zoops.allDepths)*720
Pfish0.1<-((((pi*modelParms$reactionDist^2)/3)*((3*vj^2+vi^2)/vj)*0.1)/modelParms$zoops.allDepths)*720
Pfish0.25<-((((pi*modelParms$reactionDist^2)/3)*((3*vj^2+vi^2)/vj)*0.25)/modelParms$zoops.allDepths)*720
Pfish0.5<-((((pi*modelParms$reactionDist^2)/3)*((3*vj^2+vi^2)/vj)*0.5)/modelParms$zoops.allDepths)*720
Pfish1<-((((pi*modelParms$reactionDist^2)/3)*((3*vj^2+vi^2)/vj)*1)/modelParms$zoops.allDepths)*720
Pfish5<-((((pi*modelParms$reactionDist^2)/3)*((3*vj^2+vi^2)/vj)*5)/modelParms$zoops.allDepths)*720

#model chaoborus predation risk (Encounter rate)
vj=0.0001 #Chaoborus swimming speed - essentially 0
Pchaob<-((((pi*0.002^2)/3)*((3*vj^2+vi^2)/vj)*modelParms$ind_m3)/modelParms$Daphnia)*720

#estimate mu using all fish predation rates and chaoborus
modelParms$muEst=PfishEst+Pchaob
modelParms$mu0=Pfish0+Pchaob
modelParms$mu0.01=Pfish0.01+Pchaob
modelParms$mu0.1=Pfish0.1+Pchaob
modelParms$mu0.25=Pfish0.25+Pchaob
modelParms$mu0.5=Pfish0.5+Pchaob
modelParms$mu1=Pfish1+Pchaob
modelParms$mu5=Pfish5+Pchaob

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
growthi<-r*min(c(glimP,glimEPA,glimDHA))
growthi<-growthi*exp(-0.015*abs(20-modelParms$temp[i]))
growth[i]=1*(1-exp(-0.511*growthi))
if(is.na(modelParms$mgC_L[i]) | is.na(modelParms$PC[i]) | is.na(modelParms$EPA_mgC[i]) | is.na(modelParms$DHA_mgC[i])){
	growth[i]=NA
	}
}
}

modelParms$growth=growth

#write data to Zoop-Fitness-Model folder
setwd('~/Zoop-Fitness-Model')
write.csv(modelParms,'fitnessModel_parameters_sensAnalysis.csv')

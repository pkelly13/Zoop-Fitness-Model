#change fish data to reflect new calculated CPUE and density
#Patrick Kelly 2 Sept 2014

#load parameter data
setwd('~/Zoop-Fitness-Model')

#load model parameter data
modelParms<-read.csv('fitnessModel_parameters.csv')

#load CPUE data
CPUE<-read.csv('fishCPUE_all.csv')

modelCPUE<-c()
for(i in 1:nrow(modelParms)){
	rowi=match(modelParms$lakeID[i],CPUE$lakeID)
	modelCPUE[i]=CPUE$CPUE[rowi]
}

modelParms$CPUE<-modelCPUE
modelParms$fish_m3<-0.02859*modelParms$CPUE

write.csv(modelParms,'fitnessModel_parameters.csv')
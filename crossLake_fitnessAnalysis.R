#data exploration for zooplankton fitness
#PTK 6 May 2014
#load model parameter data
setwd('~/Zoop-Fitness-Model')
modelParms<-read.csv('fitnessModel_parameters.csv')
#get rid of unused columns
modelParms<-modelParms[,-c(1:3)]

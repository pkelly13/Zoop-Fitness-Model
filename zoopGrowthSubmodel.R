
#Zooplankton growth submodel that uses P, EPA, and DHA impacts on growth 
#load desolve package
require(deSolve)

timestep<-function(t,y,params){
with(as.list(params),{	
	Pint<-y[1]
	EPAint<-y[2]
	DHAint<-y[3]
	
#starts with food quality (FQtot) as a function of the concentration of phytoplankton (PHYT) and detritus (DETc) and their food quality indices

FQtot<-FQphyt^2*sqrt(PHYT)+FQdet^2*sqrt(DETc)

#calculates carbon assimilation efficiency (aC) using food quality and growth efficiency (aC1) and half saturation constant (aC2)

aC<-(aC1*FQtot)/(aC2+FQtot)

#calculates carbon assimilation rate using assimilation efficiency, and preference for phytoplankton (wPhyt) and detritus (wDet), max grazing rate (lambda), and half sat constant (mu)

aSC<-(lambda*aC*(wPhyt*PHYT^2+wDet*DETc^2))/(mu^2+wPhyt*PHYT^2+wDet*DETc^2)

#calculate pools of P, EPA, and DHA from grazed food resource concentrations in phytoplankton (PCphyt,fEPA, fDHA) and detritus (sPC,sEPA, and sDHA)

GRAZp<-(wPhyt*PHYT^2*PCphyt+wDet*DETc^2*sPC)/(wPhyt*PHYT^2+wDet*DETc^2)

GRAZepa<-(wPhyt*PHYT^2*fEPA+wDet*DETc^2*sEPA)/(wPhyt*PHYT^2+wDet*DETc^2)

GRAZdha<-(wPhyt*PHYT^2*fDHA+wDet*DETc^2*sDHA)/(wPhyt*PHYT^2+wDet*DETc^2)

#Post-maintenance reosurce pool (Ppm, EPApm, and DHApm) of resources using internal quotas (Pint, EPAint, DHAint), biomass turnover rate (tP,tEPA,tDHA), moulting turnover rate (tM), moulting fraction (m), EPA-DHA conversion (e) and conversion efficiency of EPA-DHA (v)

Ppm<-aSC*GRAZp-Pint*(tP*(1-m)*m*tM)

#IF EPApm IS GREATER THAN THRESHOLD - DO IT THIS WAY - NEED TO MAKE A DIFFERENT FUNCTION FOR WHEN EPApm IS LESS THAN OR EQUAL TO THE THRESHOLD

EPApm<-(1-e)*(aSC*GRAZepa-EPAint*(tEPA*(1-m)+m*tM))

DHApm<-aSC*GRAZdha-DHAint*(tDHA*(1-m)+m*tM)+v*e*(aSC*GRAZepa-EPAint*(tEPA*(1-m)+m*tM))

#make if statement for if post maintenance EPA and DHA concentrations are below a threshold, C18 PUFA elongation becomes important
if(EPApm < thresh){
	EPApm<-aSC*GRAZepa-EPAint*(tEPA*(1-m)+m*tM)+(1-e)*Jpufa*p
	DHApm<-aSC*GRAZdha-DHAint*(tDHA*(1-m)+m*tM)+e*Jpufa*p*v
}

#resource saturation quotient (glimP, glimEPA, glimDHA) using internal quota of reosurces, optimal somatic concentration of resources, and minimal concentration of resources (Droop model)

glimP<-(Pint-Pmin)/(Popt-Pmin)

glimEPA<-(EPAint-EPAmin)/(EPAopt-EPAmin)

glimDHA<-(DHAint-DHAmin)/(DHAopt-DHAmin)

#now growth rate as altered by the limitation by resource limitation - uses max growth rate (r)

GROWTH<-r*glimP*glimEPA*glimDHA


#differential equations - the change in the internal quota of P, EPA, and DHA - uses the release rate of P (X), and the loss to hormone production (h) for EFAs

Pint<-Ppm-GROWTH*Pint-X*Pint

EPAint<-EPApm-GROWTH*EPAint-hEPA*EPAint

DHAint<-DHApm-GROWTH*DHAint-hDHA*DHAint

return(list(c(Pint,EPAint,DHAint)))
})
}


#test of the zooplankton growth submodel

#establish parameters
parms<-c(FQphyt=0.5,PHYT=0.4,FQdet=0.1,DETc=0.1,aC1=0.9,aC2=0.03,lambda=0.6,wPhyt=0.5,wDet=0.5,mu=0.035,PCphyt=0.05,fEPA=0.02,fDHA=0.001,sPC=0.00001,sEPA=0.00001,sDHA=0.00001,tP=0.1,tEPA=0.1,tDHA=0.1,tM=0.05,m=0.05,v=0.5,e=0.05,Pmin=0.009,Popt=0.05,EPAmin=0.0007,EPAopt=0.0082,DHAmin=0.0001,DHAopt=0.0014,X=0.25,hEPA=0.8,hDHA=0.8,r=0.9,p=0.5,thresh=0.05,Jpufa=0.0118)

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
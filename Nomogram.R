######################## nomgram #################################
source(knitr::purl("C:\\Users\\hajar.hasannejadasl\\Documents\\prospect2022\\FinalModels+calibration/LR-UI-1year.rmd"))

library(rms)
combinedatatrain <-cbind(trainingSet,binaryOutcomeTraining)
dd = datadist(combinedatatrain)
options(datadist = 'dd')

f = lrm(binaryOutcomeTraining ~ diabetes  + cardiovascularDisease + epic26_1_urineverlies1
        + epic26_11_vakerontlasting1 +epic26_13_bloedontlasting1
        + epic26_4_nadruppelen1 + epic26_2_urineophouden1
        +  treatments + hormoneTherapy , data = combinedatatrain)


#summary(f)
nom = nomogram(f, fun=function(x)1/(1+exp(-x)), # or fun=plogis
               fun.at=c(.001,.01,.05,seq(0,1,by=.1),.95,.99,.999),
               funlabel="Outcome to test Probability", lp=F)
plot(nom, force.label=T, xfrac=0.25, cex.axis=.5, cex.var=0.5, tcl=-0.35, ia.space = 0.1)

SMOTE2glmProfile$coefficients

SMOTE2glmProfile$model$`factor(outcomeToTest)`

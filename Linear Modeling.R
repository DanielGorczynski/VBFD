##Test for temporal trends
x <- lm(Occupancy.Avg ~ Year, data = Volcan.Barva.Analysis)
summary(x)

x <- lm(Fdis ~ Year, data = Volcan.Barva.Analysis)
summary(x)

##Volcan Barva Occupancy Average Model Selection
library(MuMIn)
fitFD <- lm(Occupancy.Avg~ FL_VB + Var.Temp + FM_ZOI_90 + FL_ZOI, 
            data=Volcan.Barva.Analysis, na.action = "na.fail")

allFD.dredge <- dredge(fitFD, beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE, extra=list("confint", "adjR^2"))
allFD.dredge
x <- lm(Occupancy.Avg ~ FM_ZOI_90, data = Volcan.Barva.Analysis)
summary(x)

#Volcan Barva Fdis Model Selection
library(MuMIn)
fitFD <- lm(Fdis~ FL_VB + Var.Temp + FM_ZOI_90 + FL_ZOI, 
            data=Volcan.Barva.Analysis, na.action = "na.fail")

allFD.dredge <- dredge(fitFD, beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE, extra=list("confint", "adjR^2"))
allFD.dredge




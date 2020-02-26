##Test for temporal trends
x <- lm(Occupancy.Avg ~ Year, data = Volcan.Barva.Analysis)
summary(x)

x <- lm(Fdis ~ Year, data = Volcan.Barva.Analysis)
summary(x)

##Volcan Barva Occupancy Average Model Selection
library(MuMIn)
fitFD <- lm(Fdis~ FL_VB + Var.Temp + FM_ZOI_90 + FL_ZOI, 
            data=VBAnalysis_Ordered, na.action = "na.fail")

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

##Test for temporal trends in CWM values
p <- lm(VB.FD$CWM$Body.Mass ~ Year, data = Volcan.Barva.Analysis)
p <- lm(VB.FD$CWM$Diet ~ Year, data = Volcan.Barva.Analysis)
p <- lm(VB.FD$CWM$Social.Group.Size ~ Year, data = Volcan.Barva.Analysis) ##Significant
p <- lm(VB.FD$CWM$Habitat ~ Year, data = Volcan.Barva.Analysis)
p <- lm(VB.FD$CWM$Activity.Period ~ Year, data = Volcan.Barva.Analysis) ##Significant
p <- lm(VB.FD$CWM$Avg..Litter.Size ~ Year, data = Volcan.Barva.Analysis)
summary(p)

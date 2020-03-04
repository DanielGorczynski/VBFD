

##Test for temporal trends

x <- lm(Fdis ~ Year, data = VBAnalysis_Ordered)
summary(x)


#Volcan Barva Fdis Model Selection
library(MuMIn)
fitFD <- lm(Fdis~ FL_VB + Var.Temp + FM_ZOI_90 + FL_ZOI, 
            data=Volcan.Barva.Analysis, na.action = "na.fail")

allFD.dredge <- dredge(fitFD, beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE, extra=list("confint", "adjR^2"))
allFD.dredge

##Test for temporal trends in traits
##Make ordered categories for clm ordinal regression
Ordinal.trait.analysis$Diet <- ordered(Ordinal.trait.analysis$Diet, 
                                       levels = c("Browser","Browser/Frugivore",
                                                  "Frugivore","Omnivore","Insectivore","Zoophage","Carnivore"))
Ordinal.trait.analysis$Social <- ordered(Ordinal.trait.analysis$Social, 
                                         levels = c("Solitary/pairs","Coalitions",
                                                    "Family groups","Gregarious"))
Ordinal.trait.analysis$Habitat <- ordered(Ordinal.trait.analysis$Habitat, 
                                          levels = c("terrestrial","scansorial"))
Ordinal.trait.analysis$Activity <- ordered(Ordinal.trait.analysis$Activity, 
                                           levels = c("Diurnal","Non-restricted",
                                                      "Crepuscular","Nocturnal/Crepuscular","Nocturnal"))

##linear and ordinal models
library(ordinal)
x <- lm(Body.Mass ~ Study.Year, data = Ordinal.trait.analysis, weights = Occupancy)
x <- clm(Diet~Study.Year, data = Ordinal.trait.analysis, weights = Occupancy)
x <- clm(Social~Study.Year, data = Ordinal.trait.analysis, weights = Occupancy)
x <- clm(Habitat~Study.Year, data = Ordinal.trait.analysis, weights = Occupancy)
x <- clm(Activity~Study.Year, data = Ordinal.trait.analysis, weights = Occupancy)
x <- lm(Litter ~ Study.Year, data = Ordinal.trait.analysis, weights = Occupancy)

##Test for environmental and anthropogenic predictors of traits
fitFD <- lm(Litter~ FL_VB + Tot.Prec + FM_ZOI_90 + FL_ZOI, 
            data=Ordinal.trait.analysis, na.action = "na.fail", weights = Occupancy)
fitFD <- lm(Body.Mass~ FL_VB + Tot.Prec + FM_ZOI_90 + FL_ZOI, 
            data=Ordinal.trait.analysis, na.action = "na.fail")
fitFD <- clm(Diet~ FL_VB + Tot.Prec + FM_ZOI_90 + FL_ZOI, 
            data=Ordinal.trait.analysis, na.action = "na.fail", weights = Occupancy)

allFD.dredge <- dredge(fitFD, beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE, extra=list("confint", "adjR^2"))
allFD.dredge

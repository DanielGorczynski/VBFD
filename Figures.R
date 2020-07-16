##Plot Functional Diversity Over Time [Figure 1] 
FDis.plot <- ggplot()+
  #geom_ribbon(data = Volcan.Barva.Analysis, aes(x = Year, ymin = Func.Null.Low, ymax = Func.Null.High), fill = "grey90") +
  geom_point(data = VBAnalysis_Ordered , aes(x = Year, y = Fdis, col = "Functional Diversity"))+
  geom_line(data = VBAnalysis_Ordered, aes(x = Year, y = Fdis, col = "Functional Diversity")) +
  #geom_errorbar(data = Annual.Functional.Diversity, aes(x = Year, ymin = Abund.Null.Low, ymax = Abund.Null.High), width = .2) +
  #geom_point(data = Dpw.Values.10000, aes(x = Year, y = Null.Dpw, col = "Null Model Turnover", group = Park)) +
  #geom_line(data = Dpw.Values.10000, aes(x = Year, y = Null.Dpw, col = "Null Model Turnover", group = Park)) +
  scale_colour_manual("", 
                      breaks = c("Functional Diversity"),
                      values = c("#252525"))+
  theme_classic()+
  theme(axis.text = element_text(size = rel(.9)))+
  labs( title = "Annual mammalian functional dispersion", y = "FDis", x = "Year")+
  ylim(.17,.23 )+
  theme(legend.position = "none")

library(ggplot2)
##Plot Environmental Variables over time 
Prec.plot <- ggplot()+
  geom_point(data = VBAnalysis_Ordered, aes(x = Year, y = Tot.Prec, col = "Prec"))+
  geom_line(data = VBAnalysis_Ordered, aes(x = Year, y = Tot.Prec, col = "Prec")) +
  scale_colour_manual("", 
                      breaks = c("Prec"),
                      values = c("#bdbdbd"))+
  theme_classic()+
  theme(axis.text = element_text(size = rel(.9)))+
  labs( title = "Annual precipitation", y = "Prec. (mm)")+
  ylim(1100, 2100)+
  theme(legend.position = "none", axis.title.x =element_blank())

Can.gap.plot <- ggplot()+
  geom_point(data = VBAnalysis_Ordered, aes(x = Year, y = FL_VB, col = "FL"))+
  geom_line(data = VBAnalysis_Ordered, aes(x = Year, y = FL_VB, col = "FL")) +
  scale_colour_manual("", 
                      breaks = c("FL"),
                      values = c("#969696"))+
  theme_classic()+
  theme(axis.text = element_text(size = rel(.9)))+
  labs( title = "Annual canopy gap emergence", y = "Area (hectares)")+
  ylim(0, 4)+
  theme(legend.position = "none", axis.title.x =element_blank())

Frag.plot <- ggplot()+
  geom_point(data = VBAnalysis_Ordered, aes(x = Year, y = FM_ZOI_90, col = "FM"))+
  geom_line(data = VBAnalysis_Ordered, aes(x = Year, y = FM_ZOI_90, col = "FM")) +
  scale_colour_manual("", 
                      breaks = c("FM"),
                      values = c("#737373"))+
  theme_classic()+
  theme(axis.text = element_text(size = rel(.9)))+
  labs( title = "Mean fragment size in ZOI", y = "Area (hectares)")+
  ylim(99, 106)+
  theme(legend.position = "none", axis.title.x =element_blank())

Defor.plot <- ggplot()+
  geom_point(data = VBAnalysis_Ordered, aes(x = Year, y = FL_ZOI, col = "ZOI"))+
  geom_line(data = VBAnalysis_Ordered, aes(x = Year, y = FL_ZOI, col = "ZOI")) +
  scale_colour_manual("", 
                      breaks = c("ZOI"),
                      values = c("#525252"))+
  theme_classic()+
  theme(axis.text = element_text(size = rel(.9)))+
  labs( title = "Annual forest loss in ZOI", y = "Area (hectares)")+
  ylim(600, 2800)+
  theme(legend.position = "none", axis.title.x =element_blank())

##Combine trends over time into single figure
library(ggpubr)
ggarrange(Prec.plot,Can.gap.plot,Frag.plot,Defor.plot, FDis.plot, ncol = 1, align = "hv", label.x = "Year")

##Export as a 5x10 PDF

#####################################################################
##Figures showing relative trait abundances over time [Figure 2]
##Make ordered categories
Qualitative.trait.analysis$Body.Mass <- ordered(Qualitative.trait.analysis$Body.Mass, 
                                                levels = c("small","moderate",
                                                           "large","very large"))
Qualitative.trait.analysis$Diet <- ordered(Qualitative.trait.analysis$Diet, 
                                           levels = c("Browser","Browser/Frugivore",
                                                      "Frugivore","Omnivore","Insectivore","Zoophage","Carnivore"))
Qualitative.trait.analysis$Social <- ordered(Qualitative.trait.analysis$Social, 
                                             levels = c("Solitary/pairs","Coalitions",
                                                        "Family groups","Gregarious"))
Qualitative.trait.analysis$Habitat <- ordered(Qualitative.trait.analysis$Habitat, 
                                              levels = c("terrestrial","scansorial"))
Qualitative.trait.analysis$Activity <- ordered(Qualitative.trait.analysis$Activity, 
                                               levels = c("Diurnal","Non-restricted",
                                                          "Crepuscular","Nocturnal/Crepuscular","Nocturnal"))
Qualitative.trait.analysis$Litter <- ordered(Qualitative.trait.analysis$Litter, 
                                             levels = c("small","moderate",
                                                        "large","very large"))




##Proportional for categories
##Body Mass
BM.prop <- ggplot(data=Qualitative.trait.analysis, aes(x=Year, y=Occupancy, fill=Body.Mass)) +
  geom_bar(stat="identity", position = "fill")+
  labs(y="Proportional Occupancy", title= "Body Mass")+
  theme(legend.title=element_blank())
##Diet
Diet.prop <- ggplot(data=Qualitative.trait.analysis, aes(x=Year, y=Occupancy, fill=Diet)) +
  geom_bar(stat="identity", position = "fill")+
  labs(y="Proportional Occupancy", title= "Diet")+
  theme(legend.title=element_blank())
##Social
Soc.prop <- ggplot(data=Qualitative.trait.analysis, aes(x=Year, y=Occupancy, fill=Social)) +
  geom_bar(stat="identity", position = "fill")+
  labs(y="Proportional Occupancy", title= "Social Group")+
  theme(legend.title=element_blank())
##Habitat
Hab.prop <- ggplot(data=Qualitative.trait.analysis, aes(x=Year, y=Occupancy, fill=Habitat)) +
  geom_bar(stat="identity", position = "fill")+
  labs(y="Proportional Occupancy", title= "Habitat")+
  theme(legend.title=element_blank())
##Activity
Act.prop <- ggplot(data=Qualitative.trait.analysis, aes(x=Year, y=Occupancy, fill=Activity)) +
  geom_bar(stat="identity", position = "fill")+
  labs(y="Proportional Occupancy", title= "Activity Period")+
  theme(legend.title=element_blank())
##Litter
Lit.prop <- ggplot(data=Qualitative.trait.analysis, aes(x=Year, y=Occupancy, fill=Litter)) +
  geom_bar(stat="identity", position = "fill")+
  labs(y="Proportional Occupancy", title= "Litter Size")+
  theme(legend.title=element_blank())
##Combine into single figure
library(ggpubr)
ggarrange(BM.prop,Diet.prop,Soc.prop,Hab.prop, Act.prop, Lit.prop, ncol = 2, nrow = 3, align = "hv")

########################################################################
##Plot Functional Redundancy, [Figure 3]
ggplot()+
  #geom_ribbon(data = Func.Redund, aes(x = Species, ymin = Null.Min, ymax = Null.Max), fill = "grey70") +
  geom_point(data = Annual.FRed, aes(x = Species.Loss, y = FDis, group = Year, col = as.character(Year)))+
  geom_line(data = Annual.FRed, aes(x = Species.Loss, y = FDis, group = Year, col = as.character(Year))) +
  #geom_point(data = Func.Redund, aes(x = Species, y = Null.Mean)) +
  #geom_line(data = Func.Redund, aes(x = Species, y = Null.Mean)) +
  scale_colour_manual("", 
                      #breaks = c("Measured Functional Diversity", "Null Model Diversity"),
                      values = c("red","orange","yellow","green","blue","darkblue","purple","black"))+
  theme_classic()+
  #scale_y_continuous()+
  theme(axis.text = element_text(size = rel(.5)))+
  theme(axis.title = element_text(size = rel(1.5)))+
  labs( title = "Functional Redundancy in Volcan Barva", y = "Functional Diversity", x = "Species Lost")
  #ylim()





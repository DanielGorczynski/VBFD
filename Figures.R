##Plot Functional Diversity Over Time 
FDis.plot <- ggplot()+
  #geom_ribbon(data = Volcan.Barva.Analysis, aes(x = Year, ymin = Func.Null.Low, ymax = Func.Null.High), fill = "grey90") +
  geom_point(data = Volcan.Barva.Analysis, aes(x = Year, y = Fdis, col = "Functional Diversity"))+
  geom_line(data = Volcan.Barva.Analysis, aes(x = Year, y = Fdis, col = "Functional Diversity")) +
  #geom_errorbar(data = Annual.Functional.Diversity, aes(x = Year, ymin = Abund.Null.Low, ymax = Abund.Null.High), width = .2) +
  #geom_point(data = Dpw.Values.10000, aes(x = Year, y = Null.Dpw, col = "Null Model Turnover", group = Park)) +
  #geom_line(data = Dpw.Values.10000, aes(x = Year, y = Null.Dpw, col = "Null Model Turnover", group = Park)) +
  scale_colour_manual("", 
                      breaks = c("Functional Diversity"),
                      values = c("#31a354"))+
  theme_classic()+
  theme(axis.text = element_text(size = rel(.9)))+
  labs( title = "Mammalian Functional Dispersion over time", y = "FDis")+
  ylim(2.1, 2.3)+
  theme(legend.position = "none", axis.title.x =element_blank())
  
##Plot Average Occpuancy Over Time 
Avg.Occ.plot <- ggplot()+
  geom_point(data = Volcan.Barva.Analysis, aes(x = Year, y = Occupancy.Avg, col = "Mean Occupancy"))+
  geom_line(data = Volcan.Barva.Analysis, aes(x = Year, y = Occupancy.Avg, col = "Mean Occupancy")) +
  scale_colour_manual("", 
                      breaks = c("Mean Occupancy"),
                      values = c("#006d2c"))+
  theme_classic()+
  theme(axis.text = element_text(size = rel(.9)))+
  labs( title = "Average Mammalian Occupancy over time", y = "Mean Occ.")+
  ylim(.11, .2)+
  theme(legend.position = "none",axis.title.x =element_blank())

##Plot Environmental Variables over time 
Prec.plot <- ggplot()+
  geom_point(data = Volcan.Barva.Analysis, aes(x = Year, y = Tot.Prec, col = "Prec"))+
  geom_line(data = Volcan.Barva.Analysis, aes(x = Year, y = Tot.Prec, col = "Prec")) +
  scale_colour_manual("", 
                      breaks = c("Prec"),
                      values = c("#edf8e9"))+
  theme_classic()+
  theme(axis.text = element_text(size = rel(.9)))+
  labs( title = "Total Precipitation over time", y = "Annual Prec.")+
  ylim(1100, 2100)+
  theme(legend.position = "none", axis.title.x =element_blank())

Can.gap.plot <- ggplot()+
  geom_point(data = Volcan.Barva.Analysis, aes(x = Year, y = FL_VB, col = "FL"))+
  geom_line(data = Volcan.Barva.Analysis, aes(x = Year, y = FL_VB, col = "FL")) +
  scale_colour_manual("", 
                      breaks = c("FL"),
                      values = c("#c7e9c0"))+
  theme_classic()+
  theme(axis.text = element_text(size = rel(.9)))+
  labs( title = "Annual Canopy Gap Emergence", y = "Area")+
  ylim(0, 4)+
  theme(legend.position = "none", axis.title.x =element_blank())

Frag.plot <- ggplot()+
  geom_point(data = Volcan.Barva.Analysis, aes(x = Year, y = FM_ZOI_90, col = "FM"))+
  geom_line(data = Volcan.Barva.Analysis, aes(x = Year, y = FM_ZOI_90, col = "FM")) +
  scale_colour_manual("", 
                      breaks = c("FM"),
                      values = c("#a1d99b"))+
  theme_classic()+
  theme(axis.text = element_text(size = rel(.9)))+
  labs( title = "Mean Fragment size over time", y = "Area")+
  ylim(99, 106)+
  theme(legend.position = "none", axis.title.x =element_blank())

Defor.plot <- ggplot()+
  geom_point(data = Volcan.Barva.Analysis, aes(x = Year, y = FL_ZOI, col = "ZOI"))+
  geom_line(data = Volcan.Barva.Analysis, aes(x = Year, y = FL_ZOI, col = "ZOI")) +
  scale_colour_manual("", 
                      breaks = c("ZOI"),
                      values = c("#74c476"))+
  theme_classic()+
  theme(axis.text = element_text(size = rel(.9)))+
  labs( title = "Annual Forest Loss in ZOI", y = "Area")+
  ylim(600, 2800)+
  theme(legend.position = "none", axis.title.x =element_blank())

##Combine trends over time into single figure
ggarrange(Prec.plot,Can.gap.plot,Frag.plot,Defor.plot, FDis.plot,Avg.Occ.plot, ncol = 1, label.x = "Year")
##Export as a 5x10 PDF

##Plot Functional Redundancy, NEEDS EXPLANATION OF VARIABLES SINCE DATA TABLE IS NOT PUT TOGETHER IN FUNCTIONS SCRIPT
ggplot()+
  #geom_ribbon(data = Func.Redund, aes(x = Species, ymin = Null.Min, ymax = Null.Max), fill = "grey70") +
  geom_point(data = Functional.Redundancy, aes(x = Species.Loss, y = Loss, col = factor(Year)))+
  geom_line(data = Functional.Redundancy, aes(x = Species.Loss, y = Loss, col = factor(Year))) +
  #geom_point(data = Func.Redund, aes(x = Species, y = Null.Mean)) +
  #geom_line(data = Func.Redund, aes(x = Species, y = Null.Mean)) +
  scale_colour_manual("", 
                      #breaks = c("Measured Functional Diversity", "Null Model Diversity"),
                      values = c("red","orange","yellow","green","blue","darkblue","purple","black"))+
  theme_classic()+
  theme(axis.text = element_text(size = rel(1.5)))+
  theme(axis.title = element_text(size = rel(1.5)))+
  labs( title = "Functional Redundancy in Volcan Barva", y = "Functional Diversity", x = "Species Lost")
  #ylim()


###OLD ITERATIONS FOR POTENTIAL USE LATER
##Plot Functional Diversity Model Predictions with confidence interval
library(ggplot2)
ggplot()+
  geom_ribbon(data = Annual.Functional.Diversity, aes(x = Year, ymin = right_lwr, ymax = right_upr ), fill = "grey60")+
  geom_point(data = Annual.Functional.Diversity, aes(x = Year, y = fit_link, col = "Functional Diversity"))+
  geom_line(data = Annual.Functional.Diversity, aes(x = Year, y = fit_link, col = "Functional Diversity"))+ 
  
  scale_colour_manual("", 
                      breaks = c("Functional Diversity"),
                      values = c("black"))+
  theme_classic()+
  theme(axis.text = element_text(size = rel(1.5)))+
  labs( title = "Predicted Functional Diversity", y = "Functional Diversity")+
  ylim(0.313, 0.325)

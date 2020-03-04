##Plot Functional Diversity Over Time 
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
  labs( title = "Annual mammalian functional dispersion", y = "FDis")+
  ylim(.17,.23 )+
  theme(legend.position = "none", axis.title.x =element_blank())


##Plot Environmental Variables over time 
Prec.plot <- ggplot()+
  geom_point(data = VBAnalysis_Ordered, aes(x = Year, y = Tot.Prec, col = "Prec"))+
  geom_line(data = VBAnalysis_Ordered, aes(x = Year, y = Tot.Prec, col = "Prec")) +
  scale_colour_manual("", 
                      breaks = c("Prec"),
                      values = c("#bdbdbd"))+
  theme_classic()+
  theme(axis.text = element_text(size = rel(.9)))+
  labs( title = "Annual precipitation", y = "Prec.")+
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
  labs( title = "Annual canopy gap emergence", y = "Area")+
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
  labs( title = "Mean fragment size in ZOI", y = "Area")+
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
  labs( title = "Annual forest loss in ZOI", y = "Area")+
  ylim(600, 2800)+
  theme(legend.position = "none", axis.title.x =element_blank())

##Combine trends over time into single figure
library(ggpubr)
ggarrange(Prec.plot,Can.gap.plot,Frag.plot,Defor.plot, FDis.plot, ncol = 1, align = "hv", label.x = "Year")

##Export as a 5x10 PDF

##Plot relationship between fragment size and average occupancy over time
ggplot()+
  geom_point(data = Volcan.Barva.Analysis, aes(x = FM_ZOI_90, y = Occupancy.Avg))+
  theme_classic()+
  geom_smooth(data = Volcan.Barva.Analysis, 
              aes(x = FM_ZOI_90, y = Occupancy.Avg), 
              method = "lm", 
              color = "black")+
  theme(axis.text = element_text(size = rel(1.25)))+
  labs(x = "Mean fragment size (hectares)", y = "Average species occupancy")+
  annotate("text",
           x = 105,
           y = .21, 
           label = "italic(p) == 0.00368",
           size = rel(5),
           parse = TRUE)+
  annotate("text",
           x = 105,
           y = .20, 
           label = "italic(R^2) == 0.7425",
           size = rel(5),
           parse = TRUE)

##Plot Functional Redundancy, NEEDS EXPLANATION OF VARIABLES SINCE DATA TABLE IS NOT PUT TOGETHER IN FUNCTIONS SCRIPT
library(ggplot2)
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


##Plot temporal trends in functional traits

##Body Mass
BM.plot <- ggplot()+
  geom_point(data = VB.FD.CWM, aes(x = Year, y = Body.Mass))+
  theme_classic()+
  geom_smooth(data = VB.FD.CWM, 
              aes(x = Year, y = Body.Mass), 
              method = "lm", 
              color = "black")+
  theme(axis.text = element_text(size = rel(1.25)), 
        axis.title = element_text(size = rel(1.5)))+
  labs(x = "Year", y = "Log. Average Body Mass")+
  annotate("text",
           x = 2008,
           y = 1.3, 
           label = "italic(p) == 0.0544",
           size = rel(5),
           parse = TRUE)+
  annotate("text",
           x = 2008,
           y = 1.27, 
           label = "italic(R^2) == 0.4012",
           size = rel(5),
           parse = TRUE)

##Diet
Diet.plot <- ggplot()+
  geom_point(data = VB.FD.CWM, aes(x = Year, y = Diet))+
  theme_classic()+
  geom_smooth(data = VB.FD.CWM, 
              aes(x = Year, y = Diet), 
              method = "lm", 
              color = "black")+
  theme(axis.text = element_text(size = rel(1.25)), 
        axis.title = element_text(size = rel(1.5)))+
  labs(x = "Year", y = "Average Diet Rank")+
  annotate("text",
           x = 2013,
           y = 5.3, 
           label = "italic(p) == 0.0978",
           size = rel(5),
           parse = TRUE)+
  annotate("text",
           x = 2013,
           y = 5.25, 
           label = "italic(R^2) == 0.2885",
           size = rel(5),
           parse = TRUE)

##Social Group
Soc.plot <- ggplot()+
  geom_point(data = VB.FD.CWM, aes(x = Year, y = Social.Group.Size))+
  theme_classic()+
  geom_smooth(data = VB.FD.CWM, 
              aes(x = Year, y = Social.Group.Size), 
              method = "lm", 
              color = "black")+
  theme(axis.text = element_text(size = rel(1.25)), 
        axis.title = element_text(size = rel(1.5)))+
  labs(x = "Year", y = "Average Social Group Size")+
  annotate("text",
           x = 2008,
           y = 1.7, 
           label = "italic(p) == 0.00107",
           size = rel(5),
           parse = TRUE)+
  annotate("text",
           x = 2008,
           y = 1.68, 
           label = "italic(R^2) == 0.8276",
           size = rel(5),
           parse = TRUE)

##Habitat
Hab.plot <- ggplot()+
  geom_point(data = VB.FD.CWM, aes(x = Year, y =Habitat))+
  theme_classic()+
  geom_smooth(data = VB.FD.CWM, 
              aes(x = Year, y = Habitat), 
              method = "lm", 
              color = "black")+
  theme(axis.text = element_text(size = rel(1.25)), 
        axis.title = element_text(size = rel(1.5)))+
  labs(x = "Year", y = "Average Habitat Rank")+
  annotate("text",
           x = 2013,
           y = 2.2, 
           label = "italic(p) == 0.1957",
           size = rel(5),
           parse = TRUE)+
  annotate("text",
           x = 2013,
           y = 2.19, 
           label = "italic(R^2) == 0.1378",
           size = rel(5),
           parse = TRUE)

##Activity Period
Act.plot <- ggplot()+
  geom_point(data = VB.FD.CWM, aes(x = Year, y =Activity.Period))+
  theme_classic()+
  geom_smooth(data = VB.FD.CWM, 
              aes(x = Year, y = Activity.Period), 
              method = "lm", 
              color = "black")+
  theme(axis.text = element_text(size = rel(1.25)), 
        axis.title = element_text(size = rel(1.5)))+
  labs(x = "Year", y = "Average Activity Period Rank")+
  annotate("text",
           x = 2013,
           y = 3.35, 
           label = "italic(p) == 0.000296",
           size = rel(5),
           parse = TRUE)+
  annotate("text",
           x = 2013,
           y = 3.3, 
           label = "italic(R^2) == 0.8869",
           size = rel(5),
           parse = TRUE)

##Litter Size
Lit.plot <- ggplot()+
  geom_point(data = VB.FD.CWM, aes(x = Year, y = Avg..Litter.Size))+
  theme_classic()+
  geom_smooth(data = VB.FD.CWM, 
              aes(x = Year, y = Avg..Litter.Size), 
              method = "lm", 
              color = "black")+
  theme(axis.text = element_text(size = rel(1.25)), 
        axis.title = element_text(size = rel(1.5)))+
  labs(x = "Year", y = "Average Litter Size")+
  annotate("text",
           x = 2013,
           y = 1.9, 
           label = "italic(p) == 0.06982",
           size = rel(5),
           parse = TRUE)+
  annotate("text",
           x = 2013,
           y = 1.87, 
           label = "italic(R^2) == 0.355",
           size = rel(5),
           parse = TRUE)

##Combine regressions into a single figure

ggarrange(BM.plot,Diet.plot,Soc.plot,Hab.plot, Act.plot,Lit.plot, ncol = 3, nrow = 2, align = "hv")


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

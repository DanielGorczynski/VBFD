##Plot Functional Diversity Over Time with Functional Diversity Null Model confidence interval
ggplot()+
  geom_ribbon(data = Annual.Functional.Diversity, aes(x = Year, ymin = Func.Null.Low, ymax = Func.Null.High), fill = "grey90") +
  geom_point(data = Annual.Functional.Diversity, aes(x = Year, y = Func.Div, col = "Functional Diversity"))+
  geom_line(data = Annual.Functional.Diversity, aes(x = Year, y = Func.Div, col = "Functional Diversity")) +
  #geom_errorbar(data = Annual.Functional.Diversity, aes(x = Year, ymin = Abund.Null.Low, ymax = Abund.Null.High), width = .2) +
  #geom_point(data = Dpw.Values.10000, aes(x = Year, y = Null.Dpw, col = "Null Model Turnover", group = Park)) +
  #geom_line(data = Dpw.Values.10000, aes(x = Year, y = Null.Dpw, col = "Null Model Turnover", group = Park)) +
  scale_colour_manual("", 
                      breaks = c("Functional Diversity", "Null Model"),
                      values = c("black", "grey70"))+
  theme_classic()+
  theme(axis.text = element_text(size = rel(1.5)))+
  labs( title = "Mammalian Functional Diversity in Volcan Barva over time", y = "Functional Diversity")+
  ylim(0.27, 0.34)



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
  labs( title = "Functional Redundancy in Volcan Barva", y = "Functional Diversity", x = "Species Lost")+
  ylim(0.295,0.325)

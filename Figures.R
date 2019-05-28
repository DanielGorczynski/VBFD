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
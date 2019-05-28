##Linear Models of environmental predictors
library(AICcmodavg)
a <- lm(Func.Div ~ FL_VB, data = Annual.FD.with.Enviro.Var, weights = Variance)
b <- lm(Func.Div ~ FM_10km, data = Annual.FD.with.Enviro.Var, weights = Variance)
c <- lm(Func.Div ~ Tot.Prec, data = Annual.FD.with.Enviro.Var, weights = Variance)
d <- lm(Func.Div ~ Stem.Count, data = Annual.FD.with.Enviro.Var, weights = Variance)
e <- lm(Func.Div ~ FL_VB+FM_10km, data = Annual.FD.with.Enviro.Var, weights = Variance)
f <- lm(Func.Div ~ FL_VB+Tot.Prec, data = Annual.FD.with.Enviro.Var, weights = Variance)
g <- lm(Func.Div ~ FL_VB+Stem.Count, data = Annual.FD.with.Enviro.Var, weights = Variance)
h <- lm(Func.Div ~ FM_10km+Tot.Prec, data = Annual.FD.with.Enviro.Var, weights = Variance)
i <- lm(Func.Div ~ FM_10km+Stem.Count, data = Annual.FD.with.Enviro.Var, weights = Variance)
j <- lm(Func.Div ~ Tot.Prec+Stem.Count, data = Annual.FD.with.Enviro.Var, weights = Variance)
k <- lm(Func.Div ~ FL_VB+FM_10km+Stem.Count, data = Annual.FD.with.Enviro.Var, weights = Variance)
l <- lm(Func.Div ~ FL_VB+FM_10km+Tot.Prec, data = Annual.FD.with.Enviro.Var, weights = Variance)
m <- lm(Func.Div ~ FL_VB+Tot.Prec+Stem.Count, data = Annual.FD.with.Enviro.Var, weights = Variance)
n <- lm(Func.Div ~ FM_10km+Tot.Prec+Stem.Count, data = Annual.FD.with.Enviro.Var, weights = Variance)
o <- lm(Func.Div ~ 1, data = Annual.FD.with.Enviro.Var, weights = Variance)
p <- list(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
names <- c("FL","FM","TP","SC","FL+FM","FL+TP","FL+SC","FM+TP","FM+SC","TP+SC","FL+FM+SC","FL+FM+TP","FL+TP+SC","FM+TP+SC","NULL")
q <- aictab(p, modnames = names)

#Obtain confidence interval for best fit model predictions where a is best fit model, 
#Annual.FD.with.Enviro.Var is dataframe that has Functional Diversity and Predictive Variable Values
## add fit and se.fit on the **link** scale
ilink <- family(a)$linkinv

Annual.FD.with.Enviro.Var <- bind_cols(Annual.FD.with.Enviro.Var, 
                                       setNames(as_tibble(predict(a, Annual.FD.with.Enviro.Var, se.fit = TRUE)[1:2]),
                                                c('fit_link','se_link')))
## create the interval and backtransform
Annual.Functional.Diversity <- mutate(Annual.FD.with.Enviro.Var,
                                      fit_resp  = ilink(fit_link),
                                      right_upr = ilink(fit_link + (2 * se_link)),
                                      right_lwr = ilink(fit_link - (2 * se_link)))
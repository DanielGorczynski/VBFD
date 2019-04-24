#Functional Diversity- Gives average functional diversity of species weigthed by abundance
#Similar to Botta-Dukat's Rao's Quadratic Entropy, but the abundance only factors in at the end
#Is this okay??
library(dplyr)
library(FactoMineR)
library(cluster)

Func.Div <- function(x,y,ft){

Community_Div<-vector("numeric")
Pres.Abs<- filter(x,x[[y]] > 0)
Community<-Pres.Abs$Species
Comm.Funct<-ft[ft[["Species"]] %in% Community,]
rownames(Comm.Funct)<-Comm.Funct$Species
Comm.Funct$Species<-NULL


  
 
  res.pca<-PCA(Comm.Funct, graph = F) #PCA
  Funct.Coord<-res.pca$ind$coord
  Funct.Coord.5<-as.data.frame(Funct.Coord)
  #Funct.Coord.4 <- Funct.Coord.5[c(1:4)]
  Funct.Coord.3<-Funct.Coord.5[c(1:3)] #parse out first 3 dimmensions of PCA only
  #Funct.Coord.2 <- Funct.Coord.5[c(1:2)]
  Func.Dist.Matrix<-daisy(Funct.Coord.3, metric = "gower") #Gower on first 3 PCA dimmensions
  Func.Dendrogram<-hclust(Func.Dist.Matrix, method = "average") #Construct Dendrogram
  Dend.Dist.Mat <- cophenetic(Func.Dendrogram) #Dendrogram difference between all species
  Dend.Dist.Mat <- as.matrix(Dend.Dist.Mat)
  Dend.Dist.df <- as.data.frame(Dend.Dist.Mat)
  Avg.Func.Div <- data.frame(row.names = rownames(Dend.Dist.df), Means=rowMeans(Dend.Dist.df))

  Sigma<-cbind(Avg.Func.Div,Pres.Abs[[y]]) #incorporate funct. div in calculation table
  colnames(Sigma) <- c("Means","Abundance")
  #Sigma$Means <- sample(Sigma$Means, replace = FALSE)
  #Sigma["Influence"]<-(Sigma$Means*Sigma$Abundance) #product of funct. div and change in occurence
  Sigma["Influence"]<-(Sigma$Means*(Sigma$Abundance/sum(Sigma$Abundance)))
  FD<-(sum(Sigma$Influence)) #Sums species influences for Turnover
return(FD)
  }

###Functional Null Model
Func.Div.NULL <- function(x,y,ft){
  
  Community_Div<-vector("numeric")
  Pres.Abs<- filter(x,x[[y]] > 0)
  Community<-Pres.Abs$Species
  Comm.Funct<-ft[ft[["Species"]] %in% Community,]
  rownames(Comm.Funct)<-Comm.Funct$Species
  Comm.Funct$Species<-NULL
  
  
  
  
  res.pca<-PCA(Comm.Funct, graph = F) #PCA
  Funct.Coord<-res.pca$ind$coord
  Funct.Coord.5<-as.data.frame(Funct.Coord)
  #Funct.Coord.4 <- Funct.Coord.5[c(1:4)]
  Funct.Coord.3<-Funct.Coord.5[c(1:3)] #parse out first 3 dimmensions of PCA only
  #Funct.Coord.2 <- Funct.Coord.5[c(1:2)]
  Func.Dist.Matrix<-daisy(Funct.Coord.3, metric = "gower") #Gower on first 3 PCA dimmensions
  Func.Dendrogram<-hclust(Func.Dist.Matrix, method = "average") #Construct Dendrogram
  Dend.Dist.Mat <- cophenetic(Func.Dendrogram) #Dendrogram difference between all species
  Dend.Dist.Mat <- as.matrix(Dend.Dist.Mat)
  Dend.Dist.df <- as.data.frame(Dend.Dist.Mat)
  Avg.Func.Div <- data.frame(row.names = rownames(Dend.Dist.df), Means=rowMeans(Dend.Dist.df))
  
  Sigma<-cbind(Avg.Func.Div,Pres.Abs[[y]]) #incorporate funct. div in calculation table
  colnames(Sigma) <- c("Means","Abundance")
  Sigma$Means <- sample(Sigma$Means, replace = FALSE)
  #Sigma["Influence"]<-(Sigma$Means*Sigma$Abundance) #product of funct. div and change in occurence
  Sigma["Influence"]<-(Sigma$Means*(Sigma$Abundance/sum(Sigma$Abundance)))
  FD<-(sum(Sigma$Influence)) #Sums species influences for Turnover
  return(FD)
}

Null_vector <- replicate(10000, Func.Div.NULL(Occurance,"2010",Volcan.Barva.Traits))
print(Null_vector)
mean(Null_vector)
var(Null_vector)
quantile(Null_vector, probs = seq(0.025,.975,1))
quantile(Null_vector, probs = seq(.975,1))


### Occurence Null model

Func.Div.NULL.2 <- function(x,y,ft){
  
  x[is.na(x)] <- 0
  x <- subset(x, class == "MAMMALIA")
  Species <- x$bin
  Species <- unique(Species) #Obtain list of species in community
  Year <- matrix(c(2007:2014), nrow=8, ncol=1) #Create empty matrix
  
  for (p in Species){ #run loop to fill in matrix one vector (species) at a time
    Psivector <- vector("numeric")
    for (i in 2007:2014){ #run loop to fill in vector one value (year) at a time
      Mams <- subset(x, bin == p)
      Psi1000 <- subset(Mams, year == i)
      Samp <- sample_n(Psi1000, size = 1)
      Samp <- Samp$psi
      Psivector[[i-2006]] <- Samp
    }
    Year <- cbind(Year, Psivector)
  }
  
  rownames(Year) <- Year[,1] #Some rearrangement of matrix to give data frame compatible with Gower Distance
  Year <-  Year [,-1] 
  colnames(Year) <- Species
  Year <- t(Year)
  Occurance <- as.data.frame(Year)
  Occurance <- cbind( "Species" = rownames(Occurance), Occurance) 
  rownames(Occurance) <- NULL
  Occurance <- as.data.frame(Occurance)
  
  Community_Div<-vector("numeric")
  Pres.Abs<- filter(Occurance,Occurance[[y]] > 0)
  Community<-Pres.Abs$Species
  Comm.Funct<-ft[ft[["Species"]] %in% Community,]
  rownames(Comm.Funct)<-Comm.Funct$Species
  Comm.Funct$Species<-NULL
  
  res.pca<-PCA(Comm.Funct, graph = F) #PCA
  Funct.Coord<-res.pca$ind$coord
  Funct.Coord.5<-as.data.frame(Funct.Coord)
  #Funct.Coord.4 <- Funct.Coord.5[c(1:4)]
  Funct.Coord.3<-Funct.Coord.5[c(1:3)] #parse out first 3 dimmensions of PCA only
  #Funct.Coord.2 <- Funct.Coord.5[c(1:2)]
  Func.Dist.Matrix<-daisy(Funct.Coord.3, metric = "gower") #Gower on first 3 PCA dimmensions
  Func.Dendrogram<-hclust(Func.Dist.Matrix, method = "average") #Construct Dendrogram
  Dend.Dist.Mat <- cophenetic(Func.Dendrogram) #Dendrogram difference between all species
  Dend.Dist.Mat <- as.matrix(Dend.Dist.Mat)
  Dend.Dist.df <- as.data.frame(Dend.Dist.Mat)
  Avg.Func.Div <- data.frame(row.names = rownames(Dend.Dist.df), Means=rowMeans(Dend.Dist.df))
  
  Sigma<-cbind(Avg.Func.Div,Pres.Abs[[y]]) #incorporate funct. div in calculation table
  colnames(Sigma) <- c("Means","Abundance")
  #Sigma$Means <- sample(Sigma$Means, replace = FALSE)
  #Sigma["Influence"]<-(Sigma$Means*Sigma$Abundance) #product of funct. div and change in occurence
  Sigma["Influence"]<-(Sigma$Means*(Sigma$Abundance/sum(Sigma$Abundance)))
  FD<-(sum(Sigma$Influence)) #Sums species influences for Turnover
  return(FD)
}

Null_vector <- replicate(1000, Func.Div.NULL.2(VBdata_psi,"2014",Volcan.Barva.Traits))
print(Null_vector)
mean(Null_vector)
var(Null_vector)
quantile(Null_vector, probs = seq(0.025,.975,1))
quantile(Null_vector, probs = seq(.975,1))


##Figure for Functional Diversity Over Time
#Figure for Functional diversity 
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


##Linear Models
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

##Get Coefficient Values and Standard error, a is model of interest
coef(summary(a))

##Plot Coefficient Figure
ggplot()+
  geom_point(data = Coeff.models.1.and.2, aes(x = Variable, y = Coefficient, col = c("Forest Loss1", "Forest Loss2", "Mean Fragment Area"))) +
  geom_errorbar(data = Coeff.models.1.and.2, aes(x = Variable, ymin = Coefficient - Standard.Error, ymax = Coefficient + Standard.Error, col = c ("Forest Loss1", "Forest Loss2", "Mean Fragment Area")), width = .2)+
  theme_classic()+
  scale_colour_manual("", 
                      breaks = c("Forest Loss1", "Forest Loss2", "Mean Fragment Area"),
                      values = c("red", "blue","green"))+
  geom_hline(yintercept = 0)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_vline(xintercept =  1.5, linetype = "dotted" )

#Obtain confidence interval for model predictions where a is best fit model, 
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


##Plot Functional Diversity and Model Predictions
library(ggplot2)
ggplot()+
  geom_ribbon(data = Annual.Functional.Diversity, aes(x = Year, ymin = right_lwr, ymax = right_upr ), fill = "grey60")+
  geom_point(data = Annual.Functional.Diversity, aes(x = Year, y = fit_link, col = "Functional Diversity"))+
  geom_line(data = Annual.Functional.Diversity, aes(x = Year, y = fit_link, col = "Functional Diversity"))+ 
  
  #geom_point(data = Annual.Functional.Diversity, aes(x = Year, y = a$fitted.values, col = "Canopy Gaps"))+
  #geom_line(data = Annual.Functional.Diversity, aes(x = Year, y = a$fitted.values, col = "Canopy Gaps")) +
  #geom_point(data = Annual.Functional.Diversity, aes(x = Year, y = e$fitted.values, col = "Forest Loss and Mean Fragment Area"))+
  #geom_line(data = Annual.Functional.Diversity, aes(x = Year, y = e$fitted.values, col = "Forest Loss and Mean Fragment Area"))+
  scale_colour_manual("", 
                      breaks = c("Functional Diversity", "Canopy gap model", "Forest Loss and Mean Fragment Area"),
                      values = c("black", "grey","red"))+
  theme_classic()+
  theme(axis.text = element_text(size = rel(1.5)))+
  labs( title = "Comparing Measured Functional Diversity with Predicted Functional Diversity", y = "Functional Diversity")+
  ylim(0.313, 0.325)

##Functional Redundancy 
Func.Red <- function(x,y,ft,i){
  
  Community_Div<-vector("numeric")
  Pres.Abs<- filter(x,x[[y]] > 0)
  Pres.Abs.red <- Pres.Abs[sample(nrow(Pres.Abs),i,replace = FALSE),]
  Community<-Pres.Abs$Species
  Comm.Funct<-ft[ft[["Species"]] %in% Community,]
  rownames(Comm.Funct)<-Comm.Funct$Species
  Comm.Funct$Species<-NULL
  Pres.Abs.red <- as.data.frame(Pres.Abs.red[c('Species',y )])
  rownames(Pres.Abs.red)<-Pres.Abs.red$Species
  Pres.Abs.red$Species<-NULL

  res.pca<-PCA(Comm.Funct, graph = F) #PCA
  Funct.Coord<-res.pca$ind$coord
  Funct.Coord.5<-as.data.frame(Funct.Coord)
  #Funct.Coord.4 <- Funct.Coord.5[c(1:4)]
  Funct.Coord.3<-Funct.Coord.5[c(1:3)] #parse out first 3 dimmensions of PCA only
  #Funct.Coord.2 <- Funct.Coord.5[c(1:2)]
  Func.Dist.Matrix<-daisy(Funct.Coord.3, metric = "gower") #Gower on first 3 PCA dimmensions
  Func.Dendrogram<-hclust(Func.Dist.Matrix, method = "average") #Construct Dendrogram
  Dend.Dist.Mat <- cophenetic(Func.Dendrogram) #Dendrogram difference between all species
  Dend.Dist.Mat <- as.matrix(Dend.Dist.Mat)
  Dend.Dist.df <- as.data.frame(Dend.Dist.Mat)
  Avg.Func.Div <- data.frame(row.names = rownames(Dend.Dist.df), Means=rowMeans(Dend.Dist.df))
  #Avg.Func.Div<-Avg.Func.Div[rownames(Avg.Func.Div) %in% rownames(Pres.Abs.red),]
  
  Sigma<-merge(Avg.Func.Div,Pres.Abs.red,by="row.names") #incorporate funct. div in calculation table
  colnames(Sigma) <- c("Species","Means","Abundance")
  #Sigma$Means <- sample(Sigma$Means, replace = FALSE)
  #Sigma["Influence"]<-(Sigma$Means*Sigma$Abundance) #product of funct. div and change in occurence
  Sigma["Influence"]<-(Sigma$Means*(Sigma$Abundance/sum(Sigma$Abundance)))
  FD<-(sum(Sigma$Influence)) #Sums species influences for Turnover
  return(FD)
}

#Get functional redundancy for a year (FD of all sub-community sizes)
mean.FR <- vector("numeric")
for (i in 2:22) {
  Red_vector <- replicate(1000, Func.Red(Occurance,"2014",Volcan.Barva.Traits,i))
  mean.FR[i] <- mean(Red_vector)
}
cat(mean.FR, sep = "\n")


Red_vector <- replicate(1000, Func.Red(Occurance,"2007",Volcan.Barva.Traits,11))
print(Red_vector)
mean(Red_vector)
var(Red_vector)
quantile(Red_vector, probs = seq(0.025,.975,1))
quantile(Red_vector, probs = seq(.975,1))

##Functional Redundancy NULL model
Func.Red.NULL <- function(x,y,ft,i){
  
  Community_Div<-vector("numeric")
  Pres.Abs<- filter(x,x[[y]] > 0)
  Pres.Abs.red <- Pres.Abs[sample(nrow(Pres.Abs),i,replace = FALSE),]
  Community<-Pres.Abs$Species
  Comm.Funct<-ft[ft[["Species"]] %in% Community,]
  rownames(Comm.Funct)<-Comm.Funct$Species
  Comm.Funct$Species<-NULL
  Pres.Abs.red <- as.data.frame(Pres.Abs.red[c('Species',y )])
  rownames(Pres.Abs.red)<-Pres.Abs.red$Species
  Pres.Abs.red$Species<-NULL
  
  res.pca<-PCA(Comm.Funct, graph = F) #PCA
  Funct.Coord<-res.pca$ind$coord
  Funct.Coord.5<-as.data.frame(Funct.Coord)
  #Funct.Coord.4 <- Funct.Coord.5[c(1:4)]
  Funct.Coord.3<-Funct.Coord.5[c(1:3)] #parse out first 3 dimmensions of PCA only
  #Funct.Coord.2 <- Funct.Coord.5[c(1:2)]
  Func.Dist.Matrix<-daisy(Funct.Coord.3, metric = "gower") #Gower on first 3 PCA dimmensions
  Func.Dendrogram<-hclust(Func.Dist.Matrix, method = "average") #Construct Dendrogram
  Dend.Dist.Mat <- cophenetic(Func.Dendrogram) #Dendrogram difference between all species
  Dend.Dist.Mat <- as.matrix(Dend.Dist.Mat)
  Dend.Dist.df <- as.data.frame(Dend.Dist.Mat)
  Avg.Func.Div <- data.frame(row.names = rownames(Dend.Dist.df), Means=rowMeans(Dend.Dist.df))
  #Avg.Func.Div<-Avg.Func.Div[rownames(Avg.Func.Div) %in% rownames(Pres.Abs.red),]
  
  Sigma<-merge(Avg.Func.Div,Pres.Abs.red,by="row.names") #incorporate funct. div in calculation table
  colnames(Sigma) <- c("Species","Means","Abundance")
  #Sigma$Means <- sample(Sigma$Means, replace = FALSE)
  #Sigma["Influence"]<-(Sigma$Means*Sigma$Abundance) #product of funct. div and change in occurence
  Sigma$Means <- sample(Sigma$Means, replace = FALSE)
  Sigma["Influence"]<-(Sigma$Means*(Sigma$Abundance/sum(Sigma$Abundance)))
  FD<-(sum(Sigma$Influence)) #Sums species influences for Turnover
  return(FD)
}

#Get functional redundancy null model for a year (mean, min and max FD of all sub-community sizes)
mean.FR <- vector("numeric")
min.FR <- vector("numeric")
max.FR <- vector("numeric")
for (i in 2:22) {
  Red_vector <- replicate(1000, Func.Red.NULL(Occurance,"2014",Volcan.Barva.Traits,i))
  mean.FR[i] <- mean(Red_vector)
  min.FR[i] <- quantile(Red_vector, probs = seq(0.025,.975,1))
  max.FR[i] <- quantile(Red_vector, probs = seq(.975,1))
}
cat(mean.FR, sep = "\n")
cat(min.FR, sep = "\n")
cat(max.FR, sep = "\n")

Red_vector_null <- replicate(1000, Func.Red.NULL(Occurance,"2011",Volcan.Barva.Traits,2))
print(Red_vector_null)
mean(Red_vector_null)
var(Red_vector_null)
quantile(Red_vector_null, probs = seq(0.025,.975,1))
quantile(Red_vector_null, probs = seq(.975,1))


###Redundancy Break Points
z <- glm(Diversity ~ Species, data = Functional.Redundancy)
y <- segmented(z, seg.Z = ~Species, psi = c(3,6))
plot(y)
summary(y)

#Model Selection for breakpoints!!

v <- segmented(z, seg.Z = ~Species, psi = c())
w <- segmented(z, seg.Z = ~Species, psi = c(6))
x <- segmented(z, seg.Z = ~Species, psi = c(3,6))
y <- segmented(z, seg.Z = ~Species, psi = c(3,6,14))
mods <- list(w,x,y)
mod.names <- c("one","two","three")
aictab(mods, modnames = mod.names)

#Figures

  ggplot()+
  geom_ribbon(data = Functional.Redundancy, aes(x = Species, ymin = Null.Min, ymax = Null.Max), fill = "grey70") +
  geom_point(data = Functional.Redundancy, aes(x = Species, y = Diversity, col = "Measured Functional Diversity"))+
  geom_line(data = Functional.Redundancy, aes(x = Species, y = Diversity, col = "Measured Functional Diversity")) +
  geom_point(data = Functional.Redundancy, aes(x = Species, y = Null.Mean, col = "Null Model Diversity")) +
  geom_line(data = Functional.Redundancy, aes(x = Species, y = Null.Mean, col = "Null Model Diversity")) +
  scale_colour_manual("", 
                      breaks = c("Measured Functional Diversity", "Null Model Diversity"),
                      values = c("orange", "purple"))+
  theme_classic()+
  labs( title = "Functional Redundancy in Volcan Barva", y = "Functional Diversity", x = "Species Richness")+
  ylim(0.27, 0.39)
  
  ggplot()+
    #geom_ribbon(data = Func.Redund, aes(x = Species, ymin = Null.Min, ymax = Null.Max), fill = "grey70") +
    geom_point(data = Functional.Redundancy, aes(x = Species, y = Diversity, col = factor(Year)))+
    geom_line(data = Functional.Redundancy, aes(x = Species, y = Diversity, col = factor(Year))) +
    #geom_point(data = Func.Redund, aes(x = Species, y = Null.Mean)) +
    #geom_line(data = Func.Redund, aes(x = Species, y = Null.Mean)) +
    scale_colour_manual("", 
                        #breaks = c("Measured Functional Diversity", "Null Model Diversity"),
                        values = c("red","orange","yellow","green","blue","darkblue","purple","black"))+
    theme_classic()+
    theme(axis.text = element_text(size = rel(1.5)))+
    theme(axis.title = element_text(size = rel(1.5)))+
    labs( title = "Functional Redundancy in Volcan Barva", y = "Functional Diversity", x = "Species Richness")+
    ylim(0.295,0.325)

##Plot for Functional Diversity versus Functional Redundancy  
  ggplot()+
    #geom_ribbon(data = Functional.Redundancy, aes(x = Species, ymin = Null.Min, ymax = Null.Max), fill = "grey70") +
    geom_point(data = FD.vs.FR, aes(x = FD, y = Redundancy, col = factor(Richness)))+
    #geom_line(data = Functional.Redundancy, aes(x = Species, y = Diversity, col = "Measured Functional Diversity", group = Year)) +
    #geom_point(data = Functional.Redundancy, aes(x = Species, y = Null.Mean, col = "Null Model Diversity")) +
    #geom_line(data = Functional.Redundancy, aes(x = Species, y = Null.Mean, col = "Null Model Diversity")) +
    scale_colour_manual("", 
                        #breaks = c("Measured Functional Diversity", "Null Model Diversity"),
                        values = c("orange", "purple","green","blue"))+
    theme_classic()+
    labs( title = "Functional Diversity vs Functional Redundancy", y = "Functional Redundancy", x = "Functional Diversity")
    #ylim(0.295, 0.325)
  
  #v.low.red <- filter(FD.vs.FR, FD.vs.FR$Richness == 1)
  low.red <- filter(FD.vs.FR, FD.vs.FR$Richness == 1)
  med.red <- filter(FD.vs.FR, FD.vs.FR$Richness == 2)
  high.red <- filter(FD.vs.FR, FD.vs.FR$Richness == 3)
  
  #vl <- lm(Redundancy ~ FD, data = v.low.red)
  l <- lm(Redundancy ~ FD, data = low.red)
  m <- lm(Redundancy ~ FD, data = med.red)
  h <- lm(Redundancy ~ FD, data = high.red)
  
  
###Linear model for Functional redundancy values
  x <- lmer(Div.Dif~as.factor(Species)-1 + (1|Year), data = Functional.Redundancy)
  f <- fixef(x)
  cat(f, sep = "\n")
  plot(3:22, f)
  

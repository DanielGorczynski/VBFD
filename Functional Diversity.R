##FUNCTIONS USED TO CALCULATE FUNCTIONAL DIVERSITY, FUNCTIONAL DIVERSITY NULL MODEL AND FUNCTIONAL REDUNDANCY IN THIS STUDY
##



###FUNCTIONAL DIVERSITY- Function for average alpha functional diversity of a community weighted by species abundances
library(dplyr)
library(FactoMineR)
library(cluster)

#Function inputs- x is median occurences obtained from Occ function, y is the year of interest e.g. "2007"
#ft is a table of functional traits of species in the community where rows are species and columns are ranked
#functional traits
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
  Sigma["Influence"]<-(Sigma$Means*(Sigma$Abundance/sum(Sigma$Abundance)))
  FD<-(sum(Sigma$Influence)) #Sums species influences for Turnover
return(FD)
  }



###FUNCTIONAL DIVERSITY NULL MODEL- decouples species functional distinctiveness and occupancy values and resdistributes
#them in the community
library(dplyr)
library(FactoMineR)
library(cluster)

#Function inputs- same as above
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
  Sigma["Influence"]<-(Sigma$Means*(Sigma$Abundance/sum(Sigma$Abundance)))
  FD<-(sum(Sigma$Influence)) #Sums species influences for Turnover
  return(FD)
}

#Replicates Functional Diversity Null Model 10000 times and obtains 95% confidence interval
Null_vector <- replicate(10000, Func.Div.NULL(Occurance,"2010",Volcan.Barva.Traits))
mean(Null_vector)
quantile(Null_vector, probs = seq(0.025,.975,1))
quantile(Null_vector, probs = seq(.975,1))



##FUNCTIONAL REDUNDANCY- bootstraping process that calculates functional diversity with random species loss from the 
#community and fits segmented linear models to the curve to find breakpoints in functional diversity declines
library(dplyr)
library(FactoMineR)
library(cluster)

#Function to calculate functional diversity of a community of size i in year y. Input variable x and ft are the same as
#above
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

#Calculate fuctional diversity for all community sizes for a single year
#Input variables remain the same
mean.FR <- vector("numeric")
for (i in 2:22) {
  Red_vector <- replicate(1000, Func.Red(x,y,ft,i))
  mean.FR[i] <- mean(Red_vector)
}
cat(mean.FR, sep = "\n")

##Compile Functional Diversity values for all years into data frame (df) with column Diversity as all Functional
#Diversity measurements and Species as the number of species in the community (i)
##Model selection for breakpoints in functional diversity declines with species loss
z <- glm(Diversity ~ Species, data = df)
w <- segmented(z, seg.Z = ~Species, psi = c(6))
x <- segmented(z, seg.Z = ~Species, psi = c(3,6))
y <- segmented(z, seg.Z = ~Species, psi = c(3,6,14))
#Model with lowest AIC value (of w,x,y,z) selected as best fit model



####
#EDITED Functional Redundancy calculation using FD function
## functional diversity of a community of size i in year y. Input variable x and ft are the same as
#above
Func.Red <- function(ab,ft,i){
  Pres.Abs <- ab
  Pres.Abs.red <- Pres.Abs[sample(nrow(Pres.Abs),i,replace = FALSE),]
  Pres.Abs.red <- Pres.Abs.red[order(row.names(Pres.Abs.red)),]
  Community <- rownames(Pres.Abs.red)
  Comm.Funct <- ft[rownames(ft) %in% Community,]
  Pres.Abs.red <- t(Pres.Abs.red)
  x <- dbFD(ft, Pres.Abs.red)
  return(x$FDis["2007"])
}

#Calculate functional diversity for all community sizes for a single year
#Input variables remain the same
mean.FR <- vector("numeric")
for(i in 2:21) {
  Red_vector <- replicate(1000, Func.Red(Occurence, VB_taxa_traits, i))
  mean.FR[i] <- mean(Red_vector)
}
cat(mean.FR, sep = "\n")

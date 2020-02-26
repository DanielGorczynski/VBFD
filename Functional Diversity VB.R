##Function to extract occupancy values for Volcan Barva
Occ <- function(x,y){
  x[is.na(x)] <- 0 
  x <- subset(x, class == "MAMMALIA")
  x <- subset(x, sitecode == y)
  Species <- x$bin
  Species <- unique(Species) #Obtain list of species in community
  Year <- matrix(c(2007:2014), nrow=8, ncol=1) #Create empty matrix
  
  for (p in Species){ #run loop to fill in matrix one vector (species) at a time
    Psivector <- vector("numeric")
    for (i in 2007:2014){ #run loop to fill in vector one value (year) at a time
      Mams <- subset(x, bin == p)
      Psi1000 <- subset(Mams, year == i)
      MedPsi <- median(Psi1000$psi)
      Psivector[[i-2006]] <- MedPsi
    }
    Year <- cbind(Year, Psivector)
  }
  
  rownames(Year) <- Year[,1] #Some rearrangement of matrix to make data frame compatible with later functions
  Year <-  Year [,-1] 
  colnames(Year) <- Species
  Year <- t(Year)
  Occurence <- as.data.frame(Year)
  Occurence <- cbind( "Species" = rownames(Occurence), Occurence) 
  rownames(Occurence) <- NULL
  Occurence <- as.data.frame(Occurence)
  
  
  
  return(Occurence)
  #write.csv(Occurence, file = paste(y, "_occurence", sep = ""))
}

#Import and order functional traits
VB.Ordered.Categories <- read.csv("~/Documents/Projects/VBFD/VB Ordered Categories.csv", row.names=1)

VB.Ordered.Categories$Diet <- ordered(VB.Ordered.Categories$Diet, 
                                      levels = c("Browser","Browser/Frugivore",
                                                 "Frugivore","Omnivore","Insectivore","Zoophage","Carnivore"))
VB.Ordered.Categories$Social.Group <- ordered(VB.Ordered.Categories$Social.Group, 
                                              levels = c("Solitary/pairs","Coalitions",
                                                         "Family groups","Gregarious"))
VB.Ordered.Categories$Habitat <- ordered(VB.Ordered.Categories$Habitat, 
                                         levels = c("terrestrial","scansorial"))
VB.Ordered.Categories$Activity.Period <- ordered(VB.Ordered.Categories$Activity.Period, 
                                                 levels = c("Diurnal","Non-restricted",
                                                            "Crepuscular","Nocturnal/Crepuscular","Nocturnal"))

VB.Ordered.Categories <- VB.Ordered.Categories[ order(row.names(VB.Ordered.Categories)), ]



##Calculate FD metrics for Volcan Barva
library(FD)
PsiAll_w_AllCovariates_2015.05.15 <- read.csv("~/Documents/Projects/VBFD/PsiAll_w_AllCovariates_2015-05-15.csv")
Occurence <- Occ(PsiAll_w_AllCovariates_2015.05.15,"VB")
Occurence <- Occurence[-21,]
rownames(Occurence) <- Occurence$Species
Occurence$Species <- NULL
Occurence <- t(Occurence)
VB.FD <- dbFD(VB.Ordered.Categories,Occurence)



#EDITED Functional Redundancy calculation using FD function and ordinal traits
## functional diversity of a community of size i in year y. Input variable x and ft are the same as
#above
Func.Red <- function(ab,ft,i){
  
  Pres.Abs<- t(ab)
  Pres.Abs.red <- Pres.Abs[sample(nrow(Pres.Abs),i,replace = FALSE),]
  Pres.Abs.red <- Pres.Abs.red[order(row.names(Pres.Abs.red)),] 
  Community<-rownames(Pres.Abs.red)
  Comm.Funct<-ft[rownames(ft) %in% Community,]
  Pres.Abs.red <- t(Pres.Abs.red)
  x <- dbFD(Comm.Funct, Pres.Abs.red, calc.CWM= FALSE, calc.FRic = FALSE, calc.FDiv = FALSE, corr = "cailliez")
  return(x$FDis)
}

#Calculate fuctional diversity for all community sizes for a single year
#Input variables remain the same
mean.FR.cal <- matrix(c("0"), ncol = 8, nrow=21)
for (i in 4:21) {
  Red_vector <- replicate(1000, Func.Red(Occurence,VB.Ordered.Categories,i))
  mean.FR.cal[i,] <- as.numeric(rowMeans(Red_vector))
}
cat(mean.FR, sep = "\n")


colnames(mean.FR.cal) <- c("2007","2008","2009","2010","2011","2012","2013","2014")
mean.FR.cal[c(1:3),] <- NA
mean.FR.cal.1 <- mean.FR.cal
row.names(mean.FR.cal.1) <- c("21","20","19","18","17","16","15","14",
        "13","12","11","10","9","8","7","6","5","4","3","2","1")
mean.FR.cal.1 <- mean.FR.cal.1[-c(1:3),]
Annual.FRed <- melt(mean.FR.cal.1)
colnames(Annual.FRed) <- c("Species.Loss","Year","FDis")

##Average across years
mean.FR.cal.1 <- unlist(mean.FR.cal)
mean.FR.cal.1 <- as.data.frame(mean.FR.cal.1)
mean.FR.cal.1$`2007` <- as.numeric(as.character(mean.FR.cal.1$`2007`))
mean.FR.cal.1$`2008` <- as.numeric(as.character(mean.FR.cal.1$`2008`))
mean.FR.cal.1$`2009` <- as.numeric(as.character(mean.FR.cal.1$`2009`))
mean.FR.cal.1$`2010` <- as.numeric(as.character(mean.FR.cal.1$`2010`))
mean.FR.cal.1$`2011` <- as.numeric(as.character(mean.FR.cal.1$`2011`))
mean.FR.cal.1$`2012` <- as.numeric(as.character(mean.FR.cal.1$`2012`))
mean.FR.cal.1$`2013` <- as.numeric(as.character(mean.FR.cal.1$`2013`))
mean.FR.cal.1$`2014` <- as.numeric(as.character(mean.FR.cal.1$`2014`))
mean.FR.cal.1$Means <- rowMeans(mean.FR.cal.1)
mean.FR.cal.1$Species.Richness <- rownames(mean.FR.cal.1)
mean.FR.cal.1$Species.Richness <- as.numeric(mean.FR.cal.1$Species.Richness)

mean.FR.cal.1 <- mean.FR.cal.1[-c(1:3),]

library(dplyr)

##Compile Functional Diversity values for all years into data frame (df) with column Diversity as all Functional
#Diversity measurements and Species as the number of species in the community (i)
##Model selection for breakpoints in functional diversity declines with species loss
library(segmented)


#Model with lowest AIC value selected as best fit model,
#h represents the minimum segment length
library(strucchange)
mod.sel <- breakpoints(Means~ Species.Richness, h = 0.2, data =mean.FR.cal.1)
AIC(mod.sel)

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

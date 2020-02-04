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

##Calculate FD metrics for Volcan Barva
library(FD)
VB_taxa_traits <- read.csv("~/Documents/Projects/VBFD/VB_taxa_traits", row.names=1)
VB_taxa_traits <- VB_taxa_traits[-21, -c(4,7)]
PsiAll_w_AllCovariates_2015.05.15 <- read.csv("~/Documents/Projects/VBFD/PsiAll_w_AllCovariates_2015-05-15.csv")
Occurence <- Occ(PsiAll_w_AllCovariates_2015.05.15,"VB")
Occurence <- Occurence[-21,]
rownames(Occurence) <- Occurence$Species
Occurence$Species <- NULL
Occurence <- t(Occurence)
VB.FD <- dbFD(VB_taxa_traits,Occurence)

##Compile Functional Diversity values for all years into data frame (df) with column Diversity as all Functional
#Diversity measurements and Species as the number of species in the community (i)
##Model selection for breakpoints in functional diversity declines with species loss
library(segmented)


#Model with lowest AIC value selected as best fit model,
#h represents the minimum segment length
library(strucchange)
mod.sel <- breakpoints(Fdis~ Species.Loss, h = .15, data =Functional.Redundancy.average)
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

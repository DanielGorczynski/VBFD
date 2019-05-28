#Function for extracting occupancy medians from raw TEAM data
#Resulting Occurence data frame can be inputed into later functions
Occ <- function(x){
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
}

Occurence <- Occ(VBdata_psi)



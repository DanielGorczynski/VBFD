
#Function for extracting occurance medians from raw TEAM data
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

rownames(Year) <- Year[,1] #Some rearrangement of matrix to give data frame compatible with Gower Distance
Year <-  Year [,-1] 
colnames(Year) <- Species
Year <- t(Year)
Occurance <- as.data.frame(Year)
Occurance <- cbind( "Species" = rownames(Occurance), Occurance) 
rownames(Occurance) <- NULL
Occurance <- as.data.frame(Occurance)
return(Occurance)
}

Occurance <- Occ(VBdata_psi)






#Test
for (i in 2007:2014){
  Mams <- subset(VBdata_psi, bin == "Procyon lotor")
  Psi1000 <- subset(Mams, year == i)
  MedPsi <- median(Psi1000$psi)
  Psivector[[i-2006]] <- MedPsi
}

#Rearrange Occurance table to get change over time
a <- as.data.frame(Occurance$`2007`)
a <- cbind(a, Occurance$Species)
a <- cbind(a, 2007)
colnames(a) <- c("Occurance","Species", "Year")

b <- as.data.frame(Occurance$`2014`)
b <- cbind(b, Occurance$Species)
b <- cbind(b, 2014)
colnames(b) <- c("Occurance","Species", "Year")

data <- rbind(data,b)

#for removing abundant species
dat <- data
dat <- subset(dat, Species != "Mazama temama")

#For getting just carnivores
carn <- dat
carn <- subset(carn, Species == "Eira barbara" | Species == "Leopardus pardalis" | Species == "Puma concolor" 
                 | Species == "Puma yagouaroundi" | Species == "Leopardus wiedii" | Species == "Panthera onca")

#Obtain ggplot
ggplot(data = carn, aes(x = Year, y = Occurance, group = Species, color = Species))+
  geom_line() 


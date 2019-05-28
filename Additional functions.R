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


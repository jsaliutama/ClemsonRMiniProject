

#----------------------------------ANISO DATA FILE----------------------------------------------
rm(list=ls())
aniso <- read.csv("aniso.csv")

#Using K Means Clustering
library(ggplot2)
library(factoextra)
library(purrr)
km <- kmeans(aniso, 3, nstart = 20)
fviz_cluster(km,data=aniso[,1:2],geom="point",ellipse = FALSE)


#Using Affinity Clustering
library(apcluster)      #Download this from CRAN
#Afinity clustering needs a lxl similarity matrix.
simMatrixAniso <- negDistMat(aniso, r=200)
#Do affinity clustering
ac <- apcluster(simMatrixAniso, details=TRUE)
plot(ac, aniso,
     xlab=names(aniso[1]),
     ylab=names(aniso[2]),
     connect=FALSE)


#Using Mean Shift
#Idea taken from https://rdrr.io/cran/MeanShift/man/msClustering.html
library(meanShiftR)     #Download this from CRAN
plot(aniso, col=rep(c('red','green'),each=n/2),
     cex=2, xlab=names(aniso[1]),ylab=names(aniso[2]),pch=20)
aniso_matrix <- as.matrix(aniso)
ms <- meanShift(aniso_matrix, aniso_matrix,
                #nNeighbors = NROW(aniso_matrix),
                algorithm = "LINEAR",
                bandwidth = rep(1,NCOL(aniso_matrix)),
                alpha=0,
                iterations = 1000)
ms_assignment <- ms$assignment
plot(aniso, col=sapply(ms_assignment,function(x)c('red','green','blue')[x]),
     cex=1.5, xlab=names(aniso[1]),ylab=names(aniso[2]),pch=20)


#Using Spectral Clustering
library(anocva)
#Spectral clustering needs a lxl similarity matrix. 
simMatrixAniso <- linSimMat(aniso, w=1)
#After similarity, do clustering
specAniso <- spectralClustering(simMatrixAniso, k=4)
plot(aniso, col=sapply(specAniso,function(x)c('red','green','blue')[x]),
     cex=1, xlab=names(aniso[1]),ylab=names(aniso[2]),pch=20)


#Using Ward
#Idea taken from https://uc-r.github.io/hc_clustering
library(tidyverse) 
library(cluster)    
library(factoextra) 
#Ward's clustering needs dissimilairty matrix
aniso_matrix <- as.matrix(aniso)
disAniso <- dist(aniso_matrix, method="euclidean")
#After dissimilarity, do clustering
hcAniso <- hclust(disAniso, method="ward.D", members=NULL)
sub_grp <- cutree(hcAniso, k=3)    #Cut tree into 3 clusters
fviz_cluster(list(data = aniso, cluster = sub_grp),geom="point",ellipse = FALSE)


#Using Agglomerative Clustering
#Idea taken from https://www.r-bloggers.com/how-to-perform-hierarchical-clustering-using-r/
library(tidyverse) 
library(cluster)    
library(factoextra) 
#Agglomerative clustering needs dissimilairty matrix
aniso_matrix <- as.matrix(aniso)
disAniso <- dist(aniso_matrix, method="euclidean")
#After dissimilarity, do clustering
hcAniso <- hclust(disAniso, method="average", members=NULL)
sub_grp <- cutree(hcAniso, k=3)    #Cut tree into 3 clusters
fviz_cluster(list(data = aniso, cluster = sub_grp),geom="point",ellipse = FALSE)


#Using DBSCAN
library(dbscan)      #Install this from CRAN
db <- dbscan (aniso, eps=0.5, minPts = 20)
fviz_cluster(list(data = aniso, cluster = db$cluster),geom="point",ellipse = FALSE)

#Using Birch
library(birch)       #Have to be donwloaded from tar.gz
aniso_matrix <- as.matrix(aniso)
brcAniso <- birch(aniso_matrix, radius = 1)
kMeansBrcAniso <- kmeans.birch(brcAniso, centers = 3, nstart = 2) 
fviz_cluster(list(data = aniso, cluster = kMeansBrcAniso$clust$obs),geom="point",ellipse = FALSE)


#Using Gaussian Mixture
#I have trouble using Gaussian Mixture Model. So, I decided to use Gaussian Finite Mixture Model.
#Idea from https://www.youtube.com/watch?v=nktiUUd6X_U
library(mclust)
gmmAniso <- Mclust(aniso)
plot(gmmAniso, what="classification")



#----------------------------------BLOBS DATA FILE----------------------------------------------
rm(list=ls())
blobs <- read.csv("blobs.csv")

#Using K Means Clustering
library(ggplot2)
library(factoextra)
library(purrr)
kmBlobs <- kmeans(blobs, 3, nstart = 1)
fviz_cluster(kmBlobs,data=blobs[,1:2],geom="point",ellipse = FALSE)


#Using Affinity Clustering
library(apcluster)      #Download this from CRAN
#Afinity clustering needs a lxl similarity matrix.
simMatrixBlobs <- negDistMat(blobs, r=5)
#Do affinity clustering
ac <- apcluster(simMatrixBlobs, details=TRUE)
plot(ac, blobs,
     xlab=names(blobs[1]),
     ylab=names(blobs[2]),
     connect=FALSE)


#Using Mean Shift
#Idea taken from https://rdrr.io/cran/MeanShift/man/msClustering.html
library(meanShiftR)     #Download this from CRAN
plot(blobs, col=rep(c('red','green'),each=n/2),
     cex=2, xlab=names(blobs[1]),ylab=names(blobs[2]),pch=20)
blobs_matrix <- as.matrix(blobs)
ms <- meanShift(blobs_matrix, blobs_matrix,
                #nNeighbors = NROW(aniso_matrix),
                algorithm = "LINEAR",
                bandwidth = rep(1,NCOL(blobs_matrix)),
                alpha=0,
                iterations = 1000)
ms_assignment <- ms$assignment
plot(blobs, col=sapply(ms_assignment,function(x)c('red','green','blue')[x]),
     cex=1.5, xlab=names(blobs[1]),ylab=names(blobs[2]),pch=20)


#Using Spectral Clustering
library(anocva)
#Spectral clustering needs a lxl similarity matrix. 
simMatrixBlobs <- linSimMat(blobs, w=1)
#After similarity, do clustering
specBlobs <- spectralClustering(simMatrixBlobs, k=5)
plot(blobs, col=sapply(specBlobs,function(x)c('red','green','blue')[x]),
     cex=1, xlab=names(blobs[1]),ylab=names(blobs[2]),pch=20)


#Using Ward
#Idea taken from https://uc-r.github.io/hc_clustering
library(tidyverse) 
library(cluster)    
library(factoextra) 
#Ward's clustering needs dissimilairty matrix
blobs_matrix <- as.matrix(blobs)
disBlobs <- dist(blobs_matrix, method="euclidean")
#After dissimilarity, do clustering
hcBlobs <- hclust(disBlobs, method="ward.D", members=NULL)
sub_grp <- cutree(hcBlobs, k=3)    #Cut tree into 3 clusters
fviz_cluster(list(data = blobs, cluster = sub_grp),geom="point",ellipse = FALSE)


#Using Agglomerative Clustering
#Idea taken from https://www.r-bloggers.com/how-to-perform-hierarchical-clustering-using-r/
library(tidyverse) 
library(cluster)    
library(factoextra) 
#Agglomerative clustering needs dissimilairty matrix
blobs_matrix <- as.matrix(blobs)
disBlobs <- dist(blobs_matrix, method="euclidean")
#After dissimilarity, do clustering
hcAniso <- hclust(disBlobs, method="average", members=NULL)
sub_grp <- cutree(hcBlobs, k=3)    #Cut tree into 3 clusters
fviz_cluster(list(data = blobs, cluster = sub_grp),geom="point",ellipse = FALSE)


#Using DBSCAN
library(dbscan)      #Install this from CRAN
db <- dbscan (blobs, eps=2, minPts = 10)
fviz_cluster(list(data = blobs, cluster = db$cluster),geom="point",ellipse = FALSE)

#Using Birch
library(birch)       #Have to be donwloaded from tar.gz
blobs_matrix <- as.matrix(blobs)
brcBlobs <- birch(blobs_matrix, radius = 1)
kMeansBrcBlobs <- kmeans.birch(brcBlobs, centers = 3, nstart = 2) 
fviz_cluster(list(data = blobs, cluster = kMeansBrcBlobs$clust$obs),geom="point",ellipse = FALSE)


#Using Gaussian Mixture
#I have trouble using Gaussian Mixture Model. So, I decided to use Gaussian Finite Mixture Model.
#Idea from https://www.youtube.com/watch?v=nktiUUd6X_U
library(mclust)
gmmBlobs <- Mclust(blobs)
plot(gmmBlobs, what="classification")



#----------------------------------NO STRUCTURE DATA FILE----------------------------------------------
rm(list=ls())
noStructure <- read.csv("no_structure.csv")

#Using K Means Clustering
library(ggplot2)
library(factoextra)
library(purrr)
kmNs <- kmeans(noStructure, 3, nstart = 50)
fviz_cluster(kmNs,data=noStructure[,1:2],geom="point",ellipse = FALSE)


#Using Affinity Clustering
library(apcluster)      #Download this from CRAN
#Afinity clustering needs a lxl similarity matrix.
simMatrixNs <- negDistMat(noStructure, r=20)
#Do affinity clustering
acNs <- apcluster(simMatrixNs, details=TRUE)
plot(acNs, noStructure,
     xlab=names(noStructure[1]),
     ylab=names(noStructure[2]),
     connect=FALSE)


#Using Mean Shift
#Idea taken from https://rdrr.io/cran/MeanShift/man/msClustering.html
library(meanShiftR)     #Download this from CRAN
#plot(noStructure, col=rep(c('red','green'),each=n/2),
#     cex=2, xlab=names(noStructure[1]),ylab=names(noStructure[2]),pch=20)
ns_matrix <- as.matrix(noStructure)
ms <- meanShift(ns_matrix, ns_matrix,
                #nNeighbors = NROW(aniso_matrix),
                algorithm = "LINEAR",
                bandwidth = rep(1,NCOL(ns_matrix)),
                alpha=0,
                iterations = 1000)
ms_assignment <- ms$assignment
plot(noStructure, 
     col=sapply(ms_assignment,function(x)c('red','green','blue')[x]),
     cex=1.5, 
     xlab=names(noStructure[1]),
     ylab=names(noStructure[2]),
     pch=20)


#Using Spectral Clustering
library(anocva)
#Spectral clustering needs a lxl similarity matrix. 
simMatrixNs <- linSimMat(noStructure, w=1)
#After similarity, do clustering
specNs <- spectralClustering(simMatrixNs, k=3)
plot(noStructure, 
     col=sapply(specNs,function(x)c('red','green','blue')[x]),
     cex=1, 
     xlab=names(noStructure[1]),
     ylab=names(noStructure[2]),
     pch=20)


#Using Ward
#Idea taken from https://uc-r.github.io/hc_clustering
library(tidyverse) 
library(cluster)    
library(factoextra) 
#Ward's clustering needs dissimilairty matrix
ns_matrix <- as.matrix(noStructure)
disNs <- dist(ns_matrix, method="euclidean")
#After dissimilarity, do clustering
hcNs <- hclust(disNs, method="ward.D", members=NULL)
sub_grp <- cutree(hcNs, k=3)    #Cut tree into 3 clusters
fviz_cluster(list(data = noStructure, cluster = sub_grp),geom="point",ellipse = FALSE)


#Using Agglomerative Clustering
#Idea taken from https://www.r-bloggers.com/how-to-perform-hierarchical-clustering-using-r/
library(tidyverse) 
library(cluster)    
library(factoextra) 
#Agglomerative clustering needs dissimilairty matrix
ns_matrix <- as.matrix(noStructure)
disNs <- dist(ns_matrix, method="euclidean")
#After dissimilarity, do clustering
hcNs <- hclust(disNs, method="complete", members=NULL)
sub_grp <- cutree(hcNs, k=3)    #Cut tree into 2 clusters
fviz_cluster(list(data = noStructure, cluster = sub_grp),geom="point",ellipse = FALSE)


#Using DBSCAN
library(dbscan)      #Install this from CRAN
db <- dbscan (noStructure, eps=2, minPts = 10)
fviz_cluster(list(data = noStructure, cluster = db$cluster),geom="point",ellipse = FALSE)

#Using Birch
library(birch)       #Have to be donwloaded from tar.gz
ns_matrix <- as.matrix(noStructure)
brcNs <- birch(ns_matrix, radius = 0.01)
kMeansBrcNs <- kmeans.birch(brcNs, centers = 3, nstart = 3)
fviz_cluster(list(data = noStructure, cluster = kMeansBrcNs$clust$obs),geom="point",ellipse = FALSE)


#Using Gaussian Mixture
#Note: this Gaussian Mixture does not work. It did not give the intended result. Weird...
#I have trouble using Gaussian Mixture Model. So, I decided to use Gaussian Finite Mixture Model.
#Idea from https://www.youtube.com/watch?v=nktiUUd6X_U
library(mclust)
gmmNs <- Mclust(noStructure, G=1:3)
plot(gmmNs, what="classification")



#----------------------------------NOISY CIRCLES DATA FILE----------------------------------------------
rm(list=ls())
noisyCircles <- read.csv("noisy_circles.csv")

#Using K Means Clustering
library(ggplot2)
library(factoextra)
library(purrr)
kmNs <- kmeans(noisyCircles, 2, nstart = 50)
fviz_cluster(kmNs,data=noisyCircles[,1:2],geom="point",ellipse = FALSE)


#Using Affinity Clustering
library(apcluster)      #Download this from CRAN
#Afinity clustering needs a lxl similarity matrix.
simMatrixNoisyCircle <- negDistMat(noisyCircles, r=20)
#Do affinity clustering
acNc <- apcluster(simMatrixNoisyCircle, details=TRUE)
plot(acNc, noisyCircles,
     xlab=names(noisyCircles[1]),
     ylab=names(noisyCircles[2]),
     connect=FALSE)


#Using Mean Shift
#Note: this one does not work. It did not give the intended result.
#Idea taken from https://rdrr.io/cran/MeanShift/man/msClustering.html
library(meanShiftR)     #Download this from CRAN
#plot(noStructure, col=rep(c('red','green'),each=n/2),
#     cex=2, xlab=names(noStructure[1]),ylab=names(noStructure[2]),pch=20)
nc_matrix <- as.matrix(noisyCircles)
ms <- meanShift(nc_matrix, nc_matrix,
                algorithm = "LINEAR",
                bandwidth = rep(1,NCOL(nc_matrix)),
                alpha=0,
                iterations = 1000)
ms_assignment <- ms$assignment
plot(noisyCircles, 
     col=sapply(ms_assignment,function(x)c('red','green','blue')[x]),
     cex=1.5, 
     xlab=names(noisyCircles[1]),
     ylab=names(noisyCircles[2]),
     pch=20)


#Using Spectral Clustering
#Alright, this one also does not do what it supposed to do. 
library(anocva)
#Spectral clustering needs a lxl similarity matrix. 
simMatrixNc <- negDistMat(noisyCircles, r=1)
#After similarity, do clustering
specNc <- spectralClustering(simMatrixNc, k=2)
plot(noisyCircles, 
     col=sapply(specNc,function(x)c('red','green','blue')[x]),
     cex=1, 
     xlab=names(noisyCircles[1]),
     ylab=names(noisyCircles[2]),
     pch=20)


#Using Ward
#Idea taken from https://uc-r.github.io/hc_clustering
library(tidyverse) 
library(cluster)    
library(factoextra) 
#Ward's clustering needs dissimilairty matrix
nc_matrix <- as.matrix(noisyCircles)
disNc <- dist(nc_matrix, method="euclidean")
#After dissimilarity, do clustering
hcNc <- hclust(disNc, method="ward.D", members=NULL)
sub_grp <- cutree(hcNc, k=2)    #Cut tree into 3 clusters
fviz_cluster(list(data = noisyCircles, cluster = sub_grp),geom="point",ellipse = FALSE)


#Using Agglomerative Clustering
#Idea taken from https://www.r-bloggers.com/how-to-perform-hierarchical-clustering-using-r/
library(tidyverse) 
library(cluster)    
library(factoextra) 
#Agglomerative clustering needs dissimilairty matrix
nc_matrix <- as.matrix(noisyCircles)
disNc <- dist(nc_matrix, method="euclidean")
#After dissimilarity, do clustering
hcNc <- hclust(disNc, method="single", members=NULL)
sub_grp <- cutree(hcNc, k=2)    #Cut tree into 3 clusters
fviz_cluster(list(data = noisyCircles, cluster = sub_grp),geom="point",ellipse = FALSE)


#Using DBSCAN
#Note: this one does not work. Did not give the intended result. 
library(dbscan)      #Install this from CRAN
db <- dbscan (noisyCircles, eps=5, minPts = 1, borderPoints = FALSE)
fviz_cluster(list(data = noisyCircles, cluster = db$cluster),geom="point",ellipse = FALSE)

#Using Birch
library(birch)       #Have to be donwloaded from tar.gz
nc_matrix <- as.matrix(noisyCircles)
brcNc <- birch(nc_matrix, radius = 0.1)
kMeansBrcNc <- kmeans.birch(brcNc, centers = 2, nstart = 1)
fviz_cluster(list(data = noisyCircles, cluster = kMeansBrcNc$clust$obs),geom="point",ellipse = FALSE)


#Using Gaussian Mixture
#I have trouble using Gaussian Mixture Model. So, I decided to use Gaussian Finite Mixture Model.
#Idea from https://www.youtube.com/watch?v=nktiUUd6X_U
library(mclust)
gmmNs <- Mclust(noisyCircles, G=1:2)
plot(gmmNs, what="classification")




#----------------------------------NOISY MOONS DATA FILE----------------------------------------------
rm(list=ls())
noisyMoons <- read.csv("noisy_moons.csv")

#Using K Means Clustering
library(ggplot2)
library(factoextra)
library(purrr)
kmNs <- kmeans(noisyMoons, 2, nstart = 50)
fviz_cluster(kmNs,data=noisyMoons[,1:2],geom="point",ellipse = FALSE)


#Using Affinity Clustering
library(apcluster)      #Download this from CRAN
#Afinity clustering needs a lxl similarity matrix.
simMatrixNm <- negDistMat(noisyMoons, r=25)
#Do affinity clustering
acNm <- apcluster(simMatrixNm, details=TRUE)
plot(acNm, noisyMoons,
     xlab=names(noisyMoons[1]),
     ylab=names(noisyMoons[2]),
     connect=FALSE)


#Using Mean Shift
#Note: this one does not work. It did not give the intended result.
#Idea taken from https://rdrr.io/cran/MeanShift/man/msClustering.html
library(meanShiftR)     #Download this from CRAN
#plot(noStructure, col=rep(c('red','green'),each=n/2),
#     cex=2, xlab=names(noStructure[1]),ylab=names(noStructure[2]),pch=20)
nm_matrix <- as.matrix(noisyMoons)
ms <- meanShift(nm_matrix, nm_matrix,
                algorithm = "LINEAR",
                nNeighbors = 10,
                bandwidth = rep(1,NCOL(nm_matrix)),
                alpha=0,
                iterations = 1000)
ms_assignment <- ms$assignment
plot(noisyMoons, 
     col=sapply(ms_assignment,function(x)c('red','green','blue')[x]),
     cex=1.5, 
     xlab=names(noisyMoons[1]),
     ylab=names(noisyMoons[2]),
     pch=20)


#Using Spectral Clustering
#Alright, this one also does not do what it supposed to do. 
library(anocva)
#Spectral clustering needs a lxl similarity matrix. 
simMatrixNm <- negDistMat(noisyMoons, r=1)
#After similarity, do clustering
specNc <- spectralClustering(simMatrixNm, k=2)
plot(noisyMoons, 
     col=sapply(specNc,function(x)c('red','green','blue')[x]),
     cex=1, 
     xlab=names(noisyMoons[1]),
     ylab=names(noisyMoons[2]),
     pch=20)


#Using Ward
#Idea taken from https://uc-r.github.io/hc_clustering
library(tidyverse) 
library(cluster)    
library(factoextra) 
#Ward's clustering needs dissimilairty matrix
nm_matrix <- as.matrix(noisyMoons)
disNm <- dist(nm_matrix, method="euclidean")
#After dissimilarity, do clustering
hcNm <- hclust(disNm, method="ward.D", members=NULL)
sub_grp <- cutree(hcNm, k=2)    #Cut tree into 2 clusters
fviz_cluster(list(data = noisyMoons, cluster = sub_grp),geom="point",ellipse = FALSE)


#Using Agglomerative Clustering
#Idea taken from https://www.r-bloggers.com/how-to-perform-hierarchical-clustering-using-r/
library(tidyverse) 
library(cluster)    
library(factoextra) 
#Agglomerative clustering needs dissimilairty matrix
nm_matrix <- as.matrix(noisyMoons)
disNm <- dist(nm_matrix, method="euclidean")
#After dissimilarity, do clustering
hcNm <- hclust(disNm, method="single", members=NULL)
sub_grp <- cutree(hcNm, k=2)    #Cut tree into 3 clusters
fviz_cluster(list(data = noisyMoons, cluster = sub_grp),geom="point",ellipse = FALSE)


#Using DBSCAN
#Note: this one does not work. Did not give the intended result. 
library(dbscan)      #Install this from CRAN
db <- dbscan (noisyMoons, eps=5, minPts = 1, borderPoints = FALSE)
fviz_cluster(list(data = noisyMoons, cluster = db$cluster),geom="point",ellipse = FALSE)

#Using Birch
library(birch)       #Have to be donwloaded from tar.gz
nm_matrix <- as.matrix(noisyMoons)
brcNm <- birch(nm_matrix, radius = 0.1)
kMeansBrcNm <- kmeans.birch(brcNm, centers = 2, nstart = 1)
fviz_cluster(list(data = noisyMoons, cluster = kMeansBrcNm$clust$obs),geom="point",ellipse = FALSE)


#Using Gaussian Mixture
#I have trouble using Gaussian Mixture Model. So, I decided to use Gaussian Finite Mixture Model.
#Idea from https://www.youtube.com/watch?v=nktiUUd6X_U
library(mclust)
gmmNm <- Mclust(noisyMoons, G=1:2)
plot(gmmNm, what="classification")





#----------------------------------VARIED DATA FILE----------------------------------------------
rm(list=ls())
varied <- read.csv("varied.csv")

#Using K Means Clustering
library(ggplot2)
library(factoextra)
library(purrr)
kmVar <- kmeans(varied, 3, nstart = 50)
fviz_cluster(kmVar,data=varied[,1:2],geom="point",ellipse = FALSE)


#Using Affinity Clustering
#Note: This does not work. It didn't give the intended result. 
library(apcluster)      #Download this from CRAN
#Afinity clustering needs a lxl similarity matrix.
simMatrixVar <- negDistMat(varied, r=10)
#Do affinity clustering
acVar <- apcluster(simMatrixVar, details=TRUE)
plot(acVar, varied,
     xlab=names(varied[1]),
     ylab=names(varied[2]),
     connect=FALSE)


#Using Mean Shift
#Note: this one does not work. It did not give the intended result.
#Idea taken from https://rdrr.io/cran/MeanShift/man/msClustering.html
library(meanShiftR)     #Download this from CRAN
#plot(noStructure, col=rep(c('red','green'),each=n/2),
#     cex=2, xlab=names(noStructure[1]),ylab=names(noStructure[2]),pch=20)
var_matrix <- as.matrix(varied)
ms <- meanShift(var_matrix, var_matrix,
                algorithm = "LINEAR",
                bandwidth = rep(1,NCOL(var_matrix)),
                alpha=0,
                iterations = 1000)
ms_assignment <- ms$assignment
plot(varied, 
     col=sapply(ms_assignment,function(x)c('red','green','blue')[x]),
     cex=1.5, 
     xlab=names(varied[1]),
     ylab=names(varied[2]),
     pch=20)


#Using Spectral Clustering
#Alright, this one also does not do what it supposed to do. 
library(anocva)
#Spectral clustering needs a lxl similarity matrix. 
simMatrixVar <- negDistMat(varied, r=1)
#After similarity, do clustering
specNc <- spectralClustering(simMatrixVar, k=3)
plot(varied, 
     col=sapply(specNc,function(x)c('red','green','blue')[x]),
     cex=1, 
     xlab=names(varied[1]),
     ylab=names(varied[2]),
     pch=20)


#Using Ward
#Idea taken from https://uc-r.github.io/hc_clustering
library(tidyverse) 
library(cluster)    
library(factoextra) 
#Ward's clustering needs dissimilairty matrix
var_matrix <- as.matrix(varied)
disVar <- dist(var_matrix, method="euclidean")
#After dissimilarity, do clustering
hcVar <- hclust(disVar, method="ward.D", members=NULL)
sub_grp <- cutree(hcVar, k=3)    #Cut tree into 2 clusters
fviz_cluster(list(data = varied, cluster = sub_grp),geom="point",ellipse = FALSE)


#Using Agglomerative Clustering
#Idea taken from https://www.r-bloggers.com/how-to-perform-hierarchical-clustering-using-r/
library(tidyverse) 
library(cluster)    
library(factoextra) 
#Agglomerative clustering needs dissimilairty matrix
var_matrix <- as.matrix(varied)
disVar <- dist(var_matrix, method="euclidean")
#After dissimilarity, do clustering
hcVar <- hclust(disVar, method="average", members=NULL)
sub_grp <- cutree(hcVar, k=3)    #Cut tree into 3 clusters
fviz_cluster(list(data = varied, cluster = sub_grp),geom="point",ellipse = FALSE)


#Using DBSCAN
#Note: this one does not work. Did not give the intended result. 
library(dbscan)      #Install this from CRAN
db <- dbscan (varied, eps=5, minPts = 5, borderPoints = FALSE)
fviz_cluster(list(data = varied, cluster = db$cluster),geom="point",ellipse = FALSE)

#Using Birch
library(birch)       #Have to be donwloaded from tar.gz
var_matrix <- as.matrix(varied)
brcVar <- birch(var_matrix, radius = 0.1)
kMeansBrcNm <- kmeans.birch(brcVar, centers = 3, nstart = 1)
fviz_cluster(list(data = varied, cluster = kMeansBrcNm$clust$obs),geom="point",ellipse = FALSE)


#Using Gaussian Mixture
#I have trouble using Gaussian Mixture Model. So, I decided to use Gaussian Finite Mixture Model.
#Idea from https://www.youtube.com/watch?v=nktiUUd6X_U
library(mclust)
gmmVar <- Mclust(varied, G=1:3)
plot(gmmVar, what="classification")

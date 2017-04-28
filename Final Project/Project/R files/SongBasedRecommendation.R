library("lsa")
data <- read.csv("lastfm-matrix-germany.csv")
data=as.matrix(data,header=FALSE)
data1 <- data[,2:ncol(data)]

#calculate similarity matrix using cosine similarity
song_sim = cosine(data1)
similarity <- as.data.frame(song_sim)

neighbours <- matrix(NA, nrow=ncol(similarity),ncol=11,dimnames=list(colnames(similarity)))

#calculate the song top 10 neighbours based on similarity and recommend them.
for(i in 1:ncol(data1)) 
{
  neighbours[i,] <- (t(head(n=11,rownames(similarity[order(similarity[,i],decreasing=TRUE),][i]))))
}

#file creation
write.csv(neighbours,"songbasedrecommendation.csv")

#################################
#################################
#######CLUSTERING################
#################################

#calculate distance using binary method as our data is binary
d<-dist(data1,method="binary")

#hierarchical clustering using different methods
h1<-hclust(d,method="complete")
h2<-hclust(d,method="ward.D")
h3<-hclust(d,method="single")
h4<-hclust(d,method="average")
h5<-hclust(d,method="centroid")

# Dendrograms
plot(h1,col=44)
plot(h2,col=53)
plot(h3,col=59)
plot(h4,col=99)
plot(h5,col=142)

#plots of clusters
plot(cutree(h1, k = 10),col=44)
plot(cutree(h2, k = 10),col=53)
plot(cutree(h3, k = 10),col=59)
plot(cutree(h4, k = 10),col=99)
plot(cutree(h5, k = 10),col=142)

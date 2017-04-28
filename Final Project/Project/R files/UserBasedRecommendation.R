
library("lsa")
# used a smaller subset of lastfm-germany dataset as it is taking lot of time, with same number of dimensions
data <- read.csv("smalldf.csv");
data=as.matrix(data,header=FALSE);
data1 <- data[,2:ncol(data)];

#similarity calculation
song_sim = cosine(data1);
data_similarity <- as.data.frame(song_sim);

#function to get the score
getScore <- function(a, b)
{
  x <- sum(a*b)/sum(b);
}

#recommendation matrix
recmnd <- matrix(NA, nrow=nrow(data),ncol=ncol(data)-1,dimnames=list((data[,1]),colnames(data[,-1])));

#Loop through each user
for(i in 1:nrow(recmnd)) 
{
  #Loop through each song
  for(j in 1:ncol(recmnd)) 
  {
    #present user and song name
    user <- rownames(recmnd)[i];
    song <- colnames(recmnd)[j];
    
    # checking whether the user has listened to this song or not, so that we dont recommend same song to him again
    if(as.integer(data[which(data[,1]==user),song]) == 1)
    { 
      recmnd[i,j]<-"";
    } else {
      
      # Getting song top 10 similarities
      top10<-((head(n=11,(data_similarity[order(data_similarity[,song],decreasing=TRUE),][song]))));
      top10_names <- as.character(rownames(top10));
      top10_similarities <- as.numeric(top10[,1]);
      
      # As songs arranged in decreasing order of similarities, first song will be the same song, so drop it.
      top10_similarities<-top10_similarities[-1];
      top10_names<-top10_names[-1];
      
      # Get user history whether he has listened to this song or not
      top10_history<- data[,c("user",top10_names)];
      top10_userHistory<-top10_history[which(top10_history[,1]==user),];
      top10_userHistory <- as.numeric(top10_userHistory[!(names(top10_userHistory) %in% c("user"))]);
      
      # Calculate scores
      recmnd[i,j]<-getScore(a=top10_similarities,b=top10_userHistory);
      
    }
  }   
} 
scores <- recmnd;
# Taking out top 100 songs based on the scores and recommending top 100.
scores_final <- matrix(NA, nrow=nrow(scores),ncol=100,dimnames=list(rownames(scores)));
for(i in 1:nrow(scores)) 
{
  scores_final[i,] <- names(head(n=100,(scores[,order(scores[i,],decreasing=TRUE)])[i,]));
}

write.csv(scores_final,"userbasedrecommendation.csv")
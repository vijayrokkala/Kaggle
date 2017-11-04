movies<-read.table("movieLens.txt",header=FALSE,sep="|",quote="\"")
str(movies)

colnames(movies)=c("ID","Title","releasedate", "VideoReleaseDate","IMDB","Unknown",
                   "Action","Adventure","Animation","Childrens", "Comedy", "Crime",
"Documentary", "Drama", "Fantasy", "FilmNoir",
                   "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller",
                   "War", "Western")

movies$ID=NULL
movies$releasedate=NULL
movies$VideoReleaseDate=NULL
movies$IMDB=NULL

movies=unique(movies)

table(movies$Romance,movies$Drama)

distances=dist(movies[2:20],method="euclidean")

clusterMovies = hclust(distances, method = "ward.D")

clusterGroups=cutree(clusterMovies,k=10)

tapply(movies$Action,clusterGroups,mean)

tapply(movies$Romance,clusterGroups,mean)

subset(movies,Title=="Men in Black (1997)")

cluster2=subset(movies,clusterGroups==2)

cluster2=subset(movies,clusterGroups==2)

cluster2$Title[1:10]

clusterGroups1=cutree(clusterMovies,k=2)


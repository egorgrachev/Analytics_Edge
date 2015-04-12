setwd("AnalyticsEdge/Unit 6/")
movies = read.table("movieLens.txt", header=FALSE, sep="|", quote="\"")
str(movies)
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
str(movies)
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
movies = unique(movies)
str(movies)

nrow(subset(movies, movies$Comedy == 1))
nrow(subset(movies, movies$Western == 1))
nrow(subset(movies, movies$Romance == 1 & movies$Drama == 1))

distances = dist(movies[2:20], method="euclidean")
clusterMovies = hclust(distances, method="ward.D")
plot(clusterMovies)
clusterGroups = cutree(clusterMovies, k = 10)
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]
cluster2 = subset(movies, clusterGroups == 2)
cluster2$Title[1:10]


clusterGroups2 = cutree(clusterMovies, k = 2)
cluster2=subset(movies, clusterGroups2 == 2)
summary(cluster2)

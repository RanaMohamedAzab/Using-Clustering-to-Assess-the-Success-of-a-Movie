# Read Data
Movies = read.csv("tmdb_5000_movies.csv")
str(Movies)
summary(Movies)
Movies = unique(Movies)
table(Movies$status)

# Create a subset of all released movies
Released = subset(Movies, Movies$status == "Released")

#Remove unimportant variables
Released$gen_id1 = NULL
Released$gen_id2 = NULL
Released$gen_id3 = NULL
Released$gen_id4 = NULL
Released$gen_id5 = NULL
Released$homepage = NULL
Released$film_id = NULL
Released$key1 = NULL
Released$key2 = NULL
Released$key3 = NULL
Released$key4 = NULL
Released$key5 = NULL
Released$kid1 = NULL
Released$kid2 = NULL
Released$kid3 = NULL
Released$kid4 = NULL
Released$kid5 = NULL
Released$original_title = NULL
Released$overview = NULL
Released$status = NULL
Released$tagline = NULL
Released$title = NULL
Released$vote_count = NULL
Released$compid1 = NULL
Released$compid2 = NULL
Released$compid3 = NULL
Released$compid4 = NULL
Released$compid5 = NULL
Released$country_code1 = NULL
Released$country_code2 = NULL
Released$country_code3 = NULL
Released$country_code5 = NULL
str(Released)

# Create a subset of all released movies with only variables measuring success and budget to indicate cost to see successfull movies
Released_Edited = Released[c("budget", "popularity", "revenue", "vote_average")]
summary(Released_Edited)

# Normalization
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}
Released_Edited$budget = normalize(Released_Edited$budget)
Released_Edited$popularity = normalize(Released_Edited$popularity)
Released_Edited$revenue = normalize(Released_Edited$revenue)
Released_Edited$vote_average = normalize(Released_Edited$vote_average)
summary(Released_Edited)

# Grouping Movies into clusters based on budget, revenue, popularity and vote_average using k-means
set.seed(1)
KMC1 = kmeans(Released_Edited, centers = 1, iter.max = 10000)
KMC2 = kmeans(Released_Edited, centers = 2, iter.max = 10000)
KMC3 = kmeans(Released_Edited, centers = 3, iter.max = 10000)
KMC4 = kmeans(Released_Edited, centers = 4, iter.max = 10000)
KMC5 = kmeans(Released_Edited, centers = 5, iter.max = 10000)
KMC6 = kmeans(Released_Edited, centers = 6, iter.max = 10000)
KMC7 = kmeans(Released_Edited, centers = 7, iter.max = 10000)
KMC8 = kmeans(Released_Edited, centers = 8, iter.max = 10000)
KMC9 = kmeans(Released_Edited, centers = 9, iter.max = 10000)
KMC10 = kmeans(Released_Edited, centers = 10, iter.max = 10000)

# plot
Num_of_Clusters = seq(1,10,1)
Total_Withiniss = c(KMC1$tot.withinss, KMC2$tot.withinss, KMC3$tot.withinss, KMC4$tot.withinss, KMC5$tot.withinss, KMC6$tot.withinss, KMC7$tot.withinss, KMC8$tot.withinss, KMC9$tot.withinss, KMC10$tot.withinss)
plot(Num_of_Clusters, Total_Withiniss, type="b")

# From the graph, 7 clusters are suitable where adding more clusters will not significantly improve the total withiniss
str(KMC7)
Released$Clusters = KMC7$cluster
tapply(Released$budget, KMC7$cluster, mean)
tapply(Released$popularity, KMC7$cluster, mean)
tapply(Released$revenue, KMC7$cluster, mean)
tapply(Released$vote_average, KMC7$cluster, mean)

#Study Cluster 6
Cluster_6 = subset(Released, KMC7$cluster == 6)

#check if there is any movie that has no genre in cluster 6
No_genre = subset(Cluster_6, Cluster_6$gen_name1 == "" & Cluster_6$gen_name2 == "" & Cluster_6$gen_name3 == "" & Cluster_6$gen_name4 == "" & Cluster_6$gen_name5 == "")
# 0 observations in cluster 6 with no genre

#show the genre of these successful movies
genre_6 = Cluster_6[c("gen_name1", "gen_name2", "gen_name2", "gen_name4", "gen_name5")]
genre_6 = as.matrix(genre_6)
genre_6 = as.vector(genre_6)
genre_6 = as.data.frame(table(genre_6))
colnames(genre_6)[1] = "genre"
colnames(genre_6)[2] = "No. of Movies"
str(genre_6)
genre_6 = subset(genre_6, genre_6$genre != "")
genre_6 = genre_6[order(-genre_6$`No. of Movies`),]

#Plot Top 10 Genres in cluster 6
top_10_genres_6 = head(genre_6, n = 10)
library(ggplot2)
ggplot(top_10_genres_6, aes(x = reorder(genre, -`No. of Movies`), y = `No. of Movies`)) + geom_bar(stat = "identity")+ geom_col(aes(fill = `No. of Movies`)) + scale_fill_gradient2(low = "white", high = "blue") + xlab("Genre") + ggtitle("Top 10 Genres of Movies in Cluster 6 generating high profits and popularity") + theme_classic()

#check if there is any movie that has no production company stated
No_company = subset(Cluster_6, Cluster_6$company1 == "" & Cluster_6$company2 == "" & Cluster_6$company3 == "" & Cluster_6$company4 == "" & Cluster_6$company5 == "")
# 0 observations in cluster 6 with no no production company stated

#show the companies that produced these successful movies
company_6 = Cluster_6[c("company1", "company2", "company3", "company4", "company5")]
company_6 = as.matrix(company_6)
company_6 = as.vector(company_6)
company_6 = as.data.frame(table(company_6))
colnames(company_6)[1] = "company"
colnames(company_6)[2] = "No. of Movies"
company_6 = subset(company_6, company_6$company != "")
company_6[order(-company_6$`No. of Movies`),]

#Plot Top 9 Companies in cluster 6
Top_9_Companies_6 = company_6[order(-company_6$`No. of Movies`),][1:9,]
library(ggplot2)
ggplot(Top_9_Companies_6, aes(x = reorder(company, -`No. of Movies`), y = `No. of Movies`)) + geom_bar(stat = "identity")+ geom_col(aes(fill = `No. of Movies`)) + scale_fill_gradient2(low = "white", high = "blue") + xlab("Companies") + ggtitle("Top 9 Companies producing Movies in cluster 6 with high profits and popularity") + theme_classic()


#check if there is any movie that has no production country stated
No_country = subset(Cluster_6, Cluster_6$country1 == "" & Cluster_6$country2 == "" & Cluster_6$country3 == "" & Cluster_6$country5 == "")
# 0 observations in cluster 6 with no production country stated

#show the countries that produced these successful movies
countries_6 = Cluster_6[c("country1", "country2", "country3", "country5")]
countries_6 = as.matrix(countries_6)
countries_6 = as.vector(countries_6)
countries_6 = as.data.frame(table(countries_6))
colnames(countries_6)[1] = "country"
colnames(countries_6)[2] = "No. of Movies"
str(countries_6)
countries_6 = subset(countries_6, countries_6$country != "")
countries_6[order(-countries_6$`No. of Movies`),]

#Plot Top 10 Countries in cluster 6
Top_10_Countries_6 = countries_6[order(-countries_6$`No. of Movies`),][1:10,]
library(ggplot2)
ggplot(Top_10_Countries_6, aes(x = reorder(country, -`No. of Movies`), y = `No. of Movies`)) + geom_bar(stat = "identity")+ geom_col(aes(fill = `No. of Movies`)) + scale_fill_gradient2(low = "white", high = "blue") + xlab("Country") + ggtitle("Top 10 Countries producing Movies in cluster 6 with high profits and popularity") + theme_classic()


#Study Cluster 4
Cluster_4 = subset(Released, KMC7$cluster == 4)

#check if there is any movie that has no genre in cluster 4
No_genre_4 = subset(Cluster_4, Cluster_4$gen_name1 == "" & Cluster_4$gen_name2 == "" & Cluster_4$gen_name3 == "" & Cluster_4$gen_name4 == "" & Cluster_4$gen_name5 == "")
# 0 observations in cluster 4 with no genre

#show the genre of these successful movies
genre_4 = Cluster_4[c("gen_name1", "gen_name2", "gen_name2", "gen_name4", "gen_name5")]
genre_4 = as.matrix(genre_4)
genre_4 = as.vector(genre_4)
genre_4 = as.data.frame(table(genre_4))
colnames(genre_4)[1] = "genre"
colnames(genre_4)[2] = "No. of Movies"
genre_4 = subset(genre_4, genre_4$genre != "")
genre_4[order(-genre_4$`No. of Movies`),]

#Plot Top 10 Genre in cluster 4
top_10_genres_4 = head(genre_4, n = 10)
library(ggplot2)
ggplot(top_10_genres_4, aes(x = reorder(genre, -`No. of Movies`), y = `No. of Movies`)) + geom_bar(stat = "identity")+ geom_col(aes(fill = `No. of Movies`)) + scale_fill_gradient2(low = "white", high = "blue") + xlab("Genre") + ggtitle("Top 10 Genres of Movies in Cluster 4 generating high rating") + theme_classic()

#check if there is any movie that has no production company stated
No_company_4 = subset(Cluster_4, Cluster_4$company1 == "" & Cluster_4$company2 == "" & Cluster_4$company3 == "" & Cluster_4$company4 == "" & Cluster_4$company5 == "")
# 0 observations in cluster 4 with no production company stated

#show the companies that produced these successful movies
company_4 = Cluster_4[c("company1", "company2", "company3", "company4", "company5")]
company_4 = as.matrix(company_4)
company_4 = as.vector(company_4)
company_4 = as.data.frame(table(company_4))
colnames(company_4)[1] = "company"
colnames(company_4)[2] = "No. of Movies"
company_4 = subset(company_4, company_4$company != "")
company_4[order(-company_4$`No. of Movies`),]


#Plot Top 9 Companies in cluster 4
Top_9_Companies_4 = company_4[order(-company_4$`No. of Movies`),][1:9,]
library(ggplot2)
ggplot(Top_9_Companies_4, aes(x = reorder(company, -`No. of Movies`), y = `No. of Movies`)) + geom_bar(stat = "identity")+ geom_col(aes(fill = `No. of Movies`)) + scale_fill_gradient2(low = "white", high = "blue") + xlab("Companies") + ggtitle("Top 9 Companies producing Movies in cluster 4 with high rating") + theme_classic()

#check if there is any movie that has no production country stated
No_country_4 = subset(Cluster_4, Cluster_4$country1 == "" & Cluster_4$country2 == "" & Cluster_4$country3 == "" & Cluster_4$country5 == "")
countries_4 = Cluster_4[c("country1", "country2", "country3", "country5")]
countries_4 = as.matrix(countries_4)
countries_4 = as.vector(countries_4)
countries_4 = as.data.frame(table(countries_4))
colnames(countries_4)[1] = "country"
colnames(countries_4)[2] = "No. of Movies"
str(countries_4)
countries_4 = subset(countries_4, countries_4$country != "")
countries_4[order(-countries_4$`No. of Movies`),]


#Plot Top 10 Countries in cluster 4
Top_10_Countries_4 = countries_4[order(-countries_4$`No. of Movies`),][1:10,]
library(ggplot2)
ggplot(Top_10_Countries_4, aes(x = reorder(country, -`No. of Movies`), y = `No. of Movies`)) + geom_bar(stat = "identity")+ geom_col(aes(fill = `No. of Movies`)) + scale_fill_gradient2(low = "white", high = "blue") + xlab("Country") + ggtitle("Top 10 Countries producing Movies in cluster 4 with high rating") + theme_classic()

##
## Purpose: Final Poster
## Author: Alejandro Mora
##

## Import Dataset
## Source: https://www.kaggle.com/yamaerenay/spotify-dataset-19212020-160k-tracks
##
## Data set size requirements
# (NumberOfColumns * 4) * (NumberOfRows/100) >= 100
# (19 * 4 ) * ( 174389 / 100 ) >= 100
# 132535.6 >= 100

#Import Data
fname <- file.choose()
spotify <- read.csv(file = fname
                  ,header = TRUE
                  ,stringsAsFactors = FALSE
)

#View structure and data types
str(spotify)
colnames(spotify)
#NA Values?
row.names(is.na(spotify))

##Plot 1 (Distribution, density)
##Popularity of Songs over the years
library(ggplot2)

col.vec <- rep(rgb(15,6,97, maxColorValue = 255, alpha = 80)
               , nrow(spotify))

ggplot(spotify) + aes(y = popularity, x = year, color = popularity) + geom_bin2d(bins = 100) + xlim(1920, 2021) + scale_fill_gradient(low = 'springgreen4', high = 'black')

##Plot 2 (Distribution, box plot)
##Popularity of songs based on whether or not they have explicit content or not
library(vioplot)
vioplot(spotify$popularity[spotify$explicit == 1]
        ,spotify$popularity[spotify$explicit == 0]
        , xlab = "Explicit Content"
        , ylab = "Popularity"
        , main = "Distribution of Popular songs"
        , las = 1
        , col = c("#840D00", "#006416")
        , bty = NA
        )

##Plot 3 (histogram of number of songs throughout the decades)
hist(spotify$year
     , main = "Frequency of Songs"
     , xlab = "Year"
     , ylab = "Frequency"
     , breaks = 20
     , ylim = c(0, 16000)
     , col = "springgreen4"
     )


#Multidimensional Plot
##Most popular Artists

par(mfrow = c(2,2))
par(mfrow = c(1,1))

#Group popularity totals and take the top 20 artists
group.popularity.sum <- aggregate(spotify$popularity, list(spotify$artists), sum)
group.popularity.sum <- group.popularity.sum[order(-group.popularity.sum$x),]
group.popularity.sum <- group.popularity.sum[1:20,1:2]

barplot(group.popularity.sum$x
        , names.arg = group.popularity.sum$Group.1
        , main = "Most Popular Artists"
        , ylab = "Sum of Popularity"
        , xlab = "Artist"
        , las =2
        , ylim = c(0,20000)
        , col = "springgreen4"
        )


#Line Chart showing time series of song traits over the decade by mean of every year
group.danceability.year <- aggregate(spotify$danceability, list(spotify$year), mean)
group.instrumentalness.year <- aggregate(spotify$instrumentalness, list(spotify$year), mean)
group.acousticness.year <- aggregate(spotify$acousticness, list(spotify$year), mean)
group.energy.year <- aggregate(spotify$energy, list(spotify$year), mean)
group.loudness.year <- aggregate(spotify$loudness, list(spotify$year), mean)
group.speechiness.year <- aggregate(spotify$speechiness, list(spotify$year), mean)

par(bg = 'white')
plot(group.danceability.year
     , spotify$year[group.danceability.year == spotify$year]
     , main = "Average Trait Values of Songs"
     , col = "#006416"
     , cex = 1
     , pch = 16
     , las = 1
     , type = "l"
     , ylim = c(0, 1.00)
     , lwd = 2
     , ylab = "Trait Value"
)
points(group.energy.year$Group.1, group.energy.year$x, pch = 19, type ="l", col = "blue", lwd = 2.5)
points(group.instrumentalness.year$Group.1, group.instrumentalness.year$x, pch = 19, type ="l", col = "orange", lwd = 2.5)
points(group.acousticness.year$Group.1, group.acousticness.year$x, pch = 19, type ="l", col = "red", lwd = 2.5)
points(group.speechiness.year$Group.1, group.speechiness.year$x, pch = 19, type ="l", col = "purple", lwd = 2.5)
legend("topright", legend = c("Danceability", "Energy", "Instrumentalness","Acousticness", "Speechness")
       , col = c("#006416", "blue", "orange", "red", "purple")
       , lwd = 2
       , cex = 0.65
       , bty = "n"
       )


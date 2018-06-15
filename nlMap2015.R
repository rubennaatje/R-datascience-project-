library(sp)
library(rgdal)
URL <-"http://www.twiav.nl/files/NL_Museums_Amsterdam.csv"
ams.df <-read.csv(URL)
ams.spdf <-SpatialPointsDataFrame(coords = ams.df[3:4], data = ams.df[1:2], proj4string = CRS("+init=epsg:28992"))
ams.spdf
plot(ams.spdf, pch = 19, col = "blue", axes = TRUE, main = "Enkele bezienswaardigheden in Amsterdam")
text(coordinates(ams.spdf), as.character(ams.spdf$Name), cex = .7, pos = 4, col = "blue")
pos.vector <- rep(4, length(ams.spdf))
pos.vector[ams.spdf$Name == "Stedelijk Museum"] <- 2
pos.vector
plot(ams.spdf, pch = 19, col = "blue", axes = TRUE, main = "Enkele bezienswaardigheden in Amsterdam")
text(coordinates(ams.spdf), as.character(ams.spdf$Name), cex = .7, pos = pos.vector, col = "blue")
proj4string(ams.spdf) <-CRS("+init=epsg:28992")




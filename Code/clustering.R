
label_encoder = function(vec){
  levels = sort(unique(vec))
  function(x){
    match(x, levels)
  }
}

terrorism_data = subset(terrorism, terrorism$iyear >= 2000 & (terrorism$region == 3), select = c(iyear, imonth, iday, country, region, latitude, longitude, multiple, attacktype1, targtype1, targsubtype1, weaptype1, nkill, nwound, nkillter, gname) )
encoder = label_encoder(terrorism_data$gname)
encoded_groups = encoder(terrorism_data$gname)
encoded_groups

terrorism_data$gname = encoded_groups
terrorism_data = na.omit(terrorism_data)


library("Imap")
library("rworldmap")


# Geographical coordinates
locs <- as.data.frame(list(terrorism_data$longitude, terrorism_data$latitude))
colnames(locs) <- c("lon", "lat")
coordinates(locs) <- c("lon", "lat")
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(locs) <- crs.geo
# "CountriesCoarse" dataset is used for plotting the coarse map of the countries of the world
data("countriesCoarse")
windows(800,600,10) # In a new window
plot(locs, pch=20, col='red', lwd=1)
plot(countriesCoarse, add=T)

no_loc_terr = subset(terrorism_data, select = -c(longitude, latitude, country, region, gname))

#Q2.2.2
n = nrow(no_loc_terr)
k = 2
kmeans.result = kmeans(no_loc_terr, k)
plot(locs, pch=20, col=kmeans.result$cluster, lwd=1)
plot(countriesCoarse, add=T)

w <- (kmeans.result$tot.withinss/kmeans.result$totss)*100
b <- (kmeans.result$betweenss/kmeans.result$totss)*100
print(paste("WSS=",round(w,2),"%")) # "WSS= 17.04  %"
print(paste("BSS=",round(b,2),"%")) # "BSS= 82.96 %"

BH_index <- w / k
print(paste("BH index=",round(BH_index,2)))
CH_index <- (b / (k - 1)) / (w / (n - k))
print(paste("CH index=",round(CH_index,2)))


############
k = 4
kmeans.result = kmeans(no_loc_terr, k)
plot(locs, pch=20, col=kmeans.result$cluster, lwd=1)
plot(countriesCoarse, add=T)

w <- (kmeans.result$tot.withinss/kmeans.result$totss)*100
b <- (kmeans.result$betweenss/kmeans.result$totss)*100
print(paste("WSS=",round(w,2),"%")) # "WSS= 10.87 %"
print(paste("BSS=",round(b,2),"%")) # "BSS= 89.13 %"

BH_index <- w / k
print(paste("BH index=",round(BH_index,2)))
CH_index <- (b / (k - 1)) / (w / (n - k))
print(paste("CH index=",round(CH_index,2)))

############
k = 5
kmeans.result = kmeans(no_loc_terr, 5)
plot(locs, pch=20, col=kmeans.result$cluster, lwd=1)
plot(countriesCoarse, add=T)

w <- (kmeans.result$tot.withinss/kmeans.result$totss)*100
b <- (kmeans.result$betweenss/kmeans.result$totss)*100
print(paste("WSS=",round(w,2),"%")) # "WSS= 8.46 %"
print(paste("BSS=",round(b,2),"%")) # "BSS= 91.54 %"

BH_index <- w / k
print(paste("BH index=",round(BH_index,2)))
CH_index <- (b / (k - 1)) / (w / (n - k))
print(paste("CH index=",round(CH_index,2)))

############
k = 6
kmeans.result = kmeans(no_loc_terr, k)
plot(locs, pch=20, col=kmeans.result$cluster, lwd=1)
plot(countriesCoarse, add=T)

w <- (kmeans.result$tot.withinss/kmeans.result$totss)*100
b <- (kmeans.result$betweenss/kmeans.result$totss)*100
print(paste("WSS=",round(w,2),"%")) # "WSS=   7.3 %"
print(paste("BSS=",round(b,2),"%")) # "BSS= 92.7 %"

BH_index <- w / k
print(paste("BH index=",round(BH_index,2)))
CH_index <- (b / (k - 1)) / (w / (n - k))
print(paste("CH index=",round(CH_index,2)))

############
k = 7
kmeans.result = kmeans(no_loc_terr, k)
plot(locs, pch=20, col=kmeans.result$cluster, lwd=1)
plot(countriesCoarse, add=T)

w <- (kmeans.result$tot.withinss/kmeans.result$totss)*100
b <- (kmeans.result$betweenss/kmeans.result$totss)*100
print(paste("WSS=",round(w,2),"%")) # "WSS=    5.55 %"
print(paste("BSS=",round(b,2),"%")) # "BSS= 94.45 %"

BH_index <- w / k
print(paste("BH index=",round(BH_index,2)))
CH_index <- (b / (k - 1)) / (w / (n - k))
print(paste("CH index=",round(CH_index,2)))
print(kmeans.result)

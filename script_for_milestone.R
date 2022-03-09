library(raster)
library(caret)
library(rgdal)
library(rpart)
library(rasterVis)
library(terra)
detach("package:terra", unload=TRUE)


WA1<-stack("G:/SHOWCASE_dati/result/images/rgb/all/WA1_RGB.tif")

area_buffer<-shapefile("C:/Users/MiTorresani/OneDrive - Alma Mater Studiorum Università di Bologna/unibo/SHOWCASE/dati SHOWCASE/shp/test/WA1/buffer1m.shp")
WA1_cropped<-crop(WA1, extent(area_buffer))
WA1_cropped_2<-mask(WA1_cropped, area_buffer)
setwd("G:/SHOWCASE_dati/result/images/rgb/buffer1m/")
writeRaster(WA1_cropped_2, "WA1_rgb_buffer1m.tif" , overwrite=TRUE)

#load raster
img_all<-list.files("G:/SHOWCASE_dati/result/images/rgb/buffer1m_res1cm/", full.names = T)

#load shp
shp<-list.files("C:/Users/MiTorresani/OneDrive - Alma Mater Studiorum Università di Bologna/unibo/SHOWCASE/dati SHOWCASE/shp/test/", recursive = T, pattern="flowers_and_grass_rgb.shp", full.names = T)

setwd("G:/SHOWCASE_dati/result/machine_learning/rf/1cm")

for (i in 1:length(img_all)) {
  img<-stack(img_all[i])
  samp<-shapefile(shp[[i]])
  ptsamp <- spsample(samp, 5000, type='regular') 
  # add the land cover class to the points 
  ptsamp$class <- over(ptsamp, samp)$id 
  # extract values with points 
  df <- raster::extract(img, ptsamp) 
  # To see some of the reflectance values 
  head(df) 
  
  #plt <- levelplot(img, col.regions = classcolor, main = 'Distribution of Training Sites') 
  #print(plt + layer(sp.points(ptsamp, pch = 3, cex = 0.5, col = 1))) 
  
  
  # Extract the layer values for the locations 
  sampvals <- extract(img, ptsamp, df = TRUE) 
  # sampvals no longer has the spatial information. To keep the spatial information you use `sp=TRUE` argument in the `extract` function. 
  # drop the ID column 
  sampvals <- sampvals[, -1] 
  # combine the class information with extracted values 
  sampdata <- data.frame(classvalue = ptsamp$class, sampvals) 
  
  
  # Train the model 
  cart <- rpart(as.factor(classvalue)~., data=sampdata, method = 'class', minsplit = 4) 
  # print(model.class) 
  # Plot the trained classification tree 
  plot(cart, uniform=TRUE, main="Classification Tree") 
  text(cart, cex = 0.8) 
  
  # Now predict the subset data based on the model; prediction for entire area takes longer time 
  pr2020 <- predict(img, cart, type='class') 
  pr2020 
  nome<-paste(unlist(strsplit(shp[[i]], split = '/', fixed = T))[11], sep = "" )
  writeRaster(pr2020, paste0("rf_",nome,"_rgb_1cm.tiff"), overwrite=T)
  #detach("package:terra", unload=TRUE)
  
}

#assess area size of both flowers and grass
a<- rast("G:/SHOWCASE_dati/result/rf/rf_WA1_rgb.tif")
ncell(a)
p <- as.polygons(a)
expanse(p)





####evaluation of the model 
library(dismo)
set.seed(99)
j <- kfold(sampdata, k = 5, by=sampdata$classvalue)
table(j)


x <- list()
for (k in 1:5) {
  train <- sampdata[j!= k, ]
  test <- sampdata[j == k, ]
  cart <- rpart(as.factor(classvalue)~., data=train, method = 'class', minsplit = 5)
  pclass <- predict(cart, test, type='class')
  # create a data.frame using the reference and prediction
  x[[k]] <- cbind(test$classvalue, as.integer(pclass))
}



y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
conmat <- table(y)
# change the name of the classes
colnames(conmat) <- classdf$classnames
rownames(conmat) <- classdf$classnames
conmat


# number of cases
n <- sum(conmat)
n

# number of correctly classified cases per class
diag <- diag(conmat)
# Overall Accuracy
OA <- sum(diag) / n
OA



# observed (true) cases per class
rowsums <- apply(conmat, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(conmat, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
kappa



PA <- diag / colsums
# User accuracy
UA <- diag / rowsums
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)
outAcc
#untar multiple file
setwd("e:/exam/data/download")
wd <- getwd()
# Set path for downloads folder
path <- "e:/exam/data/download"
# Retrieve list of .tar.gz files in downloads folder
filenames <- list.files(path, pattern = ".zip", full.names = TRUE)
# Create folder names for each dataset without extension
foldername <- sub("\\..*$", "", basename(filenames))
# loop through each dataset and untar to new folders
for (i in filenames) {
  foldername <- sub("\\..*$", "", basename(i))
  dir.create(foldername)
  file <- file.path(wd, foldername)
  untar(i, exdir = file)
}



#########Preprocessing########
##########prefire######################
library(raster)

setwd("e:/exam/data/download/S2B_MSIL2A_20190412T033739_N0211_R061_T48QUJ_20190412T093531_resampled.data")
b2_pre <- raster("B2.img")
b3_pre <- raster("B3.img")
b4_pre <- raster("B4.img")
b8_pre <- raster("B8.img")
b12_pre <- raster("b12.img")

#clip
library(sp)
library(rgdal)
aoi <- readOGR("E:/exam/data/vector/AOI_area.shp")
aoi <- spTransform(aoi, CRS(proj4string(b2_pre)))


clip_b2_pre <- crop(b2_pre,aoi)
clip_b3_pre <- crop(b3_pre,aoi)
clip_b4_pre <- crop(b4_pre,aoi)
clip_b8_pre <- crop(b8_pre,aoi)
clip_b12_pre <- crop(b12_pre,aoi)

setwd("e:/exam/data/download/S2B_MSIL2A_20190412T033739_N0211_R061_T48QUJ_20190412T093531_resampled.data/clip")
writeRaster(clip_b2_pre,"clip_B2_pre.tif", overwrite = TRUE)
writeRaster(clip_b3_pre,"clip_B3_pre.tif", overwrite = TRUE)
writeRaster(clip_b4_pre,"clip_B4_pre.tif", overwrite = TRUE)
writeRaster(clip_b8_pre,"clip_B8_pre.tif", overwrite = TRUE)
writeRaster(clip_b12_pre,"clip_B12_pre.tif", overwrite = TRUE)


#stack raster
setwd("e:/exam/data/download/S2B_MSIL2A_20190412T033739_N0211_R061_T48QUJ_20190412T093531_resampled.data/clip")
clip_stack_pre <- stack(clip_b2_pre,clip_b3_pre,clip_b4_pre,
                        clip_b8_pre,clip_b12_pre)
writeRaster(clip_stack_pre,"clip_stack_pre.tif", overwrite = TRUE)



##########post-fire1######################
library(raster)
setwd("e:/exam/data/download/S2B_MSIL2A_20190422T033539_N0211_R061_T48QUJ_20190422T074708_resampled.data")
b2_post1 <- raster("B2.img")
b3_post1 <- raster("B3.img")
b4_post1 <- raster("B4.img")
b8_post1 <- raster("B8.img")
b12_post1 <- raster("b12.img")

#clip
library(sp)
library(rgdal)
aoi <- readOGR("E:/exam/data/vector/AOI_area.shp")
aoi <- spTransform(aoi, CRS(proj4string(b2_post1)))

clip_b2_post1 <- crop(b2_post1,aoi)
clip_b3_post1 <- crop(b3_post1,aoi)
clip_b4_post1 <- crop(b4_post1,aoi)
clip_b8_post1 <- crop(b8_post1,aoi)
clip_b12_post1 <- crop(b12_post1,aoi)

setwd("e:/exam/data/download/S2B_MSIL2A_20190422T033539_N0211_R061_T48QUJ_20190422T074708_resampled.data/clip")
writeRaster(clip_b2_post1,"clip_B2_post1.tif", overwrite = TRUE)
writeRaster(clip_b3_post1,"clip_B3_post1.tif", overwrite = TRUE)
writeRaster(clip_b4_post1,"clip_B4_post1.tif", overwrite = TRUE)
writeRaster(clip_b8_post1,"clip_B8_post1.tif", overwrite = TRUE)
writeRaster(clip_b12_post1,"clip_B12_post1.tif", overwrite = TRUE)


#stack raster
setwd("e:/exam/data/download/S2B_MSIL2A_20190422T033539_N0211_R061_T48QUJ_20190422T074708_resampled.data/clip")
clip_stack_post1 <- stack(clip_b2_post1,clip_b3_post1,clip_b4_post1,
                        clip_b8_post1,clip_b12_post1)
writeRaster(clip_stack_post1,"clip_stack_post1.tif", overwrite = TRUE)



##########post-fire2######################
setwd("e:/exam/data/download/S2A_MSIL2A_20200312T033531_N0214_R061_T48QUJ_20200312T081053_resampled.data")
b2_post2 <- raster("B2.img")
b3_post2 <- raster("B3.img")
b4_post2 <- raster("B4.img")
b8_post2 <- raster("B8.img")
b12_post2 <- raster("b12.img")

#clip
aoi <- readOGR("E:/exam/data/vector/AOI_area.shp")
aoi <- spTransform(aoi, CRS(proj4string(b2_post2)))

clip_b2_post2 <- crop(b2_post2,aoi)
clip_b3_post2 <- crop(b3_post2,aoi)
clip_b4_post2 <- crop(b4_post2,aoi)
clip_b8_post2 <- crop(b8_post2,aoi)
clip_b12_post2 <- crop(b12_post2,aoi)

setwd("e:/exam/data/download/S2A_MSIL2A_20200312T033531_N0214_R061_T48QUJ_20200312T081053_resampled.data/clip")
writeRaster(clip_b2_post2,"clip_B2_post2.tif", overwrite = TRUE)
writeRaster(clip_b3_post2,"clip_B3_post2.tif", overwrite = TRUE)
writeRaster(clip_b4_post2,"clip_B4_post2.tif", overwrite = TRUE)
writeRaster(clip_b8_post2,"clip_B8_post2.tif", overwrite = TRUE)
writeRaster(clip_b12_post2,"clip_B12_post2.tif", overwrite = TRUE)


#stack raster
setwd("e:/exam/data/download/S2A_MSIL2A_20200312T033531_N0214_R061_T48QUJ_20200312T081053_resampled.data/clip")
clip_stack_post2 <- stack(clip_b2_post2,clip_b3_post2,clip_b4_post2,
                          clip_b8_post2,clip_b12_post2)
writeRaster(clip_stack_post2,"clip_stack_post2.tif", overwrite = TRUE)




##########post-fire3######################
setwd("e:/exam/data/download/S2B_MSIL2A_20210401T033539_N0300_R061_T48QUJ_20210401T080138_resampled.data")
b2_post3 <- raster("B2.img")
b3_post3 <- raster("B3.img")
b4_post3 <- raster("B4.img")
b8_post3 <- raster("B8.img")
b12_post3 <- raster("b12.img")

#clip
aoi <- readOGR("E:/exam/data/vector/AOI_area.shp")
aoi <- spTransform(aoi, CRS(proj4string(b2_post3)))

clip_b2_post3 <- crop(b2_post3,aoi)
clip_b3_post3 <- crop(b3_post3,aoi)
clip_b4_post3 <- crop(b4_post3,aoi)
clip_b8_post3 <- crop(b8_post3,aoi)
clip_b12_post3 <- crop(b12_post3,aoi)

setwd("e:/exam/data/download/S2B_MSIL2A_20210401T033539_N0300_R061_T48QUJ_20210401T080138_resampled.data/clip")
writeRaster(clip_b2_post3,"clip_B2_post3.tif", overwrite = TRUE)
writeRaster(clip_b3_post3,"clip_B3_post3.tif", overwrite = TRUE)
writeRaster(clip_b4_post3,"clip_B4_post3.tif", overwrite = TRUE)
writeRaster(clip_b8_post3,"clip_B8_post3.tif", overwrite = TRUE)
writeRaster(clip_b12_post3,"clip_B12_post3.tif", overwrite = TRUE)


#stack raster
setwd("e:/exam/data/download/S2B_MSIL2A_20210401T033539_N0300_R061_T48QUJ_20210401T080138_resampled.data/clip")
clip_stack_post3 <- stack(clip_b2_post3,clip_b3_post3,clip_b4_post3,
                          clip_b8_post3,clip_b12_post3)
writeRaster(clip_stack_post3,"clip_stack_post3.tif", overwrite = TRUE)



##############co-registration#############
library(raster)
library(reshape2)
library(RStoolbox)

reference <- clip_stack_pre

regis_img <- clip_stack_post1



regis_clip_stack_post1 <- coregisterImages(regis_img, master = reference,
                          nSamples = 500, reportStats = TRUE)

regis_clip_stack_post1 <- alignExtent(reference,regis_img)

setwd("e:/exam/data/download/S2B_MSIL2A_20190422T033539_N0211_R061_T48QUJ_20190422T074708_resampled.data/clip")
writeRaster(regis_clip_stack_post1,"regis_clip_stack_post1.tif", overwrite = TRUE)




############NDVI###################
############prefire###########
library(RStoolbox)
library(ggplot2)

# claculate ndvi
ndvi_pre <- (clip_b8_pre - clip_b4_pre)/(clip_b8_pre + clip_b4_pre)
# print results
ndvi_pre

# write raster to tif
aoi <- readOGR("E:/exam/data/vector/AOI_ThuanChau.shp")
aoi <- spTransform(aoi, CRS(proj4string(ndvi_pre)))
ndvi_pre <- mask(ndvi_pre, aoi)
ndvi_pre <- crop(ndvi_pre, aoi)
setwd("E:/exam/output/03_ndvi")
writeRaster(ndvi_pre, "ndvi_pre.tif")


############post-fire1###########
# claculate ndvi
ndvi_post1 <- (clip_b8_post1 - clip_b4_post1)/(clip_b8_post1 + clip_b4_post1)
# print results
ndvi_post1

# write raster to tif

aoi <- readOGR("E:/exam/data/vector/AOI_ThuanChau.shp")
aoi <- spTransform(aoi, CRS(proj4string(ndvi_post1)))
ndvi_post1 <- mask(ndvi_post1, aoi)
ndvi_post1 <- crop(ndvi_post1, aoi)
setwd("E:/exam/output/03_ndvi")
writeRaster(ndvi_post1, "ndvi_post1.tif")



############post-fire2###########
# claculate ndvi
ndvi_post2 <- (clip_b8_post2 - clip_b4_post2)/(clip_b8_post2 + clip_b4_post2)
# print results
ndvi_post2

# write raster to tif

aoi <- readOGR("E:/exam/data/vector/AOI_ThuanChau.shp")
aoi <- spTransform(aoi, CRS(proj4string(ndvi_post2)))
ndvi_post2 <- mask(ndvi_post2, aoi)
ndvi_post2 <- crop(ndvi_post2, aoi)
setwd("E:/exam/output/03_ndvi")
writeRaster(ndvi_post2, "ndvi_post2.tif")


############post-fire3###########
# claculate ndvi
ndvi_post3 <- (clip_b8_post3 - clip_b4_post3)/(clip_b8_post3 + clip_b4_post3)
# print results
ndvi_post3

# write raster to tif

aoi <- readOGR("E:/exam/data/vector/AOI_ThuanChau.shp")
aoi <- spTransform(aoi, CRS(proj4string(ndvi_post3)))
ndvi_post3 <- mask(ndvi_post3, aoi)
ndvi_post3 <- crop(ndvi_post3, aoi)
setwd("E:/exam/output/03_ndvi")
writeRaster(ndvi_post3, "ndvi_post3.tif")



#################NBR####################
#################pre-fire###############
############prefire###########
# claculate NBR
nbr_pre <- (clip_b8_pre - clip_b12_pre)/(clip_b8_pre + clip_b12_pre)
# print results
nbr_pre

aoi <- readOGR("E:/exam/data/vector/AOI_ThuanChau.shp")
aoi <- spTransform(aoi, CRS(proj4string(nbr_pre)))
nbr_pre <- mask(nbr_pre, aoi)
nbr_pre <- crop(nbr_pre,aoi) 


# write raster to tif
setwd("E:/exam/output/04_nbr")
writeRaster(nbr_pre, "nbr_pre.tif")


############post-fire1###########
# claculate NBR
nbr_post <- (clip_b8_post1 - clip_b12_post1)/(clip_b8_post1 + clip_b12_post1)
# print results
nbr_post

# write raster to tif
setwd("E:/exam/output/04_nbr")
writeRaster(nbr_post, "nbr_post.tif")


#dNBR
dnbr <- nbr_pre - nbr_post

aoi <- readOGR("E:/exam/data/vector/AOI_ThuanChau.shp")
aoi <- spTransform(aoi, CRS(proj4string(dnbr)))
# masks the image to the extent of the shapefile (product to be masked, shapefile used)
dnbr <- mask(dnbr, aoi)
# crops the image to the extent of the shapefile (product to be masked, shapefile used)
dnbr <- crop(dnbr,aoi) 

setwd("E:/exam/output/04_nbr")
writeRaster(dnbr, "dNBR.tif")
dnbr <- raster("dNBR.tif")


#####################RBR#########################

setwd("E:/exam/output/04_nbr")
nbr_pre <- raster("nbr_pre.tif")


dnbr <- raster("dNBR.tif")

rbr = dnbr/(nbr_pre + 1.001)

setwd("E:/exam/output/05_rbr")
writeRaster(rbr,"rbr.tif")


#################RdNBR######################
setwd("E:/exam/output/04_nbr")
nbr_pre <- raster("nbr_pre.tif")
dnbr <- raster("dNBR.tif")


rdnbr = dnbr/sqrt(abs(nbr_pre))

setwd("E:/exam/output/06_rdnbr")
writeRaster(rdnbr,"rdnbr.tif")


#####################Burn Severity map#####################
####################dNBR#######################
# set ranges for the Burn Severity classes
dNBR_ranges <- c(-Inf,-.5,0, # out of range - no data
                 -.5,-.25,1, #enhanced regrowth, high
                 -.25,-.1,2,     # enhanced regrowth, low
                 -.1,0.1, 3,    # unburned area
                 0.1, 0.27, 4,  # low severity
                 0.27, 0.44, 5, # moderate-low severity
                 0.44, 0.66, 6, # moderate-high severity
                 0.66, 1.4, 7,  # high severity
                 1.4, +Inf, 0)  # out of range - no data

# set classification matrix
class.matrix <- matrix(dNBR_ranges, ncol = 3, byrow = T)

# use matrix to reclassify dNBR_scaled
dNBR_reclass <- reclassify(dnbr, class.matrix, right=NA)

# view frequencies of reclassification values
freq(dNBR_reclass)


# build the attribute table for the legend 
dNBR_reclass <- ratify(dNBR_reclass) 
rat <- levels(dNBR_reclass)[[1]]

# create the text that will be on the legend
rat$legend  <- c("No Data",
                 "High enhanced regrowth",
                 "Low enhanced regrowth",
                 "Unburned",
                 "Low Severity",
                 "Moderate-low Severity",
                 "Moderate-high Severity",
                 "High Severity") 
levels(dNBR_reclass) <- rat 


# colour setting for the severity map
#my_col=c("#FFFFFF","#556b2f","#6e8b3d","#32CD32", "#EEEE00","#ee9a00", "#FF0000", "#800080")
my_col=c("white", "darkolivegreen","darkolivegreen4","limegreen", "yellow2", "orange2", "red", "purple")

# plots the burn severity map with title
plot(dNBR_reclass,
     legend=F,box=F,axes=F,
     col=my_col,
     cex.main=0.9,
     main="Sentinel-2 derived Burn Severity") 

# plot the legend on the right side of the burn severity map
legend(x='topright', legend =rat$legend,
       fill = my_col, y='topright',bty='n',
       inset=c(-0.15,0), xpd=T) 

### export post-fire raster data
setwd("E:/exam/output/05_burnMap")
burnseverity <- writeRaster(dNBR_reclass,"burnSeverity.tif", overwrite=T)


#Estimate fire affected area size
## get frequencies of the burn sverity classes
freq(dNBR_reclass)
label  <- cbind(c("No Data",
                 "High enhanced regrowth",
                 "Low enhanced regrowth",
                 "Unburned",
                 "Low Severity",
                 "Moderate-low Severity",
                 "Moderate-high Severity",
                 "High Severity"))

## convert frequencies matrix to data frame
df <- NA
df <- cbind.data.frame(label,(freq(dNBR_reclass)[1:8,]))
## adjust the column name
colnames(df)[3] <- "pixelcount"
## calculate and add the percentage from the pixelcounts
df$pixelperc <- round((100*df$pixelcount)/sum(df$pixelcount),1)
## calculate the hectares from the pixelcounts
## sentinel pixel size is 20m X 20m --> the area is 400sqm = 0.04 hectares
df$area_ha <- round(((df$pixelcount*(xres(dNBR_reclass)^2))/10000),0)

library(writexl)
setwd("E:/exam/output/05_burnMap")
write_xlsx(df, "burn area statistic.xlsx")


#Create a barplot from the data frame
dnbr <- ggplot(data = df, aes(x=label, y=area_ha))+
        geom_bar(stat = "identity", width = 0.5, color = "black", fill=my_col)+
        geom_text(aes(label=paste0(c(area_ha),'ha','\n[',c(pixelperc),'%]')), vjust=-0.3, size=3.5)+
        scale_x_discrete(limits=c("No Data", "High enhanced regrowth", 
                                  "Low enhanced regrowth","Unburned","Low Severity",
                                  "Moderate-low Severity", "Moderate-high Severity",
                                  "High Severity"))+
        labs(title="dNBR", x="Burn Severity class", y="Area (ha)")+
        theme_bw()+
        theme(plot.title = element_text(hjust = 0.5))

setwd("E:/exam/output/05_burnmap/dNBR")
png("dNBR_bar.png", res=200,width=1200,height=800)
dnbr
dev.off()



#bar plot 2
library(readxl)
burnSeverity <- read_excel("output/07_burnSeverity/burnSeverity.xlsx", 
                             +     sheet = "R_Overall")

setwd("E:/exam/output/07_burnSeverity")
png("burn severity are plot.png", res = 200, width = 1000, height = 800)
ggplot(data = burnSeverity ,aes(x=`Burn severity class`, 
                             y= `Area (ha)`))+
  geom_bar(stat = "identity", aes(fill= Indices), width = 0.7,
           position = "dodge")+
  scale_fill_manual("Legend", values = c("No Data"="#FFFFFF",
                                         "High enhanced regrowth"="#556b2f",
                                         "Low enhanced regrowth"="#6e8b3d",
                                         "Unburned"="#32CD32",
                                         "Low Severity"="#EEEE00",
                                         "Moderate-low Severity"="#ee9a00",
                                         "Moderate-high Severity"="#FF0000",
                                         "High Severity"="#800080"))+
  geom_text(label=`Area (ha)`,color="black",size=4,
            vjust=-0.2,hjust = 0.5,
            position = position_dodge(width= 0.9))+
  labs(title="Distribution of the classified class area")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
  theme(legend.title = element_text(face = "bold"))
dev.off()




####################RBR#######################
# set ranges for the Burn Severity classes
rbr_ranges <- c(-Inf,-.5,0, # out of range - no data
                -.5,-.25,1, #enhanced regrowth, high
                -.25,-.1,2,     # enhanced regrowth, low
                -.1,0.1, 3,    # unburned area
                0.1, 0.27, 4,  # low severity
                0.27, 0.44, 5, # moderate-low severity
                0.44, 0.66, 6, # moderate-high severity
                0.66, 1.4, 7,  # high severity
                1.4, +Inf, 0)  # out of range - no data

# set classification matrix
class.matrix <- matrix(rbr_ranges, ncol = 3, byrow = T)

# use matrix to reclassify RBR_scaled
rbr_reclass <- reclassify(rbr, class.matrix, right=NA)

# view frequencies of reclassification values
freq(rbr_reclass)


# build the attribute table for the legend 
rbr_reclass <- ratify(rbr_reclass) 
rat <- levels(rbr_reclass)[[1]]

# create the text that will be on the legend
rat$legend  <- c("No Data",
                 "High enhanced regrowth",
                 "Low enhanced regrowth",
                 "Unburned",
                 "Low Severity",
                 "Moderate-low Severity",
                 "Moderate-high Severity",
                 "High Severity") 
levels(rbr_reclass) <- rat 


# colour setting for the severity map
#my_col=c("#FFFFFF","#556b2f","#6e8b3d","#32CD32", "#EEEE00","#ee9a00", "#FF0000", "#800080")
my_col=c("white", "darkolivegreen","darkolivegreen4","limegreen", "yellow2", "orange2", "red", "purple")

# plots the burn severity map with title
plot(rbr_reclass,
     legend=F,box=F,axes=F,
     col=my_col,
     cex.main=0.9,
     main="Sentinel-2 derived Burn Severity") 

# plot the legend on the right side of the burn severity map
legend(x='topright', legend =rat$legend,
       fill = my_col, y='topright',bty='n',
       inset=c(-0.15,0), xpd=T) 

### export post-fire raster data
setwd("E:/exam/output/07_burnMap/rbr")
writeRaster(rbr_reclass,"burnSeverity_rbr.tif", overwrite=T)


#Estimate fire affected area size
## get frequencies of the burn sverity classes
freq(rbr_reclass)
label  <- cbind(c("No Data",
                  "High enhanced regrowth",
                  "Low enhanced regrowth",
                  "Unburned",
                  "Low Severity",
                  "Moderate-low Severity",
                  "Moderate-high Severity",
                  "High Severity"))

## convert frequencies matrix to data frame
df <- NA
df <- cbind.data.frame(label,(freq(rbr_reclass)[1:8,]))
## adjust the column name
colnames(df)[3] <- "pixelcount"
## calculate and add the percentage from the pixelcounts
df$pixelperc <- round((100*df$pixelcount)/sum(df$pixelcount),1)
## calculate the hectares from the pixelcounts
## sentinel pixel size is 20m X 20m --> the area is 400sqm = 0.04 hectares
df$area_ha <- round(((df$pixelcount*(xres(rbr_reclass)^2))/10000),0)

library(writexl)
setwd("E:/exam/output/07_burnMap/rbr")
write_xlsx(df, "burn area statistic_rbr.xlsx")



####################RdNBR#######################
# set ranges for the Burn Severity classes
rdnbr_ranges <- c(-Inf,-.5,0, # out of range - no data
                -.5,-.25,1, #enhanced regrowth, high
                -.25,-.1,2,     # enhanced regrowth, low
                -.1,0.1, 3,    # unburned area
                0.1, 0.27, 4,  # low severity
                0.27, 0.44, 5, # moderate-low severity
                0.44, 0.66, 6, # moderate-high severity
                0.66, 1.4, 7,  # high severity
                1.4, +Inf, 0)  # out of range - no data

# set classification matrix
class.matrix <- matrix(rdnbr_ranges, ncol = 3, byrow = T)

# use matrix to reclassify RBR_scaled
rdnbr_reclass <- reclassify(rdnbr, class.matrix, right=NA)

# view frequencies of reclassification values
freq(rdnbr_reclass)


# build the attribute table for the legend 
rdnbr_reclass <- ratify(rdnbr_reclass) 
rat <- levels(rdnbr_reclass)[[1]]

# create the text that will be on the legend
rat$legend  <- c("No Data",
                 "High enhanced regrowth",
                 "Low enhanced regrowth",
                 "Unburned",
                 "Low Severity",
                 "Moderate-low Severity",
                 "Moderate-high Severity",
                 "High Severity") 
levels(rdnbr_reclass) <- rat 


# colour setting for the severity map
#my_col=c("#FFFFFF","#556b2f","#6e8b3d","#32CD32", "#EEEE00","#ee9a00", "#FF0000", "#800080")
my_col=c("white", "darkolivegreen","darkolivegreen4","limegreen", "yellow2", "orange2", "red", "purple")

# plots the burn severity map with title
plot(rdnbr_reclass,
     legend=F,box=F,axes=F,
     col=my_col,
     cex.main=0.9,
     main="Sentinel-2 derived Burn Severity") 

# plot the legend on the right side of the burn severity map
legend(x='topright', legend =rat$legend,
       fill = my_col, y='topright',bty='n',
       inset=c(-0.15,0), xpd=T) 

### export post-fire raster data
setwd("E:/exam/output/07_burnMap/rdnbr")
writeRaster(rdnbr_reclass,"burnSeverity_rdnbr.tif", overwrite=T)


#Estimate fire affected area size
## get frequencies of the burn sverity classes

label  <- cbind(c("No Data",
                  "High enhanced regrowth",
                  "Low enhanced regrowth",
                  "Unburned",
                  "Low Severity",
                  "Moderate-low Severity",
                  "Moderate-high Severity",
                  "High Severity"))

## convert frequencies matrix to data frame
df <- NA
df <- cbind.data.frame(label,(freq(rdnbr_reclass)[1:8,]))
## adjust the column name
colnames(df)[3] <- "pixelcount"
## calculate and add the percentage from the pixelcounts
df$pixelperc <- round((100*df$pixelcount)/sum(df$pixelcount),1)
## calculate the hectares from the pixelcounts
## sentinel pixel size is 20m X 20m --> the area is 400sqm = 0.04 hectares
df$area_ha <- round(((df$pixelcount*(xres(rdnbr_reclass)^2))/10000),0)

library(writexl)
setwd("E:/exam/output/07_burnMap/rdnbr")
write_xlsx(df, "burn area statistic_rdnbr.xlsx")




###################burn points statistic###################
library(readxl)
burnPoint <- read_excel("burnPoint.xlsx",sheet = "R_burnPoint")

setwd("E:/exam/output/08_burnMap")
png("active fire percentage.png", res = 140, width = 800, height = 600)
ggplot(data = burnPoint ,aes(x=`Burnt Severity Indices`,
                             y= burnPoint$`Percentage of Active Fire Points (%)`))+
  geom_bar(stat = "identity", aes(fill= Type), width = 0.7,
           position = "dodge")+
  scale_fill_manual("Legend", values = c("Unburnt"="#32CD32","Burnt"="#FF0000"))+
  geom_text(label=burnPoint$`Percentage of Active Fire Points (%)`,color="black",size=4,
            vjust=-0.2,hjust = 0.5,
            position = position_dodge(width= 0.9))+
  labs(title="Burnt area percentage versus Active Fire Points")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))
dev.off()

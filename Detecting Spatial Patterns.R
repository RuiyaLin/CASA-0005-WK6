# Chapter 6 Detecting Spatial Patterns (Blue Plaques)
## Research Questions
### For any given London Borough, are the Blue Plaques within that borough distributed randomly or do they exhibit some kind of dispersed or clustered pattern?”

## 6.1. Import libraries 
install.packages("spatstat")
library(spatstat)
library(here)
library(sp)
library(tmap)
library(sf)
library(tmaptools)

## 6.2. Setting up your data 
### Read Spatial data (RF1)
LondonBoroughs <- st_read(here::here("Data", "Spatial data","London_Borough_Excluding_MHW.shp"))
#### Pull out London: str_detect() -->RF2
library(stringr)
BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)
#### Quick map 
qtm(BoroughMap)
#### Check RF2
summary(BoroughMap)

### Read data about Blue Plaques (DF3)
BluePlaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson") %>%
  st_transform(.,27700)
#### Check the data
summary(BluePlaques)
#### Plot the blue plaques in the city 
tmap_mode("plot")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")

## 6.3. Data Cleaning 
#### 将边界外的点移回来并删除重复的数据
### Remove duplicates
library(tidyverse)
library(sf)
BluePlaques <- distinct(BluePlaques)

### Spatial subsetting 
BluePlaquesSub <- BluePlaques[BoroughMap,]
#### check to see that they've been removed (plot again)
tmap_mode("plot")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")
##### 當我們像這樣的空間子集數據時，我們可以指定不同的拓撲關係。預設值為 intersects，但我們也可以使用 BluePlaquesSub <- BluePlaques[BoroughMap, , op = st_within] ，並將運算符或操作設置為 st_within ，以識別完全位於行政區輪廓內的點，或者使用各種其他選項，例如 st_overlaps 、 st_touches st_contains st_disjoint 。您能想到的函數的任何可能的拓撲關係都會為它存在......視覺上這看起來像......其中，每個刻度表示應用於多邊形的關係。請注意，在幾種情況下，多個拓撲關係是可行的。
##### Select points or polygons in a polygon = Selecting data by location = spatial sub-setting
##### 選擇多邊形中的點或多邊形 = 按位置選擇資料 = 空間子集
#### Determine where datasets overlap (or touch, or don’t overlap) and extract those parts = spatial clipping
##### 確定數據集重疊（或接觸或不重疊）的位置並提取這些部分 = 空間裁剪
##### Join two spatial datasets together = spatial joining, which can use spatial subsetting functions as the default is st_intersects(). This function joins spatial data.
##### 將兩個空間資料集聯接在一起 = 空間聯接 ，可以使用空間子集函數，因為預設值為 st_intersects() 。此函數聯接空間數據。

### Spatial clipping or joining (section 6.5.4. skipped)

### Study area (选择一个行政区进行分析)
#### extract the borough
#### select by attribute
Harrow <- BoroughMap %>%
  filter(., NAME=="Harrow")
#### Check to see that the correct borough has been pulled out
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5)
#### clip the data to our single borough
BluePlaquesSub <- BluePlaques[Harrow,]
#### check that it's worked
tmap_mode("plot")
#### Plot to see it
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")
#### Set a window as the borough boundary 
window <- as.owin(Harrow)
plot(window)
#### create a spatial point (sp) object
BluePlaquesSub<- BluePlaquesSub %>%
  as(., 'Spatial')
#### create a Point Pattern (ppp) object
BluePlaquesSub.ppp <- ppp(x=BluePlaquesSub@coords[,1],
                          y=BluePlaquesSub@coords[,2],
                          window=window)

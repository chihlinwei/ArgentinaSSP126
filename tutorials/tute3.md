Extract seafloor climate change data by coordinate
================
Chih-Lin Wei
2024-07-26

``` r
library(ArgentinaSSP126)
library(ggplot2)
library(dplyr)
library(tidyr)
```

# Species occurrence data

We first download Argentine hake and Demospongiae occurrence data from
[OBIS](https://obis.org/area/7) using the
[occurrence](https://www.rdocumentation.org/packages/robis/versions/2.11.3/topics/occurrence)
function from the
[robis](https://www.rdocumentation.org/packages/robis/versions/2.11.3)
package. You may choose any other species from
[OBIS](https://obis.org/area/7) and no. 7 is the area ID for Argentina.

``` r
library(robis)

sponge <- occurrence(scientificname = "Demospongiae", areaid = 7)
hake <- occurrence(scientificname = "Merluccius hubbsi", areaid = 7)
```

Here, let’s look at the first ten records of the Argentine hake and
Demospongiae occurrence data.

``` r
library(knitr)

head(hake[, c(6:7, 21)], 10) %>% kable
```

| decimalLatitude | decimalLongitude | scientificName    |
|----------------:|-----------------:|:------------------|
|       -45.63333 |        -66.11667 | Merluccius hubbsi |
|       -45.65000 |        -66.35000 | Merluccius hubbsi |
|       -45.30000 |        -65.68333 | Merluccius hubbsi |
|       -45.09917 |        -65.74970 | Merluccius hubbsi |
|       -45.30000 |        -65.53333 | Merluccius hubbsi |
|       -45.76667 |        -64.95000 | Merluccius hubbsi |
|       -46.61667 |        -66.88333 | Merluccius hubbsi |
|       -45.08333 |        -65.80000 | Merluccius hubbsi |
|       -46.01667 |        -65.53333 | Merluccius hubbsi |
|       -45.28333 |        -65.25000 | Merluccius hubbsi |

``` r
head(sponge[, c(2:3, 6)], 10) %>% kable
```

| decimalLatitude | decimalLongitude | scientificName                          |
|----------------:|-----------------:|:----------------------------------------|
|       -38.01000 |        -57.08000 | Lissodendoryx (Ectyodoryx) nobilis      |
|       -44.21000 |        -65.11000 | Trachytedania spinata                   |
|       -42.64083 |        -64.24525 | Demospongiae                            |
|       -42.62378 |        -64.26614 | Clathria                                |
|       -52.50000 |        -67.23300 | Tedania (Tedania)                       |
|       -42.62406 |        -64.26022 | Demospongiae                            |
|       -37.28330 |        -53.86670 | Latrunculia (Aciculatrunculia) apicalis |
|       -42.62406 |        -64.26022 | Clathria                                |
|       -53.83330 |        -67.66670 | Haliclona (Reniera) topsenti            |
|       -37.28330 |        -53.86670 | Semisuberites cribrosa                  |

There are a total of 2617 occurrence records within the [Argentina
EEZ](https://marineregions.org/gazetteer.php?p=details&id=8466). We can
then overlay them on top of the
[etopo2022](https://www.ncei.noaa.gov/products/etopo-global-relief-model)
raster to see the data distribution.

``` r
bathy <- etopo2022 %>% mask(eez) %>% as.data.frame(xy = TRUE) %>% na.omit
occ <- rbind(cbind(hake[, c(6:7, 21)], Taxa="Argentine hake"), cbind(sponge[, c(2:3, 6)], Taxa="Demosponge"))
ggplot(bathy) +
  geom_raster(aes(x=x, y=y, fill=-layer))+
  geom_polygon(data=arg, aes(x=X, y=Y, group=PID), fill="bisque2", colour="transparent")+
  geom_polygon(data=fortify(eez), aes(x=long, y=lat, group=id), fill="transparent", colour="red")+
  geom_contour(data=bathy, aes(x=x, y=y, z=layer), breaks=-200, linetype=2, colour="gray50")+
  geom_contour(data=bathy, aes(x=x, y=y, z=layer), breaks=-4000, linetype=1, colour="gray50")+
  geom_point(data=occ, aes(x=decimalLongitude, y=decimalLatitude), size=0.8)+
  facet_wrap(~Taxa)+
  scale_fill_gradientn(colours=terrain.colors(7))+
  scale_x_continuous(expand = expansion(mult = 0))+
  scale_y_continuous(expand = expansion(mult = 0))+
  labs(x=NULL, y=NULL, fill="Depth\n(m)")+
  coord_fixed(1.52)+
  theme_bw() %+replace% theme(legend.position = "right", legend.key.width =  unit(0.5, 'cm'))
```

![](tute3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

# Environmental predictors

Besides extracting data for each coordinate, we can also apply these
data for simple habitat suitability modeling using
[Maxent](https://www.rdocumentation.org/packages/dismo/versions/1.3-14/topics/maxent)
from the
[dismo](https://www.rdocumentation.org/packages/dismo/versions/1.3-14)
package. Here, we’d like to model the Demospongiae occurrence using the
historical projection from 1950 to 2000 and then use the
[Maxent](https://www.rdocumentation.org/packages/dismo/versions/1.3-14/topics/maxent)
model to predict the habitat suitability of Demospongiae in 2041 to 2060
and 2081 and 2100. We need three sets of climate change data for 1950 to
2000, 2041 to 2060, and 2081 to 2100. Because water depth is usually the
most critical factor controlling the Demospongiae distribution, we also
add the
[etopo2022](https://www.ncei.noaa.gov/products/etopo-global-relief-model)
global relief model into the predictors. The predictor names are
modified (e.g., removing the years) to be consistent across three
periods for the convenience of modeling and predictions.

``` r
hist <- addLayer(etopo2022, cmip6_1950_2000_av) %>% mask(eez)
names(hist)[-1] <- gsub("_av_1950_to_2000", "", names(cmip6_1950_2000_av))

proj1 <- addLayer(etopo2022, cmip6_2041_2060_av) %>% mask(eez)
names(proj1)[-1] <- gsub("_av_2041_2060", "", names(cmip6_2041_2060_av))

proj2 <- addLayer(etopo2022, cmip6_2081_2100_av) %>% mask(eez)
names(proj2)[-1] <- gsub("_av_2081_2100", "", names(cmip6_2081_2100_av))
```

# Species distribution modeling

The commands in
[Maxent](https://www.rdocumentation.org/packages/dismo/versions/1.3-14/topics/maxent)
are pretty simple. We only need a raster brick of predictors and
coordinates of species occurrence to create a model. The historical and
future habitat suitability of Demospongiae can then be
[predicted](https://www.rdocumentation.org/packages/dismo/versions/1.3-14/topics/predict)
using seafloor climate change data from the model. As a demonstration,
we will skip all other details, such as the model validation,
performance or variable importance, etc. More details on applying Maxent
and other species distribution modeling methods can be found in [Hijmans
and Elith (2023)](https://rspatial.org/raster/sdm/raster_SDM.pdf).

``` r
library(dismo)

coordinates(occ) <- c("decimalLongitude", "decimalLatitude")

# Maxnet modeling for Argentine hake
me1 <- maxent(hist, subset(occ, Taxa=="Argentine hake"))
r1 <- addLayer(predict(me1, hist), predict(me1, proj1), predict(me1, proj2))
names(r1) <- c("Year_1950_to_2000", "Year_2041_to_2060", "Year_2081_to_2100")

# Maxnet modeling for demosponge
me2 <- maxent(hist, subset(occ, Taxa=="Demosponge"))
r2 <- addLayer(predict(me2, hist), predict(me2, proj1), predict(me2, proj2))
names(r2) <- c("Year_1950_to_2000", "Year_2041_to_2060", "Year_2081_to_2100")
```

We can use the
[extract](https://www.rdocumentation.org/packages/raster/versions/3.6-23/topics/extract)
function to take a quick look at the historical predictors used in the
model. This predictor table is corresponded to the previous Argentine
hake and Demospongiae occurrence table.

``` r
raster::extract(hist, subset(occ, Taxa=="Argentine hake")) %>% head(10) %>% kable(digits=3)
```

|   layer |    epc |    o2 |    ph | thetao | arag | calc |   co3 | co3satarag | co3satcalc | aragsat | calcsat |
|--------:|-------:|------:|------:|-------:|-----:|-----:|------:|-----------:|-----------:|--------:|--------:|
| -94.252 | 78.079 | 0.269 | 8.068 |  7.812 |    0 |    0 | 0.133 |      0.069 |      0.043 |   2.280 |   2.523 |
| -93.348 | 80.378 | 0.269 | 8.068 |  7.784 |    0 |    0 | 0.132 |      0.069 |      0.043 |   2.270 |   2.520 |
| -96.112 | 81.301 | 0.271 | 8.070 |  7.898 |    0 |    0 | 0.133 |      0.069 |      0.043 |   2.271 |   2.532 |
| -60.670 | 77.979 | 0.271 | 8.071 |  7.987 |    0 |    0 | 0.134 |      0.069 |      0.043 |   2.278 |   2.546 |
| -96.112 | 81.301 | 0.271 | 8.070 |  7.898 |    0 |    0 | 0.133 |      0.069 |      0.043 |   2.271 |   2.532 |
| -89.512 | 82.104 | 0.274 | 8.066 |  7.295 |    0 |    0 | 0.129 |      0.069 |      0.043 |   2.198 |   2.457 |
| -73.043 | 89.896 | 0.271 | 8.070 |  7.530 |    0 |    0 | 0.132 |      0.069 |      0.043 |   2.259 |   2.521 |
| -42.596 | 78.423 | 0.271 | 8.071 |  8.048 |    0 |    0 | 0.134 |      0.069 |      0.043 |   2.274 |   2.542 |
| -83.863 | 82.649 | 0.272 | 8.067 |  7.532 |    0 |    0 | 0.131 |      0.069 |      0.043 |   2.241 |   2.481 |
| -89.355 | 82.644 | 0.272 | 8.070 |  7.813 |    0 |    0 | 0.133 |      0.069 |      0.043 |   2.251 |   2.528 |

``` r
raster::extract(hist, subset(occ, Taxa=="Demosponge")) %>% head(10) %>% kable(digits=3)
```

|     layer |    epc |    o2 |    ph | thetao | arag | calc |   co3 | co3satarag | co3satcalc | aragsat | calcsat |
|----------:|-------:|------:|------:|-------:|-----:|-----:|------:|-----------:|-----------:|--------:|--------:|
|   -41.299 | 40.493 | 0.265 | 8.094 |  9.675 |    0 |    0 | 0.151 |      0.068 |      0.043 |   2.129 |   2.850 |
|   -52.222 | 70.588 | 0.272 | 8.079 |  8.644 |    0 |    0 | 0.141 |      0.068 |      0.043 |   2.359 |   2.648 |
|        NA |     NA |    NA |    NA |     NA |   NA |   NA |    NA |         NA |         NA |      NA |      NA |
|   -72.107 | 62.435 | 0.266 | 8.096 | 10.680 |    0 |    0 | 0.157 |      0.068 |      0.043 |   2.563 |   2.953 |
|   -92.298 | 97.392 | 0.297 | 8.082 |  5.849 |    0 |    0 | 0.124 |      0.069 |      0.043 |   1.911 |   2.334 |
|   -72.107 | 62.435 | 0.266 | 8.096 | 10.680 |    0 |    0 | 0.157 |      0.068 |      0.043 |   2.563 |   2.953 |
| -1136.211 | 25.874 | 0.211 | 7.952 |  5.026 |    0 |    0 | 0.117 |      0.104 |      0.064 |   1.223 |   1.702 |
|   -72.107 | 62.435 | 0.266 | 8.096 | 10.680 |    0 |    0 | 0.157 |      0.068 |      0.043 |   2.563 |   2.953 |
|        NA |     NA |    NA |    NA |     NA |   NA |   NA |    NA |         NA |         NA |      NA |      NA |
| -1136.211 | 25.874 | 0.211 | 7.952 |  5.026 |    0 |    0 | 0.117 |      0.104 |      0.064 |   1.223 |   1.702 |

# Habitat suitability projections

Finally, we map the projected habitat suitability of Argentine hake for
the years 1950 to 2000, 2041 to 2060, and 2081 to 2100.

``` r
dat <- r1 %>% as.data.frame(xy = TRUE) %>% na.omit %>% gather(-x, -y, key = "var", value = "value")
ggplot(dat) +
  geom_raster(aes(x=x, y=y, fill=value))+
  geom_polygon(data=arg, aes(x=X, y=Y, group=PID), fill="bisque2", colour="transparent")+
  geom_polygon(data=fortify(eez), aes(x=long, y=lat, group=id), fill="transparent", colour="red")+
  geom_contour(data=bathy, aes(x=x, y=y, z=layer), breaks=-200, linetype=2, colour="gray50")+
  geom_contour(data=bathy, aes(x=x, y=y, z=layer), breaks=-4000, linetype=1, colour="gray50")+
  geom_point(data=hake, aes(x=decimalLongitude, y=decimalLatitude), size=0.2)+
  facet_wrap(~var, nrow=1)+
  scale_fill_gradientn(colours=jet.colors(10))+
  scale_x_continuous(expand = expansion(mult = 0))+
  scale_y_continuous(expand = expansion(mult = 0))+
  labs(x=NULL, y=NULL, fill="Habitat\nSuitability")+
  coord_fixed(1.52)+
  theme_bw() %+replace% theme(legend.position = "right", legend.key.width =  unit(0.5, 'cm'))
```

![](tute3_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Habitat suitability of Demosponge for the years 1950 to 2000, 2041 to
2060, and 2081 to 2100

``` r
dat <- r2 %>% as.data.frame(xy = TRUE) %>% na.omit %>% gather(-x, -y, key = "var", value = "value")
ggplot(dat) +
  geom_raster(aes(x=x, y=y, fill=value))+
  geom_polygon(data=arg, aes(x=X, y=Y, group=PID), fill="bisque2", colour="transparent")+
  geom_polygon(data=fortify(eez), aes(x=long, y=lat, group=id), fill="transparent", colour="red")+
  geom_contour(data=bathy, aes(x=x, y=y, z=layer), breaks=-200, linetype=2, colour="gray50")+
  geom_contour(data=bathy, aes(x=x, y=y, z=layer), breaks=-4000, linetype=1, colour="gray50")+
  geom_point(data=sponge, aes(x=decimalLongitude, y=decimalLatitude), size=0.2)+
  facet_wrap(~var, nrow=1)+
  scale_fill_gradientn(colours=jet.colors(10))+
  scale_x_continuous(expand = expansion(mult = 0))+
  scale_y_continuous(expand = expansion(mult = 0))+
  labs(x=NULL, y=NULL, fill="Habitat\nSuitability")+
  coord_fixed(1.52)+
  theme_bw() %+replace% theme(legend.position = "right", legend.key.width =  unit(0.5, 'cm'))
```

![](tute3_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

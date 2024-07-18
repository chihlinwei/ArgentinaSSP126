## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  message=FALSE,
  warning=FALSE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ArgentinaSSP126)
library(ggplot2)
library(dplyr)
library(patchwork)
library(tidyr)
library(RColorBrewer)

## ----fig.width=4, fig.height=7------------------------------------------------
bathy <- etopo2022  %>% mask(eez) %>% as.data.frame(xy = TRUE) %>% na.omit
ggplot(bathy) +
      geom_raster(aes(x=x, y=y, fill=-layer))+
      geom_polygon(data=arg, aes(x=X, y=Y, group=PID), fill="bisque2", colour="transparent")+
      geom_polygon(data=fortify(eez), aes(x=long, y=lat, group=id), fill="transparent", colour="red")+
      geom_contour(data=bathy, aes(x=x, y=y, z=layer), breaks=-200, linetype=2, colour="gray50")+
      geom_contour(data=bathy, aes(x=x, y=y, z=layer), breaks=-4000, linetype=1, colour="gray50")+
      scale_fill_gradientn(colours=terrain.colors(7))+
      scale_x_continuous(expand = expansion(mult = 0))+
      scale_y_continuous(expand = expansion(mult = 0))+
      labs(x=NULL, y=NULL, fill=NULL)+
      coord_fixed(1.52)+
      theme_bw() %+replace% theme(legend.position = "top", legend.key.width =  unit(1, 'cm'))

## -----------------------------------------------------------------------------
plot_fun <- function(r, colours=NULL, q_limits=c(0.001, 0.999)){
  
  # Convert raster to data frame and then to list
  cmip6_list <- as.data.frame(r, xy = TRUE) %>% na.omit %>%
  gather(-x, -y, key = "var", value = "value") %>%
  group_split(var)
  
  # Depth
  bathy <- mask(etopo2022, eez)%>% as.data.frame(xy = TRUE) %>% na.omit
  
  # ggolot list
  gg_list = lapply(cmip6_list, function(dat) {
    
    # Color key limits and colours
    lim1 <- quantile(dat$value, q_limits, na.rm=TRUE)
    lim2 <- max(abs(quantile(dat$value, q_limits, na.rm=TRUE)))
    if(min(lim1) >= 0) {
      lims <- lim1; cols <- jet.colors(7)
    } else {
      lims <- c(-lim2, lim2); cols <- jet.colors3(7)}
    if(is.null(colours)) cols <- cols else cols <- colours
      
    # Plot raster layer
    ggplot(dat) +
      geom_raster(aes(x=x, y=y, fill=value))+
      geom_polygon(data=arg, aes(x=X, y=Y, group=PID), fill="bisque2", colour="transparent")+
      geom_polygon(data=fortify(eez), aes(x=long, y=lat, group=id), fill="transparent", colour="red")+
      geom_contour(data=bathy, aes(x=x, y=y, z=layer), breaks=-200, linetype=2, colour="gray50")+
      geom_contour(data=bathy, aes(x=x, y=y, z=layer), breaks=-4000, linetype=1, colour="gray50")+
      scale_fill_gradientn(colours=cols, limits=lims)+
      scale_x_continuous(expand = expansion(mult = 0))+
      scale_y_continuous(expand = expansion(mult = 0))+
      labs(x=NULL, y=NULL, fill=NULL)+
      coord_fixed(1.52)+
      facet_wrap(~ var) +
      theme_bw() %+replace% theme(legend.position = "top", legend.key.width =  unit(1, 'cm'))
      })
  
  # Wrap ggplot list
  wrap_plots(gg_list, nrow=1)
}

## ----fig.width=11, fig.height=7-----------------------------------------------
plot_fun(r=cmip6_2041_2060_exsd %>% subset(1:4) %>% mask(eez))

## ----fig.width=11, fig.height=5-----------------------------------------------
out <- addLayer(etopo2022, cmip6_2041_2060_exsd %>% subset(1:4)) %>% mask(eez) %>% as.data.frame(xy = TRUE) %>% na.omit
out$Region <- cut(-out$layer, c(0, 200, 5000), labels=c("Shelf", "Slope"))

ggplot(data=out %>% gather(-x, -y, -layer, -Region, key = "var", value = "value"))+
  geom_violin(aes(x=Region, y=value))+
  facet_wrap(~var, scales="free", nrow=1)+
  labs(y="Climate Change Hazards")

## ----fig.width=11, fig.height=7-----------------------------------------------
plot_fun(cmip6_extoe_early %>% subset(1:4) %>% mask(eez), colours = brewer.pal(10, 'RdYlBu'), q_limits = c(0, 1))

## ----fig.width=11, fig.height=5-----------------------------------------------
out <- addLayer(etopo2022, cmip6_extoe_early %>% subset(1:4)) %>% mask(eez) %>% as.data.frame(xy = TRUE) %>% na.omit
out$Region <- cut(-out$layer, c(0, 200, 5000), labels=c("Shelf", "Slope"))

ggplot(data=out %>% gather(-x, -y, -layer, -Region, key = "var", value = "value"))+
  geom_violin(aes(x=Region, y=value))+
  facet_wrap(~var, scales="free", nrow=1)+
  labs(y="Time of Emergence of Climate Change")

## -----------------------------------------------------------------------------
all <- overlay(subset(cmip6_extoe_early, 1:4), fun=max) %>% mask(eez) %>% as.data.frame(xy = TRUE) %>% na.omit
bathy <- etopo2022 %>% mask(eez) %>% as.data.frame(xy = TRUE) %>% na.omit
p1 <- ggplot(all) +
      geom_raster(aes(x=x, y=y, fill=layer))+
      geom_polygon(data=arg, aes(x=X, y=Y, group=PID), fill="bisque2", colour="transparent")+
      geom_polygon(data=fortify(eez), aes(x=long, y=lat, group=id), fill="transparent", colour="red")+
      geom_contour(data=bathy, aes(x=x, y=y, z=layer), breaks=-200, linetype=2, colour="gray50")+
      geom_contour(data=bathy, aes(x=x, y=y, z=layer), breaks=-4000, linetype=1, colour="gray50")+
      scale_fill_gradientn(colours=brewer.pal(10, 'RdYlBu'))+
      scale_x_continuous(expand = expansion(mult = 0))+
      scale_y_continuous(expand = expansion(mult = 0))+
      labs(x=NULL, y=NULL, fill=NULL)+
      coord_fixed(1.52)+
      theme_bw() %+replace% theme(legend.position = "right", legend.key.height =  unit(1.8, 'cm'))

# Violin plots
out <- addLayer(etopo2022, overlay(subset(cmip6_extoe_early, 1:4), fun=max)) %>% mask(eez) %>% as.data.frame(xy = TRUE) %>% na.omit
out$Region <- cut(-out$layer.1, c(0, 200, 5000), labels=c("Shelf", "Slope"))

p2 <- ggplot(data=out)+
  geom_violin(aes(x=Region, y=layer.2))+
  labs(y="Time of Emergence of Climate Change")

## ----fig.width=7, fig.height=7------------------------------------------------
p2+p1+plot_layout(widths = c(1, 2))

## -----------------------------------------------------------------------------
cum_imp <- function(r){
  # Negative cumulative impact (exposure to climate change hazards for epc<0, o2<0, ph<0, and thetao>0)
  n2050 <- addLayer(calc(subset(r, 1:3), fun=function(x){x[x>0]<-NA; return(-x)}),
                    calc(subset(r, 4), fun=function(x){x[x<0]<-NA; return(x)})
                    ) %>%　overlay(fun=function(x)sum(x, na.rm=T))

  # Positive cumulative impact (exposure to climate change hazards for epc>0, o2>0, ph>0, and thetao<0)
  p2050 <- addLayer(calc(subset(r, 1:3), fun=function(x){x[x<0]<-NA; return(x)}),
                    calc(subset(r, 4), fun=function(x){x[x>0]<-NA; return(-x)})
                    ) %>%　overlay(fun=function(x)sum(x, na.rm=T))
  
  out <- addLayer(n2050, p2050)
  names(out) <- c("Negative", "Positive")
  out <- mask(out, etopo2022)
  return(out)
}

## ----fig.width=6, fig.height=7------------------------------------------------
plot_fun(r=cum_imp(cmip6_2041_2060_exsd) %>% mask(eez))

## ----fig.width=6, fig.height=3.5----------------------------------------------
out <- addLayer(etopo2022, cum_imp(cmip6_2041_2060_exsd)) %>% mask(eez) %>% as.data.frame(xy = TRUE) %>% na.omit
out$Region <- cut(-out$layer, c(0, 200, 5000), labels=c("Shelf", "Slope"))

ggplot(data=out %>% gather(-x, -y, -layer, -Region, key = "var", value = "value"))+
  geom_violin(aes(x=Region, y=value))+
  facet_wrap(~var, scales="free")+
  labs(y="Cumulative Climate Change Impact")

## ----fig.width=11, fig.height=7-----------------------------------------------
plot_fun(cmip6_2041_2060_voccMeg %>% subset(1:4) %>% mask(eez), q_limits=c(0.01, 0.99))

## ----fig.width=11, fig.height=5-----------------------------------------------
out <- addLayer(etopo2022, cmip6_2041_2060_voccMeg %>% subset(1:4)) %>% mask(eez) %>% as.data.frame(xy = TRUE) %>% na.omit
out$Region <- cut(-out$layer, c(0, 200, 5000), labels=c("Shelf", "Slope"))

ggplot(data=out %>% gather(-x, -y, -layer, -Region, key = "var", value = "value"))+
  geom_violin(aes(x=Region, y=value))+
  facet_wrap(~var, scales="free", nrow=1)+
  labs(y="Climate Velocity Magnitudes")

## ----fig.width=6, fig.height=7------------------------------------------------
plot_fun(r=cum_imp(cmip6_2041_2060_voccMeg) %>% mask(eez), q_limits = c(0, 0.99))

## ----fig.width=6, fig.height=3.5----------------------------------------------
out <- addLayer(etopo2022, cum_imp(cmip6_2041_2060_voccMeg)) %>% mask(eez) %>% as.data.frame(xy = TRUE) %>% na.omit
out$Region <- cut(-out$layer, c(0, 200, 5000), labels=c("Shelf", "Slope"))

ggplot(data=out %>% gather(-x, -y, -layer, -Region, key = "var", value = "value"))+
  geom_violin(aes(x=Region, y=value))+
  facet_wrap(~var, scales="free")+
  labs(y="Cumulative Climate Change Impact")


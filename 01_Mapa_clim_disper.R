
library(sf)
library(ggplot2)
library(sp)
Mundo = st_read("SHP/Mundo.geojson")  %>% st_as_sf()
Mundo <- st_transform(Mundo ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Mundo_xy <- cbind(Mundo, st_coordinates(st_centroid(Mundo$geometry)))

Area= st_read("SHP/Area.shp")  %>% st_as_sf()

library(elevatr)
library(raster)
elev = get_elev_raster(Area , z=4)
plot(elev)
Poligo_alt    <- crop(elev, Mundo)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Mundo)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

col<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")

Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")


Prec        <- getData("worldclim", var = "tmean", res=0.5, lon= 0, lat= 10)


Bioclim      <-getData(name = 'worldclim', var = 'bio', res = 10)     # https://worldclim.org/data/v1.4/formats.html
plot(Bioclim[[12]])                                                   # BIO12 = Precipitación anual
plot(Bioclim[[1]])                                                    # BIO1 = temperatura media anual


Temperatura    <- crop(Bioclim[[1]], Area)
Temperatura    <- Temperatura<- mask(Temperatura,Area)
Temperatura_anual <- Temperatura %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame()
Temperatura_anual$bio1= Temperatura_anual$bio1/10

colpre<- c('#ff4223','#f19e21','#ffee21','#00ffff', '#10aebe', '#165dff','#9331dc')
coltem <- c('#d8e2dc', '#8ecae6', '#023e8a', '#03045e', '#184e77', '#40916c', '#80b918',
            '#55a630','#aacc00','#d4d700','#eeef20','#ffff3f','#ff9e00','#ff9100','#ff6d00','#e36414'
            ,'#9a031e')


Precipitación    <- crop(Bioclim[[12]], Area)
Precipitación    <- Precipitación<- mask(Precipitación,Area)
Precipitación_anual <- Precipitación %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame()

colpre<- c('#ff4223','#f19e21','#ffee21','#00ffff', '#10aebe', '#165dff','#9331dc')

Precipitación_seco   <- crop(Bioclim[[14]], Area)
Precipitación_seco     <- Precipitación_seco <- mask(Precipitación_seco ,Area)
Precipitación_seco_MES <- Precipitación_seco  %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame()

colpre<- c('#ff4223','#f19e21','#ffee21','#00ffff', '#10aebe', '#165dff','#9331dc')

Precipitación_LLuvia  <- crop(Bioclim[[13]], Area)
Precipitación_LLuvia     <- Precipitación_LLuvia <- mask(Precipitación_LLuvia ,Area)
Precipitación_LLuvia_MES <- Precipitación_LLuvia  %>%
  as("SpatialPixelsDataFrame") %>%
  as.data.frame()

colpre<- c('#ff4223','#f19e21','#ffee21','#00ffff', '#10aebe', '#165dff','#9331dc')

library(rgbif)
Loxodonta<- occ_search(scientificName="Loxodonta africana")
Loxodonta_ele  <- subset(Loxodonta$data , 
                                scientificName == "Loxodonta africana (Blumenbach, 1797)")

Adansonia<- occ_search(scientificName="Adansonia digitata")
Adansonia_digitata  <- subset(Adansonia$data , 
                         scientificName == "Adansonia digitata L.")


library(ggnewscale) 
A=ggplot() + 
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = col, 
                       breaks = c(0,800,2500,6000),
                       na.value = 'white',
                       name='Elevacion (msnm)',
                       guide = guide_legend( direction = "horizontal",
                                             keyheight = unit(0.3, units = "cm"), 
                                             keywidth=unit(0.001, units = "cm"), 
                                             label.position = "bottom", title.position = 'top', nrow=1)) +
  geom_sf(data = Mundo  , fill=NA, color="black", size=0.05)+
  geom_sf_text(data = Mundo_xy , aes(label = PAÍS ), 
               family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
               size = 2, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  coord_sf(xlim = c(-20 ,60), ylim = c(-5  ,20),expand = FALSE)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        
        legend.position = c(0.15, 0.15),
        legend.title=element_text(color="black", size=7, family="serif"),
        legend.text=element_text(size=7, family="serif", angle=25),
        legend.background = element_blank(),
        
        panel.background = element_rect(fill = "#a2d2ff"),
        axis.text.y  = element_text(color="black", size=7,angle=90),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = c(-5,0,5,10,15,20), color = "gray50",linetype = "dashed", size = 0.05)+
  geom_vline(xintercept = c(-20,10,0,10,20,30,40,50), color = "gray50",linetype = "dashed", size = 0.1)+
  labs(color = '',  x = '', y = '')+
  annotate(geom = "text", x = -19, y = 18, hjust = 0, vjust = 1, 
           label = "A",
           size = 5, family="serif", color = "black")

B =ggplot()+
  geom_raster(data = Precipitación_anual, aes(x = x, y = y, fill = bio12))+
  scale_fill_gradientn(colours = colpre, 
                       breaks = c(500, 1000, 2000, 3000, 4000, 5000),
                       na.value = 'white',
                       name='Precipitación \n(mm)',
                       guide = guide_legend( direction = "horizontal",
                                             keyheight = unit(0.3, units = "cm"), 
                                             keywidth=unit(0.001, units = "cm"), 
                                             label.position = "bottom", title.position = 'top', nrow=1))+
  geom_sf(data = Mundo  , fill=NA, color="black", size=0.05)+
  geom_sf_text(data = Mundo_xy , aes(label = PAÍS ), 
               family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
               size = 2, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  coord_sf(xlim = c(-20 ,60), ylim = c(-5  ,20),expand = FALSE)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        
        legend.position = c(0.22, 0.15),
        legend.title=element_text(color="black", size=7, family="serif"),
        legend.text=element_text(size=7, family="serif", angle=25),
        legend.background = element_blank(),
        
        panel.background = element_rect(fill = "#a2d2ff"),
        axis.text.y  = element_text(color="black", size=7,angle=90),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = c(-5,0,5,10,15,20), color = "gray50",linetype = "dashed", size = 0.05)+
  geom_vline(xintercept = c(-20,10,0,10,20,30,40,50), color = "gray50",linetype = "dashed", size = 0.1)+
  labs(color = '',  x = '', y = '')+
  annotate(geom = "text", x = -19, y = 18, hjust = 0, vjust = 1, 
           label = "B",
           size = 5, family="serif", color = "black")

C =ggplot()+
  geom_raster(data = Temperatura_anual, aes(x = x, y = y, fill = bio1))+
  scale_fill_gradientn(colours = coltem, 
                       breaks = c(5,10,20,25,30,35),
                       na.value = 'white',
                       name="Temperatura \nPromedio Anual ºC",
                       guide = guide_legend( direction = "horizontal",
                                             keyheight = unit(0.3, units = "cm"), 
                                             keywidth=unit(0.001, units = "cm"), 
                                             label.position = "bottom", title.position = 'top', nrow=1))+
  geom_sf(data = Mundo  , fill=NA, color="black", size=0.05)+
  geom_sf_text(data = Mundo_xy , aes(label = PAÍS ), 
               family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
               size = 2, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  coord_sf(xlim = c(-20 ,60), ylim = c(-5  ,20),expand = FALSE)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        
        legend.position = c(0.15, 0.15),
        legend.title=element_text(color="black", size=7, family="serif"),
        legend.text=element_text(size=7, family="serif", angle=25),
        legend.background = element_blank(),
        
        panel.background = element_rect(fill = "#a2d2ff"),
        axis.text.y  = element_text(color="black", size=7,angle=90),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = c(-5,0,5,10,15,20), color = "gray50",linetype = "dashed", size = 0.05)+
  geom_vline(xintercept = c(-20,10,0,10,20,30,40,50), color = "gray50",linetype = "dashed", size = 0.1)+
  labs(color = '',  x = '', y = '')+
  annotate(geom = "text", x = -19, y = 18, hjust = 0, vjust = 1, 
           label = "C",
           size = 5, family="serif", color = "black")


D=ggplot()+
  geom_raster(data = Precipitación_seco_MES, aes(x = x, y = y, fill = bio14))+
  scale_fill_gradientn(colours = colpre, 
                       breaks = c(10, 20, 100, 160),
                       na.value = 'white',
                       name='Precipitación (mm) \n del mes más seco',
                       guide = guide_legend( direction = "horizontal",
                                             keyheight = unit(0.3, units = "cm"), 
                                             keywidth=unit(0.001, units = "cm"), 
                                             label.position = "bottom", title.position = 'top', nrow=1))+
  geom_sf(data = Mundo  , fill=NA, color="black", size=0.05)+
  geom_sf_text(data = Mundo_xy , aes(label = PAÍS ), 
               family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
               size = 2, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  coord_sf(xlim = c(-20 ,60), ylim = c(-5  ,20),expand = FALSE)+
  theme_classic()+
  theme(axis.text.x = element_text(color="black", size=7),
        
        legend.position = c(0.15, 0.15),
        legend.title=element_text(color="black", size=7, family="serif"),
        legend.text=element_text(size=7, family="serif", angle=25),
        legend.background = element_blank(),
        
        panel.background = element_rect(fill = "#a2d2ff"),
        axis.text.y  = element_text(color="black", size=7, angle=90))+
  geom_hline(yintercept = c(-5,0,5,10,15,20), color = "gray50",linetype = "dashed", size = 0.05)+
  geom_vline(xintercept = c(-20,10,0,10,20,30,40,50), color = "gray50",linetype = "dashed", size = 0.1)+
  labs(color = '',  x = '', y = '')+
  annotate(geom = "text", x = -19, y = 18, hjust = 0, vjust = 1, 
           label = "D",
           size = 5, family="serif", color = "black")



E=ggplot()+
  geom_raster(data = Precipitación_LLuvia_MES, aes(x = x, y = y, fill = bio13))+
  scale_fill_gradientn(colours = colpre, 
                       breaks = c(50, 150, 500, 1600),
                       na.value = 'white',
                       name='Precipitación (mm) \nPrecipitación del mes más lluvioso',
                       guide = guide_legend( direction = "horizontal",
                                             keyheight = unit(0.3, units = "cm"), 
                                             keywidth=unit(0.001, units = "cm"), 
                                             label.position = "bottom", title.position = 'top', nrow=1))+
  geom_sf(data = Mundo  , fill=NA, color="black", size=0.05)+
  geom_sf_text(data = Mundo_xy , aes(label = PAÍS ), 
               family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
               size = 2, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  coord_sf(xlim = c(-20 ,60), ylim = c(-5  ,20),expand = FALSE)+
  theme_classic()+
  theme(axis.text.x = element_text(color="white", size=7),
        
        legend.position = c(0.15, 0.15),
        legend.title=element_text(color="black", size=7, family="serif"),
        legend.text=element_text(size=7, family="serif", angle=25),
        legend.background = element_blank(),
        
        panel.background = element_rect(fill = "#a2d2ff"),
        axis.text.y  = element_text(color="white", size=7, angle=90),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = c(-5,0,5,10,15,20), color = "gray50",linetype = "dashed", size = 0.05)+
  geom_vline(xintercept = c(-20,10,0,10,20,30,40,50), color = "gray50",linetype = "dashed", size = 0.1)+
  labs(color = '',  x = '', y = '')+
  annotate(geom = "text", x = -19, y = 18, hjust = 0, vjust = 1, 
           label = "E",
           size = 5, family="serif", color = "black")


library(RColorBrewer)
library(grid)
library(png)
library(ggimage)
img <- readPNG("PNG/Elefante.png", FALSE)
g <- rasterGrob(img, x = unit(0.9, "npc"),y = unit(0.2, "npc"), width = unit(0.2, "npc"))



FF=ggplot()+
  geom_sf(data = Mundo  , fill="gray80", color="black", size=0.05)+
  geom_sf_text(data = Mundo_xy , aes(label = PAÍS ), 
               family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
               size = 2, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  geom_hex(data=Loxodonta_ele ,  aes(x=decimalLongitude, 
                                     y = decimalLatitude),
           
           size=0.01,
           bins=70)+
  scale_fill_gradientn(colours = brewer.pal(n=8,name="PiYG"),trans="sqrt",
                       breaks = c(10,20,30,40),
                       name="Número de registros \nLoxodonta africana", 
                       guide = guide_legend(direction = "horizontal",
                                            keyheight = unit(0.3, units = "cm"), 
                                            keywidth=unit(0.001, units = "cm"), 
                                            label.position = "bottom", title.position = 'top', nrow=1)
  )+
  coord_sf(xlim = c(-20 ,60), ylim = c(-5  ,20),expand = FALSE)+
  theme_classic()+
  theme(axis.text.x = element_text(color="white", size=7),
        
        legend.position = c(0.15, 0.15),
        legend.title=element_text(color="black", size=7, family="serif"),
        legend.text=element_text(size=7, family="serif", angle=25),
        legend.background = element_blank(),
        
        panel.background = element_rect(fill = "#a2d2ff"),
        axis.text.y  = element_text(color="white", size=7, angle=90),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = c(-5,0,5,10,15,20), color = "gray50",linetype = "dashed", size = 0.05)+
  geom_vline(xintercept = c(-20,10,0,10,20,30,40,50), color = "gray50",linetype = "dashed", size = 0.1)+
  labs(color = '',  x = '', y = '')+
  annotate(geom = "text", x = -19, y = 18, hjust = 0, vjust = 1, 
           label = "F",
           size = 5, family="serif", color = "black")+
  annotation_custom(g)

ig <- readPNG("PNG/Planta.png", FALSE)
gg <- rasterGrob(ig, x = unit(0.9, "npc"),y = unit(0.2, "npc"), width = unit(0.1, "npc"))

G =ggplot()+
  geom_sf(data = Mundo  , fill="gray80", color="black", size=0.05)+
  geom_sf_text(data = Mundo_xy , aes(label = PAÍS ), 
               family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
               size = 2, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  geom_point(data = Adansonia_digitata, aes( x=decimalLongitude, y = decimalLatitude, 
                                color=scientificName) ,size=2, alpha=0.3)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  coord_sf(xlim = c(-20 ,60), ylim = c(-5  ,20),expand = FALSE)+
  theme_classic()+
  theme(axis.text.x = element_text(color="white", size=7),
        
        legend.position = c(0.15, 0.15),
        legend.title=element_text(color="black", size=7, family="serif"),
        legend.text=element_text(size=7, family="serif", angle=25),
        legend.background = element_blank(),
        
        panel.background = element_rect(fill = "#a2d2ff"),
        axis.text.y  = element_text(color="white", size=7),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = c(-5,0,5,10,15,20), color = "gray50",linetype = "dashed", size = 0.05)+
  geom_vline(xintercept = c(-20,10,0,10,20,30,40,50), color = "gray50",linetype = "dashed", size = 0.1)+
  labs(color = '',  x = '', y = '', color="leyend")+
  annotate(geom = "text", x = -19, y = 18, hjust = 0, vjust = 1, 
           label = "G",
           size = 5, family="serif", color = "black")+
  annotation_custom(gg)


coll<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")

J =ggplot()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt))+
  scale_fill_gradientn(colours = coll, 
                       breaks = c(0,500, 800,2500,6000),
                       na.value = 'white',
                       name='Elevacion (msnm)',
                       guide = guide_legend( direction = "horizontal",
                                             keyheight = unit(0.3, units = "cm"), 
                                             keywidth=unit(0.001, units = "cm"), 
                                             label.position = "bottom", title.position = 'top', nrow=1)) +
  geom_sf(data = Mundo  , fill=NA, color="black", size=0.05)+
  geom_sf_text(data = Mundo_xy , aes(label = PAÍS ), 
               family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
               size = 2, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  coord_sf(xlim = c(-20 ,60), ylim = c(-5  ,20),expand = FALSE)+
  theme_classic()+
  theme(axis.text.x = element_text(color="white", size=7),
        
        legend.position = c(0.15, 0.15),
        legend.title=element_text(color="black", size=7, family="serif"),
        legend.text=element_text(size=7, family="serif", angle=25),
        legend.background = element_blank(),
        
        panel.background = element_rect(fill = "#a2d2ff"),
        axis.text.y  = element_text(color="white", size=7),
        axis.ticks.x = element_blank())+
  geom_hline(yintercept = c(-5,0,5,10,15,20), color = "gray50",linetype = "dashed", size = 0.05)+
  geom_vline(xintercept = c(-20,10,0,10,20,30,40,50), color = "gray50",linetype = "dashed", size = 0.1)+
  labs(color = '',  x = '', y = '', color="leyend")+
  annotate(geom = "text", x = -19, y = 18, hjust = 0, vjust = 1, 
           label = "J",
           size = 5, family="serif", color = "black")
J


library(cowplot)
Mapa=ggdraw() +
  coord_equal(xlim = c(0, 30), ylim = c(0, 21), expand = FALSE) +
  draw_plot(A  , width =15, height = 15,x = 0, y = 10)+
  draw_plot(B  , width =15, height = 15,x = 0, y = 5)+
  draw_plot(C  , width =15, height = 15,x = 0, y = 0)+
  draw_plot(D  , width =15, height = 15,x = 0, y = -5)+
  
  draw_plot(E   , width =15, height = 15,x = 15, y = 10)+
  draw_plot(FF  , width =15, height = 15,x = 15, y = 5)+
  draw_plot(G  , width =15, height = 15,x = 15, y = 0)+
  draw_plot(J  , width =15, height = 15,x = 15, y = -5)+
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))+
  annotate(geom = "text", x = -75, y = -17, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)

ggsave(plot=Mapa ,"Mapa Agrupado.png",units = "cm",width = 30, #alto
       height = 21, #ancho
       dpi=1200)




























































































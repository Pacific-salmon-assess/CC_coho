# See: https://github.com/bcgov/bcmaps
# see: https://cran.r-project.org/web/packages/bcmaps/bcmaps.pdf
# availble layers in bcmaps:
# https://gist.github.com/ateucher/86674e12ffba66ecce87cceb7bffbf41
# https://github.com/poissonconsulting/fwabc#<https://github.com/poissonconsulting/fwabc>
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html

buffer <- 25000

library(dbplyr)
library(bcdata)
library(dplyr)
library(bcmaps)
library(sf)
library(sp)
library(ggplot2)
library(ggspatial) # this is for adding the arrows
library(rgdal) #use this for data conversion
library(ggrepel) # to offset labels using geom_sf_label_repel  --> Not done here
library(riverdist) # to snap points to River --> Not done here
library(bcmapsdata)
library(viridis)
library(ggnewscale)
library(tidyr)

#library(USAboundaries)

# # Set plot box.  Set Port Hardy as centre and box is 20km to each side
# plot_area1 <- bc_cities() %>%   #Extract datafram of all bc cities location
#   filter(NAME == "Klemtu") %>%
#   #filter(NAME == "Port Hardy") %>%
#   st_buffer(dist = 2000000)%>%    # 20000 is a decent zoom in one that covers the Keogh
#   st_bbox() %>%                 # Turn into a square
#   st_as_sfc()                   # converts to sfc object

co_pops<-read.table("Data/coho_groups.txt",header=TRUE)
### lumping Rivers Smith Inlet with Area 7-8
co_pops[which(co_pops$group==7),4]<-6
group_names <- c("Central Coast (South)","Hecate Lowlands","Inner Waters","Haida Gwaii","Skeena","Nass")
group_names <- group_names[c(4,6,5,2,3,1)]
group_names <- factor(group_names,levels=c("Haida Gwaii","Nass","Skeena","Hecate Lowlands","Inner Waters","Central Coast (South)"))
co_pops$region <- group_names[co_pops$group]

NCC_coho <- read.csv("Data/NCC coho.csv",header=TRUE)
NCC_coho$region <- co_pops$region[match(NCC_coho$population,co_pops$population)]
NCC_coho$lon <- NCC_coho$Longitude
NCC_coho$lat <- NCC_coho$Latitude

data2 <- NCC_coho[,c("lon","lat")] #%>% mutate(UTM_W=as.numeric(UTM_W),UTM_N=as.numeric(UTM_N)) #
sputm <- SpatialPoints(data2, proj4string=CRS("+proj=longlat"))
spgeo <- spTransform(sputm, CRS("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
data_points <- as_tibble(NCC_coho) %>% mutate(lat=spgeo@coords[,2],lon=spgeo@coords[,1]) %>%
  st_as_sf(coords=c("lon","lat"),crs=3005)

## Subset the points data to be a single point for each location (for labelling Locations)
data_point_labels <- data_points %>% group_by(population) %>% slice(1)

ncc_extent <- raster::extent(data_points)
ncc_extent@xmin <- ncc_extent@xmin - 3*buffer
ncc_extent@ymin <- ncc_extent@ymin - buffer 
ncc_extent@xmax <- ncc_extent@xmax + buffer 
ncc_extent@ymax <- ncc_extent@ymax + buffer 
plot_area_ncc <- ncc_extent %>%
  st_bbox() %>%                 # Turn into a square
  st_as_sfc(crs=st_crs(data_points))
st_crs(plot_area_ncc) <- st_crs(data_points)
#bcdc_get_record("https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-rivers")
rivers_in_plot_area <- bcdc_query_geodata('f7dac054-efbf-402f-ab62-6fc4b32a619e') %>%
  #filter(STREAM_ORDER %in% c(3,4,5)) %>%  #Defines as only streams order 3,4,5 (too many including 1 &2)
  filter(INTERSECTS(plot_area_ncc)) %>%      # not sure about this line
  collect() %>%                           #Extracts the data
  st_intersection(plot_area_ncc)             #Where it intersects with plot line

#bcdc_get_record("https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-watersheds-groups")
watersheds <- bcdc_query_geodata('51f20b1a-ab75-42de-809d-bf415a0f9c62') %>%
  filter(INTERSECTS(plot_area_ncc)) %>%      # not sure about this line
  collect() %>%                           #Extracts the data
  st_intersection(plot_area_ncc)             #Where it intersects with plot line

bcdc_get_record("https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-stream-network")
all_streams <- bcdc_query_geodata("92344413-8035-4c08-b996-65a9b3f62fca") %>%
   filter(INTERSECTS(plot_area_ncc)) %>%
   filter(STREAM_ORDER%in% c(4,5,6)) %>%
   collect() %>%                           #Extracts the data
   st_intersection(plot_area_ncc)             #Where it intersects with plot line

rivers_index <- data_point_labels %>%
  st_nearest_feature(rivers_in_plot_area,data_point_labels)
rivers_ncc <- rivers_in_plot_area[rivers_index,]

rivernames <- rivers_ncc$GNIS_NAME_1
rivernames2 <- data_point_labels$population

major_riverGroup <- substr(rivers_ncc$FWA_WATERSHED_CODE,1,3)
major_riverID <- paste(major_riverGroup,paste(rep("000000",20),collapse="-"),sep="-")
major_groups <- substr(rivers_in_plot_area$FWA_WATERSHED_CODE,1,3)
major_riverID <- ifelse(as.numeric(major_riverGroup)>=900,rivers_ncc$FWA_WATERSHED_CODE,"x")

nass <- rivers_in_plot_area[rivers_in_plot_area$GNIS_NAME_1%in%"Nass River" & rivers_in_plot_area$WATERSHED_GROUP_CODE=="LNAR",]
skeena <- rivers_in_plot_area[rivers_in_plot_area$GNIS_NAME_1%in%"Skeena River" & rivers_in_plot_area$WATERSHED_GROUP_CODE=="LSKE",]

major_rivers <- rivers_in_plot_area[match(major_riverID,rivers_in_plot_area$FWA_WATERSHED_CODE,nomatch = 0),]
major_rivers <- dplyr::bind_rows(major_rivers,nass,skeena)

data_point_labels$river_name <- rivernames
major_riverGroup[which(data_point_labels$population=="lachmach")] <- "500"
data_point_labels$waterbodyid <- ifelse(as.numeric(major_riverGroup)>=900,rivers_ncc$FWA_WATERSHED_CODE,ifelse(as.numeric(major_riverGroup)==500,nass$FWA_WATERSHED_CODE,skeena$FWA_WATERSHED_CODE))
data_point_labels$drainage <- ifelse(as.numeric(major_riverGroup)>=900,rivers_ncc$FWA_WATERSHED_CODE,ifelse(as.numeric(major_riverGroup)==500,"Nass","Skeena"))
#major_rivers <- rivers_in_plot_area %>%
#  filter(LEFT_RIGHT_TRIBUTARY=='NONE') %>%
#  filter(WATERSHED_KEY%in%unique(rivers_ncc$WATERSHED_KEY))
#major_rivers <- distinct(major_rivers)

## --- Collect Mapping layers
# below grabs the coastline data within plot box
bchres <- bc_bound_hres()
coast_line <- bchres %>% st_intersection(plot_area_ncc)

## coastlines taken from freshwater atlas
## This looks the same as above but takes a lot longer to run
#coastline_k <- bcdc_query_geodata("freshwater-atlas-coastlines") %>% collect() %>%  st_intersection(square_round_ph)

# Locations of all cities within plot box
city_df<- bc_cities(ask=FALSE) %>% st_intersection(plot_area_ncc)

#Below is to get a dataset of lakes in plot box
lakes_in_plot_area <- bcdc_query_geodata("freshwater-atlas-lakes") %>%
  filter(INTERSECTS(plot_area_ncc)) %>%
  collect() %>%
  st_intersection(plot_area_ncc)

# Ocean colouring - this doesn't work well  because the resolution isn't the same
ocean_colour <- bc_neighbours(ask=FALSE) %>% 
  filter(type=="Ocean") %>% 
  st_intersection(plot_area_ncc)
ocean <- bc_neighbours(ask=FALSE) %>% 
  filter(type=="Ocean")

river_mouth <- major_rivers %>%
  group_by(BLUE_LINE_KEY) %>%
  st_nearest_points(ocean)
pts <- st_cast(river_mouth, "POINT")[seq(2, 2*length(river_mouth), 2)]

major_rivers$ocean_entry <- pts
data_point_labels$ocean_entry <- major_rivers$ocean_entry[match(data_point_labels$waterbodyid,major_rivers$FWA_WATERSHED_CODE,nomatch = NA)]
data_point_labels$ocean_entry[is.na(data_point_labels$ocean_entry)] <- ifelse(data_point_labels$drainage=='Skeena' & is.na(data_point_labels$ocean_entry),major_rivers$ocean_entry[major_rivers$GNIS_NAME_1%in%"Skeena River"],major_rivers$ocean_entry[major_rivers$GNIS_NAME_1%in%"Nass River"])


ggplot(data=watersheds) +
  geom_sf(data = watersheds, fill = "grey90") +
  #geom_sf(data=rivers_ncc,lwd=1.5,colour = "black") +
  geom_sf(data=major_rivers,lwd=1,colour = "black") +
  geom_sf(data=pts,pch=22,cex=2,bg='seagreen') +
  geom_sf(data=data_points,pch=21,cex=1.5,bg='orange')+
  scale_shape_manual(values=c("Rivers"=21,"Ocean entry"=22),breaks=c("Rivers","Ocean entry"))

ggplot(data=watersheds) +
  geom_sf(data = watersheds, fill = "grey90") +
  #geom_sf(data=rivers_ncc,lwd=1.5,colour = "black") +
  geom_sf(data=major_rivers,lwd=1,colour = "black") +
  geom_sf(data=data_point_labels$ocean_entry[data_point_labels$population=="bella_coola"],pch=22,cex=2,bg='seagreen')

#alaska <- bcmaps::bc_neighbours() %>% st_intersection(plot_area_ncc)
#alaska_full <- bcmaps::bc_neighbours()
pfma <- st_read("Data/PFMA/DFO_PFMA_SUBAREAS_SP/DFO_SBAREA_polygon.shp")
pfma <- pfma[pfma$MGMT_AREA>=2 & pfma$MGMT_AREA<=10,]
catch_area <- pfma %>% st_intersection(plot_area_ncc) %>% st_difference(bchres)
bc_sub <- bchres %>% st_intersection(plot_area_ncc)
bc_fish <- bchres %>% st_intersection(catch_area)
bec <- bcmaps::bec(ask=FALSE)
#bec <- bec %>%  dplyr::group_by(ZONE) %>% dplyr::summarise(across(geometry, ~ sf::st_combine(.)), .groups = "keep") %>% dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "drop")

bec <- bec %>% st_intersection(plot_area_ncc)# %>% st_intersection(bchres)
bec <- bec %>% st_intersection(bc_sub)

alaska <- st_transform(USAboundaries::us_states(states = "Alaska", resolution = "high"),st_crs(plot_area_ncc))
alaska_full <- alaska %>% st_intersection(bchres)
alaska <- alaska  %>% st_intersection(plot_area_ncc)
###########

# load forestry data
library(raster)
str_name<-'~/My Drive/SFU postdoc/Keogh river/BC_disturbance/logging_ageclass2012/logging_year.tif' 
log_year <- raster::raster(str_name)
# project forestry data into map projections
proj4string(log_year) <- CRS("+init=EPSG:3005")
# find the map extents from the raster file, and "crop" or subset the raster, otherwise its a huge file
ext <- extent(plot_area_ncc[[1]][1][[1]][1:4,])
log_year_crop <- crop(log_year,ext)
log_year_crop@data@attributes[[1]]$Rowid <- log_year_crop@data@attributes[[1]]$ID
logging_df <- as.data.frame(log_year_crop,xy=TRUE)
colnames(logging_df) <- c("x", "y","Year","count")
logging_df$Year <- as.numeric(logging_df$Year)
logging_df <- logging_df[complete.cases(logging_df),]
# maniuplate logging data to bin last logged year by 10 year bins
logging_df$Year <- round(logging_df$Year/10)*10
# anything earlier than 1950 was last logged "Pre-1950"
logging_df$Year <- ifelse(logging_df$Year<=1950,"Pre-1950",logging_df$Year)
logging_df$Year <- factor(logging_df$Year,levels=c("Pre-1950",sort(unique(logging_df$Year)[!grepl("Pre",unique(logging_df$Year))])))

## The fun part! we get to make a map
## Plot map -- Order of plotting MATTERS
ncc_map <- ggplot() +
  geom_sf(data = coast_line, fill = "grey90") +                 #Plot coastline
  geom_sf(data=alaska, fill='grey70') +
  geom_sf(data = plot_area_ncc, alpha = 0,colour='black') +        #Plot area box
  geom_tile(data=logging_df, aes(x=x, y=y, fill=Year), alpha=0.8) +
  #scale_fill_viridis(name = "Last logging year",option="viridis",direction=-1) +
  #scale_fill_gradient2(name="Last logged year",midpoint = 1920, low="darkgreen",mid="olivedrab3",high="goldenrod1",space="Lab") +
  scale_fill_manual(name="Last logged year",values=c("#1b7837","#7fbf7b","#01665e","#5ab4ac","#762a83","#d8b365","#8c510a")) +
  #scale_fill_brewer(name="Last logged year",palette = "Paired",direction=-1) +
  geom_sf(data = rivers_in_plot_area, colour = "lightblue3") +  #Plot Rivers
  geom_sf(data = rivers_ncc, colour = "lightblue3") +
  #geom_sf(data=pts,pch=22,bg="seagreen") +
  #geom_sf(data = lakes_in_plot_area, fill = "lightblue1") +     #Plot Lakes
  ggsflabel::geom_sf_label_repel(data=data_point_labels,aes(label=population),size=1.5, force = 1, nudge_x = -2, seed = 10)+ #Add labels
  coord_sf(expand = FALSE) +                                    #Expands box to axes
  xlab('Longitude') + ylab('Latitude') +                        #Axis titles
  annotation_scale(location = "bl", width_hint = 0.5) +         #Rose Compass
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering,
                         height = unit(1,"cm"), width = unit(1, "cm"))+
  theme(panel.background = element_rect('lightblue1')           #Make empty space blue to colour ocean
        , panel.grid.major = element_line('lightblue1'),
        legend.position="top",legend.text = element_text(size=6),legend.key.width=unit(0.35, "in"))
#bec <- bec[! (bec$ZONE %in% "CDF"),]

catch_area_union <- catch_area %>% group_by(MGMT_AREA) %>% summarize(geometry = st_union(geometry)) %>% st_sf() # collapse cc_sub to one polygon

ncc_bec <- ggplot() +
  geom_sf(data = catch_area_union, aes(fill=as.factor(MGMT_AREA))) +
  scale_fill_manual(name="Management areas",values=c("#7570b3","#d95f02","#e7298a","#a6761d","#6a3d9a","#fdbf6f","#66c2a4","#e6ab02","#cab2d6")) +
  new_scale("fill") +
  geom_sf(data=alaska, fill='grey85') +
  geom_sf(data = coast_line, fill = "grey90") +                 #Plot coastline
  # geom_sf(data=bec, aes(fill=ZONE,col=ZONE), alpha=0.8) +
  geom_sf(data = rivers_in_plot_area, colour = "#1f78b4") +  #Plot Rivers
  geom_sf(data = all_streams, colour = "#1f78b4",lwd=0.15) +  #Plot Rivers
  geom_sf(data = rivers_ncc, colour = "#1f78b4") +
  geom_sf(data = lakes_in_plot_area, fill = "#1f78b4",colour=NA) +     #Plot Lakes
  ggsflabel::geom_sf_label_repel(data=data_point_labels,aes(label=population,fill=region),size=1.5, force = 1, nudge_x = -2, seed = 10,max.overlaps=20)+ #Add labels
  geom_sf(data=data_point_labels,pch=21,aes(fill=region)) +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_brewer(name="Region",palette = "Paired",direction=1) +
  #geom_sf(data=pts,pch=22,fill="seagreen") +
  geom_sf(data = plot_area_ncc, alpha = 0,colour='black') +        #Plot area box
  coord_sf(expand = FALSE) +                                    #Expands box to axes
  xlab('Longitude') + ylab('Latitude') +                        #Axis titles
  annotation_scale(location = "bl", width_hint = 0.5) +         #Rose Compass
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering,
                         height = unit(1,"cm"), width = unit(1, "cm"))+
  theme(panel.background = element_rect('lightblue1'), panel.grid.major = element_line('lightblue1'),legend.position="top",legend.box.just="center",legend.box="horizontal",legend.justification = "center",legend.key.size=unit(1, "lines"),legend.margin = margin(c(0,0,0,-1),unit="lines"),legend.title=element_text(size=6),legend.text = element_text(size=5))

bc_neigh <- bc_neighbours(ask=FALSE)
bc_neigh <- bc_neigh[bc_neigh$name%in%c("Alaska"),]
bc_map <- ggplot() +
  geom_sf(data = bc_bound(ask=FALSE), fill = "grey10",colour=NA) +
  geom_sf(data=bc_neigh, fill='grey50',colour=NA) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill=adjustcolor("white",0.90), colour="black")) +
  theme(legend.justification = c(0, 1),legend.position = c(0, .95)) +
  theme(text = element_text(family = "Futura-Medium"),legend.title = element_text(family = "Futura-Bold", size = 10),legend.text = element_text(family = "Futura-Medium", size = 10))
bc <- bc_map + geom_rect(aes(xmin=ncc_extent@xmin,ymin=ncc_extent@ymin,xmax=ncc_extent@xmax,ymax=ncc_extent@ymax),fill = "tomato", colour = "grey70",alpha=0.5)

library(cowplot)
ncc_inset <- ggdraw(ncc_map) +
  draw_plot({
    bc},
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.20, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.65,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.15, 
    height = 0.15)

#Save the plot
ggsave('Figures/ncc_coho_map.pdf',plot=ncc_inset,width = 6, height = 7,units='in',dpi=800)
ggsave('Figures/ncc_coho_map.jpeg',plot=ncc_inset,width = 6, height = 7,units='in',dpi=800)
ncc_inset <- ggdraw(ncc_bec) +
  draw_plot({
    bc},
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.20, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.71,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.2, 
    height = 0.2)
ggsave('Figures/ncc_coho_bec.pdf',plot=ncc_inset,width = 6, height = 7,units='in',dpi=800)
ggsave('Figures/ncc_coho_bec.jpeg',plot=ncc_inset,width = 6, height = 7,units='in',dpi=800)

# match coho populations to their ocean entry

SR.dat<-read.table("Data/Coho_Brood_MASTER.txt", header=T)
SR.dat_full <- SR.dat
SR.dat<-subset(SR.dat,SR.dat$year<2017)

SR.dat$ocean_entry <- data_point_labels$ocean_entry[match(SR.dat$population,data_point_labels$population)]

SR.dat$logRS1<-log(SR.dat$RS_E)
SR.dat$logRS2<-log(SR.dat$RS_2)
SR.dat$logRS3<-log(SR.dat$RS_3)
SR.dat$Ut <- ifelse(!is.na(SR.dat$er_2),SR.dat$er_2,SR.dat$er_E)

SR.dat$group <- co_pops$group[match(SR.dat$population,co_pops$population)]
SR.dat$region <- group_names[SR.dat$group]

SR_agg <- SR.dat %>%
  group_by(population) %>%
  mutate(escapement_mean=ifelse(is.na(escapement),mean(escapement,na.rm=TRUE),escapement)) %>%
  ungroup()

SR_agg <- SR_agg %>%
  group_by(year,region) %>%
  mutate(total=sum(escapement_mean,na.rm=TRUE)) %>%
  ungroup()
SR_agg$prop <- pmax(0,SR_agg$escapement_mean/SR_agg$total,na.rm=TRUE)
SR_agg$x <- st_coordinates(SR_agg$ocean_entry)[,1]
SR_agg$y <- st_coordinates(SR_agg$ocean_entry)[,2]

SR_agg$mn_x <- SR_agg$x * SR_agg$prop
SR_agg$mn_y <- SR_agg$y * SR_agg$prop

SR_agg <- SR_agg %>%
  group_by(year,region) %>%
  mutate(new_x=sum(mn_x),new_y=sum(mn_y)) %>%
  ungroup()
ggplot(data=watersheds) +
  geom_sf(data = watersheds, fill = "grey90") +
  #geom_sf(data=rivers_ncc,lwd=1.5,colour = "black") +
  geom_sf(data=major_rivers,lwd=1,colour = "black") +
  geom_sf(data=SR_agg$ocean_entry,bg='seagreen',pch=22,cex=2)


SR_agg <- SR_agg[,colnames(SR_agg)!="ocean_entry"]
SR_agg_def <- SR_agg
SR_agg <- st_as_sf(SR_agg,coords=c("new_x","new_y"),crs=sf::st_crs(SR.dat$ocean_entry))

#SR_agg <- SR_agg %>% group_by(population) %>% slice(1) %>% ungroup()
closest_mouth <- SR_agg %>%
  st_nearest_points(ocean)

pts <- st_cast(closest_mouth, "POINT")[seq(2, 2*length(closest_mouth), 2)]
st_geometry(SR_agg_def) <- pts
SR_agg_def <- st_as_sf(SR_agg_def)

ggplot(data=watersheds) +
  geom_sf(data = watersheds, fill = "grey90") +
  #geom_sf(data=rivers_ncc,lwd=1.5,colour = "black") +
  geom_sf(data=major_rivers,lwd=1,colour = "black") +
  geom_sf(data=SR_agg_def,aes(fill=region),pch=21,cex=1.5)
ggsave(filename="Figures/time_varying_ocean_entry.jpeg",units="in",height=7,width=5,dpi=600)
saveRDS(SR_agg_def,file="Data/time_varying_ocean_entry.rds")

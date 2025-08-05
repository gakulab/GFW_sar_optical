
## S1 S2 {#sec-s1-s2}

### data load {#sec-data-load}
library(bigrquery)
projectid = "fish-database-275116"
sql <- "SELECT * FROM `fish-database-275116.GFW_Data.S1_published_detections_Zihan`;"
tb <- bq_project_query(projectid, sql)
s1_dat <- bq_table_download(tb)

sql <- "SELECT * FROM `fish-database-275116.GFW_Data.S2_detections_Zihan`;"
tb <- bq_project_query(projectid, sql)
s2_dat <- bq_table_download(tb)

skim(s1_dat)
skim(s2_dat)

### data flame {#sec-data-flame}
### only missing value in ssvid
s1_dat_na <- s1_dat[is.na(s1_dat$ssvid),] #17.8%
s2_dat_na <- s2_dat[is.na(s2_dat$ssvid),] #48.5%



### data flame lat lon {#sec-data-flame-latlon}
### 0.1
temp_round <- 1
s1_dat_na_0.1 <- s1_dat_na %>% 
  mutate(lat_index = round(detect_lat, temp_round), 
         lon_index = round(detect_lon, temp_round)) %>% 
  group_by(lat_index, lon_index) %>% 
  summarize(total_ditections =n()) %>% 
  ungroup() %>% 
  mutate(lat = lat_index, lon = lon_index) %>% 
  select(lat, lon, total_ditections)

s2_dat_na_0.1 <- s2_dat_na %>% 
  mutate(lat_index = round(detect_lat, temp_round), 
         lon_index = round(detect_lon, temp_round)) %>% 
  group_by(lat_index, lon_index) %>% 
  summarize(total_ditections =n()) %>% 
  ungroup() %>% 
  mutate(lat = lat_index, lon = lon_index) %>% 
  select(lat, lon, total_ditections)

### 0.5
s1_dat_na_0.5 <- s1_dat_na %>% 
  mutate(lat_index = round(detect_lat * 2) / 2, 
         lon_index = round(detect_lon * 2) / 2) %>% 
  group_by(lat_index, lon_index) %>% 
  summarize(total_ditections =n()) %>% 
  ungroup() %>% 
  mutate(lat = lat_index, lon = lon_index) %>% 
  select(lat, lon, total_ditections)

s2_dat_na_0.5 <- s2_dat_na %>% 
  mutate(lat_index = round(detect_lat * 2) / 2, 
         lon_index = round(detect_lon * 2) / 2) %>% 
  group_by(lat_index, lon_index) %>% 
  summarize(total_ditections =n()) %>% 
  ungroup() %>% 
  mutate(lat = lat_index, lon = lon_index) %>% 
  select(lat, lon, total_ditections)



### visual

## EEZ
eez_sf1 <-  st_read("~/Google Drive/Shared drives/gakuLab_data/Original/NEDO/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp") |> dplyr::filter(stringr::str_detect(UNION, "Japan")) |>
  st_transform(4326)

### ポリゴンを結合
eez_sf2 <- eez_sf1 |>
  st_union()

eez_sf <-   eez_sf2

# Japan land
#japan_sf <- ne_countries(country = c('japan','russia','south korea','north korea','china','taiwan','united states of america'),scale="large", returnclass = "sf") |>　st_transform(crs = 4326)
japan_sf <- ne_countries(country = c('japan'),scale="large", returnclass = "sf") |>　st_transform(crs = 4326)

### plot
#### S1(0.1)
fig_s1_0.1 <- ggplot() +  
  geom_tile(data = s1_dat_na_0.1, aes(x=lon, y=lat, fill=total_ditections))+ #, alpha = temp_alpha
  #scale_fill_viridis_c(option=temp_virid_op ) +
  geom_sf(data = eez_sf, alpha = .1, col = "#D0104C") +
  # Japan Land
  geom_sf(data = japan_sf) +
  xlim(125, 146) + ylim(25, 47) +
  scale_fill_gradient(low = "blue", high = "yellow") +
  labs(fill = "S1 detections") +
  theme(legend.box = "vertical",
        legend.position = "bottom",
        axis.text = element_text(size=10),
        panel.background = element_rect(fill="lightblue"),
        panel.grid = element_blank(),
        legend.text = element_text(angle=45,hjust=1,size=13),
        legend.title = element_text(size=15))  #+
  #ggspatial::annotation_scale( location = "br") + 
  #ggspatial::annotation_north_arrow(location = "tr", which_north = "true") 
fig_s1_0.1

#### S2(0.1)
fig_s2_0.1 <- ggplot() + 
  geom_tile(data = s2_dat_na_0.1, aes(x=lon, y=lat, fill=total_ditections))+ #, alpha = temp_alpha
  #scale_fill_viridis_c(option=temp_virid_op ) +
  geom_sf(data = eez_sf, alpha = .1, col = "#D0104C") +
  # Japan Land
  geom_sf(data = japan_sf) +
  xlim(125, 146) + ylim(25, 47) +
  scale_fill_gradient(low = "blue", high = "yellow") +
  labs(fill = "S2 detections") +
  theme(legend.box = "vertical",
        legend.position = "bottom",
        axis.text = element_text(size=10),
        panel.background = element_rect(fill="lightblue"),
        panel.grid = element_blank(),
        legend.text = element_text(angle=45,hjust=1,size=13),
        legend.title = element_text(size=15))  #+
  #ggspatial::annotation_scale( location = "br") + 
  #ggspatial::annotation_north_arrow(location = "tr", which_north = "true") 
fig_s2_0.1



#### S1(0.5)
fig_s1_0.5 <- ggplot() + 
  geom_tile(data = s1_dat_na_0.5, aes(x=lon, y=lat, fill=total_ditections))+ #, alpha = temp_alpha
  #scale_fill_viridis_c(option=temp_virid_op ) +
  geom_sf(data = eez_sf, alpha = .1, col = "#D0104C") +
  # Japan Land
  geom_sf(data = japan_sf) +
  xlim(125, 146) + ylim(25, 47) +
  scale_fill_gradient(low = "blue", high = "yellow") +
  labs(fill = "S1 detections") +
  theme(legend.box = "vertical",
        legend.position = "bottom",
        axis.text = element_text(size=10),
        panel.background = element_rect(fill="lightblue"),
        panel.grid = element_blank(),
        legend.text = element_text(angle=45,hjust=1,size=13),
        legend.title = element_text(size=15))  #+
  #ggspatial::annotation_scale( location = "br") + 
  #ggspatial::annotation_north_arrow(location = "tr", which_north = "true") 
fig_s1_0.5

#### S2(0.5)
fig_s2_0.5 <- ggplot() +  
  geom_tile(data = s2_dat_na_0.5, aes(x=lon, y=lat, fill=total_ditections))+ #, alpha = temp_alpha
  #scale_fill_viridis_c(option=temp_virid_op ) +
  geom_sf(data = eez_sf, alpha = .1, col = "#D0104C") +
  # Japan Land
  geom_sf(data = japan_sf) +
  xlim(125, 146) + ylim(25, 47) +
  scale_fill_gradient(low = "blue", high = "yellow") +
  labs(fill = "S2 detections") +
  theme(legend.box = "vertical",
        legend.position = "bottom",
        axis.text = element_text(size=10),
        panel.background = element_rect(fill="lightblue"),
        panel.grid = element_blank(),
        legend.text = element_text(angle=45,hjust=1,size=13),
        legend.title = element_text(size=15))  #+
  #ggspatial::annotation_scale( location = "br") + 
  #ggspatial::annotation_north_arrow(location = "tr", which_north = "true") 
fig_s2_0.5



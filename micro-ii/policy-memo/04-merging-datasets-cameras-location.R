
## Getting the data of speed tickets in Chicago
speed_tickets <- data.table::fread(input = "micro-ii/policy-memo/data/Speed_Camera_Violations.csv") %>% 
  as_tibble() %>% 
  mutate(DATE = lubridate::mdy(`VIOLATION DATE`))

## Getting the oficial position of the speed cameras
df_camera_position <- fread(input = "micro-ii/policy-memo/data/Speed_Camera_Locations.csv") %>% 
  as_tibble() %>% 
  mutate(ADDRESS = str_trim(str_remove(toupper(ADDRESS), pattern = ".(SPEED CAMERA)."))) %>% 
  arrange(ADDRESS) %>% 
  dplyr::select(ADDRESS, LATITUDE_OFFICIAL, LONGITUDE_OFFICIAL)

## Merging the dataset for the cameras with missing positions
df_cameras_position <- speed_tickets %>% 
  group_by(ADDRESS, year(DATE), `CAMERA ID`, LATITUDE, LONGITUDE) %>% 
  summarize(total_violatons = sum(as.numeric(VIOLATIONS))) %>%
  arrange(ADDRESS) %>% 
  left_join(df_camera_position, by = "ADDRESS") %>% 
  mutate(LATITUDE_OFFICIAL = case_when(is.na(LATITUDE) ~ LATITUDE_OFFICIAL,
                                       TRUE ~ LATITUDE),
         LONGITUDE_OFFICIAL = case_when(is.na(LONGITUDE) ~ LONGITUDE_OFFICIAL,
                                        TRUE ~ LONGITUDE)) %>% 
  dplyr::select(ADDRESS, `CAMERA ID`, LATITUDE_OFFICIAL, LONGITUDE_OFFICIAL, total_violatons) %>% 
  arrange(LATITUDE_OFFICIAL) %>% 
  ungroup() %>% 
  dplyr::select(-LATITUDE) %>% 
  ## Here we filter for 2021 only, because for the map, tickets from 2020 doesn't matter much
  filter(`year(DATE)` == 2021)

## Mannualy changing value for Humboldt camera
df_cameras_position[c(160), 4] <- 41.90143091
df_cameras_position[c(160), 5] <- -87.70212117

## Putting the name year in the date col:
colnames(df_cameras_position)[1] <- "date"

### LOOP for the merge ###

# ## Cameras in 2021
# df_2020_cameras <- df_cameras_position %>%
#   arrange(`CAMERA ID`)
# 
# ## Pre-loop for distance
# teste <- df_2020_cameras[1, ]
#   
# df_final <- df_crashes_fat_sev %>% 
#   bind_cols(teste)
# 
# for (i in seq_along(1:nrow(df_final))) {
# 
#   a<-df_final$LONGITUDE[i]
#   b<-df_final$LATITUDE[i]
#   c<-df_final$LONGITUDE_OFFICIAL[i]
#   d<-df_final$LATITUDE_OFFICIAL[i]
# 
#   df_final$distance[i]<-distm(c(a,b),c(c,d), fun = distHaversine)
#   
# }
# 
# ## Loop for distance
# for (i in c(2:nrow(df_2020_cameras))) {
#   
#   x <- df_2020_cameras[i, ]
#   
#   y <- df_crashes_fat_sev %>% 
#   bind_cols(x)
#   
#   for (i in seq_along(1:nrow(y))) {
# 
#     a<-y$LONGITUDE[i]
#     b<-y$LATITUDE[i]
#     c<-y$LONGITUDE_OFFICIAL[i]
#     d<-y$LATITUDE_OFFICIAL[i]
# 
#     y$distance[i]<-distm(c(a,b),c(c,d), fun = distHaversine)
#   
#   }
#   
#   df_final <- df_final %>% 
#     bind_rows(y)
#   
# }
# 
# glimpse(df_final)

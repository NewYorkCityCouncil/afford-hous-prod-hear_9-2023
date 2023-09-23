source("code/00_load_dependencies.R")
b_url <-
  "https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=Shapefile"
boro_zip <- unzip_sf(b_url)
boro_sf <- st_read(boro_zip) %>%
  st_as_sf(crs = 4326) %>%
  st_transform(2263) %>%
  select("geometry")


url_hp <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/nychdb_22q4_shp.zip"
hpz <- unzip_sf(url_hp)
hp <- st_read(hpz)

hpdt <- as.data.table(hp)
nbdt <- hpdt[Job_Type %in% "New Building" & !ResidFlag %in% "Non-Residential", ]
pfp <- nbdt[grep("Private For-Profit:", Ownership), ]
pfp[, PermitYear := year(as.Date(PermitYear, format = "%Y"))]
pfp[, CompltYear := year(as.Date(CompltYear, format = "%Y"))]

n_pfp <- pfp[!is.na(CompltYear), .N, by = c("CompltYear", "Ownership")]
p_completions <- ggplot(n_pfp, aes(x=CompltYear, y=N, group=Ownership, color=Ownership)) + geom_line() + 
  theme_bw() + ggtitle("Number of Completions per Year by Ownership")

n_perms <- pfp[!is.na(PermitYear), .N, by = c("PermitYear", "Ownership")]
q_permits <- ggplot(n_perms, aes(x=PermitYear, y=N, group=Ownership, color=Ownership)) + geom_line() + 
  theme_bw() + ggtitle("Number of Permits per Year by Ownership")

own <- hp$Ownership[grep("Private For-Profit:",unique(hp$Ownership))]
nb <- hp[hp$Job_Type %in% "New Building" & hp$PermitYear %in% c("2022")
         & hp$Ownership %in% own & !hp$ResidFlag %in% "Non-Residential", ] %>% 
  select(c("Job_Number", "Ownership", "geometry")) %>% 
           distinct() %>% 
  st_transform(2263)
nb$Ownership <- as.character(nb$Ownership)

gridnb <- boro_sf %>%
  st_make_grid(cellsize = c(5000, 5000), square = FALSE) %>% 
  st_sf() %>% # from sfc to sf
  st_intersection(boro_sf)  %>%
  distinct() %>% 
  mutate(id = row_number()) %>% 
  st_join(nb) %>% 
  na.omit(Job_Number)

grmap <- gridnb %>% 
  select(c("id", "Ownership", "geometry", "Job_Number")) %>% 
  distinct()
# dt for ease
dts_sub <- grmap %>% 
  as.data.table()



n_own <- unique(dts_sub[, .(n_own = length(unique(Job_Number))), by = .(id, Ownership)])
n_own[, total := sum(n_own), by = "id"]
# n_own[, sum(n_own), by = "Ownership"]

# subset
grmap2 <- grmap %>% 
  left_join(n_own, by = c("id", "Ownership")) %>% 
  st_transform('+proj=longlat +datum=WGS84')
# subset again - need to do this better
d1 <- grmap2[grmap2$Ownership %in% "Private For-Profit: Corporation", ]
d2 <- grmap2[grmap2$Ownership %in% "Private For-Profit: Individual", ]


pal1 <- leaflet::colorBin(palette = "Reds", domain = unique(d1$n_own), bins = 6)
pal2 <- leaflet::colorBin(palette = "Blues", domain = unique(d2$n_own), bins = 6)

map22 <- leaflet() %>%
  addCouncilStyle(add_dists = TRUE) %>% 
  leaflet::addPolygons(data = d1, 
                      color = "#CACACA", 
                      fillColor = ~pal1(n_own), 
                      label = d1$n_own, 
                      popup = paste0("Total Permits: ", d1$total),
                      opacity = .1,
                      fillOpacity = .8,
                      smoothFactor = .5,
                      group = "Private For-Profit: Corporation") %>% 
  leaflet::addPolygons(data = d2, 
                       color = "#CACACA", 
                       fillColor = ~pal2(n_own), 
                       label = d2$n_own, 
                       popup = paste0("Total Permits: ", d2$total),
                       opacity = .1,
                       fillOpacity = .5,
                       smoothFactor = .5,
                       group = "Private For-Profit: Individual") %>% 
  addLayersControl(baseGroups = c("Private For-Profit: Corporation", "Private For-Profit: Individual"), 
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  addLegend(pal = pal1, 
            values = unique(d1$n_own), 
            title = "# Permits, Corporations (2022)", 
            position = "topleft") %>% 
  addLegend(pal = pal2, 
            values = unique(d2$n_own), 
            title = "# Permits, Individuals (2022)", 
            position = "topleft")


map22
              # labelFormat(suffix = "%")))
          

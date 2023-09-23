source("code/00_load_dependencies.R")

# affordable housing production by income level 
ah <- fread("https://data.cityofnewyork.us/resource/hg8x-zxpr.csv?$limit=999999999")
ah[, project_start_date := as.Date(project_start_date)]
ah[, project_completion_date := as.Date(project_completion_date)]

cols1 <-names(ah)[grep("income|bbl|date|counted_rental_units|counted_homeownership_units|all_counted_units|latitude|longitude", 
                       names(ah))]
ahsub <- ah[,..cols1]
cols2 <- names(ahsub)[grep("units", names(ahsub))]
ahsub_long <- melt(ahsub, id.vars = c("bbl", "project_start_date", "project_completion_date", "counted_rental_units", "latitude",
                                      "longitude"), measure.vars = c(cols2[c(1:5)]))
ahsub_long[, year_start := year(project_start_date)][, year_complete := year(project_completion_date)]
ahsub_long[, sum_units_prog := sum(value), by = c("variable", "year_start")]
ahsub_long[, tot_rent_units := sum(counted_rental_units), by = "year_start"]

plot_props <- unique(ahsub_long[, .(prop_ = sum_units_prog/tot_rent_units), by = c("year_start", "variable")])

prop_plot <- ggplot(plot_props, aes(x = year_start, y = prop_, group = variable, color = variable)) + 
  geom_line() + theme_bw() + 
  ylab("Proportion of Units") + 
  xlab("Start Year")

# map ---------------------------------------------------------------------
b_url <-
  "https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=Shapefile"
boro_zip <- unzip_sf(b_url)
boro_sf <- st_read(boro_zip) %>%
  st_as_sf(crs = 4326) %>%
  st_transform(2263) %>%
  select("geometry")

ahmap <- ahsub_long[!is.na(bbl) & !is.na(latitude) & year_start %in% "2022", ]
ahsf <- ahmap %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% 
  st_transform(2263)

gridnb <- boro_sf %>%
  st_make_grid(cellsize = c(5000, 5000), square = FALSE) %>% 
  st_sf() %>% # from sfc to sf
  st_intersection(boro_sf)  %>%
  distinct() %>% 
  mutate(id = row_number()) %>% 
  st_join(ahsf) %>% 
  na.omit(bbl) %>% 
  select(-c("sum_units_prog", "tot_rent_units")) %>% 
  st_transform('+proj=longlat +datum=WGS84')

# unique(gridnb$id)

gidb <- gridnb %>% 
  as.data.table()

gidb[, total_income_units := sum(value), by = .(id, variable)]
gidb[, total_rent := sum(counted_rental_units), by = "id"]
gidb[, pct_units := round((total_income_units/total_rent)*100, 2), by = .(id, variable)]
gidb[, n_bbl := length(unique(bbl)), by = "id"]
gidb <- gidb %>% 
  distinct()
# gidb[id %in% 212, ]

gidbl <- split(gidb, gidb$variable)
gidsf <- lapply(gidbl, st_as_sf)

palw <- pal_nycc("warm")
palbin <- leaflet::colorBin(palette = palw, domain = gidb$pct_units, n = 7)

m <- leaflet() %>%
  addCouncilStyle(add_dists = TRUE)

# use walk from per to return m after adding each layer
names(gidsf) %>%                     
  walk(function(x)                         # then walk through vector of names one at a time
    m <<- 
      m %>% leaflet::addPolygons(             
        data = gidsf[[x]],
        label = paste0(gidsf[[x]]$pct_units,"%"),
        popup = ~paste0(councilPopup("Total BBLs: "), 
                        gidsf[[x]]$n_bbl, 
                        councilPopup("Total Rental Units: "), 
                        gidsf[[x]]$total_rent,
                        councilPopup("Total Rentals in Selected Income:"), 
                        gidsf[[x]]$total_income_units),
        color = ~palbin(gidsf[[x]]$pct_units),
        group = x))

# add layer controls to map 
m <- 
  m %>% 
  addLayersControl(
    baseGroups = names(gidsf),
    options = layersControlOptions(collapsed = FALSE)
  )

m <- m %>% 
  addLegend(pal = palbin, 
            values = gidb$pct_units, 
            labFormat = labelFormat(suffix = "%"), 
            title = "Percent of Units", 
            position = "topleft")

m
# 
# ahsub[, propxl := extremely_low_income_units/counted_rental_units
#       ][, propvl := very_low_income_units/counted_rental_units
#         ][, propl := low_income_units/counted_rental_units
#           ][, propmod := moderate_income_units/counted_rental_units
#             ][, propmid := middle_income_units/counted_rental_units]

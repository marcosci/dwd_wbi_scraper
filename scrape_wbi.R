library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(glue)
library(purrr)
library(lubridate)
library(sf)
library(tidygeocoder)
library(readr)
library(rnaturalearth)
library(stars)
library(gstat)
library(automap)
library(ggplot2)
library(rcartocolor)

Sys.setenv(GOOGLEGEOCODE_API_KEY = Sys.getenv('GOOGLEGEOCODE_API_KEY'))

counties_long <- c("Baden-Württemberg",
              "Bayern",
              "Berlin",
              "Brandenburg",
              "Bremen",
              "Hamburg",
              "Hessen",
              "Mecklenburg-Vorpommern",
              "Niedersachen",
              "Nordrhein-Westfalen",
              "Rheinland-Pfalz",
              "Saarland",
              "Sachsen",
              "Sachsen-Anhalt",
              "Schleswig-Holstein",
              "Thüringen"
              )

counties_short <- c("BW",
              "BY",
              "BE",
              "BB",
              "HB",
              "HH",
              "HE",
              "MV",
              "NI",
              "NW",
              "RP",
              "SL",
              "SN",
              "ST",
              "SH",
              "TH"
)

wbi <- map_dfr(seq_along(counties_long), function(id){
  url <- glue("https://www.dwd.de/DWD/warnungen/agrar/wbx/wbx_tab_alle_{counties_short[id]}.html")
  wbi <- url %>%
    read_html() %>%
    html_nodes(xpath='/html/body/article/section[2]/figure/table') %>%
    html_table()
  wbi <- wbi[[1]]
  wbi$county <- counties_long[id]
  if(wbi$Stationsname[length(wbi$Stationsname)] == "Stationsname") wbi <- wbi[-nrow(wbi), ]
  wbi <- wbi %>% 
    mutate(Statiosname_long = glue("{Stationsname}, {county}")) %>% 
    geocode(Statiosname_long, method = "google")
  
  wbi_long <- wbi %>% 
    tidyr::pivot_longer(cols = contains("."), 
                        values_to = "wbi", 
                        names_to = "date") %>% 
    mutate(date = str_sub(date, start = 4),
           date = glue("{date}{year(Sys.Date())}"),
           date = dmy(date),
           type = case_when(date == Sys.Date() ~ "measured",
                            TRUE               ~ "predicted")
    ) %>% 
    arrange(date) 
  wbi_long
})

write_csv(
  wbi,
  "data/wbi_current.csv"
)

write_csv(
  wbi,
  "data/wbi_history.csv",
  append = TRUE
)

wbi_sf <- wbi %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) 

write_sf(wbi_sf, "data/wbi_current.geojson", append = FALSE)


border <- ne_countries(country = "Germany", scale = 10, returnclass = "sf")

border <- border %>% 
  st_transform(st_crs(wbi_sf))

grid <- st_as_sfc(st_bbox(border))
grid <- stars::st_as_stars(grid, dx = 0.010, dy = 0.010)
grid <- grid[border]

library(terra)
temp_june <- terra::rast("/Users/marco/Downloads/grids_germany_daily_soil_temperature_5cm_202206/grids_germany_daily_soil_temperature_5cm_20220630.asc")
nied_june <- terra::rast("/Users/marco/Downloads/grids_germany_daily_soil_moist_202206/grids_germany_daily_soil_moist_20220630.asc")

cov_stack <- terra::rast(list(temp_june, nied_june))
names(cov_stack) <- c("temp", "nied")
crs_4326 <- "+proj=longlat +ellps=WGS84 +no_defs"
crs_3857 <- "+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs"
crs(cov_stack) <- crs_3857
y <- project(cov_stack, crs_3857)
z <- project(y, crs_4326)
plot(z)
plot(terra::vect(wbi_sf), add = TRUE)
cov_sf <- cbind(wbi_sf, terra::extract(z, terra::vect(wbi_sf)))

wbi_kriging <- cov_sf %>% 
  filter(date == Sys.Date(),
         !is.na(temp)) %>% 
  st_transform(crs_4326) %>% 
  as("Spatial")


pred_grid <- st_as_stars(z)
plot(pred_grid)

# Ordinary Kriging:
v_mod_ok <- wbi_kriging %>% 
  autofitVariogram(wbi ~ temp + nied, .)

plot(v_mod_ok)
library(raster)
y <- stack(z)
z <- as(y, "SpatialGridDataFrame")
plot(z)
g <- wbi_kriging %>% 
  krige(formula = wbi ~ temp + nied, ., z, model = v_mod_ok$var_model)

plot(st_as_stars(g))

wbi_kriging %>% 
  idw(formula = wbi ~  temp + nied, ., pred_grid, idp = 0.2) %>% 
  plot()

plot(pred_grid)

#z <- predict(g, pred_grid)
z <- z["var1.pred",,]
#names(z) <- "Waldbrandgefahrenindex"

write_stars(z, "data/wbi_current.tif")

plot(z)
b <- seq(-2.5, 2, 0.05)
plot(z, breaks = b, col = hcl.colors(length(b)-1, "Spectral", rev = TRUE), reset = FALSE)
plot(st_geometry(wbi_sf %>% 
                    filter(date == Sys.Date())), pch = 3, cex = 0.4, add = TRUE)
contour(z, breaks = b, add = TRUE)
# 
# 
# ggplot() +
#   geom_stars(data = z) +
#   scale_fill_carto_c(name = "Waldbrandgefahrenindex: ",
#                      limits = c(0,5),
#                      type = "quantitative", 
#                      palette = "Temps", 
#                      direction = 1,
#                      na.value = "transparent") +
#   theme_void()

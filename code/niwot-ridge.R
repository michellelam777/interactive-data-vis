
##########################################################################-
# 1. libraries ------------------------------------------------------------
##########################################################################-

# general use
library(here) # file organization
library(tidyverse) # manipulating
library(sf) # reading in spatial data, etc.
library(janitor) # cleaning variable names
library(lterdatasampler) # data source
library(randomcoloR) # random color generator

# Javascript package wrappers
library(leaflet) # interactive map
library(plotly) # interactive plots
library(DT) # interactive tables

##########################################################################-
# 2. spatial data ---------------------------------------------------------
##########################################################################-

# ⟞ a. project extent------------------------------------------------------

project_extent <- st_read(here::here("data", "nwt_project_extent", "shapefiles"), layer = "nwt_project_extent") %>% 
  st_transform(crs = 4326)

# ⟞ b. snow survey --------------------------------------------------------

snow2018 <- st_read(here::here("data", "ss2018", "shapefiles"), layer = "ss2018") %>% 
  st_transform(crs = 4326) %>% 
  clean_names() %>% 
  mutate(comments = case_when(
    comments == "NaN" ~ "none",
    TRUE ~ comments
  )) %>% 
  mutate(marker_text = paste(
    "Depth:", snowdepth, "<br>",
    "Time:", sampletime, "<br>",
    "Recorders:", recorders, "<br>",
    "Comments:", comments, "<br>"
  )) 

# ⟞ c. vegetation classes -------------------------------------------------

veg <- st_read(here::here("data", "veg", "shapefiles"), layer = "veg") %>% 
  st_transform(crs = 4326) %>% 
  clean_names() %>% 
  mutate(marker_text = paste(
    "Type:", type, "<br>",
    "Area:", area, "<br>",
    "Perimeter:", perimeter, "<br>"
  )) 

veg_list <- veg %>% 
  pull(type) %>% 
  unique()

colors <- c(
  "#1c6e73", randomColor(count = 23, luminosity = "random"), "#e3e5e6"
)
veg_pal <- colorFactor(colors, domain = veg$type, ordered = TRUE)


# ⟞ d. landmarks ----------------------------------------------------------

landmarks <- st_read(here::here("data", "nwt_annotation_pnt", "shapefiles"), layer = "nwt_annotation_pnt") %>% 
  st_transform(crs = 4326) %>% 
  clean_names() %>% 
  mutate(marker_text = paste(
    "Name:", name
  )) 

# ⟞ e. pikas --------------------------------------------------------------

pikas <- st_as_sf(x = nwt_pikas, coords = c("utm_easting", "utm_northing")) %>% 
  st_set_crs("+proj=utm +zone=13 +datum=NAD83 +units=m") %>% 
  st_transform("+proj=longlat +datum=WGS84") %>% 
  mutate(marker_text = paste(
    "Date:", date, "<br>",
    "Station:", station, "<br>",
    "Sex:", sex, "<br>"
  ))

##########################################################################-
# 3. leaflet map ----------------------------------------------------------
##########################################################################-

map <- leaflet() %>% 
  # base maps
  addProviderTiles(providers$Thunderforest.MobileAtlas, group = "Thunderforest") %>% 
  addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") %>%
  # addProviderTiles(providers$Stamen.Watercolor, group = "Watercolor") %>%
  
  # map layers: project boundary and vegetation classes
  addPolygons(data = project_extent, color = "green", group = "NWT project extent") %>%
  addPolygons(data = veg, group = "Vegetation", popup = ~marker_text, fillColor = ~veg_pal(type), stroke = FALSE, fillOpacity = 1) %>% 
  
  # markers
  addCircleMarkers(data = snow2018, group = "Snow surveys",
                   color = "lightblue", stroke = FALSE, fillOpacity = 1,
                   popup = ~marker_text,
                   popupOptions = popupOptions(closeOnClick = FALSE)) %>%
  addCircleMarkers(data = landmarks, group = "Landmarks", 
                   color = "yellow", stroke = FALSE, fillOpacity = 1,
                   popup = ~marker_text,  
                   popupOptions = popupOptions(closeOnClick = FALSE)) %>% 
  addCircleMarkers(data = pikas, group = "Pikas", 
                   color = "orange", stroke = FALSE, fillOpacity = 1, 
                   popup = ~marker_text, 
                   popupOptions = popupOptions(closeOnClick = FALSE)) %>%
  
  # layers control
  addLayersControl(
    baseGroups = c("Thunderforest", "OpenTopoMap"),
    overlayGroups = c("NWT project extent", "Vegetation", "Snow surveys", "Landmarks", "Pikas"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>% 
  
  # legends
  addLegend(values = 1, group = "Snow surveys", position = "bottomleft", labels = "Snow surveys", colors = "lightblue") %>%
  addLegend(values = 2, group = "Landmarks", position = "bottomleft", labels = "Landmarks", colors = "yellow") %>% 
  addLegend(values = 3, group = "Pikas", position = "bottomleft", labels = "Pikas", colors = "orange") %>% 
  
  # scale bar
  addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE))

map 

##########################################################################-
# 4. plotly graph ---------------------------------------------------------
##########################################################################-

elev_gradient <- colorRamp(c("lightblue", "darkblue"), 20)

pika_static <- ggplot(pikas, aes(x = date, y = concentration_pg_g, text = marker_text)) +
  geom_point(aes(color = elev_m, size = concentration_pg_g)) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "Date", y = "Concentration (pg/g)", 
       title = "There is no clear relationship between sampling date and hormone concentration.") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "Garamond"), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16),
        plot.title.position = "plot")

pika_static

ggplotly(pika_static,
         tooltip = c("text"), 
         showlegend = FALSE) %>% 
  layout(hoverlabel = list(
    font = list(
      family = "Garamond",
      size = 12
    )
  ))


##########################################################################-
# 5. DT table -------------------------------------------------------------
##########################################################################-

nwt_pikas %>% 
  select(date, site, station, sex, concentration_pg_g, elev_m) %>% 
  datatable(editable = "cell",
            colnames = c("Date", "Site", "Station", "Sex", "Concentration (pg/g)", "Elevation (m)"),
            filter = "top",
            options = list(pageLength = 20,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$('body').css({'font-family': 'Garamond'});",
                             "}"
                           ))) %>% 
  formatStyle(c('date', 'station', 'concentration_pg_g'), backgroundColor = 'lightblue')











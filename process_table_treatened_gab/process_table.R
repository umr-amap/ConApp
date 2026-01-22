

library(tidyverse)
library(DBI)
devtools::load_all()

mydb_extract <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'rainbio',
  host = 'dg474899-001.dbaas.ovh.net',
  port = 35699,
  # or any other port specified by your DBA
  user = "dauby",
  password = "Amap2020"
)


# dataset <- readxl::read_excel("process_table_treatened_gab/Tree_species_Gabon_040523.xlsx")

dataset <- read_csv("process_table_treatened_gab/Tree_species_Gabon_040523.csv", locale = readr::locale(encoding = "latin1"))

names(dataset) <- tolower(names(dataset))

dataset <- 
  dataset %>% 
  mutate(assessmentdate = lubridate::dmy(assessmentdate))


dataset %>% 
  select_if(is.character) %>% 
  map(~max(nchar(.), na.rm = T))

res <- dbSendQuery(mydb_extract,
                   "CREATE TABLE public.table_threat_taxa_gab (
    id serial primary key,
    acceptedtaxonkey integer,
    accepetedtaxonname varchar(46),
    tropicosnameid integer,
    fullnamewithauthors varchar(84),
    taxonrank varchar(10),
    id_name_apd integer,
    redlistcategory varchar(5),
    redlistcriteria varchar(39),
    yearpublished integer,
    assessmentdate date
  );"
)

res <- DBI::dbFetch(res)

tbl(mydb_extract, "table_threat_taxa_gab")

dbWriteTable(mydb_extract, 
             "table_threat_taxa_gab", 
             dataset %>% dplyr::select(acceptedtaxonkey, accepetedtaxonname, tropicosnameid, fullnamewithauthors, taxonrank, id_name_apd, 
                                       redlistcategory, redlistcriteria, yearpublished, assessmentdate),
             append = TRUE, 
             fileEncoding = "UTF-8", row.names=FALSE)


dbGetQuery(mydb_extract, "SELECT schema_name
                 FROM information_schema.schemata;")

####################################################


#### creation d'un troisième module d'extract des données je suppose à intéger dans conr-server.R
country_lr <- data_3_server(id = "data_country")


###################*******######
## A inclure dans ce module (data_3_server) :
# Choix du pays (un seul possible à ce stade, maos d'autres viendront, donc cette étape nous laisse la possibilité d'en ajouter)
shinyWidgets::virtualSelectInput(
  inputId = ns("country"),
  label = "Country to explore:",
  choices = c("Gabon"),
  selected = "Gabon",
  search = TRUE,
  width = "100%"
)


# Lancer l'extract une fois le pays sélectionné

dataset_rv <- reactiveValues(value = NULL)

mydb_extract <- conn_mydb_rb(pass = "Anyuser2022", user = "common") ## fonction existante dans rainbio.R
if (input$country == "Gabon") 
  full_table <- func_try_fetch(con = mydb_extract, sql = "SELECT * FROM table_threat_taxa_gab") ## fonction existante dans rainbio.R
threat_taxa <- 
  full_table %>% 
  filter(redlistcategory %in% c("CR", "EN", "VU"))

threat_taxa <- threat_taxa[1:20,]

extract_sp <- search_species_info(species_name = unique(threat_taxa$accepetedtaxonname)) ## fonction existante dans gbif.R
keys <- extract_sp$specieskey

cat_keys <- threat_taxa %>% left_join(extract_sp %>% select(provided_sciname, specieskey), 
                          by = c("accepetedtaxonname" = "provided_sciname")) %>% 
  select(specieskey, redlistcategory, accepetedtaxonname)
cat_keys <- 
  cat_keys %>% group_by(specieskey) %>% 
  summarise(redlistcategory = first(redlistcategory))

# dataset_rv$value <- retrieve_occ_data(keys) ## fonction existante dans gbif.R
dataset_rv <- retrieve_occ_data(keys)

# dataset_rv %>% 
#   filter(is.na(iucnRedListCategory)) %>% 
#   distinct(scientificName)

dataset_rv <- dataset_rv %>%
  left_join(cat_keys,
            by = c("speciesKey" = "specieskey"))


### Utilisation de fonction existante pour sélectionner les variables de manière automatique
variable_r <- data_variable_server("variable", data_r = dataset_rv$value)

observeEvent(variable_r()$data, {
  dataset_rv$value <- variable_r()$data
})

### validation automatique des occurrences
data_validated_r <- data_validation_server(
  id = "validation",
  data_r = reactive({
    # input$navs
    variable_r()$data
  })
)

observeEvent(data_validated_r(), {
  dataset_rv$value <- data_validated_r()
})

final_data_r <- eventReactive(input$go_next, {
  data_validated_r()
})



### Dés que le jeu de données est extrait, proposer un choix du type de carte avec deux possibilités
##

radioButtons("type_map", "Which kind of map",
             choices = c("Occurences" = "occ",
                         "Grid" = "grid"),
             selected = "grid")

### Si type_map$type_map == "grid
### Faire apparaite un sliderinput pour choisir la resolution
sliderInput(
  inputId = ns("resolution"),
  label = tagList(
    "Grid size:",
    btn_help(
      "Grid size"
    )
  ),
  min = 1,
  max = 30,
  value = 10,
  round = TRUE,
  step = 1,
  width = "100%"
)


### Si type_map$type_map == "grid
### Créer une carte leaflet reactive (renderleaflet) au choix de input$resolution
data_latlon <- data_validated_r() %>% dplyr::select(.__longitude, .__latitude)

data_latlon_sf <- data_latlon %>%
  dplyr::filter(!is.na(.__longitude),!is.na(.__latitude)) %>% 
  sf::st_as_sf(coords = c(".__longitude", ".__latitude"))
sf::st_crs(data_latlon_sf) <- 4326
data_latlon_sf <- 
  sf::st_transform(data_latlon_sf, 'EPSG:6933')

bbox_gab <- st_sf(1, 
                  geom = sf::st_geometry(st_polygon(list(matrix(c(8, -4, 15, -4, 15, 3, 8, 3, 8, -4), 
                                                                ncol = 2, byrow = TRUE)))))
st_crs(bbox_gab) <- 4326
bbox_gab <- st_transform(bbox_gab, 'EPSG:6933')

grid <- 
  st_make_grid(x = bbox_gab, 
               cellsize = resolution)
grid <- st_as_sf(grid) %>% 
  mutate(id_grid = 1:nrow(.))

intersect_grid <- 
  st_intersection(data_latlon_sf, grid)

sampling_cell <- 
  intersect_grid %>% 
  group_by(id_grid) %>% 
  summarise(nbe_esp = length(unique(scientificName)),
            n = length(scientificName)) %>% 
  st_set_geometry(NULL)

grid_not_null <- 
  grid %>% 
  left_join(sampling_cell) %>% 
  filter(!is.na(n))

grid_not_null <- st_transform(grid_not_null, 4326)

pal <- leaflet::colorNumeric(
  palette = "Blues",
  domain = grid_not_null$nbe_esp)

leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) %>%
  leaflet::invokeMethod(data = NULL, method = "addZoom", list(position = "topright")) %>%
  leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OSM") %>%
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri") %>%
  leaflet::addProviderTiles(leaflet::providers$OpenTopoMap, group = "Open Topo Map") %>%
  leaflet::addLayersControl(
    baseGroups = c("OSM", "Esri", "Open Topo Map"),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  ) %>%
  leaflet::addPolygons(
    data = grid_not_null,
    weight = 1,
    color = ~pal(nbe_esp),
    fill =  ~pal(nbe_esp),
    popup = paste("Region: ", grid_not_null$nbe_esp)
  )

### Exporter les données, les inputs et le grid
return(data = final_data_r,
       resolution = input$resolution,
       country = input$country,
       grid = grid_not_null,
       type_map = input$type_map)


###################*******######


## Dans conr-server, obtenir les paramètres et données du module
observeEvent(country_lr$data(), data_rv$y <- country_lr$data())
observeEvent(country_lr$data(), data_rv$grid <- country_lr$grid)

### choix mode maillage ou occurrences

input$resolution

### Nouveau module à créér de la même forme que 'summary_report_server'
summary_report_server2(
  id = "report2",
  data_r = reactive({
    req(data_rv$y, hasName(data_rv$y, "STATUS_CONR")) %>%
      dplyr::filter(STATUS_CONR == "IN")
  }),
  resolution = country_lr$resolution,
  country = country_lr$country,
  grid = data_rv$grid,
  type_map = country_lr$type_map
)


### Code à inclure dans le module 'summary_report_server2'

dataset_rv <- data_r()

rmarkdown::render(
  'D:/MonDossierR/conrappli_dreams/inst/reports/country_report.Rmd',
  params = list(
    country = country,
    resolution = resolution,
    data = dataset_rv
  ),
  output_file = paste0("report_conr", '.html')
)


occdata_georef <- data %>% filter(!is.na(decimalLongitude),
                                     !is.na(decimalLatitude)) %>% 
  select(key, scientificName, decimalLongitude, decimalLatitude)

occdata_sf <- st_as_sf(occdata_georef, 
                       coords = c("decimalLongitude", "decimalLatitude"))
st_crs(occdata_sf) <- 4326
occdata_sf <- st_transform(occdata_sf, 'EPSG:6933')

# bbox_gab <- st_buffer(st_sf(1, geom = st_geometry(st_point(c(11.5, -0.5)))), 4)
.data <-  dataset_rv


data_latlon_sf <- .data %>%
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) %>%
  mutate(decimalLongitude = jitter(decimalLongitude, factor = 0.5),
         decimalLatitude = jitter(decimalLatitude, factor = 0.5)) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"))

# data_jitter <- .data
# data_jitter$decimalLatitude <- jitter(data_jitter$decimalLatitude, factor = 0.5)
# data_jitter$decimalLongitude <- jitter(data_jitter$decimalLongitude)

# data_latlon_sf <- .data %>%
#   filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) %>%
#   st_as_sf(coords = c("decimalLongitude", "decimalLatitude"))
st_crs(data_latlon_sf) <- 4326
data_latlon_sf <- st_transform(data_latlon_sf, "EPSG:6933")
return(data_latlon_sf)



# 
# data_latlon_sf <- data_latlon_sf[100:110,]
# data_latlon_sf2 <- data_latlon_sf2[100:110,]
# 
# mapview::mapview(data_latlon_sf[,"geometry"]) + mapview::mapview(data_latlon_sf2[,"geometry"], col.regions = "red")

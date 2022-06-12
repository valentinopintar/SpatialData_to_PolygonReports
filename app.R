library(shiny)
library(shinyjs)
library(shinyalert)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(utf8)
library(sf)
library(rgeos)
library(sp)
library(raster)
library(DT)
library(leafem)
require(maptools)

ui <- dashboardPagePlus(
  dashboardHeaderPlus(
    title = tagList(
      tags$span(
        class = "logo-mini", "sGIS"
      ),
      tags$span(
        class = "logo-lg", "simplyGIS"
      )
    ),
    titleWidth = "180px"),
  dashboardSidebar(
    width = "180px",
    sidebarMenu(
      menuItem("PointsToPolygon", tabName = "PointsToPolygon", icon = icon("globe")),
      menuItem("Uskoro!!", tabName = "uskoro", icon = icon("list-alt"))
    )
  ),
  dashboardBody(
    
    tags$script(HTML("$('body').addClass('sidebar-mini');")),
    
    tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #484848;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #484848;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #484848;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #282828;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #F5F5F5;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #606060;
                              color: #000000;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #A9A9A9;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #A9A9A9;
                              }
                              '))),
    
    
    tabItems(
      tabItem(tabName = "PointsToPolygon",
              fluidRow(
                box(
                  width = 12,
                  title = "Settings",
                  status = "primary",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  solidHeader = TRUE,
                  useShinyalert(),
                  fileInput(inputId = "shp",
                            label = "Upload polygon (GRID) for output polygon reports. Upload shapefile :",
                            multiple = TRUE,
                            accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
                  fileInput(inputId = "occurrences",
                                        label = "Upload occurrences here :",
                                        multiple = TRUE,
                                        accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
                  actionButton(inputId = "select_polygon", label = "Select&Plot", 
                               style = "color: #fff; background-color: #1D8CB9; border-color: #C73232", 
                               icon = icon(name = "eraser", lib = "font-awesome")),
                  
                    # column(3,
                    #        selectInput(inputId = "projection", label = "Select output projection :",
                    #                              c('ETRS_1989_LAEA' = 'etrs', 'GCS_WGS_1984' = 'wgs', 'HTRS_TM_96' = 'htrs')
                    #                    )),
                    # downloadButton ("downloadShp", "Download"),
                    
                  actionButton(inputId = "download", label = "Download polygon"),
                  tags$style(type='text/css', "#select_polygon { width:12%; margin-top: 25px;}"),
                  tags$style(type='text/css', "#download { width:12%; margin-top: 25px;}"))),
             
                                      leafletOutput(outputId = "map"),
              tags$style(type = "text/css", "#map {height: calc(90vh - 80px) !important;}")),
      tabItem(tabName ="uskoro", plotOutput = NULL))
    
  )
)

server <- function(input, output, session) {
  
  uploadshp <- reactive ({
    req(input$shp)
    shpdf <- input$shp
    updir <- dirname(shpdf$datapath[1])
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(updir, "/", shpdf$name[i])
      )
    }
    
  uploadshp <- readOGR(paste(updir,
                             shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                             sep = "/"))
  uploadshp <- spTransform(uploadshp, CRS("+proj=longlat +datum=WGS84"))
  uploadshp
  })
  
  uploadoccurrences <- reactive ({
    req(input$occurrences)
    shpdf <- input$occurrences
    updir <- dirname(shpdf$datapath[1])
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(updir, "/", shpdf$name[i])
      )
    }
    
    upload_o <- list.files(updir, pattern = "[.]shp$", full.names = T)
    for (f in upload_o) {
      shp_paste <-paste(updir,f,sep = "")
      uploadoccurrences <- readOGR(f)
    }
    
    # uploadoccurrences <- readOGR(paste(updir,
    #                            shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
    #                            sep = "/"))
    #uploadoccurrences <- do.call(rbind, lapply())
    uploadoccurrences <- spTransform(uploadoccurrences, CRS("+proj=longlat +datum=WGS84"))
    uploadoccurrences
  })
  
  observeEvent(input$shp, {
    
    data = uploadshp()
    map = leafletProxy("map")
    
    if (!is.null(uploadshp())){
      if(inherits(data, "SpatialPolygons")){
        shinyalert::shinyalert("Successful upload !", type = "info", timer = 2000)
        cent <- gCentroid(spgeom = uploadshp(), byid = FALSE)
        leafletProxy("map") %>%
          addPolygons(data = uploadshp(),
                      stroke = TRUE,
                      weight = 2,
                      opacity = 0.5,
                      color = "black",
                      fillColor = "#00FFEC",
                      fillOpacity = 0.2,
                      group = "GRID") %>% 
          setView(cent$x, cent$y, 7) 
          
      }
      if(inherits(data, "SpatialPoints")){
        shinyalert::shinyalert("Successful upload !", type = "info")
        cent <- gCentroid(spgeom = uploadshp(), byid = FALSE)
        leafletProxy("map") %>% 
          addCircleMarkers(data = uploadshp(),
                           stroke = TRUE,
                           weight = 2,
                           color = "black",
                           fillColor = "#00FFEC",
                           radius = 3,
                           fillOpacity = 0.5,
                           group = "POINTS") 
          
        
      }
    }
    
  })
  
  observeEvent(input$occurrences, {
    
    data = uploadoccurrences()
    map = leafletProxy("map")
    if (!is.null(uploadoccurrences())){
      if(inherits(data, "SpatialPolygons")){
        shinyalert::shinyalert("Successful upload !", type = "info", timer = 2000)
        cent <- gCentroid(spgeom = uploadoccurrences(), byid = FALSE)
        leafletProxy("map") %>% 
          addPolygons(data = uploadoccurrences(),
                      stroke = TRUE,
                      weight = 2,
                      opacity = 0.5,
                      color = "black",
                      fillColor = "#00FFEC",
                      fillOpacity = 0.2,
                      group = "POLYGON") 
          
      }
      if(inherits(data, "SpatialPoints")){
        shinyalert::shinyalert("Successful upload !", type = "info")
        cent <- gCentroid(spgeom = uploadoccurrences(), byid = FALSE)
        leafletProxy("map") %>% 
          addCircleMarkers(data = uploadoccurrences(),
                           stroke = TRUE,
                           weight = 2,
                           color = "black",
                           fillColor = "#00FFEC",
                           radius = 3,
                           fillOpacity = 0.5,
                           group = "POINTS") 
          
      }
    }
  })
  
  select_poly <- reactive({

    polygon <-  uploadshp ()
    points <-  uploadoccurrences ()
    
    polygon <- as(polygon, "SpatialPolygonsDataFrame")
    points <- as(points, "SpatialPointsDataFrame")

    select <- polygon[points, ]
    select_poly <- spTransform(select, CRS("+proj=longlat +datum=WGS84"))
    select_poly
    
  })
  
  select_htrs <- reactive({
    data <- select_poly()
    select_htrs <- spTransform(data, CRS("+init=epsg:3765"))
  })

  select_etrs <- reactive({
    data <- select_poly()
    select_etrs <- spTransform(data, CRS("+init=epsg:3035"))

  })
  
  
  observeEvent(input$select_polygon, {
    data <- select_poly()
    map = leafletProxy("map")
    if (!is.null(select_poly()))
    {leafletProxy("map") %>%
      addPolygons(data = select_poly(),
                  stroke = TRUE,
                  weight = 2,
                  color = "blue",
                  fillColor = "#1D8CB9",
                  fillOpacity = 0.8,
                  group = "SELECT_POLYGON")}
  })
  # observeEvent(input$projection,
  #              if (input$projection = 'etrs'){
  #   output$downloadShp <- downloadHandler(
  #     filename = function(){
  #       paste("Shapefile","zip",sep = ".")
  #     },
  #     content = function(con){
  #       data = select_etrs
  #       tmp_dir <- tempdir()
  #       setwd(tempdir())
  #       filesToSave <- c()
  #       st_write(data, tmp_dir,"Shapefile", "ESRI Shapefile")
  #       zip(zipfile = con, files = filesToSave)
  #     },
  #     contentType = "application/zip"
  #   )
  # })
 
   
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2, maxZoom = 19, zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
    }") %>%
      setView(0, 0, 2) %>% 
      setMaxBounds(lng1 = 180
                   , lat1 = -300
                   , lng2 = -180
                   , lat2 = 300) %>% 
      addProviderTiles(providers$Esri.WorldImagery, 
                       group = "Satellite",
                       options = providerTileOptions(errorTileUrl = NULL, minZoom = 2, maxZoom = 19)) %>%
      addTiles(group = "OSM (default)",
               options = providerTileOptions(minZoom = 2, maxZoom = 19)) %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, 
                       group = "Grey",
                       options = providerTileOptions(minZoom = 2, maxZoom = 19)) %>%
      addMiniMap(toggleDisplay = T) %>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "kilometers",
        secondaryLengthUnit = "meters",
        primaryAreaUnit = "hectares"
      ) %>%
      addMouseCoordinates("detailed") %>%
      addFullscreenControl(position = "topleft") %>% 
      addEasyButton(
        easyButton(position = "bottomright", icon = "fa-globe", title = "Zoom to Level 2", 
                   onClick = JS("function(btn, map){ map.setZoom(2);
                              maxBoundsViscocity: 0;
                              map.fitBounds(bounds);}"))) %>%
    # addWMSTiles("http://globalforestwatch-624153201.us-west-1.elb.amazonaws.com:80/arcgis/services/TreeCover2000/ImageServer/WMSServer",
    #               layers = "0",
    #               options = WMSTileOptions(format = "image/png", transparent = TRUE),
    #               attribution = 'Hansen/UMD/Google/USGS/NASA, accessed through Global Forest Watch'
    #   ) %>%
    addLayersControl(
        baseGroups = c("Satellite", "OSM (default)", "Grey"),
        overlayGroups = c("GRID", "POINTS", "SELECT_POLYGON"),
        options = layersControlOptions(position = "topright", collapsed = TRUE))
  })
  

}

runApp(list(ui = ui, server = server), launch.browser = TRUE)
#runApp(list(ui = ui, server = server),host="192.168.xx.xx",port=5013, launch.browser = TRUE)

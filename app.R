# Load Required Packages ----
library(shiny)
library(shinydashboard)
library(leaflet)
library(RCurl)
library(jsonlite)
library(XML)

# Data sources:
webpageURL <- "http://mpk.wroc.pl/jak-jezdzimy/mapa-pozycji-pojazdow"
trackingURL <- "http://pasazer.mpk.wroc.pl/position.php"

## Functions to Get Source Data ----

# Scrape website for up-to-date bus and tram lines 
getLines <- function(web_url) {
  # Download page using RCurl
  webpage <- getURL(web_url)
  # Process escape characters
  webpage <- readLines(tc <- textConnection(webpage)); close(tc)
  # Parse the html tree, ignoring errors on the page
  pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
  # Extract all lines from respective HTML tag (e.g. <li class="bus_line">17</li>)
  allLines <- xpathSApply(pagetree, "//li[@class='bus_line']", xmlValue)
  # Split lines into bus and tram based on number lenght (e.g. tram lines are always 1- or 2-digit number)
  special <- c("A",   "C" ,  "D" ,  "K" ,  "N")
  other <- c("102", "106", "112", "148", "150", "206", "305", "319", "325", "331", "602", "607", "609", "612")
  buslines <- c( special, allLines[!nchar(allLines) %in% c(1,2) & !allLines %in% other] )
  tramlines <- allLines[!allLines %in% buslines]
  # Return the output
  return(list(buslines = buslines, tramlines = tramlines, other = other))
}

# Get tracking info for line(s)
getTrackingData <- function(web_url, lineNumbers) {
  # Initialize CURL handle
  curl <- getCurlHandle()
  # Select bus or tram line(s) in a form of "busList[bus][]" = "17"
  pars <- tryCatch(as.list(setNames(lineNumbers, rep("busList[bus][]",length(lineNumbers)))), warning = function(e) NULL, error = function(e) NULL)
  # send POST request to get current location and coressponding data 
  # convert JSON output into date.frame and return NULL if any error occurs
  output <- tryCatch(fromJSON(postForm(web_url, .params = pars, curl = curl)) , warning = function(e) NULL, error = function(e) NULL)
  if (is.list(output) & is.null(nrow(output))) output <- NULL
  # Change variable names 
  if(!is.null(output) ) names(output)[c(3,4)] <- c("lng", "lat")
  return(output)
}

## Create UI Components (header & body) ----

# Define UI elements for dashboard header
header <- dashboardHeader(
  title = "Wroclaw Public Transport Tracking", titleWidth = 450
)

# Define UI for dashboard body
body <- dashboardBody(
  fluidRow(
    column(width = 4,
       box(width = NULL, status = "info", 
           p("This app allows for real-time tracking of public modes of transportation. 
             Each tram and bus is equipped with a GPS device which sends tracking coordinates in short time intervals."),
           p("To see the current position of bus or tram line, simply choose from the available list below:"),
           if (!url.exists(webpageURL)) {
             tags$p(style = "font-weight:bold; color:red;", "Cannot fetch data for tram and bus lines. 
                    Probably the web service is down. Try again in couple of minutes.")
           } else {
             tagList(
               selectInput(inputId = "tramlines", 
                         label = "Tram line", 
                         choices = c("Choose one" = "", getLines(webpageURL)$tramlines), 
                         multiple = T),
               selectInput(inputId = "buslines", 
                         label = "Bus line", 
                         choices = list("Choose one" = "", "REGULAR" = getLines(webpageURL)$buslines, "OTHER" = getLines(webpageURL)$other),
                         multiple = T)
             )
           },
           p(
             class = "text-muted",
             paste("Note: You can select multiple lines at a time.")
           ),
           actionButton("zoomButton", "Zoom to fit"),
           actionButton("resetButton", "Clear the map", style="float: right;")
       ),
       box(width = NULL, status = "warning", 
           selectInput("interval", "Refresh interval",
                       choices = c(
                         "evey second" = 1,
                         "every 5 seconds" = 5,
                         "every 10 seconds" = 10,
                         "every 30 seconds" = 30,
                         "every minute" = 60
                       ),
                       selected = "10"
           ),
           actionButton("refresh", "Refresh now"),
           p(class = "text-muted",
             br(),
             "Next update in ",  htmlOutput("countDown",inline = T), " seconds"
           )
       )
    ),
    column(width = 8,
      # verbatimTextOutput("temp"),
      if (!url.exists(trackingURL)) { 
        box(width = NULL, status = "danger", tags$p(style = "font-weight:bold; color:red;", "Cannot get vehicle locations. 
          Probably the web service is temporarily unavailable. Please try again later.")
        )
      },
      box(width = NULL, solidHeader = TRUE, footer = h6(em("Source: "), a(href=webpageURL, "Internetowy Serwis Obslugi Pasazera")),
           leafletOutput("transportmap", height = 500)
      )
    )
  )
)

# Combine UI elements into single framework
ui <- dashboardPage(
        header,
        dashboardSidebar(disable = TRUE, width = 400),
        body
)

## Define Server Logic  ----
server <- shinyServer(function(input, output, session) {
   
  # Not used (for testing only)
  #session$onSessionEnded(stopApp)
  #output$temp <- renderPrint({ c(input$tramlines)  })
  
  # Current time + offest based on user choice interval 
  newTime <- reactive({
    interval <- as.numeric(input$interval)
    # Invalidate this reactive after the interval has passed, so that data is
    # fetched again.
    invalidateLater(interval * 1000, session)
    Sys.time() + interval
  }) 
  
  # Countdown timer 
  output$countDown <- renderUI({
    eventTime <- newTime()
    # Reactive timer to update Sys.time() each second
    invalidateLater(1000, session)
    dt <- difftime(eventTime, Sys.time(), units = "secs")
    format(.POSIXct(dt, tz = "GMT"), "%M:%S")
  })  

  # Fetch dataset
  getData <- reactive({
    input$refresh # Refresh if button clicked

    # Get interval 
    interval <- as.numeric(input$interval)
    # Invalidate this reactive after the interval has passed, so that data is
    # fetched again.
    invalidateLater(interval * 1000, session)
    getTrackingData(trackingURL, c(input$tramlines, input$buslines))
  })
  
  # Initialize Leaflet map 
  output$transportmap <- renderLeaflet({
    map <- leaflet() %>%
      setView(lng = 17.03, lat = 51.1, zoom=11) %>%
      # Custom made tile from Mapbox (you need to create an account there first)
      addTiles('https://api.mapbox.com/styles/v1/mapbox/streets-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiaG9sZWsiLCJhIjoiY2l0dG81ajB1MDAwNDJvcW14bWxsdWN3diJ9.Pe6D3DGCxZfZV7ZQp_-CTA')
    map
  })
  
  # Show current position for selected mode of transportation
  observe({
    lines <- c(input$tramlines, input$buslines)
    if (!is.null(lines) & (!is.null(getData()))  ) {
      dataset <- getData()
      # Define custom icons
      transportIcons <- icons(
        iconUrl = ifelse(dataset$type == "tram",
                         "Map-Marker-Marker-Outside-Azure.png",
                         "Map-Marker-Marker-Outside-Pink.png"
        ),
        iconWidth = 48, iconHeight = 48,
        iconAnchorX = 22, iconAnchorY = 22
      )
      # Define custom legend
      customLegend <- as.character(
        tagList(
          tags$img(src="Map-Marker-Marker-Outside-Azure.png", width="30px", height = "30px"),"tram",
          br(),
          tags$img(src="Map-Marker-Marker-Outside-Pink.png", width="30px", height = "30px"),"bus"
          )
      )
      # Update map with current Lat and Lng for selected tram/bus
      leafletProxy("transportmap", data = dataset) %>%  clearMarkers() %>% clearPopups() %>% clearControls() %>%
        addMarkers(lat = ~ lat , lng = ~ lng, 
                   #popup = ~paste(type,"no.",name ), 
                   icon = transportIcons, layerId = ~ k) %>%
        addControl(html = customLegend, position = "bottomright")
    } else {
      leafletProxy("transportmap") %>% clearMarkers() %>% clearPopups() %>% clearControls()
    }
  })
  
  # Define logic for Zoom button
  observeEvent( input$zoomButton, {
    lines <- isolate(c(input$tramlines, input$buslines))
    if (!is.null(lines) & (!is.null(getData()))  ) {
      dataset <- getData()
      leafletProxy(mapId = "transportmap", data = dataset) %>%  
        fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))
     }
  })

  # Define logic for Reset button
  observeEvent( input$reset, {
    # Recreate content of inputs
    updateSelectInput(session, inputId = "tramlines", label = "Tram line", choices = c("Choose one" = "", getLines(webpageURL)$tramlines))
    updateSelectInput(session, inputId = "buslines", label = "Bus line", choices = list("Choose one" = "", "REGULAR" = getLines(webpageURL)$buslines, "OTHER" = getLines(webpageURL)$other))
    # Clear the map
    leafletProxy("transportmap") %>% clearMarkers() %>% clearPopups() %>% clearControls()
  })
  
  
  # Show custom popup on mouse over
  showPopup <- function(k, lat, lng) {
    dataset <- getData()
    selectedVehicle <- dataset[dataset$k == k,]
    content <- as.character(tagList(
      tags$h4(strong(ifelse(selectedVehicle$type =="tram", "Tram", "Bus")), "line: ", strong(toupper(selectedVehicle$name))),
      p(sprintf("VehicleID: %s", as.integer(selectedVehicle$k )))
    ))
    leafletProxy("transportmap") %>% addPopups(lng, lat, content, layerId = k)
  }
  
  # Trigger function on mouse over 
  observe({
    leafletProxy("transportmap") %>% clearPopups()
    event <- input$transportmap_marker_mouseover
    if (is.null(event)) return()
    lines <- c(input$tramlines, input$buslines)
    if (!is.null(lines)) {
    isolate({
      showPopup(event$id, event$lat, event$lng)
    })
    }
  })

})

# Run the application 
shinyApp(ui = ui, server = server)

library(scales)
library(shiny)
library(leaflet)
library(dplyr)
library(tidyverse)

# --- Load & clean data once ---
data <- read.csv("merged_dataset.csv") %>%
  filter(PropType == "Residential") %>%
  mutate(
    Bdrms = as.integer(Bdrms),
    Fbath = as.integer(Fbath),
    Hbath = as.integer(Hbath)
  ) %>%
  filter(
    complete.cases(Bdrms), Bdrms >= 1, Bdrms <= 10,
    complete.cases(Fbath), Fbath >= 1, Fbath <= 10,
    complete.cases(Hbath), Hbath >= 1, Hbath <= 3
  )

# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("#mke_map { border: 10px solid #ccc; }"))
  ),
  titlePanel("Milwaukee Real Estate"),
  sidebarLayout(
    sidebarPanel(
      selectInput("District", "Select District",
        choices  = c("All", unique(sort(data$District))),
        selected = "All"
      ),
      selectInput("Bdrms", "Bedrooms",
        choices = c("All", unique(sort(data$Bdrms)))
      ),
      selectInput("Fbath", "Full Bathrooms",
        choices = c("All", unique(sort(data$Fbath)))
      ),
      selectInput("Hbath", "Half Bathrooms",
        choices = c("All", unique(sort(data$Hbath)))
      ),
      selectInput("Price_range", "Price Range",
        choices  = c("All", "Below 200,000", "200,000 - 500,000",
                     "500,000 - 1,000,000", "Above 1,000,000"),
        selected = "All"
      )
    ),
    mainPanel(
      leafletOutput("mke_map")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {

  filteredData <- reactive({
    data %>%
      filter(
        if (input$District    != "All") District == input$District else TRUE,
        if (input$Bdrms       != "All") Bdrms    == input$Bdrms    else TRUE,
        if (input$Fbath       != "All") Fbath    == input$Fbath    else TRUE,
        if (input$Hbath       != "All") Hbath    == input$Hbath    else TRUE,
        case_when(
          input$Price_range == "Below 200,000"       ~ Sale_price < 200000,
          input$Price_range == "200,000 - 500,000"   ~ Sale_price >= 200000  & Sale_price <= 500000,
          input$Price_range == "500,000 - 1,000,000" ~ Sale_price >= 500000  & Sale_price <= 1000000,
          input$Price_range == "Above 1,000,000"     ~ Sale_price > 1000000,
          TRUE                                        ~ TRUE
        )
      )
  })

  # Base map — markers handled entirely by observe()
  output$mke_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -87.90956, lat = 43.03901, zoom = 10)
  })

  # Reactive marker update
  observe({
    fd <- filteredData()
    leafletProxy("mke_map") %>%
      clearMarkers() %>%
      addCircleMarkers(
        data        = fd,
        lng         = ~Longitude,
        lat         = ~Latitude,
        color       = "blue",
        radius      = 5,
        fillOpacity = 0.8,
        popup       = paste0(
          "Address: ",    fd$Address,
          "<br>Price: $", comma(fd$Sale_price),
          "<br>Bedrooms: ", fd$Bdrms,
          "<br>Bathrooms: ", fd$Fbath + 0.5 * fd$Hbath,
          "<br>Style: ",    fd$Style,
          "<br>Year built: ", fd$Year_Built,
          "<br>Sold Date: ",  fd$Sale_date
        )
      )
  })
}

shinyApp(ui = ui, server = server)
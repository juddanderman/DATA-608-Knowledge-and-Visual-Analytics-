library(ggplot2)
library(leaflet)
library(dplyr)

pqi <- read.csv("../NYS_PQI_2015.csv", stringsAsFactors = FALSE)
pqi <- pqi %>%
  select(PQI_Number, PQI_Name) %>%
  distinct()

fluidPage(
  headerPanel("Population Health in New York State"),

  fluidRow(
    column(12,
           selectInput("pqi",
                       "Prevention Quality Indicator (PQI)", 
                       pqi$PQI_Name, 
                       selected = pqi$PQI_Name[1])
    )
    ,
    textOutput("var")
  ),
  
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Density and Scatter Plots", 
                       plotOutput("plot1", height = "250px"),
                       plotOutput("plot2", height = "250px")),
              tabPanel("Chloropleth Maps", 
                       fluidRow(
                         splitLayout(cellWidths = c("50%", "50%"), 
                                     leafletOutput("map1"),
                                     leafletOutput("map2"))
                         )
                       )
              )
)

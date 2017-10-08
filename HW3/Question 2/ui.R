library(plotly)

url <- "https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv"
mortal <- read.csv(url)

fluidPage(
  headerPanel("US Mortality Rates by Cause of Death"),
  sidebarPanel(
    selectInput("cause", "Cause", sort(unique(mortal$ICD.Chapter)), 
                selected = "Certain conditions originating in the perinatal period"),
    selectInput("state", "State", sort(unique(mortal$State)),
                selected = "AK")
  ),
  mainPanel(
    plotlyOutput("plot1", height = "600px")
  )
)
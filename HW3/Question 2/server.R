library(ggplot2)
library(plotly)
library(dplyr)

url <- "https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv"
mortal <- read.csv(url)

us <- mortal %>% group_by(ICD.Chapter, Year) %>%
  summarize(Total_Deaths = sum(Deaths),
            Total_Population = sum(Population),
            Total_Rate = Total_Deaths * 100000 / Total_Population)

function(input, output, session) {
  
  selectedData <- reactive({
    sliceMort <- mortal %>%
      filter(ICD.Chapter == input$cause & State == input$state)
  })
  
  usData <- reactive({
    usMort <- us %>%
      filter(ICD.Chapter == input$cause)
  })
  
  colors <- hcl(h = seq(15, 375, length = 3), l = 65, c = 100)[1:2]
  
  output$plot1 <- renderPlotly({
    p <- plot_ly(selectedData(), x = ~Year, y = ~Crude.Rate,
            type = "scatter", mode = "lines+markers", name = input$state,
            line = list(color = colors[1])) %>%
      layout(xaxis = list(range = c(min(mortal$Year), max(mortal$Year))),
             yaxis = list(title = "Crude Rate (Deaths per 100000)")) %>%
      add_trace(data = usData(),
                name = "US", x = ~Year, y = ~Total_Rate, mode = "lines+markers",
                line = list(color = colors[2], dash = "dash"))
    p$elementId <- NULL
    p
  })
}
library(plotly)
library(ggplot2)
library(dplyr)

url <- "https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv"
mortal <- read.csv(url)

function(input, output, session) {
  # get ggplot default colors for 2 color plot
  colors <- hcl(h = seq(15, 375, length = 3), l = 65, c = 100)[1:2]
  
  selectedData <- reactive({
    sliceMort <- mortal %>%
      filter(ICD.Chapter == input$cause, Year == input$yr) 
    if (nrow(sliceMort) > 0) {
      sliceMort <- sliceMort %>%
        arrange(desc(Crude.Rate)) %>%
        mutate(State = factor(State, State),
               Color = factor(c(colors[1], rep(colors[2], length(State) - 1)), 
                              levels = colors))
    } 
  })
  
  output$plot1 <- renderPlotly({
    if (length(selectedData()$State) > 0) {
      xmax <- 1.1 * max(selectedData()$Crude.Rate)
      xmax <- ifelse(xmax > 0, xmax, 1)
      
      p <- plot_ly(selectedData(), x = ~Crude.Rate, y = ~State, 
                   type = "bar", orientation = "h", marker = list(color = ~Color)) %>%
        layout(xaxis = list(title = "Crude Rate (Deaths per 100000)",
                            range = c(0, xmax)), 
               yaxis = list(autorange = "reversed"))
      p$elementId <- NULL
      p
    } else {
      p <- ggplot() +
        coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
        annotate("text", label = "No Data Available", x = 50, y = 50) +
        theme(line = element_blank(),
              text = element_blank(),
              title = element_blank(),
              panel.background = element_blank())
      p <- ggplotly(p)
      p$elementId <- NULL
      p
    }
  })
}
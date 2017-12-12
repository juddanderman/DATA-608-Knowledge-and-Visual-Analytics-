library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(leaflet)
library(rgdal)
library(scales)
library(plotly)

health <- read.csv("../NYS_PQI_2015.csv", stringsAsFactors = FALSE)
hh_income <- read.csv("../NYS_2015_HH_Income.csv", stringsAsFactors = FALSE)
nys <- readOGR(dsn = "../nys_2016_zcta5", layer = "nys_2016_zcta5", stringsAsFactors = FALSE)

pqi <- health %>%
  select(PQI_Number, PQI_Name) %>%
  distinct()

health <- health %>%
  mutate(Patient_Zipcode = str_pad(Patient_Zipcode, width = 5, side = "left", pad = "0"))
  
health_wide <- health %>%
  select(-PQI_Name) %>%
  spread(PQI_Number, Observed_Rate_Per_100000_People)

hh_income <- hh_income %>%
  mutate(ZCTA5 = str_pad(ZCTA5, width = 5, side = "left", pad = "0"))

nys <- merge(nys, hh_income, by.x = "ZCTA5CE10", by.y = "ZCTA5")
nys <- merge(nys, health_wide, by.x = "ZCTA5CE10", by.y = "Patient_Zipcode")

function(input, output, session) {
  healthData <- reactive({
    sliced <- health %>%
      filter(PQI_Name == input$pqi)
  })
  
  wealthData <- reactive({
    combined <- inner_join(healthData(), hh_income, by = c("Patient_Zipcode" = "ZCTA5"))
  })
  
  output$plot1 <- renderPlot({
    title <- str_replace_all(pqi[pqi$PQI_Name == input$pqi, ]$PQI_Number, "_", " ")
    
    p1 <- ggplot(healthData(), aes(Observed_Rate_Per_100000_People)) +
      geom_histogram(bins = 30, aes(y = ..density..)) +
      geom_line(stat = "density", color = "black") +
      labs(title = title, 
           x = "Observed rate per 100000 people", 
           y = "Density")
    p1
  })
  
  output$plot2 <- renderPlot({
    title <- str_replace_all(pqi[pqi$PQI_Name == input$pqi, ]$PQI_Number, "_", " ")
    title <- paste0(title, " vs. Median Household Income")
    
    p2 <- ggplot(wealthData(), aes(Median_Household_Income, Observed_Rate_Per_100000_People)) +
      geom_point() +
      geom_smooth(method = 'lm', color = "red", se = FALSE) +
      scale_x_continuous(labels = dollar_format(prefix = "$")) +
      labs(title = title, 
           x = "Median household income (2015)", 
           y = "Observed rate per 100000 people")
    p2
  })

  output$map1 <- renderLeaflet({
    bins1 <- unique(quantile(hh_income$Median_Household_Income, probs = seq(0, 1, 0.1), na.rm = TRUE))
    pal1 <- colorBin("YlOrRd", domain = hh_income$Median_Household_Income, bins = bins1)
    
    leaflet(data = nys[!is.na(nys$Median_Household_Income), ]) %>% 
      addTiles() %>% 
      addPolygons(fillColor = ~pal1(Median_Household_Income), 
                  weight = 1, 
                  opacity = 1,
                  color = "white",
                  fillOpacity = 0.7) %>% 
      addLegend(pal = pal1, 
                values = ~Median_Household_Income,
                labFormat = labelFormat(prefix = "$", between = " &ndash; $", digits = 0),
                opacity = 0.7, 
                title = "Median household income",
                position = "bottomright")
  })
  
  output$map2 <- renderLeaflet({
    leg_title <- str_replace_all(pqi[pqi$PQI_Name == input$pqi, ]$PQI_Number, "_", " ")
    
    bins2 <- unique(quantile(
      healthData()$Observed_Rate_Per_100000_People, 
      probs = seq(0, 1, 0.1),
      na.rm = TRUE))
    pal2 <- colorBin("YlGnBu", domain = healthData()$Observed_Rate_Per_100000_People, bins = bins2)
    
    col <- pqi[pqi$PQI_Name == input$pqi, ]$PQI_Number
    
    leaflet(data = nys[!is.na(eval(parse(text = paste0("nys$", col)))), ]) %>% 
      addTiles() %>% 
      addPolygons(fillColor = ~pal2(eval(parse(text = col))), 
                  weight = 1, 
                  opacity = 1,
                  color = "white",
                  fillOpacity = 0.7) %>% 
      addLegend(pal = pal2, 
                values = ~PQI_01,
                labFormat = labelFormat(digits = 2),
                opacity = 0.7, 
                title = leg_title,
                position = "bottomright")
  })
  
  observe({
    m1_zoom <- input$map1_zoom
    m1_bnds <- input$map1_bounds
    leafletProxy("map2") %>%
      setView(m1_zoom,
              lng = mean(c(m1_bnds$east, m1_bnds$west)),
              lat = mean(c(m1_bnds$north, m1_bnds$south))
      )
  })
  
  # observe({
  #   m2_zoom <- input$map2_zoom
  #   m2_bnds <- input$map2_bounds
  #   leafletProxy("map1") %>%
  #     setView(m2_zoom,
  #             lng = mean(c(m2_bnds$east, m2_bnds$west)),
  #             lat = mean(c(m2_bnds$north, m2_bnds$south))
  #     )
  # })
  
}
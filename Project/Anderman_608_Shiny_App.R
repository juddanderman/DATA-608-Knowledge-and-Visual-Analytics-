library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(leaflet)
library(rgdal)
library(scales)
library(plotly)
library(DT)
library(shiny)

# read in data

health <- read.csv("https://raw.githubusercontent.com/juddanderman/DATA-608-Knowledge-and-Visual-Analytics-/master/Project/Data/Hospital_Inpatient_Prevention_Quality_Indicators__PQI__for_Adult_Discharges_by_Zip_Code__SPARCS___Beginning_2009.csv",
                   stringsAsFactors = FALSE)
colnames(health) <- str_replace_all(colnames(health), "100\\.000", "100000")
colnames(health) <- str_replace_all(colnames(health), "\\.", "_")
health <- health %>%
  filter(!PQI_Number %in% c("PQI_13", "PQI_96", "PQI_97", "PQI_98") &
           Year == 2015)

acs_wide <- read.csv("https://raw.githubusercontent.com/juddanderman/DATA-608-Knowledge-and-Visual-Analytics-/master/Project/Data/ACS_5yr_2015_NYS.csv", 
                     stringsAsFactors = FALSE)

nys <- readOGR(dsn = "https://raw.githubusercontent.com/juddanderman/DATA-608-Knowledge-and-Visual-Analytics-/master/Project/Data/nys_2016_zcta5.GeoJSON", 
               stringsAsFactors = FALSE)

pqi_info <- read.csv("https://raw.githubusercontent.com/juddanderman/DATA-608-Knowledge-and-Visual-Analytics-/master/Project/Data/pqi_info_table.csv",
                     stringsAsFactors = FALSE)

pqi_info <- pqi_info %>%
  dplyr::rename_all(funs(str_replace_all(., "\\.", " ")))

acs_vars <- read.csv("https://raw.githubusercontent.com/juddanderman/DATA-608-Knowledge-and-Visual-Analytics-/master/Project/Data/My_ACS_Variables.csv",
                     stringsAsFactors = FALSE)
acs_vars <- acs_vars %>%
  select(name, description, concept, group)

pqi <- health %>%
  filter(Year == 2015) %>%
  select(PQI_Number, PQI_Name) %>%
  distinct()

health <- health %>%
  select(-Expected_Rate_Per_100000_People) %>%
  filter(Patient_Zipcode != "STATEWIDE") %>%
  mutate(Patient_Zipcode = str_pad(Patient_Zipcode, width = 5, side = "left", pad = "0"))

health_wide <- health %>%
  select(-PQI_Name) %>%
  spread(PQI_Number, Observed_Rate_Per_100000_People)

acs_wide <- acs_wide %>%
  mutate(ZCTA5 = str_pad(ZCTA5, width = 5, side = "left", pad = "0"))

acs <- acs_wide %>%
  gather(Key, Value, DP02_0015E:DP05_0066PE)

nys <- merge(nys, acs_wide, by.x = "ZCTA5CE10", by.y = "ZCTA5")
nys <- merge(nys, subset(health_wide, Year == 2015), by.x = "ZCTA5CE10", by.y = "Patient_Zipcode")

text <- readLines("https://raw.githubusercontent.com/juddanderman/DATA-608-Knowledge-and-Visual-Analytics-/master/Project/Data/Overview.txt")
text <- paste0(text[text != ""], collapse = "<br><br>")

init_map <- leaflet(data = nys) %>%
  addTiles() %>%
  addPolygons()

# for testing:
# combined <- inner_join(acs %>% filter(Key == "DP02_0015E"),
#                        health %>% filter(PQI_Number == "PQI_01"),
#                        by = c("ZCTA5" = "Patient_Zipcode"))

ui <- fluidPage(theme = "https://bootswatch.com/3/readable/bootstrap.min.css",
                # useShinyjs(),
                # extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click-scatter', 'null'); }"),
                # actionButton("reset", "Reset plotly click value"),
                # plotlyOutput("plot"),
                # verbatimTextOutput("clickevent"),
                
                headerPanel("Population Health in New York State"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput("pqi",
                                "Prevention Quality Indicator (PQI)", 
                                pqi$PQI_Name, 
                                selected = pqi$PQI_Name[1],
                                width = "100%"),
                    uiOutput("pqi_filt"),
                    selectInput("acs_desc",
                                "2015 American Community Survey 5-Year Estimate", 
                                acs_vars$description, 
                                selected = acs$description[1],
                                width = "100%"),
                    uiOutput("acs_filt"),
                    actionButton("button", "Reset map")
                  ),
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("Background",
                                         htmlOutput("background"),
                                         br(),
                                         tags$div("For more information see: "),
                                         tags$a(href="http://www.qualityindicators.ahrq.gov/modules/pqi_resources.aspx", 
                                                "Agency for Healthcare Quality and Research (AHRQ): PQI Resources"),
                                         br(),
                                         tags$a(href="https://www.census.gov/programs-surveys/acs/about.html",
                                                "United States Census Bureau: What is the American Community Survey?"),
                                         br(),
                                         tags$a(href="https://health.data.ny.gov/Health/Hospital-Inpatient-Prevention-Quality-Indicators-P/5q8c-d6xq",
                                                "Health Data NY: PQIs for Adult Discharges by ZIP Code beginning 2009")
                                ),
                                tabPanel("Correlation Matrix",
                                         fluidRow(
                                           column(12, 
                                                  align = "center",
                                                  plotOutput("corr_plot", width = "90%", height = "auto")
                                           )
                                         )
                                ),
                                tabPanel("Density Plots",
                                         fluidRow(
                                           splitLayout(cellWidths = c("50%", "50%"),
                                                       plotlyOutput("hist_pqi"),
                                                       plotlyOutput("hist_acs"))
                                         )
                                ),
                                tabPanel("Scatter Plot & Chloropleth",
                                         fluidRow(
                                           splitLayout(cellWidths = c("50%", "50%"), 
                                                       plotlyOutput("scat_plot"),
                                                       leafletOutput("map1"))
                                         ),
                                         br(),
                                         htmlOutput("click")
                                ),
                                tabPanel("Data by ZIP Code",
                                         dataTableOutput("comb_tab")
                                ),
                                tabPanel("PQI Information",
                                         dataTableOutput("pqi_table")
                                ),
                                tabPanel("ACS Data Dictionary",
                                         dataTableOutput("acs_dict")
                                )
                    )
                  ),
                  position = "right"
                )
)

server <- function(input, output, session) {
  healthDataInit <- reactive({
    sliced <- health %>%
      filter(PQI_Name == input$pqi)
  })
  
  acsDataInit <- reactive({
    sliced <- acs %>%
      filter(Key == acs_vars[acs_vars$description == input$acs_desc, ]$name)
  })
  
  healthData <- reactive({
    sliced <- health %>%
      filter(PQI_Name == input$pqi &
               Observed_Rate_Per_100000_People >= input$pqi_filt[1] &
               Observed_Rate_Per_100000_People <= input$pqi_filt[2])
  })
  
  acsData <- reactive({
    sliced <- acs %>%
      filter(Key == acs_vars[acs_vars$description == input$acs_desc, ]$name &
               Value >= input$acs_filt[1] &
               Value <= input$acs_filt[2])
  })
  
  output$pqi_filt <- renderUI({
    sliderInput("pqi_filt", 
                paste("Range", str_replace_all(pqi[pqi$PQI_Name == input$pqi, ]$PQI_Number, "_", " ")), 
                min = floor(min(healthDataInit()$Observed_Rate_Per_100000_People, na.rm = TRUE)),
                max = ceiling(max(healthDataInit()$Observed_Rate_Per_100000_People, na.rm = TRUE)),
                value = c(floor(min(healthDataInit()$Observed_Rate_Per_100000_People, na.rm = TRUE)),
                          ceiling(max(healthDataInit()$Observed_Rate_Per_100000_People, na.rm = TRUE)))
    )
  })
  
  output$acs_filt <- renderUI({
    sliderInput("acs_filt", 
                paste("Range", acs_vars[acs_vars$description == input$acs_desc, ]$name), 
                min = floor(min(acsDataInit()$Value, na.rm = TRUE)),
                max = ceiling(max(acsDataInit()$Value, na.rm = TRUE)),
                value = c(floor(min(acsDataInit()$Value, na.rm = TRUE)),
                          ceiling(max(acsDataInit()$Value, na.rm = TRUE)))
    )
  })
  
  output$background = renderText({
    text
  })
  
  comb_data <- reactive({
    combined <- inner_join(acsData(), 
                           subset(healthData(), Year == 2015), 
                           by = c("ZCTA5" = "Patient_Zipcode")) %>%
      select(ZCTA5, Value, Observed_Rate_Per_100000_People)
    colnames(combined) <- c("ZIP Code", input$acs_desc, input$pqi)
    combined <- as.data.frame(combined, stringsAsFactors = FALSE)
  })
  
  output$comb_tab = renderDataTable(
    comb_data()
  )
  
  output$pqi_table = renderDataTable(
    pqi_info,
    options = list(pageLength = 5, dom = "ftp")
  )
  
  output$acs_dict = renderDataTable(
    acs_vars,
    colnames = paste0(
      toupper(substring(colnames(acs_vars), 1, 1)), substring(colnames(acs_vars), 2)),
    options = list(pageLength = 5, dom = "ftp")
  )
  
  output$corr_plot <- renderPlot({
    wide_df <- inner_join(acs_wide, 
                          subset(health_wide, Year == 2015), 
                          by = c("ZCTA5" = "Patient_Zipcode"))
    wide_df <- wide_df %>%
      select(DP02_0015E:DP05_0066PE, PQI_01:PQI_S03)
    
    corr_mat <- round(cor(wide_df, use = "pairwise.complete.obs"), 2)
    melted_corr_mat <- corr_mat %>% 
      reshape2::melt(corr_mat, 
                     varnames = c("variable_1", "variable_2"), 
                     value.name = "correlation")
    
    ggplot(melted_corr_mat, aes(variable_1, variable_2, fill = correlation)) +
      geom_tile(color = "white") +
      scale_y_discrete(limits = rev(levels(melted_corr_mat$variable_2))) +
      theme_minimal() + 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1, 1), space = "Lab") +
      labs(title = "Pearson correlation coefficient matrix",
           x = "",
           y = "",
           fill = "Correlation") +
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      coord_fixed()
  },
  height = function() {
    session$clientData$output_corr_plot_width
  }
  )
  
  output$hist_pqi <- renderPlotly({
    xlab <- str_replace_all(pqi[pqi$PQI_Name == input$pqi, ]$PQI_Number, "_", " ")
    xlab <- paste0(xlab, ": ", "Observed rate per 100,000 people")
    
    df <- healthData()[complete.cases(healthData()), ]
    
    fit <- density(df$Observed_Rate_Per_100000_People)
    
    h <- plot_ly(data = df, 
                 x = ~Observed_Rate_Per_100000_People,
                 type = "histogram",
                 histnorm = "probability density",
                 showlegend = FALSE,
                 hoverinfo = "none") %>%
      layout(xaxis = list(title = xlab),
             yaxis = list(title = "Density")) %>%
      add_trace(x = ~fit$x, 
                y = ~fit$y, 
                type = "scatter", 
                mode = "lines",
                line = list(color = "red"),
                hoverinfo = "none")
    h$elementId <- NULL
    h
  })
  
  output$hist_acs <- renderPlotly({
    xlab <- input$acs_desc
    
    df <- acsData()[complete.cases(acsData()), ]
    
    fit <- density(df$Value)
    
    h <- plot_ly(data = df, 
                 x = ~Value,
                 type = "histogram",
                 histnorm = "probability density",
                 showlegend = FALSE,
                 hoverinfo = "none") %>%
      layout(xaxis = list(title = xlab),
             yaxis = list(title = "Density")) %>%
      add_trace(x = ~fit$x, 
                y = ~fit$y, 
                type = "scatter", 
                mode = "lines",
                line = list(color = "red"),
                hoverinfo = "none")
    h$elementId <- NULL
    h
  })
  
  output$scat_plot <- renderPlotly({
    ylab <- str_replace_all(pqi[pqi$PQI_Name == input$pqi, ]$PQI_Number, "_", " ")
    ylab <- paste0(ylab, ": ", "Observed rate per 100,000 people")
    
    combined <- inner_join(acsData(),
                           subset(healthData(), Year == 2015),
                           by = c("ZCTA5" = "Patient_Zipcode"))
    combined <- combined[complete.cases(combined), ]
    
    p <- plot_ly(data = combined, x = ~Value, source = "scatter") %>%
      add_markers(y = ~Observed_Rate_Per_100000_People, 
                  text = ~paste("ZIP Code:", combined$ZCTA5),
                  hoverinfo = "text",
                  showlegend = FALSE,
                  key = combined$ZCTA5) %>%
      layout(xaxis = list(title = input$acs_desc), 
             yaxis = list(title = ylab)) %>%
      add_lines(y = ~fitted(lm(Observed_Rate_Per_100000_People ~ Value)),
                name = "linear", hoverinfo = "skip", line = list(color = "gray")) %>%
      add_lines(y = ~fitted(loess(Observed_Rate_Per_100000_People ~ Value)), 
                name = "loess", hoverinfo = "skip", line = list(color = "red"))
    p$elementId <- NULL
    p
  })
  
  output$click <- renderText({
    d <- event_data("plotly_click", source = "scatter")
      
    if (!is.null(d)) {
      leafletProxy("map1") %>%
        setView(zoom = 10,
                lng = mean(c(nys[nys$ZCTA5CE10 == d$key, ]@bbox[1, 1],
                             nys[nys$ZCTA5CE10 == d$key, ]@bbox[1, 2])),
                lat = mean(c(nys[nys$ZCTA5CE10 == d$key, ]@bbox[2, 1],
                             nys[nys$ZCTA5CE10 == d$key, ]@bbox[2, 2]))
        )
    }
    "<b>Click on a scatter plot point to zoom to ZIP Code</b>"
  })
  
  output$map1 <- renderLeaflet({
    combined <- inner_join(acsDataInit(),
                           subset(healthDataInit(), Year == 2015),
                           by = c("ZCTA5" = "Patient_Zipcode"))
    combined <- combined[complete.cases(combined), ]
    
    leg_title <- str_replace_all(pqi[pqi$PQI_Name == input$pqi, ]$PQI_Number, "_", " ")
    leg_title <- paste(leg_title, " / ", acs_vars[acs_vars$description == input$acs_desc, ]$name)
    
    bins <- unique(quantile(
      rescale(
        combined$Observed_Rate_Per_100000_People / 
          rescale(combined$Value, to = c(1, 100)),
        to = c(0, 1)), 
      probs = seq(0, 1, 0.1),
      na.rm = TRUE))
    pal <- colorBin("YlOrRd", 
                    domain = rescale(
                      combined$Observed_Rate_Per_100000_People / 
                        rescale(combined$Value, to = c(1, 100)),
                      to = c(0, 1)), 
                    bins = bins)
    
    col1 <- pqi[pqi$PQI_Name == input$pqi, ]$PQI_Number
    col2 <- acs_vars[acs_vars$description == input$acs_desc, ]$name
    
    leaflet(data = nys[!is.na(eval(parse(text = paste0("nys$", col1)))) &
                         !is.na(eval(parse(text = paste0("nys$", col2)))), ]) %>% 
      addTiles() %>% 
      addPolygons(fillColor = ~pal(rescale(
        eval(parse(text = col1)) / rescale(eval(parse(text = col2)), to = c(1, 100)),
        to = c(0, 1))), 
        weight = 0.5, 
        opacity = 1,
        color = "white",
        fillOpacity = 0.7) %>% 
      addLegend(pal = pal, 
                values = ~rescale(
                  eval(parse(text = col1)) / 
                    rescale(eval(parse(text = col2)), to = c(1, 100)),
                  to = c(0, 1)),
                # labels = paste0( seq(10, 100, 10), "th %"),
                labFormat = labelFormat(digits = 3),
                opacity = 0.7, 
                title = leg_title,
                position = "bottomleft")
  })
  
  observeEvent(input$button, {
    leafletProxy("map1") %>%
      fitBounds(lat1 = init_map$x$limits$lat[1],
                lat2 = init_map$x$limits$lat[2],
                lng1 = init_map$x$limits$lng[1],
                lng2 = init_map$x$limits$lng[2])
  })
}

shinyApp(ui, server)
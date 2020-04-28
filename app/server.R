# Install packages if needed

if (!require(factoextra)) install.packages('factoextra')
library(factoextra)

if (!require(heatmaply)) install.packages('heatmaply')
library(heatmaply)

if (!require(shinyHeatmaply)) install.packages('shinyHeatmaply')
library(shinyHeatmaply)

if (!require(spMaps)) install.packages('spMaps')
library(spMaps)

if (!require(shiny)) install.packages('shiny')
library(shiny)

if (!require(leaflet)) install.packages('leaflet')
library(leaflet)

if (!require(plotly)) install.packages('plotly')
library(plotly)

# Restore the object
europeSp <- readRDS(file = "../data/europeSp.rds")
rownames(europeSp@data) <- europeSp@data$name


shinyServer(function(input, output) {
  
  #For each output -> one renderFunction
  
  # Tap 1
  
  observe({
    
    k <- input$quantile
    
    res <- eclust(scale(europeSp@data[,c(3,4,5,6,7,8,9)]), "hclust", k = k)
    europeSp$Cluster <- res$cluster
    
    pal <- colorNumeric(
      palette = "Blues", 
      domain = europeSp@data[, "Cluster"], n = k)
    
    leafletProxy("clusterMap", data = europeSp ) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.5, fillOpacity = 1,
                  fillColor = ~pal(europeSp@data[,"Cluster"]), group = "cluster") 
  })
  
  
  ## Tap3:
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(
      palette = "viridis",
      domain = europeSp@data[,input$var])
  })
  
  colorpalQuantile <- reactive({
    colorQuantile(
      palette = "Blues", 
      domain = europeSp@data[,input$var], n = input$quantile)
  })
  
  labels <- reactive({
    sprintf(
      "<strong>%s</strong><br/>%g %s",
      europeSp$name, europeSp@data[,input$var], input$var
    ) %>% lapply(htmltools::HTML)
  })
  
  # Only aspects of the map that won't need to change dynamically
  output$mymap <- renderLeaflet({
    
    leaflet(europeSp) %>%
      # addTiles() %>%
      # addProviderTiles("Wikimedia") %>%
      addTiles(
        urlTemplate = "https://tile.thunderforest.com/transport/{z}/{x}/{y}.png?apikey=e1613b03828549ef8dfd7966e356605c",
        attribution = "&copy; <a href='http://www.thunderforest.com/'>Thunderforest</a>,  &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
        options = tileOptions(variant='transport-dark', apikey = 'e1613b03828549ef8dfd7966e356605c')
      ) %>%
      setView(lng = 14.43, lat = 55.07, zoom = 3)
    
  })
  
  # Incremental changes to the map (in this case, recoloring the countries when a diferent var is chosen)
  observe({
    
    filterBy <- input$var
    labels <- labels()
    
    if(input$showQuantile){
      
      pal <- colorpalQuantile()
      
      m <- leafletProxy("mymap", data = europeSp ) 
      m <- addPolygons(m, stroke = FALSE, smoothFactor = 0.5, fillOpacity = 1,
                       fillColor = ~pal(europeSp@data[,input$var]), dashArray = "*", color = "white", weight = "2", 
                       highlight = highlightOptions(fillOpacity = 0.7, bringToFront = TRUE),
                       label = labels,
                       labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px",direction = "auto"),
                       group = "poligon"
      )
    }
    else{
      
      pal <- colorpal()
      
      m <- leafletProxy("mymap", data = europeSp ) 
      m <- addPolygons(m, stroke = FALSE, smoothFactor = 0.5, fillOpacity = 1,
                       fillColor = ~pal(europeSp@data[,input$var]), dashArray = "", color = "white", weight = "2", 
                       highlight = highlightOptions(fillOpacity = 0.7, bringToFront = TRUE),
                       label = labels, 
                       labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"),
                       group = "poligon"
      )
    }
    
  })
  
  
  
  # Recreating the legend as needed.
  observe({
    
    filterBy <- input$var
    pal <- colorpal()
    proxy <- leafletProxy("mymap", data = europeSp@data)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      
      if(input$showQuantile){
        pal <- colorpalQuantile()
        proxy %>% addLegend(position = "bottomright",
                            pal = pal, values = ~europeSp@data[,filterBy], title = filterBy)
      }
      
      else{
        pal <- colorpal()
        proxy %>% addLegend(position = "bottomright",
                            pal = pal, values = ~europeSp@data[,filterBy], title = filterBy)
        
      }
    }
  })
  
  
  ## Tap2:

  output$table <- DT::renderDataTable({
    DT::datatable(europeSp@data[input$countries, input$vars, drop = FALSE], options =
                    list(pageLength = 15, scrollX=TRUE, lengthMenu = c(0, 5, 10, 15, 20, 25, 30)))
  })

  output$heatmap <- renderPlotly ({
    europeS <- t(europeSp@data[,c(3:11)])
    print(europeS[input$vars, input$countries])
    heatmaply(europeS[input$vars, input$countries], scale = "row",
              k_col = input$k_col, k_row = input$k_row, main= "Heat Map of the Dataset", ylab = "Variables", xlab = "Countries",
              width = 1000, height = 600, hide_colorbar = TRUE, grid_gap = 1)
  })
  

  ## Tap3:
  
  output$corrplot <- renderPlot({
    
    varScaled1 <- scale(europeSp@data[,input$var1])
    varScaled2 <- scale(europeSp@data[,input$var2])
    
    ggplot(europeSp@data, aes_string(x = varScaled1, y = varScaled2)) + 
      geom_point() +
      stat_smooth(method = "lm", col = "#206BB1", se=input$confidence)
    
  })
  
  output$corrplotValues <- renderTable({

    varScaled1 <- scale(europeSp@data[,input$var1])
    varScaled2 <- scale(europeSp@data[,input$var2])
    
    
    lm <- lm(varScaled1 ~ varScaled2)
    
    data.frame( 
      Parameter = c("R-squares", "Intercept", "Slope", "P-value"),
      
     Value = c(as.character(signif(summary(lm)$r.squared, 5)),
        as.character(signif(lm$coef[[1]],5 )),
        as.character(signif(lm$coef[[2]], 5)),
        as.character(signif(summary(lm)$coef[2,4], 5))))


  })
  
  
})

library(shiny)
library(leaflet)
library(plyr)
library(RCurl)
library(stringr)
library(pdftools)
library(dplyr)
library(ggmap)
library(ggplot2)
library(maps)
library(googleVis)
library(sp)
library(data.table)
library(plotly)
library(magrittr)
library(rpivotTable)
library(dygraphs)
library(xts)
library(lubridate)



#Arrest.Data <- read.csv("Arrest.Data.csv", stringsAsFactors = FALSE)

Arrest.Data$Arrest.Date <- mdy_hm(Arrest.Data$Arrest.Date, tz = "EST")

## renderLeaflet() is used at server side to render the leaflet map 
shinyServer(function(input, output) {
  
  Arrest.Data <- subset(Arrest.Data, !is.na(Arrest.Date))
  
  test <- reactive({
    
    Arrest.Data <- subset(Arrest.Data, as.Date(Arrest.Data$Arrest.Date) >= input$date1[1] & as.Date(Arrest.Data$Arrest.Date) <= input$date1[2]) %>% 
          subset(hour(Arrest.Data$Arrest.Date) >= input$time[1] & hour(Arrest.Data$Arrest.Date) <= input$time[2])
    if(!is.null(input$Charges)){
      if(input$Charges == "All"){
        Arrest.Data
      }
      else{
        Arrest.Data <- subset(Arrest.Data, Charges %in% as.character(input$Charges))
          }
    }
    
        })
  
  # http://www.treselle.com/blog/crime-analysis-with-shiny-r/
  
  output$Data <- renderDataTable({
    
    Arrest.Data
  })
  output$mymap <- renderLeaflet({
    # define the leaflet map object
    if(input$graph == 'Clusters'){
    leaflet(data = test() ) %>% 
      addTiles() %>% 
      setView(-72.69342, 41.7591, zoom = 13) %>% addMarkers( ~Arrest.long, ~Arrest.lat, popup = paste("<b>","Arrest.Date: ","</b>", test()$Arrest.Date,"<br>",
                                                              "<b>","Arrest.location: ","</b>", test()$Arrest.formatted_address, "<br>",
                                                              "<b>","Charges: ","</b>", test()$Charges, "<br>",
                                                              "<b>","Bond:","</b>", test()$Bond, "<br>",
                                                              "<b>","Release.Date","</b>", test()$Release.Date, "<br>",
                                                              "<b>","Suspect.DOB: ","</b>", test()$Suspect.DOB, "<br>",
                                                              "<b>","Published.Date: ","</b>",test()$Published.Date),clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE, removeOutsideVisibleBounds = TRUE))}
    else{
      leaflet(data = test() ) %>% 
        addTiles() %>% 
        setView(-72.69342, 41.7591, zoom = 13) %>% addMarkers( ~Arrest.long, ~Arrest.lat, popup = paste("<b>","Arrest.Date: ","</b>", test()$Arrest.Date,"<br>",
                                                                                          "<b>","Arrest.location: ","</b>", test()$Arrest.formatted_address, "<br>",
                                                                                          "<b>","Charges: ","</b>", test()$Charges, "<br>",
                                                                                          "<b>","Bond:","</b>", test()$Bond, "<br>",
                                                                                          "<b>","Release.Date","</b>", test()$Release.Date, "<br>",
                                                                                          "<b>","Suspect.DOB: ","</b>", test()$Suspect.DOB, "<br>",
                                                                                          "<b>","Published.Date: ","</b>",test()$Published.Date))
    }
    
  })
  
  

  
  output$summary <- renderPlotly({
    
      temp <- tally(group_by(Arrest.Data, hour(Arrest.Data$Arrest.Date)))
      colnames(temp) <- c("Time", "Count")
      
    plot_ly(temp, x = temp$Time, y = temp$Count)
  })

  output$summary2 <- renderPlotly({
    
    temp <- tally(group_by(Arrest.Data,weekdays(Arrest.Data$Arrest.Date, abbreviate = FALSE)))
    colnames(temp) <- c("Weekdays", "Count")
    temp$Weekdays <- factor(temp$Weekdays, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
    temp <- arrange(temp, Weekdays)
    plot_ly(temp, x = temp$Weekdays, y = temp$Count)
  })
  
  output$trend <- renderPlotly({
    trend2 <- Arrest.Data %>% group_by(as.Date(Arrest.Date)) %>% summarize(n = n())
    colnames(trend2) <- c("Date", "Count")
    plot_ly(trend2) %>% add_lines(x = trend2$Date, y = trend2$Count)
  })
  
  output$table <- renderDataTable({
    
    fnl2 <- Arrest.Data %>% group_by(Month = as.Date(Arrest.Date, format = "%y %B")) %>%
      summarize(n = n())
  })
  
 
    
})












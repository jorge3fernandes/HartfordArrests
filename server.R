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

colnames((Arrest.Data))
as.POSIXct(Arrest.Data$Arrest.Date, "%b/%d/%y %h:%m")
mdy_hm(Arrest.Data$Arrest.Date, tz = "EST")
Arrest.Data$Arrest.Date <- mdy_hm(Arrest.Data$Arrest.Date, tz = "EST")

## renderLeaflet() is used at server side to render the leaflet map 
shinyServer(function(input, output) {
  
  Arrest.Data <- subset(Arrest.Data, !is.na(Arrest.Date))
  
  test <- reactive({
    Arrest.Data <- subset(Arrest.Data, ymd_hms(Arrest.Data$Arrest.Date) >= input$date1[1] & mdy_hm(Arrest.Data$Arrest.Date <= input$date1[2])) %>% subset(hour(Arrest.Data$Arrest.Date) >= input$time[1] & hour(Arrest.Data$Arrest.Date) <= input$time[2])
    if(!is.null(input$Charges)){
      if(input$Charges == ""){
        Arrest.Data
      }
      else{
        Arrest.Data <- subset(Arrest.Data, Charges %in% as.character(input$Charges))
          }
    }
    
        })
  
  # http://www.treselle.com/blog/crime-analysis-with-shiny-r/
  
  output$Data <- renderDataTable({
    
    Full_df
  })
  output$mymap <- renderLeaflet({
    # define the leaflet map object
    if(input$graph == 'Clusters'){
    leaflet(data = test() ) %>% 
      addTiles() %>% 
      setView(-72.69342, 41.7591, zoom = 13) %>% addMarkers( ~long, ~lat, popup = paste("<b>","Arrest.Date: ","</b>", test()$Arrest.Date,"<br>",
                                                              "<b>","Arrest.location: ","</b>", test()$Arrest.formatted_address, "<br>",
                                                              "<b>","Charges: ","</b>", test()$Charges, "<br>",
                                                              "<b>","Bond:","</b>", test()$Bond, "<br>",
                                                              "<b>","Release.Date","</b>", test()$Release.Date, "<br>",
                                                              "<b>","Suspect.DOB: ","</b>", test()$Suspect.DOB, "<br>",
                                                              "<b>","Published.Date: ","</b>",test()$Published.Date),clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE, removeOutsideVisibleBounds = TRUE))}
    else{
      leaflet(data = test() ) %>% 
        addTiles() %>% 
        setView(-72.69342, 41.7591, zoom = 13) %>% addMarkers( ~long, ~lat, popup = paste("<b>","Arrest.Date: ","</b>", test()$Arrest.Date,"<br>",
                                                                                          "<b>","Arrest.location: ","</b>", test()$Arrest.formatted_address, "<br>",
                                                                                          "<b>","Charges: ","</b>", test()$Charges, "<br>",
                                                                                          "<b>","Bond:","</b>", test()$Bond, "<br>",
                                                                                          "<b>","Release.Date","</b>", test()$Release.Date, "<br>",
                                                                                          "<b>","Suspect.DOB: ","</b>", test()$Suspect.DOB, "<br>",
                                                                                          "<b>","Published.Date: ","</b>",test()$Published.Date))
    }
    
  })
  
  

  
  output$summary <- renderPlotly({
    
      temp <- tally(group_by(test(), hour(Arrest.Data$Arrest.Date)))
      colnames(temp) <- c("Time", "Count")
      
    plot_ly(temp, x = Time, y = Count)
  })

  output$summary2 <- renderPlotly({
    
    temp <- tally(group_by(test(),weekdays(Arrest.Data$Arrest.Date)))
    colnames(temp) <- c("Weekdays", "Count")
    temp$Weekdays <- factor(temp$Weekdays, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
    temp <- arrange(temp, Weekdays)
    plot_ly(temp, x = Weekdays, y = Count)
  })
  
  output$trend <- renderPlotly({
    trend <- Full_df %>% group_by(as.Date(Arrest.Date)) %>% summarize(n=n())
    colnames(trend) <- c("Date", "Count")
    plot_ly(trend, x = Date, y = Count)
  })
  
  output$table <- renderDataTable({
    
    fnl2 <- Full_df %>% group_by(Month = as.Date(Arrest.Date, format = "%y %B")) %>%
      summarize(n=n())
  })
  
 
    
})












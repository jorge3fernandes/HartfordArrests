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
  
  
  autoInvalidate <- reactiveTimer(180000)
  
  observe({
    # Invalidate and re-execute this reactive expression every time the
    # timer fires.
    autoInvalidate()
    
    # Do something each time this is invalidated.
    # The isolate() makes this observer _not_ get invalidated and re-executed
    # when input$n changes.
    download_update <- function(){
      
      folder <- "/Users/legs_jorge/Documents/Data Science Projects/RHartford/PDFs"
      
      #folder <- getwd()
      #folder <- paste0(getwd(),"/PDFs")
      setwd(folder)
      #data_2017 <- read.csv("full_HPD_calls.csv", row.names = NULL)
      #data_2017$Date <- as.POSIXct(data$Date)
      url <- "http://www.hartford.gov/images/police/ArrestLogs/blotter.pdf"
      
      
      
      download.file(url, "new.pdf", mode = "wb")
      filename <- str_extract(pdf_text("new.pdf")[2], "Date:.* ") %>% str_replace("Date:","") %>% str_replace_all("/","-") %>% str_trim()
      file.copy("new.pdf", paste0(filename,".pdf"))
      new.txt <- pdf_text("new.pdf")
      
      # if (file.exists(paste0(filename,".txt"))) {
      #   
      #   stop("No updates today, Jorge! Let's check back tomorrow! It's time to update this man.")
      # }
      
      write(new.txt, paste0(filename,".txt"))
      
      #Checking last update
      
      #main.file <- read.csv('full_HPD_calls.csv', stringsAsFactors = FALSE)
      #max(as.Date(main.file$Arrest.Date))
      ########################## Creating the dataframe ##############################
      
      # using the library stringr and regex here we extract the information 
      # from the text files and turn it into a dataframe
      new_txt <- paste0(filename,".txt")
      text <- readLines(new_txt)
      
      txt <- str_c(text, collapse = "\n")
      
      #split the text by calls
      txtparts <- unlist(str_split(txt, '--------------------------------------------------------------------------------'))
      
      #extracting specific fields
      Published.Date <- str_extract(text[2], "Date:.*") %>% str_replace("Date:","") %>% str_trim() %>% rep(length(txtparts))
      Arrest.Date <- str_trim(str_extract(txtparts, "Arrest Date: .*    ")) %>% str_replace_all("Arrest Date: ","")
      Supect.Name <- str_extract(txtparts, "Name: .*DOB:") %>% str_replace_all("Name: ","") %>% str_replace_all("DOB:","") %>% str_trim()
      Suspect.DOB <- str_extract(txtparts, "DOB:.*Sex") %>% str_replace_all("DOB:","") %>% str_replace_all("Sex","") %>% str_trim()
      Suspect.Race <- str_extract(txtparts, "Race:.*\n") %>% str_replace_all("Race:","") %>% str_replace_all("\n","") %>% str_trim()
      Suspect.Sex <- str_extract(txtparts, "Sex:.*Race") %>% str_replace_all("Sex:","") %>% str_replace_all("Race","") %>% str_trim()
      Suspect.Street <- str_extract(txtparts, "Addr:.*Hgt") %>% str_replace_all("Addr:","") %>% str_replace_all("Hgt","") %>% str_trim() %>% str_c(", ",str_extract(txtparts, "\n      .*MF") %>% str_replace_all("\n","") %>% str_replace_all("\\.","") %>% str_replace_all("MF","") %>% str_trim())
      Suspect.Hgt <- str_extract(txtparts, "Hgt:.*Wgt") %>% str_replace_all("Hgt:","") %>% str_replace_all("Wgt","") %>% str_trim()
      Suspect.Wgt <- str_extract(txtparts, "Wgt:.*Hair") %>% str_replace_all("Wgt:","") %>% str_replace_all("Hair","") %>% str_trim()
      Suspect.Hair <- str_extract(txtparts, "Hair:.*\n") %>% str_replace_all("Hair:","") %>% str_replace_all("\n","") %>% str_trim()
      MF <- str_extract(txtparts, "MF#:.*Arrest") %>% str_replace_all("MF#:","") %>% str_replace_all("Arrest","") %>% str_trim()
      Arrest.num <- str_extract(txtparts, "Arrest #:.*Case") %>% str_replace_all("Arrest #:","") %>% str_replace_all("Case","") %>% str_trim()
      Case.num <- str_extract(txtparts, "Case #:.*\n") %>% str_replace_all("Case #:","") %>% str_replace_all("\n","") %>% str_trim()
      Officers <- str_extract(txtparts, "Officers:.*\n") %>% str_replace_all("Officers:","") %>% str_replace_all("\n","") %>% str_trim()
      Arrest.location <- str_extract(txtparts, "Location:.*\n") %>% str_replace_all("Location:","") %>% str_replace_all("\n","") %>% str_c(", Hartford, CT") %>% str_trim()
      Release.Date <- str_extract(txtparts, "Release Date:.*      ") %>% str_replace_all("Release Date:","") %>% str_replace_all("      ","") %>% str_replace_all("BOND","") %>% str_trim()
      Bond <- str_extract(txtparts, "  BOND.*\n") %>% str_replace_all("  BOND","") %>% str_replace_all("\n","") %>% str_trim()
      WPTA <- str_extract(txtparts, "WPTA")
      Charges <- str_extract(txtparts, "Charges:.*\n") %>% str_replace_all("Charges:","") %>% str_replace_all("\n","") %>% str_trim()
      ##########Continue from here
      
      
      #Putting everything together
      
      HPD_log <- cbind(Published.Date,Arrest.Date, Supect.Name, Suspect.DOB, Suspect.Race, Suspect.Sex, Suspect.Street, Suspect.Hgt, Suspect.Wgt,  Suspect.Hair, MF, Arrest.num, Case.num, Officers,  Arrest.location, Release.Date, Bond, WPTA, Charges )
      HPD_log <- data.frame(HPD_log, stringsAsFactors = FALSE)
      HPD_log[HPD_log == ""] = NA 
      HPD_log$Arrest.location[is.na(HPD_log$Arrest.location)] <- 0
      # HPD_log$Date <- as.POSIXct(HPD_log$Date,format ="%m/%d/%Y %H:%M", tz = "EST")
      HPD_log <- HPD_log[rowSums(is.na(HPD_log)) < 5,] # Deleting all the row that have more than five NAs
      # HPD_log$Month <- month(HPD_log$Date)
      # HPD_log$Day <- day(HPD_log$Date)
      
      
      
      #########################################################GEOCODING##################################################
      #Geocoding Arrest Location
      HPD_log$Arrest.location <- as.character(HPD_log$Arrest.location)
      HPD_log$Arrest.Date.Formatted <- as.POSIXct(HPD_log$Arrest.Date,format = "%m/%d/%Y %H:%M", tz = "EST")
      HPD_log$Arrest.location[is.na(HPD_log$Arrest.location)] <- 0 #change from NA to 0 so geocode() can do its job
      getGeoDetails <- function(Arrest.location){ 
        Arrest.location <- as.character(Arrest.location) 
        #use the geocode function to query google servers
        geo_reply = geocode(Arrest.location, output = 'all', messaging = TRUE, override_limit = TRUE)
        #now extract the bits that we need from the returned list
        answer <- data.frame(Arrest.lat = NA, 
                             Arrest.long = NA, 
                             Arrest.accuracy = NA, 
                             Arrest.formatted_address = NA, 
                             Arrest.address_type = NA, 
                             Arrest.status = NA)
        answer$Arrest.status <- geo_reply$status
        
        #return Na's if we didn't get a match:
        if (geo_reply$status != "OK") {
          return(answer)
        }   
        #else, extract what we need from the Google server reply into a dataframe:
        answer$Arrest.lat <- geo_reply$results[[1]]$geometry$location$lat
        answer$Arrest.long <- geo_reply$results[[1]]$geometry$location$lng   
        if (length(geo_reply$results[[1]]$types) > 0) {
          answer$Arrest.accuracy <- geo_reply$results[[1]]$types[[1]]
        }
        answer$Arrest.address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
        answer$Arrest.formatted_address <- geo_reply$results[[1]]$formatted_address
        
        return(answer)
      }
      
      #initialise a dataframe to hold the results
      geocoded <- data.frame()
      
      
      for (ii in seq_along(HPD_log$Arrest.location )){
        print(paste("Working on index", ii, "of", length(HPD_log$Arrest.location )))
        #query the google geocoder - this will pause here if we are over the limit.
        result = getGeoDetails(HPD_log$Arrest.location[ii]) 
        print(result$status)     
        result$index <- ii
        #append the answer to the results file.
        geocoded <- smartbind(geocoded, result)
      }
      geocoded <- geocoded[!duplicated(geocoded),]
      HPD_log <- cbind(HPD_log, geocoded)
      
      
      #Geocoding Suspect Address
      HPD_log$Suspect.Street <- as.character(HPD_log$Suspect.Street)
      HPD_log$Suspect.Street[is.na(HPD_log$Suspect.Street)] <- 0 #change from NA to 0 so geocode() can do its job
      getGeoDetails <- function(Suspect.Street){ 
        Suspect.Street <- as.character(Suspect.Street) 
        #use the geocode function to query google servers
        geo_reply = geocode(Suspect.Street, output = 'all', messaging = TRUE, override_limit = TRUE)
        #now extract the bits that we need from the returned list
        answer <- data.frame(Suspect.lat = NA, 
                             Suspect.long = NA, 
                             Suspect.accuracy = NA, 
                             Suspect.formatted_address = NA, 
                             Suspect.address_type = NA, 
                             Suspect.status = NA)
        answer$Suspect.status <- geo_reply$status
        
        #return Na's if we didn't get a match:
        if (geo_reply$status != "OK") {
          return(answer)
        }   
        #else, extract what we need from the Google server reply into a dataframe:
        answer$Suspect.lat <- geo_reply$results[[1]]$geometry$location$lat
        answer$Suspect.long <- geo_reply$results[[1]]$geometry$location$lng   
        if (length(geo_reply$results[[1]]$types) > 0) {
          answer$Suspect.accuracy <- geo_reply$results[[1]]$types[[1]]
        }
        answer$Suspect.address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
        answer$Suspect.formatted_address <- geo_reply$results[[1]]$formatted_address
        
        return(answer)
      }
      
      #initialise a dataframe to hold the results
      geocoded <- data.frame()
      
      
      for (ii in seq_along(HPD_log$Suspect.Street )){
        print(paste("Working on Suspect.Street", ii, "of", length(HPD_log$Suspect.Street )))
        #query the google geocoder - this will pause here if we are over the limit.
        result = getGeoDetails(HPD_log$Suspect.Street[ii]) 
        print(result$status)     
        result$index <- ii
        #append the answer to the results file.
        geocoded <- smartbind(geocoded, result)
      }
      geocoded <- geocoded[!duplicated(geocoded),]
      HPD_log <- cbind(HPD_log, geocoded)
      HPD_log$Arrest.Date.Formatted <- mdy_hm(HPD_log$Arrest.Date, tz = "EST")
      HPD_log$Arrest.Date.Formatted2 <- floor_date(HPD_log$Arrest.Date.Formatted, "day")
      HPD_log$Arrest.Time <- paste0(hour(HPD_log$Arrest.Date.Formatted), ":",minute(HPD_log$Arrest.Date.Formatted))
      
      
      ###################################################################
      
      
      
      # Combining daily logs into one data frame
      
      #if(file.exists("Full_df.csv")) {
      Full_df <- read.csv("Full_df.csv") %>% smartbind(HPD_log) 
      Full_df <- unique(Full_df)
      write.csv(Full_df, "Full_df.csv", row.names = FALSE )
      #} 
      # else {
      #     Full_df <- unique(Full_df)
      #     write.csv(HPD_log, "Full_df.csv", row.names = FALSE )
      # }
      
      
      return(unique(Full_df))
    }
    # use the function to update the dataset
    download_update()
  })
  
  
  
  
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












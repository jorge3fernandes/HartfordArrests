library(plyr)
library(RCurl)
library(stringr)
library(pdftools)
library(dplyr)
library(gtools)
library(ggmap)

download_update <- function(){
  
  folder <- "/Users/legs_jorge/Documents/Data Science Projects/RHartford/PDFs"
  setwd(folder)
  #data_2017 <- read.csv("full_bpd_calls.csv", row.names = NULL)
  #data_2017$Date <- as.POSIXct(data$Date)
  url <- "http://www.hartford.gov/images/police/ArrestLogs/blotter.pdf"
  
 
  
      download.file(url, paste0(Sys.Date(),".pdf"), mode = "wb")

      write(pdf_text(paste0(Sys.Date(),".pdf")), paste0(Sys.Date(),".txt"))
  
  ########################## Creating the dataframe ##############################
  
  # using the library stringr and regex here we extract the information 
  # from the text files and turn it into a dataframe
  new_txt <- paste0(Sys.Date(),".txt")
  df <- function(new_txt) {
    text <- readLines(new_txt)
    
    txt <- str_c(text, collapse = "\n")
    
    #split the text by calls
    txtparts <- unlist(str_split(txt, '--------------------------------------------------------------------------------'))
    
    #extracting specific fields
    Arrest.Date <- str_trim(str_extract(txtparts, "Arrest Date: .*    ")) %>% str_replace_all("Arrest Date: ","")
    Supect.Name <- str_extract(txtparts, "Name: .*DOB:") %>% str_replace_all("Name: ","") %>% str_replace_all("DOB:","") %>% str_trim()
    Suspect.DOB <- str_extract(txtparts, "DOB:.*Sex") %>% str_replace_all("DOB:","") %>% str_replace_all("Sex","") %>% str_trim()
    Suspect.Race <- str_extract(txtparts, "Race:.*\n") %>% str_replace_all("Race:","") %>% str_replace_all("\n","") %>% str_trim()
    
    ##########Continue from here
    
    Arrest.location <- str_extract(txtparts, "Location:.*\n") %>% str_replace_all("Location:","") %>% str_replace_all("\n","")
    Suspect.address <- str_extract(txtparts, "Addr: .*\n") %>% str_replace_all("Location:","") %>% str_replace_all("\n","")
    
    
    
    #Since I'm capturing only the raw data, I'll save the next steps for later
    address_Geo <- str_replace_all(Response_address,' Apt\\. .*, ',', BROCKTON, ')
    address_Geo <- str_replace_all(address_Geo,"NA, BROCKTON, MA","")
    
    police_officer <- str_extract_all(txtparts, "(?s)Location\\/Address:[^\n]*\\R(.*)|(?s)Vicinity of:[^\n]*\\R(.*)") %>% str_extract_all("ID:.*\n|Patrolman.*\n")
    police_officer <- str_replace_all(police_officer,"ID:","") %>% str_replace_all("c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
    
    call_reason_action <- str_extract_all(txtparts, "                [[:digit:]]{4}.*\n")
    call_reason_action <- str_replace_all(call_reason_action, "[[:digit:]]{4}","") %>% str_replace_all("c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","")
    
    Refer_To_Arrest <- str_extract(txtparts, "Refer To Arrest:.*\n") 
    Refer_To_Arrest <- str_replace_all(Refer_To_Arrest, "Refer To Arrest:","") %>% str_replace_all("c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
    
    Refer_To_Summons <- str_extract_all(txtparts, "Refer To Summons:.*\n")
    Refer_To_Summons <- str_replace_all(Refer_To_Summons, "Refer To Summons:","") %>% str_replace_all("c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
    
    Summons <- str_extract_all(txtparts, "         Summons:    .*\n") %>% str_replace_all("Summons:","")  
    Summons <- str_replace_all(Summons, "c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
    
    Arrested <- str_extract_all(txtparts, "          Arrest:    .*\n") 
    Arrested <- str_replace_all(Arrested,"Arrest:","") %>% str_replace_all("c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
    
    Age <- str_extract_all(txtparts,"Age:.*\n") %>% str_replace_all("Age:","")
    Age <- str_replace_all(Age, "c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
    #age_m <- unlist(str_split(Age, ","))
    
    Suspect_Address <- str_extract_all(txtparts,"         Address:    .*\n") %>% str_replace_all("Address:","") %>% str_replace_all("c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
    #Occurrence_location <- str_replace_all( Occurrence_location, "c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
    
    charges <- unlist(str_extract_all(txtparts,"Charges:    .*\n") %>% str_replace_all("Charges:",""))
    charges <- str_replace_all(charges, "c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace("character\\(0","")
    
    response_time <- str_extract_all(txtparts,"Arvd.*\n") 
    response_time <- str_replace_all(response_time, "c\\(\\\"    ","") %>% str_replace_all("\\\n\"","") %>% str_replace_all("\"","") %>% str_replace_all("\\)","") %>% str_replace_all("\\\\n","") %>% str_replace_all("c\\(","") %>% str_replace("character\\(0","")
    
    
    #Putting everything together
    
    BPD_log <- cbind(Date, Call_taker, call_reason_action, Response_address, address_Geo, police_officer, Refer_To_Summons, Summons, Refer_To_Arrest, Arrested,Age,  Suspect_Address, charges, response_time)
    BPD_log <- data.frame(BPD_log, stringsAsFactors = FALSE)
    BPD_log[BPD_log == ""] = NA 
    BPD_log$Response_address[is.na(BPD_log$Response_address)] <- 0
    BPD_log <- subset(BPD_log, !is.na(Call_taker))
    # BPD_log$Date <- as.POSIXct(BPD_log$Date,format ="%m/%d/%Y %H:%M", tz = "EST")
    # BPD_log$Month <- month(BPD_log$Date)
    # BPD_log$Day <- day(BPD_log$Date)
    
    
    
    #########################################################GEOCODING##################################################
    BPD_log$address_Geo <- as.character(BPD_log$address_Geo)
    
    BPD_log$address_Geo[is.na(BPD_log$address_Geo)] <- 0
    getGeoDetails <- function(address_Geo){ 
      address_Geo <- as.character(address_Geo) 
      #use the gecode function to query google servers
      geo_reply = geocode(address_Geo, output = 'all', messaging = TRUE, override_limit = TRUE)
      #now extract the bits that we need from the returned list
      answer <- data.frame(lat = NA, 
                           long = NA, 
                           accuracy = NA, 
                           formatted_address = NA, 
                           address_type= NA, 
                           status= NA)
      answer$status <- geo_reply$status
      
      #return Na's if we didn't get a match:
      if (geo_reply$status != "OK"){
        return(answer)
      }   
      #else, extract what we need from the Google server reply into a dataframe:
      answer$lat <- geo_reply$results[[1]]$geometry$location$lat
      answer$long <- geo_reply$results[[1]]$geometry$location$lng   
      if (length(geo_reply$results[[1]]$types) > 0){
        answer$accuracy <- geo_reply$results[[1]]$types[[1]]
      }
      answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
      answer$formatted_address <- geo_reply$results[[1]]$formatted_address
      
      return(answer)
    }
    
    #initialise a dataframe to hold the results
    geocoded <- data.frame()
    
    
    for (ii in seq_along(BPD_log$address_Geo )){
      print(paste("Working on index", ii, "of", length(BPD_log$address_Geo )))
      #query the google geocoder - this will pause here if we are over the limit.
      result = getGeoDetails(BPD_log$address_Geo[ii]) 
      print(result$status)     
      result$index <- ii
      #append the answer to the results file.
      geocoded <- smartbind(geocoded, result)
    }
    geocoded <- geocoded[!duplicated(geocoded),]
    BPD_log <- cbind(BPD_log, geocoded)
    
    
    ###################################################################
    
  }
  # Combining daily logs into one data frame
  
  Full_df <- data.frame()
  for (i in seq_along(new_txt)) {
    
    Full_df <- smartbind(Full_df,df(new_txt[i])) 
    
  } 
  
  Full_df$Date <- as.POSIXct(Full_df$Date,format = "%m/%d/%Y %H:%M", tz = "EST")
  
  #new_data <- smartbind(data_2017,Full_df)
  #new_data <- new_data[!duplicated(new_data),]
  write.csv(Full_df, "full_bpd_calls.csv", row.names = FALSE)
  return(Full_df)
}
# use the function to update the dataset
test <- download_update()


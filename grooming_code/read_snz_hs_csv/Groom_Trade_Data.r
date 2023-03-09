##
##    Programme:  Groom_Trade_Data.r
##
##    Objective:  Ok, we've got all the data into R, now lets standardise the country
##                codes, and other aspects of the data.
##
##                Trick for young players - the harmonised system classification is a 
##                10 digit classification.  StatsNZ, miss the leading 0.
##                The trick is to keep this classification as a string and pad it with a 
##                leading zero if less than 10 characters in length.
##
##    Author:     James Hogan, 31 August 2014
##
   ##
   ##    Load the raw data back into memory
   ##
      #load("data\\Exports_By_Country.rda")
      #load("data\\Imports_By_Country.rda")
      #load("data\\Exports_Total.rda")
      #load("data\\Imports_Total.rda")
 
 
   ##
   ##    Make the dates actual dates.  While we're at it, correct their names.  I hate
   ##       full stops in names.
   ##
      Correct_Date <- function(dset)
         {
            if(any(names(dset) == "Month"))
               {
                  dset$Date <- as.Date(as.character(dset$Month+.01),"%Y%m.%d")
                  month(dset$Date) <- month(dset$Date) +1
                  dset$Date <- dset$Date - 1      
                  dset <- dset[ , -which(names(dset) %in% c("Month"))]            
               }
            names(dset)[1:length(dset)] <- str_trim(str_replace_all(names(dset)[1:length(dset)], "\\.", " "))
            names(dset)[1:length(dset)] <- str_replace_all(names(dset)[1:length(dset)], " ", "_")
            names(dset)[1:length(dset)] <- str_replace_all(names(dset)[1:length(dset)], "___", "_")
            dset$Harmonised_System_Code <- as.character(format(as.numeric(dset$Harmonised_System_Code), scientific = FALSE))
            dset$Harmonised_System_Code <- ifelse(str_length(str_trim(dset$Harmonised_System_Code)) < 10,
                                                  str_pad(str_trim(dset$Harmonised_System_Code), 10, pad = "0"), 
                                                  dset$Harmonised_System_Code)
            return(dset)
      }
      
      
      Exports_By_Country <- Correct_Date(Exports_By_Country)
      Imports_By_Country <- Correct_Date(Imports_By_Country)
      Exports_Total      <- Correct_Date(Exports_Total)
      Imports_Total      <- Correct_Date(Imports_Total)
      
   ##
   ##    Make Numbers of things that are numbers
   ##
       Exports_By_Country$Exports_NZD_fob       <- as.numeric(as.character(str_replace_all(Exports_By_Country$Exports_NZD_fob,       "\\,","")))
       Exports_By_Country$Exports_Qty           <- as.numeric(as.character(str_replace_all(Exports_By_Country$Exports_Qty,           "\\,","")))
       Exports_By_Country$Re_exports_NZD_fob    <- as.numeric(as.character(str_replace_all(Exports_By_Country$Re_exports_NZD_fob,    "\\,","")))
       Exports_By_Country$Re_exports_Qty        <- as.numeric(as.character(str_replace_all(Exports_By_Country$Re_exports_Qty,        "\\,","")))
       Exports_By_Country$Total_Exports_NZD_fob <- as.numeric(as.character(str_replace_all(Exports_By_Country$Total_Exports_NZD_fob, "\\,","")))
       Exports_By_Country$Total_Exports_Qty     <- as.numeric(as.character(str_replace_all(Exports_By_Country$Total_Exports_Qty,     "\\,","")))

       Exports_Total$Exports_NZD_fob            <- as.numeric(as.character(str_replace_all(Exports_Total$Exports_NZD_fob,        "\\,","")))
       Exports_Total$Exports_Qty                <- as.numeric(as.character(str_replace_all(Exports_Total$Exports_Qty,            "\\,","")))
       Exports_Total$Re_exports_NZD_fob         <- as.numeric(as.character(str_replace_all(Exports_Total$Re_exports_NZD_fob,     "\\,","")))
       Exports_Total$Re_exports_Qty             <- as.numeric(as.character(str_replace_all(Exports_Total$Re_exports_Qty,         "\\,","")))
       Exports_Total$Total_Exports_NZD_fob      <- as.numeric(as.character(str_replace_all(Exports_Total$Total_Exports_NZD_fob,  "\\,","")))
       Exports_Total$Total_Exports_Qty          <- as.numeric(as.character(str_replace_all(Exports_Total$Total_Exports_Qty,      "\\,","")))
       
       Imports_By_Country$Imports_NZD_vfd       <- as.numeric(as.character(str_replace_all(Imports_By_Country$Imports_NZD_vfd,   "\\,","")))
       Imports_By_Country$Imports_NZD_cif       <- as.numeric(as.character(str_replace_all(Imports_By_Country$Imports_NZD_cif,   "\\,","")))
       Imports_By_Country$Imports_Qty           <- as.numeric(as.character(str_replace_all(Imports_By_Country$Imports_Qty,       "\\,","")))
   
       Imports_Total$Imports_NZD_vfd            <- as.numeric(as.character(str_replace_all(Imports_Total$Imports_NZD_vfd,        "\\,","")))
       Imports_Total$Imports_NZD_cif            <- as.numeric(as.character(str_replace_all(Imports_Total$Imports_NZD_cif,        "\\,","")))
       Imports_Total$Imports_Qty                <- as.numeric(as.character(str_replace_all(Imports_Total$Imports_Qty,            "\\,","")))
      
   ##
   ##    And Save
   ##
       #save(Exports_By_Country, file = "data/Exports_By_Country.rda")
       #save(Imports_By_Country, file = "data/Imports_By_Country.rda")
       save(Exports_Total,      file = "data/Exports_Total.rda")
       save(Imports_Total,      file = "data/Imports_Total.rda")
   ##
   ##    Geocode the locations and save the lats and longs
   ##
      Export_Countries <- 
         Exports_By_Country %>% 
         mutate(Country = as.character(Country)) %>% 
         mutate(Country = ifelse(!is.na(Country), Country, as.character(Country))) %>% 
         dplyr::select(Country) %>% 
         distinct() %>%
         filter( !is.na(Country) )
         
      
      Country_Geocodes <- geocode(Export_Countries$Country)
      Country_Geocodes <- cbind(Export_Countries, Country_Geocodes) 
      save(Country_Geocodes, file = "data/Country_Geocodes.rda")
   
   
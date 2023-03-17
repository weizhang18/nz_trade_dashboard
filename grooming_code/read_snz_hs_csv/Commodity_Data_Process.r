##
##    Programme:  Commodity_Data_Process.r
##
##    Objective:  This programme is designed to automatically read in and process SNZ commodity data.
##                As part of its functionality, it identifies all the zipped files, expand them 
##                one at a time, and reads their contents into R.  From there, my automated code
##                for creating database tables transforms the information into a state where it
##                can be graphed further in R
##   Aussie Stats:  http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/5368.0Jun%202014?OpenDocument
##
##    Author:     James Hogan, 30 August 2014
##
   ##
   ##    Identify which files are imports and exports and which are totals and by country
   ##
      Files <- c( list.files(path = "data_raw/snz_country_hs", pattern = "*.csv"),
                  list.files(path = "data_raw/snz_country_hs", pattern = "*.zip"))
                          
      Files <- rbind(data.frame(path = "Exports_By_Country",
                                file = subset(Files, (str_detect(Files, "Exports") &  str_detect(Files, "by_Country"))) ,
                                stringsAsFactors = FALSE),                                                  
                                
                     data.frame(path = "Imports_By_Country",
                                file = subset(Files, (str_detect(Files, "Imports") &  str_detect(Files, "by_Country"))) ,
                                stringsAsFactors = FALSE),
                                
                     data.frame(path = "Imports_Total",
                                file = subset(Files, (str_detect(Files, "Imports") & !str_detect(Files, "by_Country"))) ,
                                stringsAsFactors = FALSE),
                                
                     data.frame(path = "Exports_Total",
                                file = subset(Files, (str_detect(Files, "Exports") & !str_detect(Files, "by_Country"))) ,
                                stringsAsFactors = FALSE)
                    )      
  
  ##
  ##    Separate exports/imports/Total by commodity, and expand each zipped file.
  ##        Read them into a list object so I can squeeze them into one dataframe.
  ##
    Focus <- unique(Files$path)
    
    cat('Importing SNZ Trade Commodities Data\n')
    
    zipdir <- tempdir()  
    for(j in 1:length(Focus))
      {
         Commodity_Data <- list()
         csvs <- Files[Files$path == Focus[j],]
         
         # Print the current focus and show progress
         cat(paste(Focus[j], "\n"))
         
         # Create a sub-progress bar for the csvs
         progress_csv <- txtProgressBar(min = 0, max = nrow(csvs), style = 3)
         
         # Create index to load data to list
         Commodity_Data_Index <- 1
         
         for(i in 1:nrow(csvs))
            {
            
            # Show progress
            setTxtProgressBar(progress_csv, i)
            
              if(str_detect(csvs$file[i], ".csv")) 
                 {
                    file.copy(paste("data_raw/snz_country_hs", csvs$file[i], sep="//"), zipdir, copy.mode = TRUE)
                 } else unzip(paste("data_raw/snz_country_hs", csvs$file[i], sep="//"), exdir=zipdir)
               
              Contents = as.data.frame(list.files(path = zipdir,  pattern = "*.csv"))
              
              ## print(csvs$file[i])
              ## print(Contents)
              ##
              ##    Read in the file contents
              ##
              
              
              for(k in 1:nrow(Contents))
              {
                  
                 
                X <- read.csv(paste0(zipdir, "\\", as.character(Contents[k,1])),
                                na.strings="..", 
                                sep=",",
                                stringsAsFactors = TRUE,
                                check.names=TRUE,
                                header=TRUE)
                
                # Remove extra columns if they creep in
                if(ncol(X) > 12) {
                   X <- X[1:12]
                }
                
                isExportByCountry <- (str_detect(as.character(Contents[k,1]), "Exports") &  str_detect(as.character(Contents[k,1]), "by_Country"))
                isExportTotal <- (str_detect(as.character(Contents[k,1]), "Exports") &  !str_detect(as.character(Contents[k,1]), "by_Country"))
                isImportByCountry <- (str_detect(as.character(Contents[k,1]), "Imports") &  str_detect(as.character(Contents[k,1]), "by_Country"))
                isImportTotal <- (str_detect(as.character(Contents[k,1]), "Imports") &  !str_detect(as.character(Contents[k,1]), "by_Country"))

                # Fix column names
                if( isExportByCountry & "hs" %in% names(X) ){
                   names(X) <- c("Month", "Harmonised.System.Code", "Harmonised.System.Description", "Unit.Qty",
                                 "Country", "Exports...NZD.fob.", "Exports.Qty", "Re.exports...NZD.fob.", "Re.exports.Qty",
                                 "Total.Exports...NZD.fob.", "Total.Exports.Qty", "Status")
                }

                if( isImportByCountry & "hs" %in% names(X) ){
                   names(X)[1:9] <- c("Month", "Harmonised.System.Code", "Harmonised.System.Description", "Unit.Qty",
                                      "Country", "Imports...NZD.vfd.", "Imports...NZD.cif.", "Imports.Qty", "Status")
                }

                if( isImportTotal & "hs" %in% names(X) ){
                   names(X)[1:8] <- c("Month", "Harmonised.System.Code", "Harmonised.System.Description", "Unit.Qty",
                                      "Imports...NZD.vfd.", "Imports...NZD.cif.", "Imports.Qty", "Status")
                }

                if( isExportTotal & "hs" %in% names(X) ){
                   names(X)[1:11] <- c("Month", "Harmonised.System.Code", "Harmonised.System.Description", "Unit.Qty",
                                      "Exports...NZD.fob.", "Exports.Qty", "Re.exports...NZD.fob.", "Re.exports.Qty",
                                      "Total.Exports...NZD.fob.", "Total.Exports.Qty", "Status")
                }
                
                Commodity_Data[[Commodity_Data_Index]] = X
                rm(X)
                
                Commodity_Data_Index <- Commodity_Data_Index + 1
                
              }
              
              unlink(paste(zipdir, "*.csv", sep="\\"))
              unlink(paste(zipdir, "*.zip", sep="\\"))
              rm(Contents)      
         }  
         close(progress_csv)
         
         #assign(Focus[j], data.frame(do.call(rbind.fill,Commodity_Data))) #rbind.fill
         assign(Focus[j], data.frame(do.call(dplyr::bind_rows,Commodity_Data))) 
         ##
         ##   Save the list
         ##
            #save(list=Focus[j], file = paste0("data/", Focus[j], ".rda"))
    }
    
    
    ###
    Exports_By_Country$Country[ which(Exports_By_Country$Country %in% c("T\xfcrkiye", "Turkiye")) ] <- "Turkey"
    Imports_By_Country$Country[ which(Imports_By_Country$Country %in% c("T\xfcrkiye", "Turkiye")) ] <- "Turkey"
   ##
   ##   Done!
   ##
    
    save(Exports_By_Country, file = "data_intermediate/Exports_By_Country.rda")
    save(Exports_Total, file = "data_intermediate/Exports_Total.rda")
    save(Imports_By_Country, file = "data_intermediate/Imports_By_Country.rda")
    save(Imports_Total, file = "data_intermediate/Imports_Total.rda")
    
    ############################
    rm( list = setdiff( ls(), keepers) )
    gc()
    ###############
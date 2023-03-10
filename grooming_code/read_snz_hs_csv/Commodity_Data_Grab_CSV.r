##
##    Programme:  Commodity_Data_Grab_CSV.r
##
##    Objective:  This programme goes to Stats NZ's website and identifies all off the trade commodity
##                data on their website.  It then checks what data has already been downloaded.  If 
##                some data has not been downloaded, the programme downloads it.
##
##    Plan of  :  This programme uses a variation of the screen scraping functionality to identify
##                all of the zip files Stats have uploaded to the net.  Once all the internet files
##                are identified, it looks into the Raw_Data directory and checks to see what data has
##                been downloaded.  If the data has been downloaded, it doesn't download it again.
##                If the data hasn't been downloaded, it downloads it and stores it in the Raw_Data
##                directory, ready for the next programme that is going to read it.
##

   

   ### get rid of CSv files as there are provisional data
   All_Files <- list.files( "data_raw//snz_country_hs" )
   All_Files_CSV <- All_Files[ grepl('[.]csv', All_Files) ]
   unlink( paste0('data_raw//snz_country_hs//', All_Files_CSV) )
   
   ## load links for .csv files 
   load("data_intermediate/links_csv.rda")
   
   # ## download csv files
   # 
   # Base_URL <- "http://archive.stats.govt.nz/browse_for_stats/industry_sectors/imports_and_exports/overseas-merchandise-trade/HS10-by-country.aspx?url=/browse_for_stats/industry_sectors/imports_and_exports/overseas-merchandise-trade/HS10-by-country.aspx"
   # ##
   # ##    This function was stolen from the XML help file :)
   # ##
   #    getLinks = function() 
   #       { 
   #           links = character() 
   #           list(a = function(node, ...) { 
   #                       links <<- c(links, xmlGetAttr(node, "href"))
   #                       node 
   #                    }, 
   #                links = function()links)
   #       }
   # 
   # ##
   # ##    Open the URL
   # ##
   #    h1 = getLinks()
   #    Trade_Data_Page <- getURL(Base_URL, curl = curl)
   #    htmlTreeParse(Trade_Data_Page, handlers = h1)
      
      ##
      ## Get all links from webpage, then subset based on whether they are final or not
      ##
      
      #Base URL that all latest year data use
      # Current_Year_Base <- "/~/media/Statistics/browse-categories/industry-sectors/imports-exports/HS10 by Country"
      # 
      # Source_Files <- data.frame(links = as.character(h1$links())) %>%
      #                      filter(str_detect(links, Current_Year_Base)) %>%
      #                      filter(#str_detect(links, "Final"), 
      #                             #!str_detect(links, "prov"), ## modified by Wei to get all data
      #                             str_detect(links, ".csv")) %>%
      #                      mutate(links = paste0("http://archive.stats.govt.nz", links))
      
      
      Source_Files <- data.frame(links = links_csv  )
      
                                  
   ##
   ##    Strip out from the last backslash to identify the filename
   ##
      Files <- str_split(Source_Files$links, "/")
      for(i in 1:length(Files)) Source_Files$File_Name[i] <- as.character(Files[[i]][length(Files[[i]])])
      
      
   ##
   ##    Scan the Raw_Data directory and identify which files aren't there
   ##
      Saved_Files <- data.frame(SavedZips = list.files(path = "data_raw/snz_country_hs", pattern = "*.csv"))
      ifelse((nrow(Saved_Files) > 0),
         {
            ##
            ##    Remove the downloaded files from the source files
            ##
                Saved_Files$In <- 1
                Download_Files <- merge(Source_Files, 
                                        Saved_Files, 
                                        by.x = c("File_Name"), 
                                        by.y = c("SavedZips"),
                                        all.x=TRUE)
                Download_Files <- Download_Files[is.na(Download_Files$In),-length(Download_Files)]
         },
         {Download_Files <- Source_Files})
   ##
   ##    Download the Download_Files:  setInternet2(TRUE) is needed for the https sites  (why did SNZ do this...?)
   ##
      if((nrow(Download_Files) > 0))
      {
         for(i in 1:nrow(Download_Files))
            {
               download.file(url = as.character(Download_Files$links[i]), 
                             mode = "wb",
                             destfile = paste("data_raw/snz_country_hs", Download_Files$File_Name[i], sep="/"))
            }
      }
   ##
   ##    ... and stop :)
   ##
      ############################
      rm( list = setdiff( ls(), keepers) )
      gc()
      ###############
      
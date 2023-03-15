##
##    Programme:  Commodity_Data_Grab.r
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
##    Author:     James Hogan, Sector Performance, Ministry of Business, Innovation and Employment
##                30 August 2014
##
   
   ##
   ##    Instance a CURL event
   ##
      # if(!exists("curl"))
      # {
      #    creds <- AskCreds(Title = "User Log In Name and Password", startuid = "", returnValOnCancel = "ID_CANCEL")
      #    curl <- getCurlHandle()
      #    curlSetOpt(.opts = list(proxy = 'http://proxybcw.wd.govt.nz:8080',
      #                            proxyusername = creds$uid,
      #                            proxypassword = creds$pwd), curl = curl)
      # }

   #Base_URL <- "http://archive.stats.govt.nz/browse_for_stats/industry_sectors/imports_and_exports/overseas-merchandise-trade/HS10-by-country.aspx?url=/browse_for_stats/industry_sectors/imports_and_exports/overseas-merchandise-trade/HS10-by-country.aspx"
   
   Base_URL <- "https://www.stats.govt.nz/large-datasets/csv-files-for-download/overseas-merchandise-trade-datasets#datasets-for-imports"
   ##
   ##    This function was stolen from the XML help file :)
   ##
      getLinks = function() 
         { 
             links = character() 
             list(a = function(node, ...) { 
                         links <<- c(links, xmlGetAttr(node, "href"))
                         node 
                      }, 
                  links = function()links)
         }

   ##
   ##    Open the URL
   ##
      h1 = getLinks()
      #Trade_Data_Page <- getURL(Base_URL, curl = curl)
      Trade_Data_Page <- getURL(Base_URL)
      htmlTreeParse(Trade_Data_Page, handlers = h1)
      
   ##
   ##    Focus on the zip files, and the files where the above name is true
   ##
      Source_Files <- data.frame(links = as.character(h1$links()))
      Source_Files <- data.frame(Trade_Data = unique(Source_Files[str_detect(Source_Files$links, regex("zip", ignore.case = TRUE)),]))
     
   ##
   ##    Strip out from the last backslash to identify the filename
   ##
      Files <- str_split(Source_Files$Trade_Data, "/")
      for(i in 1:length(Files)) Source_Files$File_Name[i] <- as.character(Files[[i]][5])
      
      Source_Files <- 
         Source_Files %>%
         ### Data from 1990 to 1999 will create error when rbind because they have different column
         ### names. Also, data from the that period is never used in the Export Intellegence report
         filter( File_Name != "1990%20-%201999%20Imports%20&%20Exports%20HS10%20by%20Country.zip" )
      
   ##
   ##    Scan the Raw_Data directory and identify which files aren't there
   ##
      Saved_Files <- data.frame(SavedZips = list.files(path = "data_raw/snz_country_hs", pattern = "*.zip"))
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
   ##       I ran this code on the 30 July 2015, and SNZ had put a redirect code on the website.  Consequently, I've
   ##       got to change the URL passed to the download.file function
   ##
      ## do not download HS10_country_port data
      Download_Files %<>%
         filter( !grepl('Port',File_Name) )
      
      if((nrow(Download_Files) > 0))
      {
         
         for(i in 1:nrow(Download_Files))
            {
               tmp_url <- as.character(Download_Files$Trade_Data[i])
               method = "wininet"
               download.file(url = ifelse( grepl( "https", tmp_url ), ## see if https is already there.
                                           tmp_url,
                                           str_replace_all( tmp_url, "http", "https")
                                          ), 
                             mode = "wb",
                             destfile = paste("data_raw/snz_country_hs", Download_Files$File_Name[i], sep="/"))
            }
      }
   ##
   ##    ... and stop :)
   ##

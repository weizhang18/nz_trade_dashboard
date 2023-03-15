##
##    Programme:  Commodity_Data_Grab.r
##
##    Objective:  This programme goes to Stats NZ's website and identifies all off the trade commodity
##                data on their website.  It then checks what data has already been downloaded.  If 
##                some data has not been downloaded, the programme downloads it.
##

   
load("data_intermediate/links_zip.rda")

      
   ##
   ##    Focus on the zip files, and the files where the above name is true
   ##
      #Source_Files <- data.frame(links = as.character(h1$links()))
      Source_Files <- data.frame(links = links_zip  )
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

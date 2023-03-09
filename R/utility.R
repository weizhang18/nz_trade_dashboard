###########################################################################
## Project description:  
##     Export and import dashboard
##   Project root folder:  
##      P:\OTSP\Export_Import_Dashboard
##	Script:	
##		./Export_Import_Dashboard/R/utility.R
## Objective:	
##    utility functions
##	History:
##    30/11/17 by Wei Zhang
###########################################################################



###########################################################################
## Table of Contents ------------------------------------------------------
###########################################################################

## function to fill out missing cells
fill_missing_cell <- 
   function( vec_all ) {
      vec <- as.character(vec_all)
      tmp_unique <- na.omit(unique( vec ))
      tmp_unique <- tmp_unique[ tmp_unique != " " ]
      
      tmp_pos <- which(  vec %in% tmp_unique )
      
      for( i in 1:length(tmp_pos) ){
         if(i==1){
            vec[2:(tmp_pos[i+1]-1)] <- vec[tmp_pos[i]]
         }
         if(i>1&i<length(tmp_pos)){
            vec[ (tmp_pos[i]+1):(tmp_pos[i+1]-1) ] <- vec[tmp_pos[i]]
         }
         if(i==length(tmp_pos)){
            vec[ (tmp_pos[i]+1):length(vec) ] <- vec[tmp_pos[i]]
         }
      }
      
      return(vec)
   }

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## first capital letter
capFirst <- function(s) {
   paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

## unrowwise a df
unrowwise <- function(x) {
   if( any(class(x) == 'rowwise_df') ){
      class(x) <- setdiff( class(x), 'rowwise_df' )
   }
   x
}

## Remove leading and trailing spaces -------------------------------------
rm_space <- function(x){
   gsub("^\\s+|\\s+$", "", x)
}


## Download file from MAKO function
###################  Function #########################
downloadMAKO <- 
   function(url, destfile, quiet = T, mode = "wb", ...){
      ## ask for MBIE credentials
      if(!exists("creds") & Sys.getenv("USERDNSDOMAIN") == "WD.GOVT.NZ"){
         creds <- AskCreds(Title = "User Log In Name and Password", startuid = "", returnValOnCancel = "ID_CANCEL")   
         options(RCurlOptions = list(proxy = 'http://proxybcw.wd.govt.nz:8080',
                                     proxyusername = creds$uid, 
                                     proxypassword = creds$pwd))
      }
      
      ## reconstruct the url
      url.reconstruct <- gsub('http://',
                              paste0('http://',creds$uid,":",creds$pwd,'@'),
                              url)            
      if(exists(destfile)){
         unlink(destfile)
      }
      download.file(url = url.reconstruct, destfile = destfile, quiet = quiet, mode = mode, ...)
   }

######################################################################
#    A funciton to streamline downloading data from NZ.Stat          #
######################################################################

# Function: download data from http://nzdotstat.stats.govt.nz/wbos/Index.aspx by providing urls
# Output: either a dataset or a dataset and a code book (with repeated values) for you to combine
# Author: Wei Zhang
# Date: 01/June/2016
# version: 0.3
downloadNZStat <- function( url_data, url_instruction ){
   if( require( rsdmx ) == FALSE ){
      install.packages( 'rsdmx' )
   }else{
      library( rsdmx )
   }
   ######################## To download code book ######################
   url_inst <- url_instruction
   
   ## read the XML file from the link and then collapse into a list of vectors
   Failed_code <- class( try( dsd <- readSDMX(url_inst), silent = TRUE ) )
   it_code <- 1
   while( Failed_code %in% c('NULL','try-error') & it_code <= 500 ){
      Failed_code <- class( try( dsd <- readSDMX(url_inst), silent = TRUE ) )
      it_code <- it_code + 1
      print( paste0( '------------ Trying to download data from NZ.Stats! ', it_code, ' try now! ----------', '\n') )
   }
   
   ## if still fail produce warning.
   if(it_code>500){ 
      warning("Unable to download data code table from NZ.Stat. Please download mannually.")
      break()
   }
   
   #get codelists from DSD
   cls <- slot(dsd, "codelists")
   
   #get list of codelists
   codelists <- sapply(slot(cls, "codelists"), function(x) slot(x, "id"))
   
   #get a codelist
   code_all <-
      sapply( codelists,
              function(i_code){
                 as.data.frame(slot(dsd, "codelists"), codelistId = i_code) 
              }
            ) %>%
      rbind_all %>%
      dplyr::select( Code = id, Code_desc = label.en)  
   
   ######################## To download the data ######################
   url_data <- url_data
   
   ## read the XML file from the link and then collapse into a list of vectors
   Failed_data <- class(try (data_tables <- readSDMX(url_data) ))
   it_data <- 1
   while( Failed_data %in% c('NULL','try-error') & it_data <= 500 ){
      Failed_data <- class(try( data_tables <- readSDMX(url_data) ))
      it_data <- it_data + 1
      print( paste0( '------------ Trying to download data from NZ.Stats! ', it_data, ' try now! ----------', '\n') )
   }
   
   ## if still fail produce warning.
   if(it_data>500){ 
      warning("Unable to download data code table from NZ.Stat. Please download mannually.")
      break()
   }
   
   ### 
   tmp_data <- as.data.frame(data_tables) 
   
   if( any( colnames(tmp_data)=='OBS_STATUS' ) ){
      tmp_data <- 
         tmp_data %>%
         #dplyr::select( -attrs.df, -obsTime, -OBS_STATUS, value = obsValue )
         dplyr::select( -obsTime, -OBS_STATUS, value = obsValue )
   }else{
      tmp_data <- 
         tmp_data %>%
         #dplyr::select( -attrs.df, -obsTime, value = obsValue )
         dplyr::select(-obsTime, value = obsValue )
   }
   
   
   ######################## join the data with code  ######################
   if( length( unique( code_all$Code) ) == nrow( code_all ) ){ ## no repeated value in code_all   
      tmp_cols <- names( tmp_data )
      tmp_cols <- tmp_cols[ tmp_cols != 'value' ]
      
      for(i in 1:length(tmp_cols) ){
         tmp_data_sub <- tmp_data[i] ## take the column out
         colnames(tmp_data_sub) <- 'Code' ## rename it to Code
         
         ## left join by Code
         data_sub <- 
            tmp_data_sub %>%
            left_join( code_all, by = 'Code' ) %>%
            select( -Code )
         
         ## rename this identifying column to its original name
         names(data_sub) <- tmp_cols[i] 
         
         ## cbind the indentifying column back to main data
         tmp_data <- 
            data_sub %>%
            bind_cols( tmp_data[,-i])      
      }
      return( tmp_data )
   }else{
      return( list( data = tmp_data,
                    code_book = code_all) 
      )
   }
}

# Function: download data from https://stats.oecd.org/by providing stat.OECD table
#           It is a rapper for get_dataset() and get_data_structure() from OECD package
# Output: either a dataset with appropriate description
# Author: Wei Zhang
# Date: 02/Nov/2016
# version: 0.1
downloadOECDStat <- function( dataset, filter = NULL, start_time =NULL, end_time = NULL, pre_formatted = FALSE, ... ){
   ## load OECD package and we need to use get_dataset() and get_data_structure()
   library(OECD)
   
   ## 1. get data
   dtf_raw_data <- get_dataset( dataset,
                                filter = filter,
                                start_time = start_time, 
                                end_time = end_time,
                                pre_formatted = pre_formatted,
                                ...)
   
   ## 2. get description of codes
   dtf_raw_code <- get_data_structure( dataset )
   
   ## merge code with description
   ( tmp_cols <- names( dtf_raw_data ) )
   
   ## get description for each column
   for(i in 1:length(tmp_cols) ){
      tmp_data_sub <- dtf_raw_data[i] ## take the column out
      colnames(tmp_data_sub) <- 'id' ## rename it to Code
      
      ## left join by Code
      tmp_code <- dtf_raw_code[[tmp_cols[i]]] 
      
      if( !is.null( tmp_code )  ){
         data_sub <- 
            tmp_data_sub %>%
            left_join( dtf_raw_code[[tmp_cols[i]]], by = 'id' ) %>%
            select( -id ) 
         
         ## rename this identifying column to its original name
         names(data_sub) <- tmp_cols[i] 
         
         ## cbind the indentifying column back to main data
         dtf_raw_data <- 
            data_sub %>%
            bind_cols( dtf_raw_data[,-i])
      }
   }
   return( dtf_raw_data )
}



########### check the available sectors and get difference from MBIE sectors ---------------
###################  Function #########################
sr_check_sectors <- function(dataf){
   if(!"Sector" %in% names(dataf)){
      stop("Column names for Sector MUST be 'Sector'!")
   } 
   
   SecInData <- levels(factor(dataf$Sector))  %>% as.matrix %>% as.character
   SecInSR <- concord_SectorToMain %>% filter( !SectorMain %in% c('Cross-cutting sectors', 'Non Standard') ) %>% select( Sector) %>% as.matrix %>% as.character
   SecInSR_all <- concord_SectorToMain %>% select( Sector) %>% as.matrix %>% as.character
   
   ## this is to make sure certain varialbes contains all ordinary sectors
   con <- SecInSR %in% SecInData
   if( any( con == FALSE) & grepl('cc', dataf$ValueName[1])==FALSE ){ ## do not check cross-cutting sectors
      stop( paste( SecInSR[ which( con  == FALSE) ], 'sector is missing from this dataset!!\n' ) )
   } 
   
   ## this is to make sure sectors names are all standerdised names!!!
   con_standard_sec <- SecInData %in% SecInSR_all
   if( any( con_standard_sec == FALSE) ){ ## do not check cross-cutting sectors
      stop( paste( SecInData[ which( con_standard_sec  == FALSE) ], 'is NOT a standardised sector!! You need to standerdise the name -- see concordSectorToMain dataframe!!\n' ) )
   } 
}




################ sr_check_dataset ------------------------
###################  Function #########################
sr_check_dataset <- 
   function(data_df, message_level = MESSAGE_LEVEL){
      if(message_level == "none") {return}
      
      if(message_level == "detailed") {
         cat("Duplication ---------------------------------------------------------------\n")
         print(data_df %>% select(one_of(names(data_df)[names(data_df) %in% c("Sector","Year","Dimension1","Dimension2")])) %>% anyDuplicated)
         cat("Sector Summary --------------------------------------------------------------\n")
         print(sr_check_sectors(data_df))      
         cat("Dateset Structure ---------------------------------------------------------\n")
         print(
            apply(data_df %>% select(-Value), 2, 
                  function(POSSIBLE_VALUE){out <- t(t(table(POSSIBLE_VALUE))); colnames(out) <- "COUNTS"; return(out)}))
         cat("Dateset completion rate ---------------------------------------------------\n")
         completion.rate <- 
            nrow(data_df)/ prod(apply( 
               data_df %>% select(one_of(names(data_df)[names(data_df) %in% c("Sector","Year","Dimension1","Dimension2")])), 2 ,
               function(x){length(unique(x))})
            ) *100
         cat(paste0(formatC(completion.rate, format = "f", digits = 2), "%"),"\n")
         cat("---------------------------------------------------------------------------\n")
      }
   }

################### update.srdb --------------------------------------------------
###################  Function #########################
update.SRdb <- 
   function (
      ## dataset to be loaded/updated into database
      data_df, 
      ## set source information to be added/updated
      ReleaseDate = NA, 
      Frequency = NA,  
      AccessDate = NA, 
      EffectiveDateFrom = NA, 
      EffectiveDateTo = NA , 
      YrEndingMth = NA, 
      Units = NA,
      Magnitudes = NA,
      TechNotes = NA,
      Dim1_desc = NA,
      Dim1_summability = NA,
      Dim2_desc = NA,
      Dim2_summability = NA, 
      ## database location (local/server/both)
      db_location = "local",
      ## other argument(s) to maintain compatibility (in case some other source information is required to be updated in this function) 
      ...
   ){
      
      ## sql not recognize NaN, Inf so make them NA first
      data_df <- 
         data_df %>% 
         mutate( Value = ifelse( is.nan(Value) | Value == Inf | Value == -Inf, NA, Value ) ) %>% 
         filter(!is.na(Value))
      
      ########################################################################
      ## Check dataset structure/columns -------------------------------------
      ## Check existence of the compulsory columns
      if(any(names_not_found <- !c("Sector","Value","ValueName","Year") %in% names(data_df))) {
         stop(paste0(c("Sector","Value","ValueName","Year")[names_not_found], " not found in dataset.\n",
                     " Stop updating database!\n"))
      }
      
      ## Check uniqueness of the ValueName and get it if so.
      if(length(VName <- as.vector(unique(data_df$ValueName))) > 1) {
         stop(paste0("The ValueName of the dataset is not unique.\n",
                     " Stop updating database!\n"))
      }
      
      ## make sure Dimension2 only presents when Dimension1 have presented.
      if("Dimension2" %in% names(data_df) & (!"Dimension1" %in% names(data_df))) {
         stop(paste0("'Dimension2' cannot be included when 'Dimension1' is not in dataset.\n",
                     " Stop updating database!\n"))
      }
      
      ## Check consistency between dataset and dataset info ------------------
      if("Dimension1" %in% names(data_df) & is.na(Dim1_desc)) {
         stop(paste0("'Dim1_desc' need to be specified.\n",
                     " Stop updating database!\n"))
      }
      if("Dimension2" %in% names(data_df) & is.na(Dim2_desc)) {
         stop(paste0("'Dim2_desc' need to be specified.\n",
                     " Stop updating database!\n"))
      }
      
      ## data format standardization and value check -------------------------
      data_df$Year <- as.integer(as.vector(data_df$Year))
      if(!all(unique(data_df$Year) %in% 1900:2100)) {
         stop(paste0("Year have to be between 1900 to 2100.\n",unique(data_df$ValueName)," failed to upload to local database!"))
      }
      
      #data_df$AreaID <- as.integer(as.vector(data_df$AreaID))
      #if(!all(concord_SectorToMain$Sector %in% unique(data_df$Sector))) {
      #   stop(paste0("Area ID have to be between 1 to 84.\n",unique(data_df$ValueName)," failed to upload to local database!"))
      #}
      
      data_df$Value <- as.numeric(as.vector(data_df$Value))
      ## add Dimension1 & Dimension2 if they are absent
      if(!"Dimension1" %in% names(data_df)) {
         data_df$Dimension1 <- NA
      }
      if(!"Dimension2" %in% names(data_df)) {
         data_df$Dimension2 <- NA
      }
      
      ########################################################################
      ## update DB source info -----------------------------------------------
      update.SRdb_Source(
         ValueName = VName, 
         ReleaseDate = ReleaseDate, 
         Frequency = Frequency,  
         AccessDate = AccessDate, 
         EffectiveDateFrom = EffectiveDateFrom, 
         EffectiveDateTo = EffectiveDateTo , 
         YrEndingMth = YrEndingMth, 
         Units = Units,
         Magnitudes = Magnitudes,
         TechNotes = TechNotes,
         Dim1_desc = Dim1_desc,
         Dim1_summability = Dim1_summability,
         Dim2_desc = Dim2_desc,
         Dim2_summability = Dim2_summability,         
         db_location = db_location
      )
      
      ## update DB data ------------------------------------------------------
      ## to local database
      if(db_location == "local" | db_location == "both"){
         ## get source info from local database
         load("data/SRdb_Source.rda")
         
         ## prepare dataset
         DataID <- as.vector(subset(SRdb_Source, ValueName == VName)$DatasetID) # get datasetID
         # sqldf(paste0("SELECT DatasetID FROM SRdb_Source WHERE ValueName = '", VName,"'")) # alternative method
         
         if(length(DataID)==0) {
            stop(paste0("ValueName has to be registered into SRdb_Source first!\n Stop updating ", VName," to local database!"))
         }
         
         data_df$DatasetID <- DataID
         
         ## standardise the dataset
         data_df <- sqldf("SELECT DatasetID, ValueName, Year, Sector, Dimension1, Dimension2, Value FROM data_df")
         
         ### remove the previously loaded data
         load("data/SRdb_Data.rda")
         SRdb_Data <- 
            sqldf(paste0("SELECT * FROM SRdb_Data WHERE DatasetID <> ", DataID))
         # subset(SRdb_Data, !DatasetID %in% DataID)
         
         ### append the new data into database
         if(nrow(SRdb_Data) == 0 & nrow(data_df) == 0) {
            print(paste0("Dataset cannot be empty!\n", VName," failed to upload to local database!"));
            next
         } else {
            if(nrow(SRdb_Data) == 0 & nrow(data_df) >0) {SRdb_Data <- data_df} else {
               SRdb_Data <- rbind(SRdb_Data, data_df)
               #                SRdb_Data  <- sqldf(c("INSERT INTO SRdb_Data SELECT * FROM data_df", "SELECT * FROM SRdb_Data"))
            }
         }
         save(SRdb_Data, file = "data/SRdb_Data.rda")
         
         print(paste0("Successfully upload '",VName,"' with datasetID ",DataID," to local database!"))
      }
      
      ## to server database
      if(db_location == "server" | db_location == "both"){
         ## get datasetID from server database
         DataID <- unlist(sqlQuery(PlayPen, paste0("SELECT DatasetID FROM SRdb_Source WHERE ValueName = '", VName,"'")))
         
         if(length(DataID)==0) {
            stop(paste0("ValueName has to be registered into SRdb_Source first!\n Stop updating ", VName," to sever database!"))
         }
         
         if(length(DataID)>2) {
            stop(paste0("ValueName is not unique for dataset!\n Stop updating ", VName," to sever database!"))
         }
         
         data_df$DatasetID <- DataID
         
         ## standardise the dataset
         data_df <- sqldf("SELECT DatasetID, ValueName, Year, Sector, Dimension1, Dimension2, Value FROM data_df")
         
         ## begin time
         ptm <- proc.time()
         
         ### remove the previously loaded data
         if(nrow(sqlQuery(PlayPen, paste0("SELECT * FROM dbo.SRdb_Data WHERE DatasetID = ",DataID))) > 0) {
            sqlQuery(PlayPen, paste0("DELETE FROM dbo.SRdb_Data WHERE DatasetID = ",DataID))
         }
         
         ### append the new data into database
         # sqlSave(PlayPen, data_df, "dbo.SRdb_Data", append = TRUE, rownames = FALSE) # this works when dataset is small
         ## upload data by bcp
         write.table(data_df, file = "data_raw/temp/data_df.dat",sep="\t",row.names=F,col.names=F,quote=F)
         system("bcp PlayPen.dbo.SRdb_Data in data_raw/temp/data_df.dat -Swin1179 -T -c")
         unlink("data_raw/temp/data_df.dat")
         
         ## end time
         print(proc.time() - ptm)
         print(paste0("Successfully upload '",VName,"' with datasetID ",DataID," to server database!"))
      }
   }
## Examples:
## update.srdb ----------------------------------------------------------



## update some columns in SRdb_Source for Dataset with ValueName --------
###################  Function #########################
update.SRdb_Source <- 
   function(
      ## ValueName of the dataset to be added/updated
      ValueName, 
      ## dataset source information to be added/updated
      ReleaseDate = NA, 
      Frequency = NA,  
      AccessDate = NA, 
      EffectiveDateFrom = NA, 
      EffectiveDateTo = NA , 
      YrEndingMth = NA, 
      Units = NA,
      Magnitudes = NA,
      TechNotes = NA,
      Dim1_desc = NA,
      Dim1_summability = NA,
      Dim2_desc = NA,
      Dim2_summability = NA, 
      ## database location (local/server/both)
      db_location = "local",
      ## other argument(s) to maintain compatibility (in case some other source information is required to be updated in this function) 
      ...
   ){
      VName <- ValueName
      
      if(db_location == "local" | db_location == "both"){
         ## load local database
         load("data/SRdb_Source.rda")
         
         ## update the SRdb_Source
         SRdb_Source <-
            sqldf(c(paste0(
               "UPDATE SRdb_Source SET ",
               ifelse(is.na(ReleaseDate),
                      paste0("   ReleaseDate = NULL,"),
                      paste0("	ReleaseDate = '",ReleaseDate,"',")
               ),
               ifelse(is.na(Frequency),
                      paste0("	Frequency = NULL,"),
                      paste0("	Frequency = '",Frequency,"',")
               ),
               ifelse(is.na(AccessDate),
                      paste0("	AccessDate = NULL,"),
                      paste0("	AccessDate = '",AccessDate,"',")
               ),
               ifelse(is.na(EffectiveDateFrom),
                      paste0("	EffectiveDateFrom = NULL,"),
                      paste0("	EffectiveDateFrom = '",EffectiveDateFrom,"',")
               ),
               ifelse(is.na(EffectiveDateTo),
                      paste0("	EffectiveDateTo = NULL,"),
                      paste0("	EffectiveDateTo = '",EffectiveDateTo,"',")
               ),
               ifelse(is.na(YrEndingMth),
                      paste0("	YrEndingMth = NULL, "),
                      paste0("	YrEndingMth = '",YrEndingMth,"',")
               ),
               ifelse(is.na(Units),
                      paste0("   Units = NULL,"),
                      paste0("	Units = '",Units,"',")
               ),
               ifelse(is.na(Magnitudes),
                      paste0("   Magnitudes = NULL,"),
                      paste0("	Magnitudes = '",Magnitudes,"',")
               ),
               ifelse(is.na(TechNotes),
                      paste0("	TechNotes = NULL,"),
                      paste0("	TechNotes = '",TechNotes,"',")
               ),
               ifelse(is.na(Dim1_desc),
                      paste0("	Dim1_desc = NULL,"),
                      paste0("	Dim1_desc = '",Dim1_desc,"',")
               ),
               ifelse(is.na(Dim1_summability),
                      paste0("	Dim1_summability = NULL,"),
                      paste0("	Dim1_summability = '",Dim1_summability,"',")
               ),
               ifelse(is.na(Dim2_desc),
                      paste0("	Dim2_desc = NULL,"),
                      paste0("	Dim2_desc = '",Dim2_desc,"',")
               ),
               ifelse(is.na(Dim2_summability),
                      paste0("	Dim2_summability = NULL "),
                      paste0("	Dim2_summability = '",Dim2_summability,"' ")
               ),
               "WHERE ValueName = '",VName,"'
               "),"SELECT * FROM main.SRdb_Source")
            )
         
         ## write back to file
         save(SRdb_Source, file = "data/SRdb_Source.rda")
      }
      
      if(db_location == "server" | db_location == "both"){
         ## update the SRdb_Source
         sqlQuery(PlayPen,
                  paste0(
                     "UPDATE dbo.SRdb_Source SET ",
                     ifelse(is.na(ReleaseDate),
                            paste0("	ReleaseDate = NULL,"),
                            paste0("	ReleaseDate = '",ReleaseDate,"',")
                     ),
                     ifelse(is.na(Frequency),
                            paste0("	Frequency = NULL,"),
                            paste0("	Frequency = '",Frequency,"',")
                     ),
                     ifelse(is.na(AccessDate),
                            paste0("	AccessDate = NULL,"),
                            paste0("	AccessDate = '",AccessDate,"',")
                     ),
                     ifelse(is.na(EffectiveDateFrom),
                            paste0("	EffectiveDateFrom = NULL,"),
                            paste0("	EffectiveDateFrom = '",EffectiveDateFrom,"',")
                     ),
                     ifelse(is.na(EffectiveDateTo),
                            paste0("	EffectiveDateTo = NULL,"),
                            paste0("	EffectiveDateTo = '",EffectiveDateTo,"',")
                     ),
                     ifelse(is.na(YrEndingMth),
                            paste0("	YrEndingMth = NULL, "),
                            paste0("	YrEndingMth = '",YrEndingMth,"',")
                     ),
                     ifelse(is.na(Units),
                            paste0("   Units = NULL,"),
                            paste0("	Units = '",Units,"',")
                     ),
                     ifelse(is.na(Magnitudes),
                            paste0("   Magnitudes = NULL,"),
                            paste0("	Magnitudes = '",Magnitudes,"',")
                     ),
                     ifelse(is.na(TechNotes),
                            paste0("	TechNotes = NULL,"),
                            paste0("	TechNotes = '",TechNotes,"',")
                     ),
                     ifelse(is.na(Dim1_desc),
                            paste0("	Dim1_desc = NULL,"),
                            paste0("	Dim1_desc = '",Dim1_desc,"',")
                     ),
                     ifelse(is.na(Dim1_summability),
                            paste0("	Dim1_summability = NULL,"),
                            paste0("	Dim1_summability = '",Dim1_summability,"',")
                     ),
                     ifelse(is.na(Dim2_desc),
                            paste0("	Dim2_desc = NULL,"),
                            paste0("	Dim2_desc = '",Dim2_desc,"',")
                     ),
                     ifelse(is.na(Dim2_summability),
                            paste0("	Dim2_summability = NULL "),
                            paste0("	Dim2_summability = '",Dim2_summability,"' ")
                     ),
                     
                     "WHERE ValueName = '",VName,"'"))
      }      
      ## check consistency between local and server database      
   }
## update some columns in SRdb_Source for Dataset with ValueName --------



##################### sr_get_dataset -------------------------------------------------------
###################  Function #########################
sr_get_dataset <- 
   function(valuename, db_location="local"){
      ## get dataset from DB -------------------------------------------------
      ## from local database
      if(db_location == "local" | db_location == "both"){
         load("data/SRdb_Source.rda");load("data/SRdb_Data.rda")
         #dtf <- dtf1 <- sqldf(paste0("
         #                              SELECT  *
         #                              FROM SRdb_Data d
         #                              WHERE d.ValueName =","'",valuename,"'"
         #                           ) 
         #                     ) %>%
         #               mutate( Sector = as.character(as.matrix(Sector)) )
         
          dtf <- dtf1 <- SRdb_Data %>%
                         dplyr::filter(ValueName == valuename) %>%
                         mutate( Sector = as.character(as.matrix(Sector)) )
      }
      ## from server database
      if(db_location == "server" | db_location == "both"){
         dtf <- dtf2 <-
            sqlQuery(PlayPen, paste0("
                                     SELECT  *
                                     FROM SRdb_Data d
                                     WHERE d.ValueName =","'",valuename,"'"
                                    )      
                     ) %>%
            mutate( Sector = as.character( as.matrix(Sector) ) )
      }
      ## from both database
      if(db_location == "both"){
         if(any(dim(dtf1) != dim(dtf2))){
            stop(paste0("The dimensions of dataset with ValueName '",valuename,"' do not match between local and server."))
         }
      }
      return(dtf)
}
## sr_get_dataset -------------------------------------------------------





## sr_crosscheck_db cross check database valuenames in tasklist, server and local
sr_crosscheck_database <- function(){
   ######################################################################
   ### Test if the SRdb_Data table in PlayPen and locally saved contains the same #  ######
   ### ValueNames defined in the task and issues manager file in  #######
   ### MAKO.                                                      #######
   ### According to the ValueNames differing between MAKO and     #######
   ### PlayPen, you can choose if you would like to initialize    #######
   ### SRdb_Data table in PlayPen.                                #######
   ######################################################################
   
   ### Download task and issues manager from MAKO in order to collect the existing ValueNames
   FileToDownload <- "http://mako.wd.govt.nz/otcsdav/nodes/29455837/Sector%20Report%202016%20task%20and%20issues%20manager.xlsx"
   DestFile <- "data_intermediate/listValueNames.xlsx"
   downloadMAKO(FileToDownload, DestFile)
   listValueNames <- as.character(read.xlsx2(DestFile, sheetName = "DB creation Overview")$ValueName)
   listValueNames <- listValueNames[listValueNames != ""]
   unlink(DestFile)
   
   ### Collect the existing ValueNames in the database
   ExistingValues_server <- as.character(sqlQuery(PlayPen, "SELECT distinct ValueName FROM dbo.SRdb_Data")$ValueName)
   
   if( file.exists('data/SRdb_Data.rda') ){
      load('data/SRdb_data.rda')
      ExistingValues_local <- unique(as.character(SRdb_Data$ValueName))
   } else{
      
   }
   
   
   ### Check if ValueNames in server database == ValueNames in MAKO
   if( !identical(sort(listValueNames), sort(ExistingValues_server)) ) {
      diffValue <- setdiff(sort(listValueNames), sort(ExistingValues_server))
      message("")
      message(paste0(diffValue, collapse = "\n"))
      print("The ValueNames above are missing from SRdb_Data in PlayPen.")
   } else {
      message("Great! The SRdb_Data table in PlayPen is identical to task and issues manager.")
   }
   
   ### Check if ValueNames in local database == ValueNames in MAKO
   if( !identical(sort(listValueNames), sort(ExistingValues_local)) ) {
      diffValue <- setdiff(sort(listValueNames), sort(ExistingValues_local))
      message("")
      message(paste0(diffValue, collapse = "\n"))
      print("The ValueNames above are missing from locally saved SRdb_Data.rda.")
   } else {
      message("Great! The locally saved SRdb_Data table is identical to task and issues manager.")
   }
}






## sector report_backup_database -----------------------------------------------------
sr_backup_database <- 
   function(backupPath = "data/backup"){
      SRdb_Sector <- sqlQuery(PlayPen, "SELECT * FROM SRdb_Sector")
      SRdb_Data <- sqlQuery(PlayPen, "SELECT * FROM SRdb_Data")
      SRdb_Source <- sqlQuery(PlayPen, "SELECT * FROM SRdb_Source")
      
      save(SRdb_Sector,SRdb_Data,SRdb_Source, file = paste0(backupPath, "/SRdb",today(),".rda"))
   }

## Examples:
#sr_backup_database()
## sr_check_dataset -----------------------------------------------------



## sr_clean_space -------------------------------------------------------
sr_clean_space <- function(){
   rm(list=setdiff(ls(), keepers))
   gc()
}
## sr_clean_space -------------------------------------------------------


## excel_col_num ----------------------------------------------------------
excel_col_num <- 
   function(col_label){
      col_num <- 0; len <- nchar(col_label)
      while(len >= 1){
         col_num <- 
            col_num * 26 + (strtoi(charToRaw(substr(col_label, 1, 1)), 16L) - strtoi(charToRaw("A"), 16L) + 1)
         len <- nchar(col_label <- substr(col_label, 2, len))
      }
      return(col_num)
   }
## Examples:
# excel_col_num("A")
# excel_col_num("J")
# excel_col_num("AA")
# excel_col_num("AAA")
# (1*26 +1)*26 +1
## excel_col_num ----------------------------------------------------------



## get_sr_dataset_pub ---------------------------------------------------
## function to get sr dataset from outside create_srdb project
get_sr_dataset_pub <-
   function(ValueName, PlayPen=odbcConnect("PlayPen_Prod")){
      ## get dataset from DB -------------------------------------------------
      dtf <- 
         sqlQuery(PlayPen, paste0("
                                  SELECT ValueName, Theme, YearPub, YrEndingMth, Units, Magnitudes, Area, AreaType, Year, Dimension1, Dim1_desc, Dim1_summability, Dimension2, Dim2_desc, Dim2_summability, Value
                                  FROM dbo.srdbReport
                                  WHERE ValueName = '",ValueName,"'")     
         )
      if(all(is.na(dtf$Dimension2))) {dtf$Dimension2 <- dtf$Dim2_summability <- dtf$Dim2_desc <- NULL}
      if(all(is.na(dtf$Dimension1))) {dtf$Dimension1 <- dtf$Dim1_summability <- dtf$Dim1_desc <- NULL}
      return(dtf)
   }

# Average_House_Price <- get_sr_dataset_pub("Average house price")
# Median_House_Price <- get_sr_dataset_pub("Median House Price")
## get_sr_dataset_pub ---------------------------------------------------



## convert "(blank)" in cells to na value ---------------------------------
blank2na <- function(x){rename.levels(factor(x), orig = "(blank)", new = NA)}
## Examples:
## convert "(blank)" in cells to na value ---------------------------------



## delete.srdb.dataset --------------------------------------------------
delete.srdb.server.dataset <-
   function(ValueName = NULL, DatasetID = NULL) {
      if(is.null(DatasetID)){
         DatasetID <- 
            sqlQuery(PlayPen, 
                     paste0("SELECT DatasetID FROM [PlayPen].[dbo].[SRdb_Source] WHERE ValueName = '", ValueName,"'"))
      }
      
      if(nrow(DatasetID) == 1) {
         sqlQuery(PlayPen, 
                  paste0("DELETE FROM [PlayPen].[dbo].[SRdb_Source] WHERE DatasetID = ", DatasetID))
         sqlQuery(PlayPen, 
                  paste0("DELETE FROM [PlayPen].[dbo].[SRdb_Data] WHERE DatasetID = ", DatasetID))
         
      }
   }
## Examples:
# delete.srdb.server.dataset(ValueName = "Annual International Visitors (YEDec)")
## delete.srdb.dataset --------------------------------------------------



## remove data frame levels -----------------------------------------------
remove.levels <- 
   function (dtf) {
      as.data.frame(apply(dtf, 2, as.vector), stringsAsFactors = FALSE)
   }
## remove data frame levels -----------------------------------------------



## reset data frame levels ------------------------------------------------
reset.levels <- 
   function (dtf) {
      as.data.frame(apply(dtf,2,function(x) {if(is.factor(x)) factor(x) else x}))
   }
## reset data frame levels ------------------------------------------------


## fix date format after read from xlsx -----------------------------------
fix.excel.date <- 
   function(date) {
      return(as.Date(as.integer(as.vector(blank2na(date)))-25569, origin = "1970-01-01"))
   }
## Examples:
# fix.excel.date(42034)
## fix date format after read from xlsx -----------------------------------


#### This is for plot rworldmap without the border color for pies
######################################################3
rwmNewMapPlot <- function(mapToPlot=getMap(),
                          oceanCol=NA,
                          mapRegion="world",
                          xlim=NA,                  
                          ylim=NA,
                          aspect=1){
   
   #browser()
   
   ## setting map extents if a mapRegion has been specified
   if (mapRegion != "world"){
      dFwesn <- setMapExtents(mapRegion)
      xlim <- c(dFwesn$we, dFwesn$ea)
      ylim <- c(dFwesn$so, dFwesn$no)
   }
   
   #getting xlim & ylim from bbox of map if they haven't been specified
   if (length(xlim)<2) xlim <- bbox(mapToPlot)['x',]  
   if (length(ylim)<2) ylim <- bbox(mapToPlot)['y',]
   
   
   plot.new()
   
   #replicate behaviour of plot.Spatial in the sp package regarding aspect
   #if the map is unprojected the aspect is set based upon the mean y coord
   #only if region not 'world'
   if (aspect == 'variable' & mapRegion != "world")
      aspect <- ifelse(is.na(proj4string(mapToPlot)) || is.projected(mapToPlot),
                       1, 1/cos((mean(ylim) * pi)/180))
   
   plot.window(xlim=xlim,ylim=ylim,asp=aspect)#,xaxs='i',yaxs='i')#,bg=oceanCol,xpd=NA)
   
   #rect(xlim[1],ylim[1],xlim[2],ylim[2],col=oceanCol,border=oceanCol)
   #making the rectangle as big as the whole map should ensure it fills
   rect(mapToPlot@bbox[1],mapToPlot@bbox[2],mapToPlot@bbox[3],mapToPlot@bbox[4],col=oceanCol,border=oceanCol)
   
   #26/3/13 returning xlim & ylim so it can be used elsewhere
   lims <- data.frame(xlim,ylim)
   invisible(lims)
   
}


###########################################################################################
mapPies2 <- 
   function (dF, nameX = "LON", nameY = "LAT", nameZs = c(names(dF)[3], 
                                                          names(dF)[4]), zColours = c(1:length(nameZs)), ratio = 1, 
             addCatLegend = TRUE, addSizeLegend = TRUE, symbolSize = 1, 
             maxZVal = NA, xlim = NA, ylim = NA, mapRegion = "world", 
             borderCol = "grey", oceanCol = NA, landCol = NA, add = FALSE, 
             main = "", lwd = 0.5, pieBorder = NA, ...) 
   {
      
      
      functionName <- as.character(sys.call()[[1]])
      if (mapRegion == "data") {
         xlim <- c(min(dF[, nameX], na.rm = TRUE), max(dF[, nameX], 
                                                       na.rm = TRUE))
         ylim <- c(min(dF[, nameY], na.rm = TRUE), max(dF[, nameY], 
                                                       na.rm = TRUE))
      }
      if (class(dF) == "SpatialPolygonsDataFrame") {
         centroidCoords <- coordinates(dF)
         dF[["nameX"]] <- centroidCoords[, 1]
         dF[["nameY"]] <- centroidCoords[, 2]
         nameX <- "nameX"
         nameY <- "nameY"
         if (!add) {
            lims <- rwmNewMapPlot(mapToPlot = dF, oceanCol = oceanCol, 
                                  mapRegion = mapRegion, xlim = xlim, ylim = ylim)
            xlim <- lims$xlim
            ylim <- lims$ylim
            
            detach('package:graphics')
            plot(dF, add = TRUE, border = borderCol, col = landCol, 
                 main = main, lwd = lwd)
            library('graphics')   
            
         }
         dF <- dF@data
      }
      else if (!add) {
         lims <- rwmNewMapPlot(mapToPlot = getMap(), oceanCol = oceanCol, 
                               mapRegion = mapRegion, xlim = xlim, ylim = ylim)
         xlim <- lims$xlim
         ylim <- lims$ylim
         
         detach('package:graphics')
         plot(getMap(), add = TRUE, border = borderCol, col = landCol, 
              main = main, lwd = lwd)
         library('graphics')
         
      }
      maxSumValues <- 0
      for (locationNum in 1:length(dF[, nameZs[1]])) {
         sumValues <- sum(dF[locationNum, nameZs], na.rm = TRUE)
         if (sumValues > maxSumValues) 
            maxSumValues <- sumValues
      }
      symbolMaxSize <- 0.02 * max(xlim[2] - xlim[1], (ylim[2] - 
                                                         ylim[1]) * ratio)
      symbolScale <- symbolMaxSize/sqrt(maxSumValues)
      cat("symbolMaxSize=", symbolMaxSize, " maxSumValues=", maxSumValues, 
          " symbolScale=", symbolScale, "\n")
      
      
      
      for (locationNum in 1:length(dF[, nameZs[1]])) {
         sliceValues <- as.numeric(dF[locationNum, nameZs])
         if (sum(sliceValues, na.rm = TRUE) == 0) 
            next
         cumulatProps <- c(0, cumsum(sliceValues)/sum(sliceValues, 
                                                      na.rm = TRUE))
         pointsInCircle = 360
         radius <- sqrt(sum(sliceValues, na.rm = TRUE)) * symbolScale
         radius <- radius * symbolSize
         for (sliceNum in 1:length(sliceValues)) {
            n <- max(2, floor((pointsInCircle * (cumulatProps[sliceNum + 
                                                                 1] - cumulatProps[sliceNum]))))
            P <- list(x = ratio * radius * cos(2 * pi * seq(cumulatProps[sliceNum], 
                                                            cumulatProps[sliceNum + 1], length = n)) + dF[locationNum, 
                                                                                                          nameX], y = radius * sin(2 * pi * seq(cumulatProps[sliceNum], 
                                                                                                                                                cumulatProps[sliceNum + 1], length = n)) + dF[locationNum, 
                                                                                                                                                                                              nameY])
            
            polygon(c(P$x, dF[locationNum, nameX]), c(P$y, dF[locationNum, 
                                                              nameY]), col = zColours[sliceNum],
                    border = pieBorder )
            
         }
      }
      
      
      if (addCatLegend) 
         legend("bottomleft", legend = nameZs, fill = zColours, 
                cex = 0.7, bg = "white")
      radius <- symbolMaxSize * symbolSize
      plotExtents <- par("usr")
      plotS <- plotExtents[3]
      plotW <- plotExtents[1]
      plotN <- plotExtents[4]
      plotE <- plotExtents[2]
      centreE <- plotW + radius * 2 * ratio
      centreN <- plotN - radius * 2
      t <- seq(0, 2 * pi, length = 100)
      P <- list(x = ratio * radius * cos(t) + centreE, y = radius * 
                   sin(t) + centreN)
      str(P)
   }


#### function
FormatDollars <- 
   function (x, endmark = "", ...) {
      x <- paste0("$", format(round(x, ...), big.mark = ","), endmark)
      x <- gsub(" ", "", x)
      return(x)
}












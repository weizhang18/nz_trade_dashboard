##
##    Programme:  Upload_To_TRED.r
##
##    Objective:  I've uploaded the classifications into TRED.  Now lets up load the
##                commodity level data into the PlayPen using the FlexiETL
##
##    Author:     James Hogan, 31 July 2015
##


# 
#    x <- ls()
#    x <- x[1:length(x)][!(x[1:length(x)] %in% c("curl", "Global_Directory"))]
#    rm(list=x)
   
   ##
   ##    Load the raw data back into memory
   ## 
      #TRED    <- odbcConnect("TRED_Prod")
      #PlayPen <- odbcConnect("PlayPen_Prod")

      ##
      ##       The below code clears the decks
      ##
      ##                  
            # SQL  <- "delete from TimeSeries.Dataset"
            # sqlQuery(PlayPen, SQL)

            # SQL  <- "delete from TimeSeries.Unit"
             # sqlQuery(PlayPen, SQL)

            # SQL  <- "delete from TimeSeries.ClassificationCode"
            # sqlQuery(PlayPen, SQL)
            
            # SQL  <- "delete from TimeSeries.Magnitude"
            # sqlQuery(PlayPen, SQL)
            
            # SQL  <- "delete from TimeSeries.ClassReference"
            # sqlQuery(PlayPen, SQL)

            # SQL  <- "delete from TimeSeries.Data"
            # sqlQuery(PlayPen, SQL)

            # SQL  <- "delete from TimeSeries.Ordered"
            # sqlQuery(PlayPen, SQL)

            # SQL  <- "delete from TimeSeries.Dataset_ToAdd"
            # sqlQuery(PlayPen, SQL)

            
   ##
   ##    Transform for ETL
   ##
      #load("data\\Exports_By_Country.rda")
      
      #Exports_By_Country <- Exports_By_Country[((Exports_By_Country$Country == 'Japan') &
      #                                          year(Exports_By_Country$Date ) == 2014), 
      #                                          !(names(Exports_By_Country) %in% c('Harmonised_System_Description','Total_Exports_NZD_fob', 'Total_Exports_Qty', 'Status'))]
      
      Exports_By_Country <- Exports_By_Country[, !(names(Exports_By_Country) %in% c('Harmonised_System_Description','Total_Exports_NZD_fob', 'Total_Exports_Qty', 'Status'))]
      Exports_By_Country <- melt(Exports_By_Country,
                                 id = c("Harmonised_System_Code", "Unit_Qty", "Country", "Date"),
                                 measure.vars = c("Exports_NZD_fob", "Exports_Qty", "Re_exports_NZD_fob", "Re_exports_Qty"))
      Exports_By_Country$Source <- "Exports"

   ##
   ##       Upload the Datafiles
   ##                  
      names(Exports_By_Country)[names(Exports_By_Country) == "variable"] = "Measure"
      
      Exports_By_Country$Unit_Qty <- rename.levels(Exports_By_Country$Unit_Qty,
                                                   orig=c("" ),
                                                    new=c("UNKNOWN" ))	 
      
      # DataSeries <- list(
      #                     `Trade Data`  = data.frame( Dataseries_Name = 'New Zealand Overseas Merchandise Trade: Exports by Commodity and Country',
      #                                                 Prefix          = 'OMT_MBIE',
      #                                                 Group           = 'Group: Overseas_Trade - MBIE',
      #                                                 Classifications = c("Harmonised_System_Code","Unit_Qty", "Country", "Measure"),
      #                                                 Time_Variable   = "Date",
      #                                                 Value           = "value",
      #                                                 Unit_of_Measure = "See_Unit_Qty",
      #                                                 Magnitude       = "Units")
      #                   )
      # 
      # 
      #                   
      # Upload_Status <- Flexi_ETL(Source_Data = Exports_By_Country,
      #                            DataSeries  = DataSeries, 
      #                            PlayPen, TRED)      
      # rm(Exports_By_Country)
      gc()

      
      
      #Source_Data <- Exports_By_Country
      
      
      ###################################################################################################################################################                           
                                                                 
      
      #load("data\\Imports_By_Country.rda")
      
      Imports_By_Country <- Imports_By_Country[, !(names(Imports_By_Country) %in% c('Harmonised_System_Description', 'Status'))]
      Imports_By_Country <- melt(Imports_By_Country,
                                 id = c("Harmonised_System_Code", "Unit_Qty", "Country", "Date"),
                                 measure.vars = c("Imports_NZD_vfd", "Imports_Qty"))  ## change from cif to vfd
      Imports_By_Country$Source <- "Imports"
                                                         
      names(Imports_By_Country)[names(Imports_By_Country) == "variable"] = "Measure"
      Imports_By_Country$Unit_Qty <- rename.levels(Imports_By_Country$Unit_Qty,
                                                   orig=c("" ),
                                                    new=c("UNKNOWN" ))	      
       
      # DataSeries <- list(
      #                     `Trade Data`  = data.frame( Dataseries_Name = 'New Zealand Overseas Merchandise Trade: Imports by Commodity and Country',
      #                                                 Prefix          = 'OMT_MBIE',
      #                                                 Group           = 'Group: Overseas_Trade - MBIE',
      #                                                 Classifications = c("Harmonised_System_Code","Unit_Qty", "Country", "Measure"),
      #                                                 Time_Variable   = "Date",
      #                                                 Value           = "value",
      #                                                 Unit_of_Measure = "See_Unit_Qty",
      #                                                 Magnitude       = "Units")
      #                   )
      #                   
      # Upload_Status <- Flexi_ETL(Source_Data = Imports_By_Country,
      #                            DataSeries  = DataSeries, 
      #                            PlayPen, TRED)      
      #                       
                                 
      save(Exports_By_Country, file = "data/Exports_By_Country_shiny.rda")
      save(Imports_By_Country, file = "data/Imports_By_Country_shiny.rda")          
      
      ###########################################################################
      ## remove unused objects
      rm(list=setdiff(ls(), keepers))
      gc()
      ###########################################################################
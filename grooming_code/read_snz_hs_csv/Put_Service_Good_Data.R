#### download service data from inforshare ---------

# ##### ------------------ Services ( Credit as exports and Debt as imports)
# var_name <- 'BPM6 Services by country, year ended in quarter (Qrtly-Mar/Jun/Sep/Dec)'
 ValueName_ex <- 'New Zealand Goods and Services Exports by Country'
 ValueName_im <- 'New Zealand Goods and Services Imports by Country'
# 
# ##### 
# dtf_raw <- ImportTS2(TRED, var_name, GroupCountries= FALSE )

#### read data
dtf_raw_csv1 <- read.csv( file_service_by_country_hist1)
dtf_raw_csv2 <- read.csv( file_service_by_country_hist2)
dtf_raw_csv_new <- read.csv( file_service_by_country )

## merge data
dtf_raw_csv <- 
   dtf_raw_csv1 %>%
   bind_rows( dtf_raw_csv2) %>%
   bind_rows( dtf_raw_csv_new )

#dtf_raw_csv <- read.csv( "data_raw/service_by_country/service_imports_exports_by_country.csv" )

dtf_raw_csv %<>%
   dplyr::select( -Data )


##
dtf_raw_csv$TimePeriod <- 
   fill_missing_cell( dtf_raw_csv$TimePeriod )

dtf_raw_csv$Type <- 
   fill_missing_cell( dtf_raw_csv$Type )

## merge two
dtf_raw_new <-
   dtf_raw_csv %>%
   dplyr::select( TimePeriod, Type, Country, Credit ) %>%
   mutate( Value = Credit, 
           Flow = "Credit") %>%
   dplyr::select( -Credit ) %>%
   bind_rows( 
      dtf_raw_csv %>%
         dplyr::select( TimePeriod, Type, Country, Debit ) %>%
         mutate( Value = Debit, 
                 Flow = "Debit")%>%
         dplyr::select( -Debit )
      ) %>%
   mutate( Value = as.numeric(Value) ) %>%
   mutate( TimePeriod = as.Date( as.yearqtr( as.numeric(substr(TimePeriod,1,4)) + (as.numeric(substr(TimePeriod, 6,6))-1)/4  ), frac =1 ) ) %>%
   dplyr::rename(CV3 = Type, CV4 = Country, CV2 = Flow)

dtf_raw <- dtf_raw_new


## lastest year_end is
(maxYear <- max(dtf_raw$TimePeriod))
(maxQtr <- substr(as.yearqtr(maxYear), 6,7))
(marketToDelet <- c("APEC",
                    "ASEAN",
                    "ASIA",
                    "EU",                     
                    "Europe",
                    "Eurozone",
                    "GCC", 
                    "Latin", 
                    "Latin America",
                    "America", 
                    "OECD", 
                    "Pacific Island Forum") 
)

## if Year need to be +1 or -1
YearQtr <- ifelse( maxQtr =='Q1', 
                   c('Q1'),
                   ifelse( maxQtr == 'Q2',
                           c('Q2','Q1'),
                           ifelse( maxQtr == 'Q3',
                                   c('Q3', 'Q2','Q1'),
                                   c('Q4','Q3', 'Q2','Q1')
                           )
                   )
)


# groom data
dtf_service <-
   dtf_raw %>%
   filter( substr(as.yearqtr( TimePeriod ), 6,7) == maxQtr  ) %>%
   mutate( Year = year(TimePeriod),
           CV2 = ifelse( CV2 == 'Credit', 'Exports (Services)', 'Imports (Services)')
   ) %>%
   dplyr::select(  Year, Sector = CV3, Dimension1 = CV4, Dimension2 = CV2, Value) %>%
   filter( !Dimension1 %in% marketToDelet ) %>% ## delete the grouped markets
   mutate( Dimension1 = rename.levels( Dimension1, 
                                       orig = c("Country unspecified", "Hong Kong (SAR)", "Samoa"),
                                       new = c("Destination Unknown - EU","Hong Kong (Special Administrative Region)","Samoa, Western")
   ) 
   ) %>%
   filter( Sector %in% concord_ExportHSCode$HS_code[concord_ExportHSCode$Type=='Services']) ## This is to align market names with Goods EX/IM data


###
dtf_service_ex <-
   dtf_service %>%
   filter( Dimension2 == 'Exports (Services)' ) %>%
   mutate( ValueName = ValueName_ex ) %>%
   dplyr::select( ValueName, Year, Dimension1 = Sector, Dimension2 = Dimension1, Value) %>%
   mutate( Sector = paste0('Total industry ', 'year ended ', maxQtr) )

##
dtf_service_im <-
   dtf_service %>%
   filter( Dimension2 == 'Imports (Services)' ) %>%
   mutate( ValueName = ValueName_im ) %>%
   dplyr::select( ValueName, Year, Dimension1 = Sector, Dimension2 = Dimension1, Value) %>%
   mutate( Sector = paste0('Total industry ', 'year ended ', maxQtr) )



###### -------------------------------------- Exports of Goods ----------------------------------------------
#dtf_ex_raw <- ImportTS2(TRED, "New Zealand Overseas Merchandise Trade: Exports by Commodity and Country")

load("data/Exports_By_Country_shiny.rda")
dtf_ex_raw <- 
   Exports_By_Country %>%
   dplyr::rename( TimePeriod = Date, CV2 = Harmonised_System_Code, CV1 = Country, CV3 = Measure, Value = value )

## lastest year_end is
( maxYear_g <- max(dtf_ex_raw$TimePeriod) )
( maxQtr_g <- substr(as.yearqtr(maxYear_g), 6,7) )
( maxMon_g <- month(max(dtf_ex_raw$TimePeriod)) )

## check if goods and services data have the same time period 
if( maxYear != maxYear_g ){
   maxYear_s_g <- min( maxYear, maxYear_g )
   maxYear_g <-  maxYear_s_g
   ( maxQtr_g <- substr(as.yearqtr(maxYear_g), 6,7) )
   ( maxMon_g <- month(max(maxYear_g)) )
   #rm(list=setdiff(ls(), keepers))
   warning("Goods and services data have DIFFERENT time period!!! Dates are aligned.")
}

## only have Q1 to Q4 month
ifelse( maxMon_g < 3, 
        maxMon_g <- 12,
        ifelse( maxMon_g < 6, maxMon_g <- 3,
                ifelse( maxMon_g < 9, maxMon_g <- 6,
                        ifelse( maxMon_g < 12, maxMon_g <- 9, maxMon_g )
                ) 
        )
)

## construct targe date -- the month falls on Dec, March, Jun, Sep
target_month <- as.yearmon( max(year( maxYear_g )) + (maxMon_g-1)/12  )

###
dtf_goods_ex <- 
   dtf_ex_raw %>%
   dplyr::select( TimePeriod, CV1, CV2, CV3, Value ) %>%
   filter( CV3 %in% c('Exports_NZD_fob','Re_exports_NZD_fob' ) ) %>% ## SNZ report total export (exports + re_exports)
   #group_by( TimePeriod, CV1, CV2, CV3 ) %>%
   #dplyr::summarise( Value = sum(Value, na.rm=T) ) %>%
   #ungroup %>%
   mutate( Dimension1 = substr(CV2, 1,6), ## HS6
           Dimension2 = CV1) %>%
   dplyr::select(-CV1, -CV2 ) %>%
   group_by( TimePeriod, Dimension1, Dimension2  ) %>%
   dplyr::summarise( Value = sum(Value, na.rm=T) ) %>%
   ungroup %>%
   filter( as.yearmon(TimePeriod) <= target_month ) %>% 
   mutate( Year = YearEnd( TimePeriod, maxMon_g ) ) %>%
   #mutate( Year_qtr = as.yearqtr(TimePeriod) ) %>%
   #group_by( Year_qtr, Dimension1, Dimension2  ) %>%
   #summarise( Value = sum(Value, na.rm=T) ) %>%
   #ungroup %>%
   #mutate( Year = ifelse( substr(Year_qtr, 6,7) == YearQtr,
   #                       as.numeric(substr(Year_qtr, 1,4)),
   #                       as.numeric(substr(Year_qtr, 1,4))+1) 
   #        )%>%
   group_by( Year, Dimension1, Dimension2  ) %>%
   dplyr::summarise( Value = sum(Value, na.rm=T)/10^6 ) %>%
   ungroup %>%
   mutate( Sector = paste0('Total industry ', 'year ended ', maxQtr_g),
           ValueName = ValueName_ex )

## caculate total good exports by country
tmp_total_ex <- 
   dtf_goods_ex %>%
   group_by( Year, Dimension2, Sector, ValueName ) %>%
   dplyr::summarise( Value = sum(Value, na.rm=T) ) %>%
   ungroup %>%
   mutate( Dimension1 = 'Total goods' )

dtf_goods_ex <- 
   dtf_goods_ex %>%
   bind_rows( tmp_total_ex )

##### -------------------------------------- Imports of Goods
#dtf_im_raw <- ImportTS2(TRED, "New Zealand Overseas Merchandise Trade: Imports by Commodity and Country")

load("data/Imports_By_Country_shiny.rda")
dtf_im_raw <- 
   Imports_By_Country %>%
   dplyr::rename( TimePeriod = Date, CV2 = Harmonised_System_Code, CV1 = Country, CV3 = Measure, Value = value )

dtf_goods_im <- 
   dtf_im_raw %>%
   dplyr::select( TimePeriod, CV1, CV2, CV3, Value ) %>%
   filter( CV3 == 'Imports_NZD_vfd' ) %>%  ## change to vdf as cif inlcudes freight and insurance costs
   mutate( Dimension1 = substr(CV2, 1,6), ## HS 6
           Dimension2 = CV1) %>%
   dplyr::select(-CV1, -CV2, -CV3) %>%
   group_by( TimePeriod, Dimension1, Dimension2  ) %>%
   dplyr::summarise( Value = sum(Value, na.rm=T) ) %>%
   ungroup %>%
   filter( as.yearmon(TimePeriod) <= target_month ) %>% 
   mutate( Year = YearEnd( TimePeriod, maxMon_g ) ) %>%
   # mutate( Year_qtr = as.yearqtr(TimePeriod) ) %>%
   # group_by( Year_qtr, Dimension1, Dimension2  ) %>%
   # summarise( Value = sum(Value, na.rm=T) ) %>%
   # ungroup %>%
   # mutate( Year = ifelse( substr(Year_qtr, 6,7) == YearQtr,
   #                        as.numeric(substr(Year_qtr, 1,4)),
   #                        as.numeric(substr(Year_qtr, 1,4))+1) 
   # )%>%
   group_by( Year, Dimension1, Dimension2  ) %>%
   dplyr::summarise( Value = sum(Value, na.rm=T)/10^6 ) %>%
   ungroup %>%
   mutate( Sector = paste0('Total industry ', 'year ended ', maxQtr_g),
           ValueName = ValueName_im )

## caculate total good imports by country
tmp_total_im <- 
   dtf_goods_im %>%
   group_by( Year, Dimension2, Sector, ValueName ) %>%
   dplyr::summarise( Value = sum(Value, na.rm=T) ) %>%
   ungroup %>%
   mutate( Dimension1 = 'Total goods' )

dtf_goods_im <- 
   dtf_goods_im %>%
   bind_rows( tmp_total_im )

## bind both ex goods and services
dtf_ex_country_yr <-
dtf_ex <- 
   dtf_goods_ex %>%
   bind_rows( dtf_service_ex )


## bind goods and services
dtf_im_country_yr <-
dtf_im <- 
   dtf_goods_im %>%
   bind_rows( dtf_service_im )


save(dtf_ex_country_yr, file = "data/Exports_shiny.rda")
save(dtf_im_country_yr, file = "data/Imports_shiny.rda")     

###########################################################################
## remove unused objects
rm(list=setdiff(ls(), keepers))
gc()
###########################################################################
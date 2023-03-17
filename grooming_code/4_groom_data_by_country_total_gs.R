##
load( paste0(output_folder_shiny,"/dtf_shiny.rda"))

##. Derive country level data by total goods and total services ---------------------------------------
dtf_shiny_country_gs <-
   dtf_shiny %>%
   filter( Commodity %in% c('Total goods','Total services'),
           Year >= 2000, 
           Country != 'New Zealand')

## get countries' ISO2
tmp_country_iso2 <- 
   data.frame(Country = sort(unique(dtf_shiny_country_gs$Country) ), 
              ISO2 =countrycode( sort(unique(dtf_shiny_country_gs$Country) ),  origin = 'country.name', destination = 'iso2c')
   )

## add ISO2 for this country
tmp_country_iso2 %<>%
   mutate( ISO2 = as.character(ISO2) ) %>%
   mutate( ISO2 = ifelse(Country== 'Micronesia', 'FM', ISO2) ) %>%
   mutate( ISO2 = ifelse(Country== 'Virgin Islands, United States', 'VI', ISO2) )

## merge with lat and long
concord_country_iso_latlon_raw <-
tmp_country_iso2_latlon <-
   tmp_country_iso2 %>%
   left_join( concord_country %>% dplyr::select( -Country ), by = 'ISO2' )

## keep in enviroment
keepers <- c(keepers, "concord_country_iso_latlon_raw")

#save( concord_country_iso_latlon_raw, file = 'shiny/concord_country_iso_latlon_raw.rda')
save( concord_country_iso_latlon_raw, file = 'shiny/data/concord_country_iso_latlon_raw.rda')

## merge with main data set
dtf_shiny_country_gs %<>%
   left_join( tmp_country_iso2_latlon, by = 'Country' ) 


## find out about the services data where countries are missed and add them back in
dtf_trade_country_snz <- read.csv( file_map_goods_services_by_country )
tmp_year <- unique( dtf_trade_country_snz$Year )
tmp_dtf_shiny_country_gs_rest <- data.frame()
tmp_dtf_shiny_rest <- data.frame()

for( i_year in tmp_year){
   ##
   print( i_year )
   ## countries from SNZ csv map data
   tmp_snz_data_country <- 
      dtf_trade_country_snz %>%
      filter( Year == i_year ) %>%
      dplyr::select( country_code ) %>%
      as.matrix %>%
      as.character 
   
   tmp_snz_data_country[ is.na( tmp_snz_data_country) ] <- 'NA' ## for Namibia
   tmp_snz_data_country <- tmp_snz_data_country[  tmp_snz_data_country != 'NZ' ] ## remove New Zealand
   
   ## countries from SNZ infoshare service by country data
   tmp_infoshare_data_country <-
      dtf_shiny_country_gs %>%
      filter( Year == i_year,
              Type_gs == 'Services',
              !is.na(ISO2) ) %>%
      dplyr::select( ISO2 ) %>%
      as.matrix %>%
      as.character %>%
      unique
      
   ## found about missing countires
   tmp_country_missing <- setdiff( tmp_snz_data_country, tmp_infoshare_data_country )
   
   print( tmp_country_missing )
   ## get the data
   tmp_snz_sub_data <-
      dtf_trade_country_snz %>%
      filter( Year == i_year,
              country_code %in% tmp_country_missing ) %>%
      dplyr::select( Year, country_code, country_name,Services_Exports, Services_Imports ) %>%
      mutate( Services_Exports = Services_Exports * 10^6 , 
              Services_Imports = Services_Imports * 10^6 ) %>%
      gather( Type, Value , -Year, -country_code, -country_name ) %>%
      mutate( Type_gs = 'Services',
              Type_ie = ifelse( grepl("Exports", Type), 'Exports', 'Imports' ),
              Commodity = 'Total services', 
              ISO2 = country_code,
              Note = unique(dtf_shiny_country_gs$Note) ) %>%
      dplyr::select( -country_code, -country_name , -Type) %>%
      left_join( tmp_country_iso2_latlon, by = 'ISO2'  )
   
   ## put data back to by country
   tmp_dtf_shiny_country_gs_rest %<>%
      bind_rows( tmp_snz_sub_data )
   
   ## put data back to dtf_shiny
   tmp_dtf_shiny_rest %<>%
      bind_rows( tmp_snz_sub_data %>%
                    dplyr::select( -lat, -lon, -ISO2 ) %>%
                    bind_rows( 
                       tmp_snz_sub_data %>%
                          dplyr::select( -lat, -lon, -ISO2 ) %>%
                          mutate( Commodity = 'Other services' )
                       )
                 )
}


## row bind back to the main data frames
dtf_shiny_country_gs %<>%
   bind_rows( tmp_dtf_shiny_country_gs_rest )

dtf_shiny %<>%
   bind_rows( tmp_dtf_shiny_rest )

## save data
#save( dtf_shiny_country_gs, file = 'shiny/dtf_shiny_country_gs.rda' ) 
save( dtf_shiny_country_gs, file = paste0(output_folder_shiny,'/dtf_shiny_country_gs.rda' ) )

## save data to shiny folder
#save( dtf_shiny, file = 'shiny/dtf_shiny.rda' )
save( dtf_shiny, file = paste0( output_folder_shiny ,'/dtf_shiny.rda' ) )


###########################################################################
## remove unused objects
rm(list=setdiff(ls(), keepers))
gc()
###########################################################################

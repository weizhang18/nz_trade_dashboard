## 2. People movement data --------------
# var_name_out <- "NZ-resident traveller departures by EVERY country of main dest and purpose (Qrtly-Mar/Jun/Sep/Dec)"
# var_name_in <-  "Visitor arrivals by EVERY country of residence and purpose (Qrtly-Mar/Jun/Sep/Dec)"
# 
# dtf_raw_out <- 
#    ImportTS2(TRED, var_name_out) 
# 
# dtf_raw_in <- 
#    ImportTS2(TRED, var_name_in) 

### group data
# dtf_in_out <- 
#    dtf_raw_out %>%
#    mutate( Type = 'NZ visitors travelling out' ) %>%
#    bind_rows( dtf_raw_in %>% mutate( Type = 'Foreign visitors travelling in' ) 
#               ) %>%
#    filter( CV2 == 'TOTAL ALL TRAVEL PURPOSES',
#            !is.na(Value),
#            Value !=0 ) %>%
#    dplyr::select( TimePeriod, Country_in_out = CV1, Type, Value ) %>%
#    mutate( Year =  YearEnd( TimePeriod, month(max(dtf_trade_tot$TimePeriod)) ),
#            Note = paste0('Year ended ', substr( as.yearqtr( max(dtf_trade_tot$TimePeriod)), 6,7 ) ) 
#            ) %>%
#    filter( Year <= max(dtf_im_country_yr$Year),
#            Year >= 2007) %>%
#    group_by( Year, Type, Country_in_out, Note ) %>%
#    summarise( Value = sum(Value, na.rm=T) ) %>%
#    ungroup %>%
#    mutate( Country_in_out  = ifelse(Country_in_out %in% c("TOTAL ALL COUNTRIES OF RESIDENCE",
#                                                           "TOTAL ALL COUNTRIES OF MAIN DESTINATION" ),
#                                     'World', 
#                                     Country_in_out ) ) %>%
#    filter( !Country_in_out %in%c("AFRICA AND THE MIDDLE EAST",
#                                  "AMERICAS",
#                                  "ASIA",
#                                  "EUROPE",
#                                  "NOT STATED",
#                                  "OCEANIA") )

## read ppl_in data ----
dtf_raw_in <-
   read.csv( "data_raw/ppl_in.csv" )

dtf_raw_in$TimePeriod <- 
   fill_missing_cell( dtf_raw_in$TimePeriod ) 

dtf_raw_in %<>%
   dplyr::rename( Value = `TOTAL.ALL.TRAVEL.PURPOSES` ) %>%
   mutate( TimePeriod = as.Date(as.yearqtr(as.character(TimePeriod), format = "%YQ%q") , frac = 1))

## read ppl_out data ----
dtf_raw_out_hist <-
   read.csv( "data_raw/ppl_out_hist.csv" ) %>%
   filter( CV2 == "TOTAL ALL TRAVEL PURPOSES"  ) %>%
   dplyr::rename( Country = CV1  ) %>%
   dplyr::select( TimePeriod, Country, Value ) %>%
   mutate( TimePeriod = as.Date(TimePeriod) )


dtf_raw_out <-
   read.csv("data_raw/ppl_out.csv")

dtf_raw_out$TimePeriod <- 
   fill_missing_cell( dtf_raw_out$TimePeriod ) 

dtf_raw_out %<>%
   dplyr::rename( Value = `TOTAL.ALL.TRAVEL.PURPOSES` ) %>%
   mutate( TimePeriod = as.Date(as.yearqtr(as.character(TimePeriod), format = "%YQ%q") , frac = 1))


## put both ppl_out data together --
dtf_raw_out_tot <-
   dtf_raw_out_hist %>%
   bind_rows( dtf_raw_out %>%
                 filter( TimePeriod > max(dtf_raw_out_hist$TimePeriod)  ))


## put both ppl in and out data togheter ------
dtf_in_out <- 
   dtf_raw_out_tot %>%
   mutate( Type = 'NZ visitors travelling out' ) %>%
   bind_rows( dtf_raw_in  %>% mutate( Type = 'Foreign visitors travelling in' )  ) %>%
   filter( !is.na(Value),
           Value !=0 ) %>%
   dplyr::select( TimePeriod, Country_in_out = Country, Type, Value ) %>%
   mutate( Year =  YearEnd( TimePeriod, month(max(dtf_trade_tot$TimePeriod)) ),
           Note = paste0('Year ended ', substr( as.yearqtr( max(dtf_trade_tot$TimePeriod)), 6,7 ) ) 
   ) %>%
   filter( Year <= max(dtf_im_country_yr$Year,na.rm=T),
           Year >= 2007) %>%
   group_by( Year, Type, Country_in_out, Note ) %>%
   dplyr::summarise( Value = sum(Value, na.rm=T) ) %>%
   ungroup %>%
   mutate( Country_in_out  = ifelse(Country_in_out %in% c("TOTAL ALL COUNTRIES OF RESIDENCE",
                                                          "TOTAL ALL COUNTRIES OF MAIN DESTINATION" ),
                                    'World', 
                                    Country_in_out ) ) %>%
   filter( !Country_in_out %in%c("AFRICA AND THE MIDDLE EAST",
                                 "AMERICAS",
                                 "ASIA",
                                 "EUROPE",
                                 "NOT STATED",
                                 "OCEANIA") )


## make sure the country name is the same as names used in export/import
tmp_dtf <- 
   data.frame( Country_in_out =  unique(dtf_in_out$Country_in_out ) )
tmp_dtf$ISO2 <- countrycode( tmp_dtf$Country_in_out , "country.name", "iso2c" )
tmp_dtf$ISO2[ tmp_dtf$Country_in_out == "Virgin Islands, United States" ] <- "VI"
tmp_country_no_iso2 <- 
   as.character( tmp_dtf$Country_in_out[ which( is.na(tmp_dtf$ISO2) ) ] )

tmp_dtf %<>%
   #dplyr::select( ISO2 ) %>%
   left_join( concord_country_iso_latlon_raw ) %>%
   filter( !is.na(ISO2)  )  %>%
   bind_rows( data.frame( Country_in_out = tmp_country_no_iso2,
                          Country = tmp_country_no_iso2 )) %>%
   filter( !is.na(Country) )

dtf_in_out %<>%
   left_join( tmp_dtf ) %>%
   filter(!is.na(Country)  ) %>%
   dplyr::select( Year, Type, Country, Value) %>%
   filter( Year>=2007 )

## save data
save(dtf_in_out, file = 'shiny/dtf_in_out.rda')
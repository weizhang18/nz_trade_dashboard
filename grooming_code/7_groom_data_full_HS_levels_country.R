##### groom data to generate a full HS data frame which contains hs2, 4, and 6 levles
load( paste0(output_folder_shiny, "/dtf_shiny.rda") )

### Total goods and services
tmp_dtf_tot_gs <- 
   dtf_shiny %>%
   filter( Commodity %in% c("Total goods", "Total services") )

#### individual services
tmp_dtf_s <-
   dtf_shiny %>%
   filter( Type_gs == 'Services', Commodity != 'Total services' )

#### individial commodity with HS level 2, 4 and 6
tmp_dtf_commodity_l6 <- 
   dtf_shiny %>%
   filter( !Commodity %in% c("Total goods", "Total services"),
           Type_gs == 'Goods') %>%
   mutate( Commodity_l2  = ifelse( Type_gs=='Goods', substr(Commodity,1,2), Commodity  ),
           Commodity_l4  = ifelse( Type_gs=='Goods', substr(Commodity,1,4), Commodity  )
   ) %>%
   mutate( Level = 'L6' )


tmp_dtf_commodity_l2 <- 
   tmp_dtf_commodity_l6 %>%
   group_by( Year, Country, Type_ie, Type_gs, Commodity_l2, Note ) %>%
   summarise( Value = sum(Value, na.rm=T) ) %>%
   ungroup %>%
   mutate( Level = 'L2' ) %>%
   rename( Commodity = Commodity_l2) 

tmp_dtf_commodity_l4 <- 
   tmp_dtf_commodity_l6 %>%
   group_by( Year, Country, Type_ie, Type_gs, Commodity_l4, Note ) %>%
   summarise( Value = sum(Value, na.rm=T) ) %>%
   ungroup %>%
   mutate( Level = 'L4' ) %>%
   rename( Commodity = Commodity_l4) 


tmp_dtf_commodity_l246 <-
   tmp_dtf_commodity_l6 %>%
   dplyr::select( -Commodity_l4, -Commodity_l2 ) %>%
   bind_rows( tmp_dtf_commodity_l4 ) %>%
   bind_rows( tmp_dtf_commodity_l2 ) %>%
   filter( Year >=2007 )

###  full HS 2,4,6 level, total goods, total services, and individual services by country
dtf_shiny_full <-
   tmp_dtf_tot_gs %>%
   bind_rows( tmp_dtf_s ) %>%
   bind_rows( tmp_dtf_commodity_l246  )

#save( dtf_shiny_full, file = 'shiny/dtf_shiny_full.rda' ) 
save( dtf_shiny_full, file = paste0(output_folder_shiny , '/dtf_shiny_full.rda' ) )

###########################################################################
## remove unused objects
rm(list=setdiff(ls(), keepers))
gc()
###########################################################################

## get lat and long data
## get countries' ISO2
# tmp_country_iso2 <-
#    data.frame(Country = sort(unique(dtf_shiny_full$Country) ),
#               ISO2 =countrycode( sort(unique(dtf_shiny_full$Country) ),  origin = 'country.name', destination = 'iso2c')
#    )
# 
# ## add ISO2 for this country
# tmp_country_iso2 %<>%
#    mutate( ISO2 = as.character(ISO2) ) %>%
#    mutate( ISO2 = ifelse(Country== 'Micronesia', 'FM', ISO2) )
# 
# ## merge with lat and long
# concord_country_iso_latlon_raw <-
# tmp_country_iso2_latlon <-
#    tmp_country_iso2 %>%
#    left_join( concord_country %>% dplyr::select( -Country ), by = 'ISO2' )
# 
# ## keep in enviroment
# keepers <- c(keepers, concord_country_iso_latlon_raw)
# 
# save( concord_country_iso_latlon_raw, file = 'shiny/concord_country_iso_latlon_raw.rda')
## merge with main data set
#dtf_shiny_full %<>%
#   left_join( tmp_country_iso2_latlon, by = 'Country' )



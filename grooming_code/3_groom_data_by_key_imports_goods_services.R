## groom key commodity and service imports

load( paste0(output_folder_shiny, "/dtf_shiny.rda") )


## 2. use SNZ's principle commodity concordance ---------------------------------------
## focus on Export merchandise only 
tmp_dtf_commodity_tot <- 
   dtf_shiny %>%
   filter( Type_ie == 'Imports',
           Type_gs == 'Goods',
           Commodity %in% c("Total goods"),
           Country == 'World',
           Year >= 2000) %>%
   rename( SNZ_commodity = Commodity )


tmp_dtf_commodity <- 
   dtf_shiny %>%
   filter( Type_ie == 'Imports',
           !Commodity %in% c("Total goods", "Total services"),
           Type_gs == 'Goods') %>%
   group_by( Year, Type_ie, Type_gs, Commodity, Note) %>%
   summarise( Value = sum(Value, na.rm=T) ) %>%
   ungroup %>%
   mutate( Country = 'World',
           Commodity_l2  = ifelse( Type_gs=='Goods', substr(Commodity,1,2), Commodity  ),
           Commodity_l4  = ifelse( Type_gs=='Goods', substr(Commodity,1,4), Commodity  ) ) %>%
   mutate( Level = 'L6' )


tmp_dtf_commodity_l2 <- 
   tmp_dtf_commodity %>%
   filter( Type_gs != "Services" ) %>%
   group_by( Year, Country, Type_ie, Type_gs, Commodity_l2, Note ) %>%
   summarise( Value = sum(Value, na.rm=T) ) %>%
   ungroup %>%
   mutate( Level = 'L2' ) %>%
   rename( Commodity = Commodity_l2) 

tmp_dtf_commodity_l4 <- 
   tmp_dtf_commodity %>%
   filter( Type_gs != "Services" ) %>%
   group_by( Year, Country, Type_ie, Type_gs, Commodity_l4, Note ) %>%
   summarise( Value = sum(Value, na.rm=T) ) %>%
   ungroup %>%
   mutate( Level = 'L4' ) %>%
   rename( Commodity = Commodity_l4) 

dtf_shiny_commodity <-
   tmp_dtf_commodity %>%
   dplyr::select( -Commodity_l4, -Commodity_l2 ) %>%
   bind_rows( tmp_dtf_commodity_l4 ) %>%
   bind_rows( tmp_dtf_commodity_l2 ) %>%
   left_join( concord_snz_ig,
              by = c('Commodity' = 'HS_codes') ) %>%
   filter( !is.na(SNZ_commodity) ) %>%
   group_by( Year, Country, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
   summarise( Value = sum(Value, na.rm=T) ) %>%
   ungroup %>%
   bind_rows( tmp_dtf_commodity_tot ) 

tmp_dtf_other_commodity <-
   dtf_shiny_commodity %>%
   group_by( Year, Country, Type_ie, Type_gs,  Note ) %>%
   summarise( Value = Value[SNZ_commodity=='Total goods'] - sum(Value[SNZ_commodity!='Total goods']) ) %>%
   ungroup %>%
   mutate( SNZ_commodity = 'Other goods' )

## services
tmp_dtf_service_tot <- 
   dtf_shiny %>%
   filter( Type_ie == 'Imports',
           Type_gs == 'Services',
           Commodity %in% c("Total services"),
           Country == 'World',
           Year >= 2000) %>%
   rename( SNZ_commodity = Commodity )

tmp_dtf_service <- 
   dtf_shiny %>%
   filter( Type_ie == 'Imports',
           !Commodity %in% c("Total goods", "Total services"),
           Type_gs == 'Services') %>%
   group_by( Year, Type_ie, Type_gs, Commodity, Note) %>%
   summarise( Value = sum(Value, na.rm=T) ) %>%
   ungroup %>%
   mutate( Country = 'World')

dtf_shiny_service <-
   tmp_dtf_service %>%
   rename(SNZ_commodity = Commodity ) %>%
   bind_rows( tmp_dtf_service_tot ) 

## just to balance the offical total service exports and the sum of service exports by country
tmp_dtf_other_service <-
   dtf_shiny_service %>%
   group_by( Year, Country, Type_ie, Type_gs,  Note ) %>%
   summarise( Value = Value[SNZ_commodity=='Total services'] - sum(Value[SNZ_commodity!='Total services']) ) %>%
   ungroup %>%
   mutate( SNZ_commodity = 'Other services' )

### bind toghter 
dtf_shiny_commodity_service_im <-
   dtf_shiny_commodity %>%
   filter( SNZ_commodity != 'Total goods' ) %>%
   bind_rows( tmp_dtf_other_commodity ) %>%
   bind_rows(
      dtf_shiny_service %>%
         filter( SNZ_commodity != 'Total services' ) %>%
         bind_rows( tmp_dtf_other_service ) %>%
         group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note, Country ) %>%
         summarise( Value = sum( Value, na.rm=T) ) %>%
         ungroup
   ) %>%
   #bind_rows( tmp_dtf_other_service ) %>%
   group_by( Country, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
   mutate( CAGR5 = CAGR( Value[Year == max(Year)]/
                            Value[Year == (max(Year)-5) ], 5) ) %>%
   ungroup


### save for use later
#save( dtf_shiny_commodity_service_im, file = 'shiny/dtf_shiny_commodity_service_im.rda' )
save( dtf_shiny_commodity_service_im, file = paste0(output_folder_shiny, '/dtf_shiny_commodity_service_im.rda' ))


###########################################################################
## remove unused objects
rm(list=setdiff(ls(), keepers))
gc()
###########################################################################

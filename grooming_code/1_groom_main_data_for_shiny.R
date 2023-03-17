### create dataset for the shiny app
## data frequency annual update quarterly i.e., year ended Qx YYY. Unit: dollars
## columns needed:
## Year Country Type_ie (import or export) Type_gc(goods or services) Commodity Value 

## 1. groom total trade into annual data ------------------------------------------
dtf_tmp_shiny_tot <- 
   dtf_trade_tot %>%
   mutate( Year = YearEnd( TimePeriod, month(max(dtf_trade_tot$TimePeriod)) ),
           Note = paste0('Year ended ', substr( as.yearqtr( max(dtf_trade_tot$TimePeriod)), 6,7 ) )
   ) %>%
   group_by( Year, CV2, Note ) %>%
   dplyr::summarise( Value = sum(Value*10^6, na.rm=T) ) %>%
   ungroup %>%
   rowwise() %>%
   mutate( Commodity = strsplit( as.character(CV2), ";")[[1]][2],
           Type_ie =  strsplit( as.character(CV2), ";")[[1]][3] ) %>%
   unrowwise() %>%
   mutate( Type_gs = Commodity,
           Commodity = gsub( 'Goods', 'Total goods', Commodity ),
           Commodity = gsub( 'Services', 'Total services', Commodity ),
           Type_ie = gsub( 'Exports [(]fob[)] total|Exports total', 'Exports', Type_ie ),
           Type_ie = gsub( 'Imports [(]fob[)] total|Imports total', 'Imports', Type_ie ),
           Country = 'World'
   ) %>%
   dplyr::select( Year,Country, Type_ie, Type_gs, Commodity, Value, Note ) 

## 2. groom export 
dtf_tmp_shiny_ex <-
   dtf_ex_country_yr %>%
   dplyr::rename( Commodity = 'Dimension1',
           Country = 'Dimension2') %>%
   mutate( Type_ie = 'Exports',
           Value = Value*10^6,
           Note = paste0('Year ended ', substr( as.yearqtr( max(dtf_trade_tot$TimePeriod)), 6,7 ) ),
           Type_gs = ifelse( !is.na(as.numeric(as.character(Commodity))), 'Goods', 
                             ifelse( as.character(Commodity) == 'Total goods', 'Goods', 'Services' )
           )
   ) %>%
   dplyr::select( Year,Country,  Type_ie, Type_gs, Commodity, Value, Note ) 

## 3. groom import 
dtf_tmp_shiny_im <-
   dtf_im_country_yr %>%
   dplyr::rename( Commodity = 'Dimension1',
           Country = 'Dimension2') %>%
   mutate( Type_ie = 'Imports',
           Value = Value*10^6,
           Note = paste0('Year ended ', substr( as.yearqtr( max(dtf_trade_tot$TimePeriod)), 6,7 ) ),
           Type_gs = ifelse( !is.na(as.numeric(as.character(Commodity))), 'Goods', 
                             ifelse( as.character(Commodity) == 'Total goods', 'Goods', 'Services' )
           )
   ) %>%
   dplyr::select( Year,Country,  Type_ie, Type_gs, Commodity, Value, Note ) 


## 4. stick data together
dtf_shiny <-
   dtf_tmp_shiny_tot %>%
   bind_rows( dtf_tmp_shiny_ex ) %>%
   bind_rows( dtf_tmp_shiny_im ) %>%
   mutate( Country = trim( Country ),
           Commodity = trim( Commodity),
           Type_ie = trim( Type_ie ),
           Type_gs = trim( Type_gs )
   ) %>%
   mutate( Country = gsub("China, People[']s Republic of", "China", Country),
           Country = gsub("Hong Kong [(]Special Administrative Region[)]", "Hong Kong", Country),
           Country = gsub("Macau [(]Special Administrative Region[)]", "Macau", Country),
           Country = gsub("Korea[,] Republic of", "South Korea", Country),
           Country = gsub("Micronesia[,] Federated States of", "Micronesia", Country),
           Country = gsub("Former Yugoslav Republic of Macedonia [(]FYROM[)]", "Macedonia", Country)
           )


## 5. check dtf_shiny data
## Total trade data and trade by country by commodity data are from different period
(unique(dtf_shiny$Note))
if( length(unique(dtf_shiny$Note)) > 1 ){
   stop( 'Total trade data and trade by country by commodity data are from different period! Check!' )
}

## check Type_ie and Type_gs
(unique(dtf_shiny$Type_ie))
(unique(dtf_shiny$Type_gs))
(unique(dtf_shiny$Year))
(sort(unique(dtf_shiny$Country)))
(sort(unique(dtf_shiny$Commodity[dtf_shiny$Type_gs=='Goods'] )))
(sort(unique(dtf_shiny$Commodity[dtf_shiny$Type_gs=='Services'] )))


### check same country different name
tmp_country_iso2 <- 
   dtf_shiny %>%
   distinct( Country ) %>%
   mutate( ISO2 = countrycode( Country, origin = 'country.name', destination = 'iso2c' ) )

tmp_duplicated <-
   tmp_country_iso2[which( duplicated(tmp_country_iso2$ISO2) & !is.na(tmp_country_iso2$ISO2) ),]

if( nrow(tmp_duplicated)>=1 ){
   tmp_country_diff_name <- data.frame( ISO2 = NA, Country_name1 = NA, Country_name2 = NA )
   for( i in 1:nrow(tmp_duplicated)){
      tmp_country_diff_name[i,] <- c(tmp_duplicated$ISO2[i],tmp_country_iso2$Country[tmp_country_iso2$ISO2%in%tmp_duplicated$ISO2[i]] )
   }
}

(tmp_country_diff_name)

## make country name consistent
dtf_shiny %<>%
   mutate( Country = gsub("East Timor","Timor-Leste", Country),
           Country = gsub("Samoa[,] Western","Samoa", Country),
           Country = gsub("Cape Verde","Cabo Verde", Country),
           Country = gsub("Czech Republic","Czechia", Country),
           Country = gsub("Swaziland","Eswatini", Country),
           Country = gsub("Cote D'Ivoire","Cote d'Ivoire", Country),
           ) %>%
   mutate( Country = case_when(Country == "Macedonia" ~ "North Macedonia", 
                               TRUE ~ as.character(Country)) 
           ) %>% 
   group_by( Year, Country, Type_ie,  Type_gs, Commodity, Note ) %>%
   dplyr::summarise( Value = sum(Value, na.rm=T) ) %>%
   ungroup

## 6 save data to shiny folder
#save( dtf_shiny, file = 'shiny/dtf_shiny.rda' )
save( dtf_shiny, file = 'shiny/data/dtf_shiny.rda' )

###########################################################################
## remove unused objects
rm(list=setdiff(ls(), keepers))
gc()
###########################################################################

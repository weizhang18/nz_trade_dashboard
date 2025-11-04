## 1. put together FDI and ODI stock data
## Import data from TRED; 
## inforshare International Investment Position - IIP
#var_name <- 'BPM6 Annual, Directional basis stock of direct investment by country (Annual-Mar)'

## get raw data
#dtf_raw <- 
#   ImportTS2(TRED, var_name) 

### read from csv
dtf_raw_csv <-
   read.csv("data_raw/fdi_odi.csv"  )

dtf_raw_csv$TimePeriod <- 
   fill_missing_cell( dtf_raw_csv$TimePeriod ) 

dtf_raw <- 
   dtf_raw_csv %>%
   mutate( TimePeriod = as.numeric(TimePeriod) ) %>%
   mutate( Value = as.numeric(as.character(Value) ))

## Groom data to  format
# dtf_fdi_odi <- 
#    dtf_raw %>% 
#    dplyr::select( TimePeriod, CV2, Value) %>%
#    mutate( Year = year(as.yearmon(TimePeriod)) , 
#            CV2 = as.matrix( as.character( CV2 ) )
#    ) %>%
#    dplyr::rowwise() %>% 
#    mutate( CV3 = ifelse( length(  strsplit(CV2, ";")[[1]] ) == 4,
#                          strsplit(CV2, ";")[[1]][2] ,
#                          substr( strsplit(CV2, ";")[[1]][2], 
#                                  1, 
#                                  nchar( strsplit(CV2, ";")[[1]][2] ) - 6 )
#                          ) %>% rm_space,
#            
#            CV4 = ifelse( length( strsplit(CV2, ";")[[1]]  ) == 4,
#                  strsplit(CV2, ";")[[1]][4],
#                  'Total'
#                  ) %>% rm_space
#            ) %>%
#    filter( !is.na(Value),
#            Value !=0 
#    ) %>%
#    mutate(Type = CV3,
#           Country_fid_odi = CV4
#    ) %>%
#    dplyr::select( Year, Value, Type, Country_fid_odi ) %>%
#    arrange( Year, Type, desc(Value) ) %>%
#    mutate( Type = ifelse( Type=="Foreign direct inv. in NZ", "FDI", "ODI" ),
#            Country_fid_odi  = ifelse(Country_fid_odi =='Total','World', Country_fid_odi ) )


### different way to read fdi and odi ---
dtf_fdi_odi <- 
   dtf_raw %>% 
   dplyr::select( TimePeriod, CV2, Value) %>%
   mutate( #Year = year(as.yearmon(TimePeriod)) , 
           Year = TimePeriod ,
           CV2 = as.character( CV2 )  )

## column for FDI or ODI
dtf_fdi_odi$CV3 <- 
   lapply( dtf_fdi_odi$CV2,
           function(i){
              ifelse( length(  strsplit(i, ";")[[1]] ) == 4,
                      strsplit(i, ";")[[1]][2] ,
                      substr( strsplit(i, ";")[[1]][2], 
                              1, 
                              nchar( strsplit(i, ";")[[1]][2] ) - 6 )
              )
           })

dtf_fdi_odi$CV3 <- unlist(rm_space(dtf_fdi_odi$CV3))
   
## Column for country names 
dtf_fdi_odi$CV4 <- 
   lapply( dtf_fdi_odi$CV2,
           function(i){
              ifelse( length( strsplit(i, ";")[[1]]  ) == 4,
                      strsplit(i, ";")[[1]][4],
                      'Total'
              )
           })

dtf_fdi_odi$CV4 <- unlist(rm_space(dtf_fdi_odi$CV4))

## more grooming 
dtf_fdi_odi %<>%
   filter( !is.na(Value),
           Value !=0 
   ) %>%
   mutate(Type = CV3,
          Country_fid_odi = CV4
   ) %>%
   dplyr::select( Year, Value, Type, Country_fid_odi ) %>%
   arrange( Year, Type, desc(Value) ) %>%
   mutate( Type = ifelse( Type=="Foreign direct inv. in NZ", "FDI", "ODI" ),
           Country_fid_odi  = ifelse(Country_fid_odi =='Total','World', Country_fid_odi ) )


## make sure the country name is the same as names used in export/import
tmp_dtf <- 
   data.frame( Country_fid_odi =  unique(dtf_fdi_odi$Country_fid_odi ) )
tmp_dtf$ISO2 <- countrycode( tmp_dtf$Country_fid_odi , "country.name", "iso2c" )
tmp_country_no_iso2 <- 
   as.character( tmp_dtf$Country_fid_odi[ which( is.na(tmp_dtf$ISO2) ) ] )

tmp_dtf %<>%
   #dplyr::select( ISO2 ) %>%
   left_join( concord_country_iso_latlon_raw ) %>%
   filter( !is.na(ISO2)  )  %>%
   bind_rows( data.frame( Country_fid_odi = tmp_country_no_iso2,
                          Country = tmp_country_no_iso2 ))


dtf_fdi_odi %<>%
   left_join( tmp_dtf ) %>%
   dplyr::select( Year, Type, Country, Value) %>%
   filter( Year>=2007 )

## save data
#save(dtf_fdi_odi, file = 'shiny/dtf_fdi_odi.rda')
save(dtf_fdi_odi, file = paste0(output_folder_shiny,'/dtf_fdi_odi.rda'))

###########################################################################
## remove unused objects
rm(list=setdiff(ls(), keepers))
gc()
###########################################################################

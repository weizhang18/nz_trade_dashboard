### prepare data from input for getting trade data from UN comtrade https://comtrade.un.org/data/Doc/api/ex/r
string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
reporters <- fromJSON(file=string)

dtf_uncomtrade_country <- 
   as.data.frame(t(sapply(reporters$results,rbind)))

colnames( dtf_uncomtrade_country ) <-
   c("UN_code", "UN_country")

dtf_uncomtrade_country %<>%
   mutate( UN_code = as.character(unlist(UN_code)),
           UN_country = as.character(unlist(UN_country)) )


## get country code
dtf_uncomtrade_country %<>%
   mutate( ISO2 = countrycode( dtf_uncomtrade_country$UN_country,
                           origin = "country.name",
                           destination = 'iso2c') )

## signal out NAs to investigate
tmp_nas <-
   dtf_uncomtrade_country %>%
   filter( is.na(ISO2) )

tmp_nas$ISO2[ tmp_nas$UN_country == 'Belgium-Luxembourg'] <- 'LU'
tmp_nas$ISO2[ tmp_nas$UN_country == 'Br. Virgin Isds'] <- 'VG'
tmp_nas$ISO2[ tmp_nas$UN_country == 'Central African Rep.'] <- 'CF'
tmp_nas$ISO2[ tmp_nas$UN_country == 'Neth. Antilles'] <- 'AN'
tmp_nas$ISO2[ tmp_nas$UN_country == 'Saint Kitts, Nevis and Anguilla'] <- 'KN'
tmp_nas$ISO2[ tmp_nas$UN_country == 'US Virgin Isds'] <- 'VI'

tmp_nas %<>%
   filter( !is.na(ISO2) )

## filter out NA's
dtf_uncomtrade_country %<>%
   filter( !is.na(ISO2) ) %>%
   bind_rows( tmp_nas ) %>%
   left_join( concord_country_iso_latlon_raw %>% 
                 dplyr::select( -Country) )


### fill out places lat and long
dtf_uncomtrade_country$lat[  dtf_uncomtrade_country$UN_country == 'Bouvet Island'] <- -54.4333316
dtf_uncomtrade_country$lon[  dtf_uncomtrade_country$UN_country == 'Bouvet Island'] <- 3.3999984

dtf_uncomtrade_country$lat[  dtf_uncomtrade_country$UN_country == 'Czechoslovakia'] <- 49.883575
dtf_uncomtrade_country$lon[  dtf_uncomtrade_country$UN_country == 'Czechoslovakia'] <- 12.872071

dtf_uncomtrade_country$lat[  dtf_uncomtrade_country$UN_country == "Dem. People's Rep. of Korea"] <- 40.3424611
dtf_uncomtrade_country$lon[  dtf_uncomtrade_country$UN_country == "Dem. People's Rep. of Korea"] <- 127.4310054

dtf_uncomtrade_country$lat[  dtf_uncomtrade_country$UN_country == "Fmr Dem. Yemen"] <- 15.352029
dtf_uncomtrade_country$lon[  dtf_uncomtrade_country$UN_country == "Fmr Dem. Yemen"] <- 44.207455

dtf_uncomtrade_country$lat[  dtf_uncomtrade_country$UN_country == "Fmr Yugoslavia"] <- 14.4832908
dtf_uncomtrade_country$lon[  dtf_uncomtrade_country$UN_country == "Fmr Yugoslavia"] <- 121.021382

dtf_uncomtrade_country$lat[  dtf_uncomtrade_country$UN_country == "Heard Island and McDonald Islands"] <- -53.0999996 
dtf_uncomtrade_country$lon[  dtf_uncomtrade_country$UN_country == "Heard Island and McDonald Islands"] <- 73.5166646

dtf_uncomtrade_country$lat[  dtf_uncomtrade_country$UN_country == "New Zealand"] <- -40.90056 
dtf_uncomtrade_country$lon[  dtf_uncomtrade_country$UN_country == "New Zealand"] <- 174.88597 

dtf_uncomtrade_country$lat[  dtf_uncomtrade_country$UN_country == "United States Minor Outlying Islands"] <- -0.374350 
dtf_uncomtrade_country$lon[  dtf_uncomtrade_country$UN_country == "United States Minor Outlying Islands"] <- -159.996719


### check any NA
#View( dtf_uncomtrade_country[ is.na(dtf_uncomtrade_country$lat ),] )

## check any duplicated countries
#View(dtf_uncomtrade_country[duplicated(dtf_uncomtrade_country$ISO2),])

## remove duplicated ISO2s
dtf_uncomtrade_country %<>%
   filter( !UN_country %in% c("Aruba",
                              "East and West Pakistan",
                              "Fmr Fed. Rep. of Germany",
                              "Fmr Dem. Rep. of Germany",
                              "Fmr Ethiopia",
                              "India, excl. Sikkim",
                              "Saint Kitts, Nevis and Anguilla",
                              "Belgium-Luxembourg",
                              "Peninsula Malaysia",
                              "Fmr Panama-Canal-Zone",
                              "Fmr Panama, excl.Canal Zone",
                              "Fmr USSR",
                              "Fmr Sudan",
                              "US Misc. Pacific Isds",
                              "USA (before 1981)",
                              "Fmr Rep. of Vietnam",
                              "Fmr Dem. Rep. of Vietnam",
                              "Fmr Rhodesia Nyas"
                              ) )

#### final check
if(  any(is.na(dtf_uncomtrade_country$lat)) | any( duplicated(dtf_uncomtrade_country$ISO2) )){
   stop("Still countries with out lat and lon and still duplicated ISO2")
}

## save data
concord_uncomtrade_country <- 
   dtf_uncomtrade_country %>%
   mutate( ISO3 = countrycode(dtf_uncomtrade_country$ISO2, origin = 'iso2c', destination = 'iso3c') )

save(concord_uncomtrade_country , file = 'shiny/concord_uncomtrade_country.rda')



### read zip data
dtf_snz_trade <- read_csv(unzip(file_stats_nz_trade, 
                     "output_csv_full.csv"))


# unique(dtf_snz_trade$time_ref)
# unique(dtf_snz_trade$account) 
# unique(dtf_snz_trade$code)
# unique(dtf_snz_trade$country_code)
# unique(dtf_snz_trade$product_type)
# unique(dtf_snz_trade$status)

dtf_snz_cc <- read_csv(unzip(file_stats_nz_trade, 
                     "country_classification.csv"))

dtf_snz_gc <- read_csv(unzip(file_stats_nz_trade, 
                        "goods_classification.csv"))

dtf_snz_sc <- read_csv(unzip(file_stats_nz_trade, 
                        "services_classification.csv"))

## delet unzipped files
unlink(c("output_csv_full.csv",
         "country_classification.csv",
         "goods_classification.csv",
         "services_classification.csv"))


### extract up to date services data by country ---------
dtf_concord_country_join_raw <- 
   dtf_snz_trade %>% 
   distinct( country_code ) %>% 
   mutate(country_code = case_when( is.na(country_code) ~ as.character("NA"),
                                    TRUE ~ as.character(country_code))) %>% 
   full_join( dtf_snz_cc   )  %>%
   filter( nchar(country_code) == 2 ) %>% 
   full_join( concord_country, by = c("country_code" = "ISO2" ))

dtf_concord_country_join_good <- 
   dtf_concord_country_join_raw %>% 
   filter( !(is.na(Country) | is.na(country_label) ))

dtf_concord_country_join_need_fix <- 
   dtf_concord_country_join_raw  %>% 
   filter( is.na(Country) | is.na(country_label) ) %>% 
   mutate( Country= case_when( !is.na(Country) ~ as.character(Country),
                                 is.na(Country) ~ as.character(country_label))  ) %>% 
   filter( !(is.na(Country) & is.na(country_label)) )

## the full country concord 
dtf_concord_country_join <- 
   dtf_concord_country_join_good %>% 
   bind_rows( dtf_concord_country_join_need_fix )


##
dtf_snz_service_country <- 
   dtf_snz_trade %>% 
   filter( product_type == "Services" ) %>% 
   #filter( nchar(code) <= 5 ) %>% 
   left_join(concord_snz_service  ) %>% 
   filter( !is.na(service_label) ) %>% 
   right_join( dtf_concord_country_join, by = c( "country_code" ) ) %>% 
   mutate( TimePeriod =  as.Date( paste0(as.character(time_ref),"01"), format = "%Y%m%d" ) ) %>% 
   mutate( CV2 = case_when(account == "Exports" ~ "Credit",
                           TRUE ~ "Debit") ) %>% 
   dplyr::select(TimePeriod, CV3 = service_label_shiny, CV4 = Country, CV2 , Value = value)


[1] "Total services"                                         "Maintenance and repair services"                       
[3] "Transportation services"                                "Travel services"                                       
[5] "Construction services"                                  "Insurance and pension services"                        
[7] "Financial services"                                     "Charges for the use of intellectual property"          
[9] "Telecommunications, computer, and information services" "Other business services"                               
[11] "Personal, cultural, and recreational services"          "Government services"                                   
[13] "Business travel services"                               "Education travel services"                             
[15] "Other personal travel services"

"TOT (OMT FOB)"
"	
TOT (BoP basis)"

"A12" "TOT" "Services"
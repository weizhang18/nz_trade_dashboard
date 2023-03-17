### read zip data
dtf_snz_trade <- read_csv(unzip(file_stats_nz_trade, 
                     "output_csv_full.csv"))


# unique(dtf_snz_trade$time_ref)
# unique(dtf_snz_trade$account) 
# unique(dtf_snz_trade$code)
# unique(dtf_snz_trade$country_code)
# unique(dtf_snz_trade$product_type)
# unique(dtf_snz_trade$status)

dtf_snz_cc <- 
   read_csv(unzip(file_stats_nz_trade, 
                     "country_classification.csv")) %>%
   mutate( country_label = case_when(country_code == "TR" ~ "Turkey",
                                     TRUE ~ as.character(country_label) ) )
   

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
   filter( !(is.na(Country) & is.na(country_label)) ) %>% 
   mutate( country_label = case_when( is.na(country_label) ~  as.character(Country),
                                      TRUE ~ as.character(country_label)) )

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
   dplyr::select(TimePeriod, CV3 = service_label_shiny, CV4 = country_label, CV2 , Value = value) %>% 
   filter( !is.na(TimePeriod) )


## need to calculate year ended by quarter data ---------
tmp_list <- list()

for( i_mon in c(3, 6, 9, 12) ){
   tmp_list[[ as.character(i_mon) ]] <- 
      dtf_snz_service_country %>% 
      mutate( Year_end = YearEnd(TimePeriod , i_mon)) %>% 
      group_by( Year_end, CV3, CV4, CV2 ) %>% 
      summarise( Value = sum(Value/10^6, na.rm = T) ) %>% 
      ungroup %>% 
      mutate( TimePeriod = as.Date( as.yearmon(Year_end + (i_mon-1)/12) ) ) %>% 
      dplyr::select( -Year_end)
}

dtf_snz_service_country_year_end_qtr <- 
  bind_rows( tmp_list )



#### get total data -------------
dtf_snz_goods_total <- 
   dtf_snz_trade %>% 
   filter( country_code %in% c("TOT (OMT VFD)","TOT (OMT FOB)") ) 

dtf_snz_service_total <- 
   dtf_snz_trade %>% 
   filter( code == "A12",
           country_code %in% c("TOT") ) 

dtf_snz_total <- 
   dtf_snz_goods_total %>% 
   bind_rows( dtf_snz_service_total ) %>% 
   mutate( TimePeriod =  as.Date( paste0(as.character(time_ref),"01"), format = "%Y%m%d" ) ) %>% 
   mutate( CV2 = case_when(country_code == "TOT (OMT FOB)" ~ "Current account; Goods; Exports (fob) total",
                           country_code == "TOT (OMT VFD)" ~ "Current account; Goods; Imports (fob) total",
                           account == "Exports" & product_type == "Services" ~ "Current account; Services; Exports total",
                           account == "Imports" & product_type == "Services" ~ "Current account; Services; Imports total"
                           ) ) %>% 
   dplyr::select(TimePeriod, CV2 , Value = value) %>% 
   filter( !is.na(TimePeriod) ) %>% 
   mutate( Value = Value/10^6 )




### save data
save(dtf_snz_service_country_year_end_qtr, file = "data_intermediate/dtf_snz_service_country_year_end_qtr.rda")
save(dtf_concord_country_join, file = "data_intermediate/dtf_concord_country_join.rda")
save(dtf_snz_total, file = "data_intermediate/dtf_snz_total.rda")

###########################################################################
## remove unused objects
rm(list=setdiff(ls(), keepers))
gc()
###########################################################################


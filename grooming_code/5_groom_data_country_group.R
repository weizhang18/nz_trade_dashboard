##
load( paste0(output_folder_shiny, "/dtf_shiny_country_gs.rda") )

## build a country list that 
dtf_country_group <-
   dtf_shiny_country_gs %>%
   distinct( Country, ISO2, .keep_all = T ) %>%
   select( Country, ISO2 ) %>%
   left_join( concord_country_group %>% select( -Country ) ) %>%
   arrange( Region, Country ) %>%
   select( Region, Country, ISO2 ) %>%
   filter( !is.na(Region) & !is.na(ISO2) ) #%>%
   # mutate( Region = factor(Region, levels = c("Australia & Pacific Islands", 
   #                                            "East Asia" , 
   #                                            "Europe & Russia", 
   #                                            "North America",
   #                                            "South East Asia", 
   #                                            "South Asia",
   #                                            "Middle East & North Africa & Central Asia",
   #                                            "Central & South America",
   #                                            "Sub-Saharan Africa")
   #                         ) 
   #         ) %>%
   # arrange( Region, Country )

## add NZ
dtf_country_group %<>%
   bind_rows( data.frame(Region = 'Australia & Pacific Islands',
                         Country = 'New Zealand',
                         ISO2 = "NZ" ) )

## get and or region
#tmp_region <- unique(dtf_country_group$Region)
tmp_region <- c("Australia & Pacific Islands",
                "East Asia" ,
                "Europe & Russia",
                "North America",
                "South East Asia",
                "South Asia",
                "Middle East & North Africa & Central Asia",
                "Central & South America",
                "Sub-Saharan Africa")

### build country list
list_country <- NULL

## add orgnization memebers
tmp_group <- sort(unique(concord_country_member$Group))
list_country[['Country groups']] <- tmp_group

## add regions
for( i_region in 1:length(tmp_region) ){
   print(i_region)
   list_country[[ as.character(tmp_region[i_region]) ]] <- sort(as.character(dtf_country_group$Country[ dtf_country_group$Region == as.character(tmp_region[i_region]) ]))
}
 

### save .rda   
#save( dtf_country_group, file = 'shiny/dtf_country_group.rda' ) 
save( dtf_country_group, file = paste0(output_folder_shiny, '/dtf_country_group.rda' ) )
#save( list_country, file = 'shiny/list_country.rda' ) 
save( list_country, file = paste0( output_folder_shiny, '/list_country.rda' ) )

###########################################################################
## remove unused objects
rm(list=setdiff(ls(), keepers))
gc()
###########################################################################
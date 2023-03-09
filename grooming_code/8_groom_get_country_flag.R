### get all country flag links
flag_url <- "http://flagpedia.net/index"
download.file( flag_url, destfile = 'data_intermediate/flag.html', quiet = T)
flag_page <- read_html('data_intermediate/flag.html')
unlink('data_intermediate/flag.html')

## get country tables
flag_table <- 
   flag_page %>%
   html_nodes('table') %>%
   html_table()
   
flag_table <-
   flag_table[[1]] %>%
   bind_rows( flag_table[[2]] %>%
                 rename( Flag = X1,
                         Country=X2,
                         Capital = X3,
                         Population = X4,
                         `Total area` = X5)) %>%
   dplyr::select( -Capital, -Population, -`Total area` )

## get flag links
country_link <-
   flag_page %>%
   html_nodes( "table" ) %>%
   html_nodes( 'a' ) %>%
   html_attr( 'href' ) %>%
   unique

### get country links
flag_table %<>%
   mutate( Country_link = paste0("http://flagpedia.net",country_link)) %>%
   rename( Flag_link = Flag )

### for each link get flag link
for( i in 1:nrow(flag_table) ){
   ## read each country page
   tmp_url <- as.character( flag_table$Country_link[i] )
   download.file( tmp_url, destfile = 'data_intermediate/tmp.html', quiet = T)
   tmp_page <- read_html('data_intermediate/tmp.html')
   unlink('data_intermediate/tmp.html')
   
   ##
   tmp_link <- 
      tmp_page %>%
      html_nodes( "#flag-detail img" ) %>%
      html_attr('src')
   
   flag_table$Flag_link[i] <- paste0('http:',tmp_link)
}

#### get ISO2 code
tmp_iso2 <- countrycode( flag_table$Country, origin = "country.name", destination = 'iso2c' )
flag_table$ISO2 <- tmp_iso2

flag_table %<>%
   mutate( ISO2 = as.character(ISO2) ) %>%
   mutate( ISO2 = ifelse(Country=='Micronesia', 'FM', ISO2) )

###
flag_table <-
   dtf_country_group %>%
   full_join( flag_table %>%
                 dplyr::select(-Country),
              by='ISO2'
              ) %>%
   dplyr::select( -Region, -Country_link ) %>%
   distinct( Country, .keep_all =T ) %>%
   filter( !is.na(ISO2) )


## to get countri without flags
flag_table_no_flag <-
   flag_table %>%
   filter( is.na(Flag_link) )

## work on each missing flags
flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='French Polynesia'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/d/db/Flag_of_French_Polynesia.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Guam'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/0/07/Flag_of_Guam.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='New Caledonia'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/6/66/Flag_of_FLNKS.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Norfolk Island'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/4/48/Flag_of_Norfolk_Island.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Northern Mariana Islands'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/e/e0/Flag_of_the_Northern_Mariana_Islands.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Pitcairn'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/8/88/Flag_of_the_Pitcairn_Islands.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Samoa, American'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/8/87/Flag_of_American_Samoa.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Tokelau'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/8/8e/Flag_of_Tokelau.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Wallis and Futuna'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/d/d2/Flag_of_Wallis_and_Futuna.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Curacao'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/b/b1/Flag_of_Cura%C3%A7ao.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Falkland Islands'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/8/83/Flag_of_the_Falkland_Islands.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='French Guiana'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/2/29/Flag_of_French_Guiana.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='South Georgia and the South Sandwich Islands'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/e/ed/Flag_of_South_Georgia_and_the_South_Sandwich_Islands.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Hong Kong'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/5/5b/Flag_of_Hong_Kong.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Macau'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/6/63/Flag_of_Macau.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Faeroe Islands'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/3/3c/Flag_of_the_Faroe_Islands.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Gibraltar'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/0/02/Flag_of_Gibraltar.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Gaza Strip/Palestine/West Bank'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b0/No_flag.svg/640px-No_flag.svg.png"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Anguilla'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/b/b4/Flag_of_Anguilla.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Aruba'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/f/f6/Flag_of_Aruba.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Bermuda'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/b/bf/Flag_of_Bermuda.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Cayman Islands'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/0/0f/Flag_of_the_Cayman_Islands.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Greenland'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/0/09/Flag_of_Greenland.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Guadeloupe'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b0/No_flag.svg/640px-No_flag.svg.png"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Martinique'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/d/da/Unofficial_flag_of_Martinique.png"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Puerto Rico'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/2/28/Flag_of_Puerto_Rico.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='St Maarten (Dutch Part)'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/d/d3/Flag_of_Sint_Maarten.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='St Pierre and Miquelon'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/7/74/Flag_of_Saint-Pierre_and_Miquelon.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Turks and Caicos Islands'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/a/a0/Flag_of_the_Turks_and_Caicos_Islands.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Virgin Islands, British'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/4/42/Flag_of_the_British_Virgin_Islands.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Mayotte'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/4/4a/Flag_of_Mayotte_%28local%29.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Reunion'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/8/8e/Proposed_flag_of_R%C3%A9union_%28VAR%29.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Montserrat'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/d/d0/Flag_of_Montserrat.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Netherlands Antilles'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/0/0f/Flag_of_the_Netherlands_Antilles_%281959-1986%29.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Virgin Islands, United States'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/f/f8/Flag_of_the_United_States_Virgin_Islands.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Antarctica'] <- 
   "https://en.wikipedia.org/wiki/Flags_of_Antarctica#/media/File:Flag_of_the_Antarctic_Treaty.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='British Indian Ocean Territory'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/6/6e/Flag_of_the_British_Indian_Ocean_Territory.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Christmas Island'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/6/67/Flag_of_Christmas_Island.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='Cocos (Keeling) Islands'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/7/74/Flag_of_the_Cocos_%28Keeling%29_Islands.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='French Southern Territories'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/a/a7/Flag_of_the_French_Southern_and_Antarctic_Lands.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='St Helena'] <- 
   "https://upload.wikimedia.org/wikipedia/commons/0/00/Flag_of_Saint_Helena.svg"

flag_table_no_flag$Flag_link[flag_table_no_flag$Country=='United States Minor Outlying Islands'] <- 
   "https://upload.wikimedia.org/wikipedia/en/a/a4/Flag_of_the_United_States.svg"

## put missing flag country back
flag_table %<>%
   filter( !is.na(Flag_link) ) %>%
   bind_rows( flag_table_no_flag )

## test if any country still missing
flag_table_no_flag <-
   flag_table %>%
   filter( is.na(Flag_link) ) #%>%
   #filter( !is.na(Country))

if( nrow(flag_table_no_flag)>=1 ){
   stop('Still countries flag missing!')
}

flag_table$Flag_link[ flag_table$Country=='Taiwan' ] <- "https://vignette.wikia.nocookie.net/hardyboys/images/b/b0/No_flag.svg/revision/latest?cb=20090207061810"

flag_table %<>%
   bind_rows( data.frame(Country = "Destination Unknown - EU", 
                            ISO2 = NA, 
                            Flag_link = "https://vignette.wikia.nocookie.net/hardyboys/images/b/b0/No_flag.svg/revision/latest?cb=20090207061810") )

## save data
save(flag_table, file = 'shiny/flag_table.rda')

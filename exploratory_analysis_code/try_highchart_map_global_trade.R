### try to plot a international trade map
source('shiny/helper_funs.R')

sankey_uncomtrade <- 
   function( cc = '0409',
             global_coverage = 0.9,
             trade_weight = 0.005,
             max_year = 2016,
             center_country = "New Zealand",
             height = 628, width = 1207,
             ...)
      {
      ## 0. work on cc
      if( is.null(cc)|cc==''|is.na(cc) ){
         stop('Commodity code (cc) cannot be empty!')
      }
      
      if( length(cc) > 1 ){
         stop('The current function can only handle ONE HS code at a time.')
      }
      
      ## 1. download world export data
      print("-------------- Download Export data -----------")
      
      tmp_ex_tot_raw_try <- try(
         tmp_ex_tot_raw <- 
            #get.Comtrade( r = 'all', p ='0' , rg = 2 , cc = cc , fmt = 'csv', ps = max_year)$data
             m_ct_search( reporters = "All", partners = 'World', trade_direction = c( "exports"), freq = "annual",
                          commod_codes = cc,
                          start_date = max_year,
                          end_date = max_year ) %>%
            dplyr::select( year, commodity, commodity_code, trade_flow, reporter, reporter_iso, reporter_code, partner, partner_iso, qty_unit,  qty, trade_value_usd) %>%
            rename( Year = year, 
                    `Commodity` = commodity ,
                    `Commodity.Code` = commodity_code ,
                    `Trade.Flow` = trade_flow,
                     Reporter = reporter,
                    `Reporter.ISO` =  reporter_iso,
                    `Reporter.Code` = reporter_code,
                     Partner = partner,
                    `Partner.ISO` = partner_iso,
                    `Qty.Unit` = qty_unit,
                    `Alt.Qty.Unit` = qty,
                    `Trade.Value..US..` = trade_value_usd )
      )
      
      if( class(tmp_ex_tot_raw_try) == 'try-error' ){
         stop("Cannot download data from UN Com Trade!")
      }
      
      ## 2. download eu export data
      tmp_ex_eu_extra_raw_try <- try(
         tmp_ex_eu_extra_raw <- 
            #get.Comtrade( r = '97', p ='0' , rg = 2 , cc = cc , fmt = 'csv', ps = max_year)$data
         m_ct_search( reporters = "EU-28", partners = 'World', trade_direction = c( "exports"), freq = "annual",
                      commod_codes = cc,
                      start_date = max_year,
                      end_date = max_year ) %>%
            dplyr::select( year, commodity, commodity_code, trade_flow, reporter, reporter_iso, reporter_code, partner, partner_iso, qty_unit,  qty, trade_value_usd) %>%
            rename( Year = year, 
                    `Commodity` = commodity ,
                    `Commodity.Code` = commodity_code ,
                    `Trade.Flow` = trade_flow,
                    Reporter = reporter,
                    `Reporter.ISO` =  reporter_iso,
                    `Reporter.Code` = reporter_code,
                    Partner = partner,
                    `Partner.ISO` = partner_iso,
                    `Qty.Unit` = qty_unit,
                    `Alt.Qty.Unit` = qty,
                    `Trade.Value..US..` = trade_value_usd )
      )
      
      if( !tmp_ex_eu_extra_raw$Classification %in% c('H1',"H2", "H4","H6") ){
         print("------------- EU28 export data NOT available ----------------")
         tmp_ex_eu_extra_raw$Year <- unique(tmp_ex_tot_raw$Year )
         tmp_ex_eu_extra_raw$Trade.Flow <- unique(tmp_ex_tot_raw$Trade.Flow)
         tmp_ex_eu_extra_raw$Reporter <- "EU-28"
         tmp_ex_eu_extra_raw$Reporter.ISO <- "EU2"
         tmp_ex_eu_extra_raw$Reporter.Code <- 97
         tmp_ex_eu_extra_raw$Partner <- "World"
         tmp_ex_eu_extra_raw$Partner.ISO <- "WLD"
         tmp_ex_eu_extra_raw$Commodity.Code <- unique(tmp_ex_tot_raw$Commodity.Code)
         tmp_ex_eu_extra_raw$Commodity <- unique(tmp_ex_tot_raw$Commodity)[1]
         tmp_ex_eu_extra_raw$Qty.Unit <- unique(tmp_ex_tot_raw$Qty.Unit)[1]
      }

      ## 3. all countries export data 
      tmp_ex_tot <-
         tmp_ex_tot_raw %>%
         dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity, Qty.Unit, Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
         mutate( Reporter = as.character(Reporter), 
                 Reporter.ISO = as.character(Reporter.ISO),
                 Partner = as.character( Partner ),
                 Partner.ISO = as.character( Partner.ISO )
                )
      
      ## 4. put all EU28 countries together and later no there will be EU28 internal trade shown
      if( tmp_ex_eu_extra_raw$Classification %in% c('H1',"H2", "H4","H6")  ){
         tmp_ex_tot_withEU <- 
            tmp_ex_tot %>%
            mutate( Reporter = ifelse( Reporter.ISO %in% concord_eu28$ISO3, "EU-28", Reporter ) ) %>%
            mutate( Reporter.ISO = ifelse( Reporter == "EU-28", "EU2" , Reporter.ISO )  ) %>%
            mutate( Reporter.Code = ifelse( Reporter == "EU-28", 97 , Reporter.Code )   ) %>%
            mutate( Partner = ifelse( Reporter == "EU-28", "World" , Partner )   ) %>%
            mutate( Partner.ISO = ifelse( Reporter == "EU-28", "WLD" , Partner.ISO )   ) %>%
            group_by(  Year, Reporter,Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity ,Trade.Flow, Qty.Unit ) %>%
            summarise(  Alt.Qty.Unit = sum( as.numeric(Alt.Qty.Unit), na.rm=T),
                        `Trade.Value..US..` = sum(  as.numeric(`Trade.Value..US..`), na.rm = T )
            ) %>%
            ungroup 
      }else{
         tmp_ex_tot_withEU <- 
             tmp_ex_tot 
         # %>%
         #    mutate( Reporter = ifelse( Reporter.ISO %in% concord_eu28$ISO3, "EU-28", Reporter ) ) %>%
         #    mutate( Reporter.ISO = ifelse( Reporter == "EU-28", "EU2" , Reporter.ISO )  ) %>%
         #    mutate( Reporter.Code = ifelse( Reporter == "EU-28", 97 , Reporter.Code )   ) %>%
         #    mutate( Partner = ifelse( Reporter == "EU-28", "World" , Partner )   ) %>%
         #    mutate( Partner.ISO = ifelse( Reporter == "EU-28", "WLD" , Partner.ISO )   ) %>%
         #    group_by(  Year, Reporter,Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity ,Trade.Flow, Qty.Unit ) %>%
         #    summarise(  Alt.Qty.Unit = sum( as.numeric(Alt.Qty.Unit), na.rm=T),
         #                `Trade.Value..US..` = sum(  as.numeric(`Trade.Value..US..`), na.rm = T )
         #    ) %>%
         #    ungroup 
      }
      
      
      ## 5. get eu export to world
      tmp_ex_eu_extra <-
         tmp_ex_eu_extra_raw %>%
         dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO,Commodity.Code, Commodity, Qty.Unit, Alt.Qty.Unit, `Trade.Value..US..`  )
      
      ## 6. Split EU exporto WLD and Intra-EU
      tmp_ex_eu_all <- 
         tmp_ex_tot_withEU %>%
         filter( Reporter == "EU-28" )
      
      ## 7. split into eu intra trade and extra trade
      tmp_ex_eu_intra <- 
         tmp_ex_eu_all %>%
         left_join( tmp_ex_eu_extra, 
                    by = c("Year", "Reporter", "Reporter.ISO", "Reporter.Code", "Partner", "Partner.ISO", "Trade.Flow", "Qty.Unit", "Commodity.Code", "Commodity" )
         ) %>%
         mutate( `Alt.Qty.Unit` = Alt.Qty.Unit.x - Alt.Qty.Unit.y, 
                 `Trade.Value..US..` =  `Trade.Value..US...x` - `Trade.Value..US...y` ) %>%
         dplyr::select( -Alt.Qty.Unit.x, -Alt.Qty.Unit.y, 
                        -`Trade.Value..US...x`,  -`Trade.Value..US...y`) %>%
         mutate( Partner = "EU-28", 
                 Partner.ISO = "EU2")
      
      ## 8. Eu export now splited into Extra-EU (to the world) and Intra-EU (within EU)
      tmp_ex_eu_split <- 
         tmp_ex_eu_intra %>%
         bind_rows( tmp_ex_eu_extra )
      
      
      ## 9. Replace the Eu total export by the EU trade splited
      tmp_ex_tot_withEU %<>%
         filter( Reporter != 'EU-28' ) %>%
         bind_rows( tmp_ex_eu_split ) %>%
         mutate( Share = as.numeric(`Trade.Value..US..`)/ sum( as.numeric(`Trade.Value..US..`), na.rm= T ) ) %>%
         mutate( Share_noIntraEU = as.numeric(`Trade.Value..US..`)/ sum( as.numeric(`Trade.Value..US..`[Partner!='EU-28']), na.rm= T )  ) %>%
         mutate( Share_noIntraEU = ifelse( Partner=='EU-28', NA, Share_noIntraEU ) ) %>%
         arrange( -Share )
      
      
      ## 10. find out how many countries cover 90% share
      tmp_target_share <- 0
      tmp_country_i <- 0
      while( tmp_target_share <= global_coverage ){
         tmp_country_i <- tmp_country_i + 1
         tmp_target_share  <- sum(tmp_ex_tot_withEU[ which(tmp_ex_tot_withEU$Partner != 'EU-28'),]$Share_noIntraEU[1:tmp_country_i], na.rm=T)
      }
      
      tmp_target_exporter <- unique(tmp_ex_tot_withEU$Reporter[1:tmp_country_i])
      tmp_target_exporter_code <- unique(tmp_ex_tot_withEU$Reporter.Code[1:tmp_country_i])
      
      
      ## 11. loop to donwload
      print("-------------- Download Export data by key exporters -----------")
      tmp_list_ex_by_country <- NULL
      tmp_list_ex_by_country <- 
         lapply( as.character(tmp_target_exporter_code),
                 function(i_country_code) {
                    i_country <- unique(tmp_ex_tot_withEU$Reporter[which(tmp_ex_tot_withEU$Reporter.Code == i_country_code)] )
                    print( paste0("----- Extracting data for ", i_country , " -----") )
                    tmp_fail <- 
                       try(
                          tmp_raw_ex_country <-
                             get.Comtrade( r = i_country_code , p = 'all' , rg = 2 , cc = cc , fmt = 'csv', ps = max_year)$data
                       )
                    
                    if( class(tmp_fail) == "try=error" ){
                       stop( paste0("Error occured when extracting data for ", i_country) )
                       next
                    }else if(class(tmp_fail) != "try=error" ) {
                       try(return(tmp_raw_ex_country))
                    }
                 })
      
      tmp_dtf_ex_by_country  <-
         do.call( rbind, tmp_list_ex_by_country  )
      
      ## 12. create a datafrme where EU28 interal trade is not counted
      tmp_dtf_ex_by_country_withEU <-
         tmp_dtf_ex_by_country %>%
         filter( Partner != 'World' ) %>%
         dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity, Qty.Unit, Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
         mutate( Reporter = as.character(Reporter), 
                 Reporter.ISO = as.character(Reporter.ISO),
                 Partner = as.character( Partner ),
                 Partner.ISO = as.character( Partner.ISO )
         ) %>%
         mutate( Partner = ifelse( Partner.ISO %in% concord_eu28$ISO3, 'EU-28', Partner ),
                 Partner.ISO = ifelse( Partner == 'EU-28', "EU2" ,Partner.ISO )) %>%
         group_by( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity, Qty.Unit ) %>%
         summarise(  Alt.Qty.Unit = sum( as.numeric(Alt.Qty.Unit), na.rm=T),
                     `Trade.Value..US..` = sum(  as.numeric(`Trade.Value..US..`), na.rm = T )
         ) %>%
         ungroup %>%
         arrange(  -`Trade.Value..US..`, Reporter  )
      
      ## 13. create data for sankey plot 
      tmp_dtf_plot_sankey <- 
         tmp_dtf_ex_by_country_withEU %>%
         filter( Partner != 'World' ) %>%
         dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner, Commodity.Code, Commodity, Qty.Unit, Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
         group_by( Year, Trade.Flow, Reporter, Commodity.Code  ) %>%
         arrange( -`Trade.Value..US..` ) %>%
         ungroup %>%
         group_by(Year ,Trade.Flow, Commodity.Code ) %>%
         mutate( Weight = (`Trade.Value..US..` /sum( `Trade.Value..US..`, na.rm=T  ))*global_coverage  ) %>%
         ungroup %>%
         mutate( Price = `Trade.Value..US..` / Alt.Qty.Unit ) %>%
         filter( Weight >= trade_weight*global_coverage )
      
      
      ## 14. use Sankey plot to visulize
      tmp_nz_partner <- tmp_dtf_plot_sankey$Partner[tmp_dtf_plot_sankey$Reporter== center_country ]
      
      tmp_nodes <- 
         data.frame( name = unique(c( unique(tmp_dtf_plot_sankey$Reporter),
                                      unique(tmp_dtf_plot_sankey$Partner) ) ) 
                     )
      
      tmp_nodes %<>%
         mutate( id = 0: (nrow(tmp_nodes)-1) ) %>%
         mutate( IS_NZ_node = ifelse( name %in% c(center_country,tmp_nz_partner),'red',NA ) )
      
      tmp_links <-
         tmp_dtf_plot_sankey %>%
         dplyr::select( source = Reporter, 
                        target = Partner, 
                        value =  Weight #`Trade.Value..US..`
                        ) %>%
         #mutate( value = round(value/10^6) ) %>%
         mutate( value = round(value*100,2) ) %>%
         left_join( tmp_nodes,
                    by = c('source' = 'name') ) %>%
         mutate( source = id) %>%
         dplyr::select( -id ) %>%
         left_join( tmp_nodes,
                    by = c('target' = 'name') ) %>%
         mutate( target = id) %>%
         dplyr::select( -id ) %>%
         mutate(IS_NZ_link = NA)
      
      if( length(tmp_nz_partner)!=0 ){
         tmp_links %<>%
            mutate( IS_NZ_link = ifelse( source == tmp_nodes$id[tmp_nodes$name== center_country ], 
                                         'red',  
                                         NA
            ) ) 
         
         sankeyNetwork(Links = tmp_links, Nodes = tmp_nodes, Source = 'source',
                       Target = 'target', Value = 'value', NodeID = 'name',
                       LinkGroup = 'IS_NZ_link', 
                       NodeGroup = "IS_NZ_node",
                       units = "%", #'USD $m', 
                       fontSize = 20, nodeWidth = 30,
                       nodePadding = 15, ...)
      }else{
         sankeyNetwork(Links = tmp_links, Nodes = tmp_nodes, Source = 'source',
                       Target = 'target', Value = 'value', NodeID = 'name',
                       # LinkGroup = 'IS_NZ_link', 
                       #NodeGroup = "IS_NZ_node",
                       units = "%", #'USD $m', 
                       fontSize = 20, nodeWidth = 30,
                       nodePadding = 15, ...)
      }
}
      


## set target hs code
tmp_hs <- '850440'
tmp_coverage <- 0.9

## 1 get data to find out biggest exporters
tmp_ex_tot_raw <-
   get.Comtrade( r = 'all', p ='0' , rg = 2 , cc = tmp_hs , fmt = 'csv', ps = 2016)$data

## 2 get EU export to world data (EU28 code is 97)
tmp_ex_eu_extra_raw <-
   get.Comtrade( r = '97', p ='0' , rg = 2 , cc = tmp_hs , fmt = 'csv', ps = 2016)$data

### all countries
tmp_ex_tot <-
   tmp_ex_tot_raw %>%
   dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity, Qty.Unit, Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
   group_by( Year, Trade.Flow, Commodity.Code  ) %>%
   #mutate( Share = as.numeric(`Trade.Value..US..`)/ sum( as.numeric(`Trade.Value..US..`), na.rm= T ) ) %>%
   mutate( Reporter = as.character(Reporter), 
           Reporter.ISO = as.character(Reporter.ISO),
           Partner = as.character( Partner ),
           Partner.ISO = as.character( Partner.ISO )
           ) %>%
   ungroup #%>%
   #arrange( -Share )


## put all EU28 countries together and later no there will be EU28 internal trade shown
tmp_ex_tot_withEU <- 
   tmp_ex_tot %>%
   mutate( Reporter = ifelse( Reporter.ISO %in% concord_eu28$ISO3, "EU-28", Reporter ) ) %>%
   mutate( Reporter.ISO = ifelse( Reporter == "EU-28", "EU2" , Reporter.ISO )  ) %>%
   mutate( Reporter.Code = ifelse( Reporter == "EU-28", 97 , Reporter.Code )   ) %>%
   mutate( Partner = ifelse( Reporter == "EU-28", "World" , Partner )   ) %>%
   mutate( Partner.ISO = ifelse( Reporter == "EU-28", "WLD" , Partner.ISO )   ) %>%
   group_by(  Year, Reporter,Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Trade.Flow, Qty.Unit, Commodity.Code,Commodity  ) %>%
   summarise(  Alt.Qty.Unit = sum( as.numeric(Alt.Qty.Unit), na.rm=T),
               `Trade.Value..US..` = sum(  as.numeric(`Trade.Value..US..`), na.rm = T )
               #,Share = sum( Share, na.rm = T ) 
               ) %>%
   ungroup #%>%
   #arrange( -Share )


### get eu export to world
tmp_ex_eu_extra <-
   tmp_ex_eu_extra_raw %>%
   dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity, Qty.Unit, Alt.Qty.Unit, `Trade.Value..US..`  ) 


## Split EU exporto WLD and Intra-EU
tmp_ex_eu_all <- 
   tmp_ex_tot_withEU %>%
   filter( Reporter == "EU-28" )

## split
tmp_ex_eu_intra <- 
   tmp_ex_eu_all %>%
   #dplyr::select( -Share ) %>%
   left_join( tmp_ex_eu_extra, 
              by = c("Year", "Reporter", "Reporter.ISO", "Reporter.Code", "Partner", "Partner.ISO", "Trade.Flow", "Qty.Unit", "Commodity.Code", "Commodity" )
              ) %>%
   mutate( `Alt.Qty.Unit` = Alt.Qty.Unit.x - Alt.Qty.Unit.y, 
          `Trade.Value..US..` =  `Trade.Value..US...x` - `Trade.Value..US...y` ) %>%
   dplyr::select( -Alt.Qty.Unit.x, -Alt.Qty.Unit.y, 
                  -`Trade.Value..US...x`,  -`Trade.Value..US...y`) %>%
   mutate( Partner = "EU-28", 
           Partner.ISO = "EU2")

## Eu export now splited into Extra-EU (to the world) and Intra-EU (within EU)
tmp_ex_eu_split <- 
   tmp_ex_eu_intra %>%
   bind_rows( tmp_ex_eu_extra ) %>%
   filter( !is.na(Alt.Qty.Unit) )
   

## Replace the Eu total export by the EU trade splited
tmp_ex_tot_withEU %<>%
   filter( Reporter != 'EU-28' ) %>%
   bind_rows( tmp_ex_eu_split ) %>%
   mutate( Share = as.numeric(`Trade.Value..US..`)/ sum( as.numeric(`Trade.Value..US..`), na.rm= T ) ) %>%
   mutate( Share_noIntraEU = as.numeric(`Trade.Value..US..`)/ sum( as.numeric(`Trade.Value..US..`[Partner!='EU-28']), na.rm= T )  ) %>%
   mutate( Share_noIntraEU = ifelse( Partner=='EU-28', NA, Share_noIntraEU ) ) %>%
   arrange( -Share )
   
   
## find out how many countries cover 90% share
tmp_target_share <- 0
tmp_country_i <- 0
while( tmp_target_share <= tmp_coverage ){
   tmp_country_i <- tmp_country_i + 1
   tmp_target_share  <- sum(tmp_ex_tot_withEU[ which(tmp_ex_tot_withEU$Partner != 'EU-28'),]$Share_noIntraEU[1:tmp_country_i], na.rm=T)
}

tmp_target_exporter <- unique(tmp_ex_tot_withEU$Reporter[1:tmp_country_i])
tmp_target_exporter_code <- unique(tmp_ex_tot_withEU$Reporter.Code[1:tmp_country_i])


## loop to donwload
tmp_list_ex_by_country <- NULL
tmp_list_ex_by_country <- 
   lapply( as.character(tmp_target_exporter_code),
           function(i_country_code) {
              i_country <- unique(tmp_ex_tot_withEU$Reporter[which(tmp_ex_tot_withEU$Reporter.Code == i_country_code)] )
              print( paste0("----- Extracting data for ", i_country , "-----") )
              tmp_fail <- 
                 try(
                    tmp_raw_ex_country <-
                       get.Comtrade( r = i_country_code , p = 'all' , rg = 2 , cc = tmp_hs , fmt = 'csv', ps = 2016)$data
                 )
              
              if( class(tmp_fail) == "try=error" ){
                 stop( paste0("Error occured when extracting data for ", i_country) )
              }else{
                 return(tmp_raw_ex_country)
              }
              
           })

tmp_dtf_ex_by_country  <-
   do.call( rbind, tmp_list_ex_by_country  )

## create a datafrme where EU28 interal trade is not counted
tmp_dtf_ex_by_country_withEU <-
   tmp_dtf_ex_by_country %>%
   filter( Partner != 'World' ) %>%
   dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity, Qty.Unit, Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
   mutate( Reporter = as.character(Reporter), 
           Reporter.ISO = as.character(Reporter.ISO),
           Partner = as.character( Partner ),
           Partner.ISO = as.character( Partner.ISO )
   ) %>%
   mutate( Partner = ifelse( Partner.ISO %in% concord_eu28$ISO3, 'EU-28', Partner ),
           Partner.ISO = ifelse( Partner == 'EU-28', "EU2" ,Partner.ISO )) %>%
   group_by( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity, Qty.Unit ) %>%
   summarise(  Alt.Qty.Unit = sum( as.numeric(Alt.Qty.Unit), na.rm=T),
               `Trade.Value..US..` = sum(  as.numeric(`Trade.Value..US..`), na.rm = T )
   ) %>%
   ungroup %>%
   bind_rows(tmp_ex_eu_intra ) %>%
   arrange(  -`Trade.Value..US..`, Reporter  )

#View(
tmp_dtf_plot_sankey <- 
   tmp_dtf_ex_by_country_withEU %>%
   filter( Partner != 'World' ) %>%
   dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner, Commodity.Code, Commodity, Qty.Unit, Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
   group_by( Year, Trade.Flow, Reporter, Commodity.Code  ) %>%
   arrange( -`Trade.Value..US..` ) %>%
   ungroup %>%
   group_by(Year ,Trade.Flow, Commodity.Code ) %>%
   mutate( Weight =`Trade.Value..US..` /sum( `Trade.Value..US..`, na.rm=T  )  ) %>%
   ungroup %>%
   mutate( Price = `Trade.Value..US..` / Alt.Qty.Unit ) %>%
   filter( Weight >= 0.005 )
#)




#### looks like can only use Sankey plot to visulize -----------------
tmp_nz_partner <- tmp_dtf_plot_sankey$Partner[tmp_dtf_plot_sankey$Reporter=='New Zealand']

tmp_nodes <- 
   data.frame( name = unique(c( unique(tmp_dtf_plot_sankey$Reporter),
                                        unique(tmp_dtf_plot_sankey$Partner) ) 
                                       )
                         )
tmp_nodes %<>%
   mutate( id = 0: (nrow(tmp_nodes)-1) ) %>%
   mutate( IS_NZ_node = ifelse( name %in% c('New Zealand',tmp_nz_partner),'red',NA ) ) %>%
   mutate( IS_ex = ifelse( name %in% tmp_dtf_plot_sankey$Reporter, 'Exporter', 'No' ) ) %>%
   mutate( IS_im = ifelse( name %in% tmp_dtf_plot_sankey$Partner, 'Importer', 'No' ) ) %>%
   mutate( IS_ex_im_both = ifelse( IS_ex == 'Exporter' & IS_im == 'Importer', 'Both', IS_ex ) ) %>%
   mutate( IS_ex_im_both = gsub( 'No', 'Importer', IS_ex_im_both)  )


tmp_links <-
   tmp_dtf_plot_sankey %>%
   dplyr::select( source = Reporter, 
                  target = Partner, 
                  #value = `Trade.Value..US..`
                  Value = Weight
                  ) %>%
   #mutate( value = round(value/10^6) ) %>%
   mutate( value = round(as.numeric(Value)*100,1) ) %>%
   left_join( tmp_nodes,
              by = c('source' = 'name') ) %>%
   mutate( source = id) %>%
   dplyr::select( -id ) %>%
   left_join( tmp_nodes,
              by = c('target' = 'name') ) %>%
   mutate( target = id) %>%
   dplyr::select( -id ) %>%
   mutate(IS_NZ_link = NA) 

if( length(tmp_nz_partner)!=0 ){
   tmp_links %<>%
      mutate( IS_NZ_link = ifelse( source == tmp_nodes$id[tmp_nodes$name==  'New Zealand' ], 
                                   'red',  
                                   NA
      ) ) 
}
   
tmp_sankey_color <- 'd3.scaleOrdinal() .domain(["Exporter", "Importer","Both"]) .range(["#97D700", "#CD5B45" , "#FBE122"])'
sankeyNetwork(Links = tmp_links, Nodes = tmp_nodes, Source = 'source',
              Target = 'target', Value = 'value', NodeID = 'name',
              LinkGroup = 'IS_NZ_link', 
              NodeGroup = "IS_ex_im_both",
              #units = 'USD $m', 
              units = '%',
              fontSize = 20, nodeWidth = 30,
              nodePadding = 15,
              colourScale=tmp_sankey_color)


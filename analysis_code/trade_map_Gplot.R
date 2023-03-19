#### produce a world map of export by country
#### Setup parameters
#output_name_basic <-  'Arrival_by_POV_basic.html'
#output_name_basic_pie <- 'Arrival_by_POV_basic_pie.html'
#output_name_fancy <- 'Arrival_by_POV_fancy_pie.html'
load( paste0(output_folder_shiny,"/dtf_shiny_country_gs.rda")) ## principle commodity from StatsNZ
output_name_fancy_stylised <- 'Twoway_trade_by_country.html'
output_path <- 'shiny/www/'

## color for each product
## a summary data frame
col.df <- data.frame( Type_ie = c( "Exports", 
                               "Imports"),
                      Color_positive = c("green4","greenyellow"), #mbie.cols( c(1, 3 ) ),
                      Color_negative = c("brown4", "brown1") #mbie.cols( c(5, 6 ) )
                      )


col.df$POV <- as.character(col.df$Type_ie)
col.df$Type_ie <- as.character(col.df$Type_ie)
col.df$Color_positive <- as.character(col.df$Color_positive)
col.df$Color_negative <- as.character(col.df$Color_negative)

#### get the data and groom it into a format that the function can use
tmp_data_pie_all <-
   dtf_shiny_country_gs %>%
   group_by( Year, Country, Type_ie, Note, ISO2, lat, lon ) %>%
   summarise( Value = sum(Value, na.rm=T) ) %>%
   ungroup

## generate twoway trade, calculate share and world share
tmp_data_pie_all %<>%
   bind_rows( 
      tmp_data_pie_all %>%
         group_by( Year, Country, Note, ISO2, lat, lon  ) %>%
         summarise( Value = sum(Value, na.rm=T) ) %>%
         ungroup %>%
         mutate( Type_ie = 'Two_way_trade' )
      ) %>%
   arrange( Year, Country, Type_ie ) %>%
   spread( Type_ie, Value ) %>%
   mutate( Exports_share = Exports/Two_way_trade,
           Imports_share = Imports/Two_way_trade) %>%
   group_by( Year ) %>%
   mutate( Exports_share_world = Exports/Exports[Country=='World'],
           Imports_share_world = Imports/Imports[Country=='World'],
           TWT_share_world = Two_way_trade/Two_way_trade[Country=='World']
           ) %>%
   ungroup

## generate CAGR for the past 5 years
tmp_data_pie_all %<>%
   left_join(
      tmp_data_pie_all %>%
      group_by( Country ) %>%
         do( Exports_cagr = CAGR(.$Exports[.$Year==max(.$Year)] / .$Exports[.$Year==(max(.$Year)-5)], 5),
             Imports_cagr = CAGR(.$Imports[.$Year==max(.$Year)] / .$Imports[.$Year==(max(.$Year)-5)], 5),
             TWT_cagr = CAGR(.$Two_way_trade[.$Year==max(.$Year)] / .$Two_way_trade[.$Year==(max(.$Year)-5)], 5) 
         ) %>%
         ungroup 
   ) %>%
   mutate( Exports_cagr = as.numeric(Exports_cagr),
           Imports_cagr = as.numeric(Imports_cagr),
           TWT_cagr = as.numeric(TWT_cagr)
           ) %>%
   filter( Year == max(Year) ) %>%
   arrange( -Two_way_trade ) 
  
## Get geo code, change some country name in order to get lat and long
tmp_data_pie_all <-
   tmp_data_pie_all %>%
   mutate( Country = gsub("[&]", "and", Country),
           Country = gsub("Georgia", "Georgia, Country", Country),
           Country = gsub("Congo, the Democratic Republic of the", "Democratic Republic of the Congo", Country), 
           Country = gsub("Micronesia, Federated States of", "Federated States of Micronesia", Country),
           Country = gsub("St Maarten [(]Dutch Part[)]", "Sint Maarten", Country),
           Country = gsub("Yemen, Democratic", "Democratic Yemen", Country),
           Country = gsub("Korea, Democratic People's Republic of", 
                          "North Korea", Country),
           Country = gsub("South Georgia, Country and the South Sandwich Islands", 
                          "South Georgia and the South Sandwich Islands", Country),
           Country = gsub("Yugoslavia/Serbia and Montenegro", "Serbia and Montenegro", Country),
           Country = gsub("Samoa, American", "American Samoa", Country)
   )

## save map into the same folder
path_orgin <- getwd()
setwd( paste0( path_orgin,'/' , output_path ) ) 

## delete all previously saved files
tmphtml <- c( output_name_fancy_stylised)
tmppng <- grep( '.png', dir(), value =T )
unlink(c(tmphtml, tmppng))
setwd(path_orgin)   

## ----------------------------------------  2. More lovely map ------------------------------------ ##
###### total  more sophisticated ploting
tmp_data_pie <- 
   tmp_data_pie_all %>%
   dplyr::select(lon,
                 lat,
                 Country,
                 Two_way_trade,
                 Year,
                 col.df$POV,
                 Exports_share,Imports_share,
                 Exports_share_world, Imports_share_world, TWT_share_world,
                 Exports_cagr, Imports_cagr, TWT_cagr        
               ) %>%
   mutate( TWT_share = 1 )

tmp_data_pie[tmp_data_pie %in% c(Inf,NaN)] <- NA

##### Total primary exprots
tmp_data_pie <- 
   tmp_data_pie %>%
   arrange( -Two_way_trade ) %>%
   as.data.frame

### for arrival
#NZ_Total_Primary_Export$Rank <- 1:nrow(NZ_Total_Primary_Export)
#NZ_Total_Primary_Export$Market_Share <- paste(round(NZ_Total_Primary_Export$Total/sum(NZ_Total_Primary_Export$Total,na.rm=T)*100,3),
#                                                '%',
#                                               sep='')


#### Primary and individual
sec.all <- c("Two_way_trade",col.df$POV)

for(i in 1:length(sec.all)){
   tmp.sec <- sec.all[i]
   
   if(tmp.sec == 'Two_way_trade'){
      tmp.sec.cagr <- paste0('TWT','_cagr')
      tmp.sec.share <- paste0('TWT','_share')
      tmp.sec.shareworld <- paste0('TWT','_share_world')
   }else{
      tmp.sec.cagr <- paste0(tmp.sec,'_cagr')
      tmp.sec.share <- paste0(tmp.sec,'_share')
      tmp.sec.shareworld <- paste0(tmp.sec,'_share_world')
   }
   
   #tmp.sec.cagr <- paste0(tmp.sec,'_cagr')
   #tmp.sec.percent <- paste0(tmp.sec,'_percent')
   tmp_data_pie <- tmp_data_pie[order(tmp_data_pie[,tmp.sec],decreasing=T),]
   
   ##
   if(i==1){
      tmp.title <- paste(
         '<th> Value </th>',
         '<th> Rank </th>',
         '<th> Share of total trade </th>',#'<th> Share by purpose </th>',
         '<th> Share of world trade </th>', #'<th> Share by market </th>',
         '<th> CAGR </th>',
         '<th> CAGR (World) </th>',
         '</tr>',
         sep=''
      )	
      
      tmp_data_pie <- cbind.data.frame(tmp_data_pie,tmp.title)	
      
      colnames(tmp_data_pie)[ncol(tmp_data_pie)] <- paste('<table>',
                                                          '<tr>',
                                                          '<th>',
                                                          'Trade',#'Purpose',
                                                          '</th>')
   }
   if(i<length(sec.all)){
      tmp_data_pie[,ncol(tmp_data_pie)] <- paste(tmp_data_pie[,ncol(tmp_data_pie)],
                                                 '<tr>',
                                                 '<td>', ifelse(tmp.sec=="Two_way_trade",'Two way trade',tmp.sec),'</td>',
                                                 '<td>', paste0( FormatDollars(round(tmp_data_pie[,tmp.sec]/10^6,0)), 'm') ,'</td>',
                                                 '<td>', paste0( (1:nrow( tmp_data_pie ))-1 ),'</td>',
                                                 '<td>', paste0( round( (tmp_data_pie[,tmp.sec.share])*100,0 ),'%'  ),'</td>',
                                                 '<td>', paste0( round( (tmp_data_pie[,tmp.sec.shareworld])*100,0 ),'%' ) ,'</td>',
                                                 '<td>', ifelse(!is.na(tmp_data_pie[,tmp.sec.cagr]),paste0( round(tmp_data_pie[,tmp.sec.cagr],1 ),'%'),"---"),'</td>',
                                                 '<td>', paste0( round(tmp_data_pie[tmp_data_pie$Country=='World',tmp.sec.cagr],1),'%'),'</td>',
                                                 '</tr>')
   }
   
   if(i==length(sec.all)){
      tmp_data_pie[,ncol(tmp_data_pie)] <- paste(tmp_data_pie[,ncol(tmp_data_pie)],
                                                 '<tr>',
                                                 '<td>', ifelse(tmp.sec=="Two_way_trade",'Two way trade',tmp.sec),'</td>',
                                                 '<td>', paste0( FormatDollars(round(tmp_data_pie[,tmp.sec]/10^6,0)), 'm') ,'</td>',
                                                 '<td>', paste0( (1:nrow( tmp_data_pie ))-1 ),'</td>',
                                                 '<td>', paste0( round( (tmp_data_pie[,tmp.sec.share])*100,0 ),'%'  ),'</td>',
                                                 '<td>', paste0( round( (tmp_data_pie[,tmp.sec.shareworld])*100,0 ),'%' ) ,'</td>',
                                                 '<td>', ifelse(!is.na(tmp_data_pie[,tmp.sec.cagr]),paste0( round(tmp_data_pie[,tmp.sec.cagr],1 ),'%'),"---"),'</td>',
                                                 '<td>', paste0( round(tmp_data_pie[tmp_data_pie$Country=='World',tmp.sec.cagr],1),'%'),'</td>',
                                                 '</tr>',
                                                 '</table>')
   }                                                                   
}

sector.exp1 <- c('lon','lat',"Country" ,"Year","Two_way_trade"
                 #"Total_Primary_Export","Rank","Market_Share"
                 #,
                 #paste(col.df$Product,'_Export',sep='')
)

sector.exp2 <-  colnames( tmp_data_pie )[ncol(tmp_data_pie)]

tmp_data_pie <- cbind.data.frame(tmp_data_pie[ sector.exp1 ],
                                 tmp_data_pie[ sector.exp2 ])


## export this data
#write.csv(NZ_Total_Primary_Export,"I:/FCS/POLICY ADVICE/Sector Development/Information Analysis/NZ primary sector exports by markets/Output/NZ export by country sector kpm.csv")

### Sources
tmp_data_pie$Source <- paste("<em> W.Zhang Analytics and Statistics New Zealand </em>")
colnames(tmp_data_pie)[which(colnames(tmp_data_pie)=="Source")] <- "<strong> Source </strong>"

## Notes
tmp_data_pie$Notes <- paste("<ul> <li> <em>Share of total trade</em> shows the share of exports or imports of the two way trade. </li>" ,
                            "<li> <em>Share of world</em> shows the share of two way trade or exports or imports of the world market. </li>",
                            "<li> <em>CAGR</em> stands for compound annual growth rate from the past 5 years. </li>",
                            "<li> <em>The last column</em> shows CAGR from the past 5 years for the world. </li> </ul>")

colnames(tmp_data_pie)[which(colnames(tmp_data_pie)=="Notes")] <- "<strong> Notes </strong>"

### Country Infomation
tmp_data_pie$"Country information" <- paste('<a',paste("href=","'",
                                                       "http://en.wikipedia.org/wiki/",
                                                       tmp_data_pie$Country,
                                                       "'",
                                                       " target=","'",
                                                       "_blank",
                                                       "'",
                                                       ">",
                                                       paste('Know more about',tmp_data_pie$Country),
                                                       sep=''),
                                            '</a>')

colnames(tmp_data_pie)[which(colnames(tmp_data_pie)=="Country information")] <- "<strong> Country information </strong>"

### put country and period into a table
colnames(tmp_data_pie)[3] <- paste('<table>',
                                   '<tr>',
                                   '<th>','Country','</th>',
                                   '<th>','Period','</th>',
                                   '</tr>'
)

tmp_data_pie[,3] <- paste('<tr>',
                          '<td>',tmp_data_pie[,3],'</td>',
                          '<td>',tmp_data_pie[,4],'</td>',
                          '</tr>',
                          '</table>'
)

tmp_data_pie <- tmp_data_pie[, -4 ]		

#### Insert mBIE logo
#tmp_data_pie <- cbind.data.frame(tmp_data_pie[,1:2],
#                                 rep('',nrow(tmp_data_pie)),
#                                 tmp_data_pie[,3:ncol(tmp_data_pie)]
#) 

#colnames(NZ_Total_Primary_Export)[3] <- paste('<img src= http://www.fish.govt.nz/Images/MPI-logo.jpg width=200 height=54>')
#colnames(tmp_data_pie)[3] <- ' '
#tmp_data_pie[,' '] <- paste('<img src= http://www.alphatech.co.nz/site/images/564015.jpg width=250 height=56.7>')									

## Take out NA values                                 
tmp_data_pie <- 
   tmp_data_pie %>%
   filter( !is.na(lat) )

## make icon   
ic <- ifelse( tmp_data_pie$Two_way_trade >= 10^9,
              iconlabels( paste0("$", round( tmp_data_pie$Two_way_trade/10^9, 0 ), 'B' ),
                          height= as.numeric(cut(round( tmp_data_pie$Two_way_trade/10^9, 0 ), breaks = 20)) + 10,
                          colPalette='white',
                          icon=F),
              iconlabels( paste0("$", round( tmp_data_pie$Two_way_trade/10^6, 0 ), 'M' ),
                          height= 5,
                          colPalette='white',
                          icon=F)
)


# remove that column
tmp_data_pie <- 
   tmp_data_pie %>% 
   select( -Two_way_trade) 

#### by sector
tmp_data_pie_sector <- 
   tmp_data_pie_all %>%
   filter( `Two_way_trade` > 0 ) %>%
   filter( !is.na(lat) ) 

tmp_data_pie_sector[is.na(tmp_data_pie_sector)] <- 0.00001    ## not tolerant for NA
tmp_data_pie_sector[tmp_data_pie_sector==0] <- 0.00001 ## not tolerant for zero    

## reorder column 
tmp_data_pie_sector <- 
   tmp_data_pie_sector %>%
   mutate( `Trade balance` = Exports - Imports ) %>%
   dplyr::select( lon, lat, Country, `Two_way_trade`, `Trade balance` , one_of(col.df$POV) )  %>%
   as.data.frame

## where trade balace is positive and negative
tmp_data_pie_sector_positive <-
   tmp_data_pie_sector %>%
   filter( `Trade balance` >=0 )

tmp_data_pie_sector_negative <-
   tmp_data_pie_sector %>%
   filter( `Trade balance` < 0 )


###################### Transform data.frame to spatial data fraom ###########


coordinates(tmp_data_pie) <- ~lon+lat    ### has to be lon and lat
proj4string(tmp_data_pie) <- CRS("+init=epsg:4326")

coordinates(tmp_data_pie_sector_positive) <- ~lon+lat    ### has to be lon and lat
proj4string(tmp_data_pie_sector_positive) <- CRS("+init=epsg:4326")

coordinates(tmp_data_pie_sector_negative) <- ~lon+lat    ### has to be lon and lat
proj4string(tmp_data_pie_sector_negative) <- CRS("+init=epsg:4326")

coordinates(tmp_data_pie_sector) <- ~lon+lat    ### has to be lon and lat
proj4string(tmp_data_pie_sector) <- CRS("+init=epsg:4326")

######################## plot .html maps ###########################
setwd( paste0( path_orgin, '/' , output_path ) ) 

m<-plotGoogleMaps(tmp_data_pie,
                  #zcol='Total_Primary_Export',
                  iconMarker=ic,
                  colPalette='green',
                  filename=output_name_fancy_stylised,
                  openMap=F,
                  add=T,
                  mapTypeId = 'ROADMAP'
)


# bb <- segmentGoogleMaps(tmp_data_pie_sector_positive,
#                         zcol=col.df$POV,
#                         colPalette=col.df$Color_positive, ## color for positive
#                         max.radius = 670000,
#                         strokeOpacity=0.01,
#                         fillOpacity = 0.95,
#                         #colPalette='yellow',
#                         filename=output_name_fancy_stylised,
#                         openMap=F,
#                         clickable=F,
#                         previousMap = m ,
#                         mapTypeId = 'ROADMAP',
#                         fitBounds=T ,
#                         add = T
# ) 
# 
# bb <- segmentGoogleMaps(tmp_data_pie_sector_negative,
#                         zcol=col.df$POV,
#                         colPalette= col.df$Color_negative,
#                         max.radius = 670000,
#                         strokeOpacity=0.01,
#                         fillOpacity = 0.95,
#                         #colPalette='yellow',
#                         filename=output_name_fancy_stylised,
#                         openMap=F,
#                         clickable=F,
#                         previousMap = bb ,
#                         mapTypeId = 'ROADMAP',
#                         fitBounds=T 
# ) 


bb <- segmentGoogleMaps(tmp_data_pie_sector,
                        zcol=col.df$POV,
                        colPalette= unlist(ifelse(tmp_data_pie_sector$`Trade balance`>=0,
                                           as.data.frame(col.df$Color_positive),
                                           as.data.frame(col.df$Color_negative)) ), ## color for positive
                        max.radius = 680000,
                        strokeOpacity=0.01,
                        fillOpacity = 0.95,
                        #colPalette='yellow',
                        filename=output_name_fancy_stylised,
                        openMap=F,
                        clickable=F,
                        previousMap = m ,
                        mapTypeId = 'ROADMAP',
                        fitBounds=T ,
                        add = F
) 

## Reset the path to original working directorate              
setwd(path_orgin)                                          

## ------------------------ post production adjustment ---------------------------- ##
webpage <- readLines( paste0( output_path,output_name_fancy_stylised) )

### taking out default stylizing and replace with new ones
#webpage[ 6:11 ] <- ''
#webpage[ 6 ] <- ' <link rel="stylesheet" type="text/css" href="TableStyle.css"> '

##map_canvas {min-height: 100%;
#height:auto; }

#webpage[7:10] <- ''
webpage[6:8] <- ''

webpage[6] <- 
   " html { height: 100% } 
 body { height: 100%; 
margin: 0px; 
padding: 0px;
}

#map_canvas {
min-height: 100% !important;
height:400px !important; 
} 

td{
background-color: PowderBlue ;
border:1px solid white;
}

th
{
   background-color: CornflowerBlue;
   border:1px solid white;
}

table{
border-collapse:collapse;
text-align:center;
}

tr:hover td{ background-color: CornflowerBlue;}

p{margin-top:0;
margin-bottom:0;
}
ul{margin-top:0;
margin-bottom:0;
padding-top:0;
padding-bottom:0
}

* {
font-size: 100%;
font-family: Calibri; /*Arial Narrow;*/
}

.legend td{text-align:left;background:transparent;border: 0;}
.legend tr:hover td{ background-color: transparent;}

#cBoxes {position:absolute;
right:0px;
top:0px;
background:rgba(255,255,255,0.5); opacity:0.89;
font-size: 70%;
background:-color:rgba(255,255,255,0.5); opacity:0.90;
} </style>"

#.legend td{text-align:left;background-color:rgba(255,255,255,0.5); opacity:0.89; background:transparent}

## Get rid of commas
webpage <- gsub( ": <img", "<img", webpage )
webpage <- gsub( "</tr>: <tr>", "</tr>  <tr>", webpage )
webpage <- gsub( "</th>: <th>", "</th> <th>", webpage )

## change open map up center
#webpage <- gsub( "-75.250973,-177.156097", "-35.46066995149529,38.671875", webpage )
#webpage <- gsub( " 71.706936,179.414413", " 69.03714171275197,-56.6015625", webpage )
webpage <- gsub( "-75.250973,-177.156097", "-35.46066995149529,20", webpage )
webpage <- gsub( " 71.706936,179.414413", " 69.03714171275197,-70", webpage )

## format legend
webpage[ ( grep('LEGEND', webpage)[1] ):( grep('LEGEND', webpage)[1]+4 ) ] <- ''  

webpage <- gsub( "<b>tmpxdataxpie</b>", '<b> Two way trade <br> (Show/hide labels) <br> (Click on labels to show<br>country level information) </b>', webpage )
#webpage <- gsub( '<table border="0">', "<table border='0' class='legend'>", webpage )
webpage <- gsub( '<table style="border-collapse:collapse; width:100%;">', "<table border='0' class='legend'>", webpage )
#webpage <- gsub( '<b> tmp_data_pie_sector_positive<b>', 'Markets with trade <b> <font color="#00CD00"> SURPLUS </font> <b> <br> <b> <font color="#008B00">Exports </font> and <font color="#ADFF2F">imports </font> <br> (Show/hide piecharts) <b>', webpage )
#webpage <- gsub( '<b> tmp_data_pie_sector_negative<b>', 'Markets with trade <b> <font color="#FF0000"> DEFICIT </font> <b> <br> <b> <font color="#8B2323">Exports </font> and <font color="#FF4040">imports </font> <br> (Show/hide piecharts) <b>', webpage )
webpage <- gsub( '<b>tmp_data_pie_sector</b>', '<b> Exports and imports by markets <b> <br> (Show/hide piecharts) <br> <br> Markets with trade <b> <font color="#00CD00"> SURPLUS </font> <b> <br> <b> <font color="#008B00">Exports </font> and <font color="#ADFF2F">imports </font> <br>  <b>  <br> Markets with trade <b> <font color="#FF0000"> DEFICIT </font> <b> <br> <b> <font color="#8B2323">Exports </font> and <font color="#FF4040">imports </font>  </b>', webpage )
webpage <- gsub( 'LEGEND', "Legend", webpage )
webpage <- gsub( '<tr> <td>Exports, Imports</td></tr>', "", webpage )


## unchecked the label first
#tmp_pos_checkbox <- which(grepl('type="checkbox"', webpage) )[1] ## the first one is for lable
#tmp_target_box <- strsplit(strsplit(webpage[tmp_pos_checkbox], " ")[[1]][4], '"')[[1]][2]
#tmp_pos_checkbox <- which(grepl( tmp_target_box, webpage))[1]
#webpage[tmp_pos_checkbox] <- gsub( "showO", 'hideO', webpage[tmp_pos_checkbox] )

## unchech boxes -- no in use at the moment
#webpage <- gsub("showO[(]mark", "hideO(mark", webpage)
#webpage <- gsub("showO[(]poly", "hideO[(]poly", webpage)

## get rid of legend since it does not work in shiny dashboard
tmp_pos_legend <- which(grepl('boxLegend', webpage))
webpage[tmp_pos_legend[1]:(tmp_pos_legend[1]+2)] <- '' ## for trade surpluse markets
#webpage[tmp_pos_legend[2]:(tmp_pos_legend[2]+2)] <- '' ## for trade deficit markets

#### get rid of functions to change opacity and linewidth
#webpage <- gsub( 'value="50"', 'value="95"', webpage )
#webpage <- gsub( 'value="1"', 'value="0"', webpage )

tmp_pos_opacity <- which(grepl('setOpac', webpage))
#tmp_pos_opacity <- tmp_pos_opacity[ (length(tmp_pos_opacity)-1) : length(tmp_pos_opacity)]
tmp_pos_opacity <- tmp_pos_opacity[ length(tmp_pos_opacity)]

webpage[(tmp_pos_opacity[1]-1):(tmp_pos_opacity[1]+7)] <- ""  ## trade surpluse
#webpage[(tmp_pos_opacity[2]-1):(tmp_pos_opacity[2]+7)] <- ""  ## trade deficit

### info box size
webpage <- gsub( 'maxWidth :330', 'maxWidth :550', webpage )

## oppcacity
webpage <- gsub( 'fillOpacity:0.95,', 'fillOpacity:0.60,', webpage )

## 5. stylize google map colors
style <- ",
styles: [
{
   featureType: 'all',
   stylers: [
   { saturation: -80 }
   ]
},{
   featureType: 'road.arterial',
   elementType: 'geometry',
   stylers: [
   { hue: '#00ffee' },
   { saturation: 50 }
   ]
}]
   }"

## find myOption parameter
tmp_pos <- which( grepl( 'streetViewControl:false' ,webpage) )
webpage[tmp_pos] <- paste0( 'streetViewControl:false',
                               style)   

## Save results back
writeLines( webpage, paste0(output_path,output_name_fancy_stylised))

#shell.exec( paste0('exploratory_output/stylised_',output_name_fancy) )
###########################################################################
rm(list= ls())

Sys.setlocale("LC_TIME", "C")

## set up R environment ---------------------------------------------------
#PrefLibPaths <- "P:/R/libraries/3.3.2-20170925"
#source("P:/R/common.Rprofile")
#source("P:/R/common.Rprofile.dev.R")
###########################################################################

### make sure open in Chrome
options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
#.libPaths("P:/R/libraries/3.3.2-20170925")
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_361')

###########################################################################
## load R libraries -------------------------------------------------------
#library(plotGoogleMaps)
#library(plyr)  ## make sure {plyr} is loaded before {dplyr}
#library(reshape2)
library(RODBC)
#library(mbieDBmisc)
#library(mbie)
#library(mbiedata)
library(xlsx)
library(lubridate)
library(stringr)
library(sqldf)
library(shiny)
library(utils)
library(data.table)
library(proj4)
#library(mbiemaps)
library(dplyr)
library(tidyr)
library(gridExtra)
library(Cairo)
#library(rsdmx)
library(ggrepel)
library(countrycode)
library(zoo)
library(withr)
library(treemap)
library(shinydashboard)
library(DT)
library(readxl)
library(highcharter)
library(leaflet)
library(leaflet.minicharts)
library(rvest)
library(magrittr)
library(rjson)
library(comtradr)
library(memoise)
library(networkD3)
library(promises)
#library(future)
#plan(multiprocess)
library(readr)

library(RCurl)
#library(plyr)
library(XML)  
#library(ggmap)

### use memoise package for ct_search in comtradr ----
m_ct_search <- memoise::memoise(ct_search)

#library(jsonlite)
#library("RCurl")
#library("XML")
#library(XLConnect)
#library(RH2)
###########################################################################


#------------------- import project-specific functions and parameters
#source("R/themes.R")
#source("R/utilities.R")
#source("R/one-off-parameters.R")


###########################################################################
## Set global variables ---------------------------------------------------

## ODBC connection to databases
#PlayPen <- odbcConnect("PlayPen_Prod") # for creating SRdb
#TRED <- odbcConnect("TRED_Prod")       # for accessing data from Infoshare etc

## load user defined functions 
source("R/utility.R")


### read concordance
## read HS2 and HS4 concordance 
## This is down load from Stats nz (http://aria.stats.govt.nz/aria/#ClassificationView:uri=http://stats.govt.nz/cms/ClassificationVersion/cvaxEKAnGBFJWdsK)
concord_hs24 <-
   read_excel('data_raw/concordances/Concordance.xlsx',
              sheet = 'HS_l2_l4') %>%
   mutate_all( as.character() ) %>%
   rowwise() %>%
   mutate( HS_level = ifelse( nchar(HS_codes)==2, as.character(2),
                              ifelse( nchar(HS_codes)==4, as.character(4), 
                                      as.character(6) )
                              ) 
           ) %>%
   dplyr::select( HS_level, HS_codes, HS_description )

save( concord_hs24, file = 'shiny/concord_hs24.rda' )

## SNZ's principle commodities
## exports goods
concord_snz_eg <- 
   read_excel('data_raw/concordances/Concordance.xlsx',
              sheet = 'export_goods_snz') %>%
   mutate_all( as.character() ) %>%
   mutate( HS_codes = gsub("[`]","", HS_codes  ) )

save( concord_snz_eg, file = 'shiny/concord_snz_eg.rda' )

## imports goods
concord_snz_ig <- 
   read_excel('data_raw/concordances/Concordance.xlsx',
              sheet = 'import_goods_snz') %>%
   mutate_all( as.character() ) %>%
   mutate( HS_codes = gsub("[`]","", HS_codes  ) )

save( concord_snz_ig, file = 'shiny/concord_snz_ig.rda' )

## contry lat and long
concord_country <-
   read_excel('data_raw/concordances/Concordance.xlsx',
              sheet = 'country_lat_long') %>%
   mutate_all( as.character() )

save( concord_country, file = 'shiny/concord_country.rda' )


## country group
concord_country_group <-
   read_excel('data_raw/concordances/Concordance.xlsx',
              sheet = 'country_group') %>%
   mutate_all( as.character() )

save( concord_country_group, file = 'shiny/concord_country_group.rda' )


## membership grou
concord_country_member <-
   read_excel('data_raw/concordances/Concordance.xlsx',
              sheet = 'country_membership') %>%
   mutate_all( as.character() )

save( concord_country_member, file = 'shiny/concord_country_member.rda' )

## EU28 country and ISO3 
concord_eu28 <- 
   concord_country_member <-
   read_excel('data_raw/concordances/Concordance.xlsx',
              sheet = 'EU28') %>%
   mutate_all( as.character() )

save( concord_eu28, file = 'shiny/concord_eu28.rda' )


## map between HS2 code and commodity definted by Coriolos and NZTE, provided by Andrew McCulumn from Sector Policy
concord_ExportHSCode <- 
   read_excel('data_raw/concordances/Concordance.xlsx',
             sheet = 'Concordance_ExportHSCode') %>%
   mutate( HS_code = as.character(gsub('`','',HS_code)),
           Commodity = as.character(Commodity) ) %>%
   mutate_all( as.character() )


## map between snz service between old and new --
concord_snz_service <-
   read_excel('data_raw/concordances/Concordance.xlsx',
             sheet = 'snz_service_new_old') %>%
   mutate_all( as.character() )

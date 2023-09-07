#######################################################################
#                                                                     #
#                                                                     #
#         Exoport and Import Dashboard                                #
#                                                                     #
#                                                                     #
#######################################################################

##################################################
## 0.0 set environment --------------------------- 
## set R, load user defined functions and some global variables
source("xxx.Rprofile") # it is necessary to run when start from outside Rstudio, and no harm to run it again



## Change character ---------------------------
Current_qtr <- "2023 Q2"   

output_folder <- paste0("data/", gsub(" ","_", Current_qtr))
output_folder_shiny <- paste0("data/", gsub(" ","_", Current_qtr), "/shiny")
dir.create( output_folder )
dir.create(output_folder_shiny )
######################################################################

## membership group --- run this outside .Rprofile because it does not run properly in the file
concord_country_member <-
   read_excel('data_raw/concordances/Concordance.xlsx',
              sheet = 'country_membership') %>%
   mutate_all( as.character() )

save( concord_country_member, file = 'shiny/data/concord_country_member.rda' )

## list of raw files -- need to mannually download from inforshare website
file_service_by_country_hist1 <- "data_raw/service_by_country/service_imports_exports_by_country_2007_2012.csv"
file_service_by_country_hist2 <- "data_raw/service_by_country/service_imports_exports_by_country_2013_2018.csv" 
file_service_by_country <- "data_raw/service_by_country/service_imports_exports_by_country.csv" ## Q1 2020 updated
file_total_goods_services <- "data_raw/Total_ex_im_g_s.csv" ## updated mannually using SNZ data Q4 2019
file_map_goods_services_by_country <- "data_raw/goods-and-services-trade-by-country-year-ended-march-2020-map-csv.csv" # Q1 2020 updated
file_fdi_odi <- "data_raw/fdi_odi.csv" ## only annual data Year ended Mar 2019
file_ppl_in <- "data_raw/ppl_in.csv"  ## Q1 2020 updated
file_ppl_out <- "data_raw/ppl_out.csv" ## Q1 2020 updated
#file_stats_nz_trade <- "data_raw/intel_trade_data/international-trade-december-2022-quarter.zip"
#file_stats_nz_trade <- "data_raw/intel_trade_data/international-trade-march-2023-quarter.csv.zip"
file_stats_nz_trade <- "data_raw/intel_trade_data/international-trade-june-2023-quarter.zip"

###########################################################################
## create list of objects to be reserved when cleaning workspace ----------
keepers <- ls()
keepers <- ls()
###########################################################################

## 0. Download and format import and export data by country and commodity -------
source("grooming_code/read_snz_hs_csv/get_snz_trade_data_links.R")
source("grooming_code/read_snz_hs_csv/Commodity_Data_Grab.r")
source("grooming_code/read_snz_hs_csv/Commodity_Data_Grab_CSV.r")
source("grooming_code/read_snz_hs_csv/Commodity_Data_Process.r")
source("grooming_code/read_snz_hs_csv/Groom_Trade_Data.r")
source("grooming_code/read_snz_hs_csv/Upload_To_TRED.r")

## manipulate new snz trade data ----------
source("grooming_code/xx_groom_stats_nz_intel_trade.R")

#detach("package:plyr", unload=TRUE) ## detach packages ----
source("grooming_code/read_snz_hs_csv/Put_Service_Good_Data.R")

## 1. read data, required only when there are new data
source('grooming_code/read_export_import_data.R')

## 2. groom data for shiny
source('grooming_code/1_groom_main_data_for_shiny.R')
source('grooming_code/4_groom_data_by_country_total_gs.R')
source('grooming_code/2_groom_data_by_key_exports_goods_services.R')
source('grooming_code/3_groom_data_by_key_imports_goods_services.R')
#source('grooming_code/4_groom_data_by_country_total_gs.R')
source('grooming_code/5_groom_data_country_group.R')
source('grooming_code/6_groom_data_pre-defined_commodity_SNZ.R')
source('grooming_code/7_groom_data_full_HS_levels_country.R')
#source('grooming_code/8_groom_get_country_flag.R')
source('grooming_code/9_groom_fdi_odi.R')
source('grooming_code/10_groom_ppl_movement.R')
#source('grooming_code/11_uncomtrade_country.R')

## copy all shiny files into shiny folder ---
file.copy(from = list.files(output_folder_shiny, "*.rda", full.names = T), 
          to = "shiny/data", 
          overwrite = T)

###########################################################################
## remove unused objects
rm(list=setdiff(ls(), keepers))
gc()
###########################################################################

################  Shiny Dashboard ######################
## read data
## read all trade data
# source("shiny/helper_funs.R")
# load("shiny/dtf_shiny.rda")
# load("shiny/dtf_shiny_full.rda")
# load("shiny/dtf_shiny_commodity_service_ex.rda") ## principle commodity from StatsNZ -- exports
# load("shiny/dtf_shiny_commodity_service_im.rda") ## principle commodity from StatsNZ -- imports
# load("shiny/dtf_shiny_country_gs.rda") ## commodity by country data
# load("shiny/dtf_country_group.rda") ## Country grouped by region
# load("shiny/list_country.rda") ## Country grouped by region
# load("shiny/list_snz_commodity.rda") ## Country grouped by region
# load("shiny/flag_table.rda")
# load("shiny/concord_country_iso_latlon_raw.rda")
# load("shiny/dtf_fdi_odi.rda")
# load("shiny/concord_uncomtrade_country.rda")

## generate maps etc for shiny
## 1. Two way trade map
source( 'analysis_code/trade_map_Gplot.R' )


## --- Shiny app deployment ---------------

###########
## shiny ##
###########
library(shiny)
#library(rsconnect, lib.loc = "P:/R/libraries/3.3.2-20190901/")
library(rsconnect)
library(packrat)
options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
detach("package:plyr", unload=TRUE) ## detach packages ----

## Relative path to the shiny app directory
shinydir = "shiny"

## suffix appended to "sector_report_" when deploying shiny app
## e.g. "test"  -> https://mbienz.shinyapps.io/trade_intelligence_test
##      "prod" -> https://mbienz.shinyapps.io/trade_intelligence_prod
## If deploysuffix = NULL, shiny app is not deployed
deploysuffix = "test"
# deploysuffix = "prod"
# deploysuffix = NULL

## ----- test locally ------
runApp('shiny')

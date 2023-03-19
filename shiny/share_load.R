### Share load should be sourced by both ui and server.
##  load library --------------------
library(rjson)
library(shinydashboard)
#library(shinydashboardPlus)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(lubridate)
library(stringr)
library(withr)
library(treemap)
library(DT)
library(shinyBS)
library(shinyjs)
library(WDI)
library(geosphere)
library(magrittr)
library(shinycssloaders)
options(spinner.color="#006272")
library(timevis)
#library(RCurl)
#library(jsonlite)
library(comtradr)
library(memoise)
library(networkD3)
library(promises)
library(future)
#plan(multiprocess)

### use memoise package for ct_search in comtradr ----
m_ct_search <- memoise::memoise(ct_search)

## load functions
source('helper_funs.R')

## load concordance
load('data/concord_hs24.rda')
load('data/concord_snz_eg.rda')
load('data/concord_snz_ig.rda')
load('data/concord_country.rda')
load('data/concord_country_group.rda')
load('data/concord_country_member.rda')
load('data/concord_country_iso_latlon_raw.rda')
load('data/flag_table.rda')
#load('data/concord_eu28.rda')
load('data/concord_eu27.rda')

## load data
load("data/dtf_shiny_full.rda")
#load("dtf_shiny.rda")
#source("groom_data_full_HS_levels_country.R")
load("data/dtf_shiny_commodity_service_ex.rda") ## principle commodity from StatsNZ -- exports
load("data/dtf_shiny_commodity_service_im.rda") ## principle commodity from StatsNZ -- imports
load("data/dtf_shiny_country_gs.rda") ## commodity by country data
load("data/dtf_country_group.rda") ## Country grouped by region
load("data/list_country.rda") ## Country grouped by region
load("data/list_snz_commodity_ex.rda") ## pre-defined commodity list form SNZ
load("data/list_snz_commodity_im.rda") ## pre-defined commodity list form SNZ
#load("list_snz_commodity.rda") ## pre-defined commodity list form SNZ
load("data/dtf_fdi_odi.rda") ## FDI and ODI data
load("data/dtf_in_out.rda") ## ppl movement visitor in and out
load("data/concord_uncomtrade_country.rda")

## setup global variables
maxYear <- tolower(paste0(dtf_shiny_full$Note[1],' ', max(dtf_shiny_full$Year)))
maxYear <- gsub('q1', 'March', maxYear)
maxYear <- gsub('q2', 'June', maxYear)
maxYear <- gsub('q3', 'September', maxYear)
maxYear <- gsub('q4', 'December', maxYear)

## Stats NZ's monthly update link -- update very month
#SNZ_link <- "https://www.stats.govt.nz/information-releases/?filters=Balance%20of%20payments%2CImports%20and%20exports%2CTourism"
SNZ_link <- "https://statisticsnz.shinyapps.io/trade_dashboard/"
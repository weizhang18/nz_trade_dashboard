####
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

## load data
load("data/list_snz_commodity_ex.rda") ## pre-defined commodity list form SNZ
load("data/list_snz_commodity_im.rda") ## pre-defined commodity list form SNZ
load("data/list_country.rda") ## Country grouped by region
load("data/dtf_shiny_commodity_service_ex.rda") ## principle commodity from StatsNZ -- exports

## setup global variables
maxYear <- tolower(paste0(dtf_shiny_commodity_service_ex$Note[1],' ', max(dtf_shiny_commodity_service_ex$Year)))
maxYear <- gsub('q1', 'March', maxYear)
maxYear <- gsub('q2', 'June', maxYear)
maxYear <- gsub('q3', 'September', maxYear)
maxYear <- gsub('q4', 'December', maxYear)

maxYear_lb <- gsub('year ended March', 'Mar', maxYear)
maxYear_lb <- gsub('year ended June', 'Jun', maxYear_lb )
maxYear_lb <- gsub('year ended September', 'Sep', maxYear_lb )
maxYear_lb <- gsub('year ended December', 'Dec', maxYear_lb )

maxYear_lb <- paste0( substr(maxYear_lb, 1, 3 ),
                      " ",
                      substr(maxYear_lb, nchar(maxYear_lb)-1, nchar(maxYear_lb) ))  


## load functions
source('helper_funs.R')
source("ga_Rfuncs.R")

#source('share_load.R')

## build ui.R -----------------------------------
## 1. header -------------------------------
header <- 
   dashboardHeader( title = HTML("New Zealand Trade Intelligence"), 
                    disable = FALSE, 
                    titleWidth  = 550,
                    dropdownMenuCustom( type = 'message',
                                        customSentence = customSentence,
                                  messageItem(
                                     from = "Raise issues on Github",#'Feedback and suggestions',
                                     message =  "",#paste0("TR_SharedMailbox@mbie.govt.nz" ),
                                     icon = icon("envelope"),
                                     href = "https://github.com/weizhang18/nz_trade_dashboard/issues"
                                  ),
                                  icon = icon('comment')
                                 ),
                    dropdownMenuCustom( type = 'message',
                                        customSentence = customSentence_share,
                                  icon = icon("share-alt"),
                                  messageItem(
                                     from = 'Twitter',
                                     message = "",
                                     icon = icon("twitter"),
                                     href = "https://twitter.com/intent/tweet?url=http%3A%2F%2Fwzhanganalytics.shinyapps.io/nztradeintelligence&text=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                  ),
                                  messageItem(
                                     from = 'Facebook',
                                     message = "",
                                     icon = icon("facebook"),
                                     href = "https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Fwzhanganalytics.shinyapps.io/nztradeintelligence"
                                  ),
                                  messageItem(
                                     from = 'Google+',
                                     message = "",
                                     icon = icon("google-plus"),
                                     href = "https://plus.google.com/share?url=http%3A%2F%2Fwzhanganalytics.shinyapps.io/nztradeintelligence"
                                  ),
                                  messageItem(
                                     from = 'Sina Weibo',
                                     message = "",
                                     icon = icon("weibo"),
                                     href = "http://service.weibo.com/share/share.php?url=http://example.com&appkey=&title=New%20Zealand%20Trade%20Intelligence%20Dashboard%20http%3A%2F%2Fwzhanganalytics.shinyapps.io/nztradeintelligence&pic=&ralateUid=&language=zh_cn"
                                  ),
                                  messageItem(
                                     from = 'Pinterest',
                                     message = "",
                                     icon = icon("pinterest-p"),
                                     href = "http://pinterest.com/pin/create/button/?url=http%3A%2F%2Fwzhanganalytics.shinyapps.io/nztradeintelligence&media=&description=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                  ),
                                  messageItem(
                                     from = 'LinkedIn',
                                     message = "",
                                     icon = icon("linkedin"),
                                     href = "http://www.linkedin.com/shareArticle?mini=true&url=http%3A%2F%2Fwzhanganalytics.shinyapps.io/nztradeintelligence&title=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                  ),
                                  messageItem(
                                     from = 'Tumblr',
                                     message = "",
                                     icon = icon("tumblr"),
                                     href = "http://www.tumblr.com/share?v=3&u=http%3A%2F%2Fwzhanganalytics.shinyapps.io/nztradeintelligence&t=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                  )
                                  )
                    
   )

header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(id = "dblogo",
                                             href='https://nzprimarysectortrade.wordpress.com/', #http://www.mbie.govt.nz
                                             tags$img(src='wzLogo/wz-logo_svg_black.svg',
                                                      width="240", height="40", padding = "0, 0, 0, 0"),  #MBIELogo/logo_reserve_small_corp1.png
                                             target = '_blank') #,height='67',width='228.6', align = 'left'
# header$children[[3]]$children[[4]]  <- header$children[[3]]$children[[3]]
# header$children[[3]]$children[[3]] <- HTML('<span class="logo" style = "width:310px" > New Zealand Trade Intelligence</span>')
# header$children[[2]]$children[[1]] <- NULL
# header$children[[2]]$children[[1]] <- tags$a(href='http://www.mbie.govt.nz',
#                                              tags$img(src='logo_reserve_small.png'),
#                                              target = '_blank') #,height='67',width='228.6', align = 'left'


## 2. siderbar ------------------------------
siderbar <- 
   dashboardSidebar( 
      width = 200,
      sidebarMenu(
         id = 'sidebar',
         style = "position: relative; overflow: visible;",
         #style = "position: relative; overflow: visible; overflow-y:scroll",
         #style = 'height: 90vh; overflow-y: auto;',
         ## 1st tab show the Main dashboard -----------
         menuItem( "Main Dashboard", tabName = 'dashboard', icon = icon('dashboard'),
                   badgeLabel = maxYear_lb, badgeColor = "green" ),
         
         ## add conditional panel to show more
         # conditionalPanel( "input.sidebar === 'dashboard'",
         #                   actionButton("btn_show_more",
         #                                paste0(' Show more details'),
         #                                icon = icon('chevron-circle-down'),
         #                                style='padding-top:0px; padding-bottom:0px;padding-left:3px;padding-right:3px; '
         #                                ) 
         #                   ),
         
         ## 2nd Second tab shows the country/region level tab --------------
         menuItem("Market Intelligence", tabName = 'country_intel', icon = icon('globe') ),
         div( id = 'sidebar_cr',
              conditionalPanel("input.sidebar === 'country_intel'",
                               selectizeInput("select_country",
                                              "Select or search for one or multiple markets", 
                                              choices =  list_country, 
                                              selected = NULL,  width = "200px",
                                              multiple = T), #,
                               #actionButton('btn_country','Submit')
                               
                               ## action button to build report
                               actionButton('btn_build_country_report', 
                                            paste0('Build Report'),
                                            icon = icon('wrench')),
                               
                               ## reset side bar selectoin
                               actionButton('btn_reset_cr',
                                            'Reset',
                                            icon = icon('refresh') )
                               
              )),
         
         ## 3rd tab shows commodity intel ----------
         menuItem( "Commodity Intelligence", tabName = "commodity_intel", icon = icon('barcode'), startExpanded = F,
                   menuSubItem('Exports', tabName = "ci_exports", icon = icon('export', lib = 'glyphicon')),
                   menuSubItem('Imports', tabName = "ci_imports", icon = icon('import', lib = 'glyphicon')),
                   menuSubItem('Intelligence by HS code', tabName = "ci_intel_by_hs", icon = icon("bolt") )
                   ),
         
         ## Show panel only when Commodity intelligence sidebar is selected
         useShinyjs(),
         
         ## give sidebar inputs a id so that it can be manipulated by css
         div( id = 'sidebar_ci_exports',
              conditionalPanel("input.sidebar === 'ci_exports'",
                               
                               ## radio buttons to ask user to choose prebuilt commodity groups or build their owns
                               radioButtons("rbtn_prebuilt_diy_ex",
                                            tags$p("Step 1:",tags$br(),"Select commodities:"), 
                                            choices = c("Pre-defined", 'Self-defined'),
                                            selected = 'Pre-defined',
                                            inline = F,
                                            width = "200px"),
                               
                               ## conditional on select pre-built ones 
                               conditionalPanel( "input.rbtn_prebuilt_diy_ex == 'Pre-defined'",
                                                 selectizeInput("select_comodity_ex",
                                                                tags$p("Step 2:",tags$br(),"Select or search commodities"), 
                                                                choices =  list_snz_commodity_ex, 
                                                                selected = NULL,  width = "200px",
                                                                multiple = T)
                               ),
                               ## conditonal on build your own report
                               conditionalPanel( "input.rbtn_prebuilt_diy_ex == 'Self-defined'",
                                                 fileInput("file_comodity_ex",
                                                           tags$p("Step 2:",tags$br(),"Upload self-defined HS codes groupings"), 
                                                           accept = c(".csv"),  
                                                           width = "200px",
                                                           multiple = F,
                                                           buttonLabel = 'Upload CSV'
                                                 )
                               ),
                               ## action button to build report
                               actionButton('btn_build_commodity_report_ex', 
                                            paste0('Build Report'),
                                            icon = icon('wrench')),
                               
                               ## reset side bar selectoin
                               actionButton('btn_reset_ci_ex',
                                            'Reset',
                                            icon = icon('refresh') )
                               
              )),
         
         ## Show panel only when Commodity intelligence sidebar is selected
         div( id = 'sidebar_ci_imports',
              conditionalPanel("input.sidebar === 'ci_imports'",
                               
                               ## radio buttons to ask user to choose prebuilt commodity groups or build their owns
                               radioButtons("rbtn_prebuilt_diy_im",
                                            tags$p("Step 1:",tags$br(),"Select commodities:"), 
                                            choices = c("Pre-defined", 'Self-defined'),
                                            selected = 'Pre-defined',
                                            inline = F,
                                            width = "200px"),
                               
                               ## conditional on select pre-built ones 
                               conditionalPanel( "input.rbtn_prebuilt_diy_im == 'Pre-defined'",
                                                 selectizeInput("select_comodity_im",
                                                                tags$p("Step 2:",tags$br(),"Select or search commodities"), 
                                                                choices =  list_snz_commodity_im, 
                                                                selected = NULL,  width = "200px",
                                                                multiple = T)
                               ),
                               ## conditonal on build your own report
                               conditionalPanel( "input.rbtn_prebuilt_diy_im == 'Self-defined'",
                                                 fileInput("file_comodity_im",
                                                           tags$p("Step 2:",tags$br(),"Upload self-defined HS codes groupings"), 
                                                           accept = c(".csv"),  
                                                           width = "200px",
                                                           multiple = F,
                                                           buttonLabel = 'Upload CSV'
                                                 )
                               ),
                               
                               ## action button to build report
                               actionButton('btn_build_commodity_report_im', 
                                            paste0('Build Report'),
                                            icon = icon('wrench')),
                               
                               ## reset side bar selectoin
                               actionButton('btn_reset_ci_im',
                                            'Reset',
                                            icon = icon('refresh') )
              )),
         
         ## Show panel only when Commodity intelligence sidebar is selected
         div( id = 'sidebar_ci_intel_by_hs',
              conditionalPanel("input.sidebar === 'ci_intel_by_hs'",
                               ## radio buttons to ask user to choose prebuilt commodity groups or build their owns
                               radioButtons("rbtn_intel_by_hs",
                                            tags$p("Intelligence reported on:"), 
                                            choices = c("Exports", 'Imports'),
                                            selected = 'Exports',
                                            inline = F,
                                            width = "200px")
              )),
         
         ## 4th tab HS finder -------------------------
         #menuItem("HS code finder", tabName = 'hs_finder', icon = icon('search') ),
         
         ## 5th tab Data source, definition , i.e., help ---------------
         menuItem( "FAQs", tabName = 'help', icon = icon('question-circle') ) ,
         
         ## 6th tab monthly update ----------------------
         menuItem( "Stats NZ Releases", tabName = 'monthly_update', icon = icon('bell'),
                   badgeLabel = "new", badgeColor = "green" )
         )
   )

## 3. body --------------------------------
body <- dashboardBody( 
   ## 3.0. CSS styles in header ----------------------------
   tags$head(
      # ## JS codes
      # tags$script(src = "fixedElement.js" ),
      # tags$style(HTML(".scroller_anchor{height:0px; margin:0; padding:0;}; 
      #                  .scroller{background: white; 
      #                   border: 1px solid #CCC; 
      #                   margin:0 0 10px; 
      #                   z-index:100; 
      #                   height:50px; 
      #                   font-size:18px; 
      #                   font-weight:bold; 
      #                   text-align:center; 
      #                  width:500px;}")),
      
      #tags$script(src = "world.js" ),
      tags$script("document.title = 'New Zealand Trade Intelligence Dashboard'"),
      
      ## add google analytics
      #includeScript("ga_tracker.js"),
      #ga_common(),
      #includeScript("gtm.js"),  ## add Google Tag manager suggested by the Web team
      
      ### include Styles 
      includeCSS("www/trade_intel_style.css") 
      ),

   
   ## add things to body
   #tags$noscript(
   #         tags$iframe( src = "https://www.googletagmanager.com/ns.html?id=GTM-MNWF7H5",
   #                      height = "0",
   #                      width = "0",
   #                      style = "display:none;visibility:hidden")
   # ),
         
   ## 3.1 Dashboard body --------------
   tabItems(
      ## 3.1 Main dashboard ----------------------------------------------------------
      tabItem( tabName = 'dashboard',
               ## contents for the dashboard tab
               div(id = 'main_wait_message',
                   h1('Please note the initial load may take up to 10 seconds.',
                      style = "color:darkblue;" , align = "center" ) ,
                    textOutput("Loading") %>% withSpinner(type=4) 
                   #tags$hr()
                   ),
               
               ## 0.0 Decommission message ----
               #div( id = 'attention_messsage',
               #     style = 'border: 3px solid rgba(255, 0, 0, .6); border-radius: 15px; padding: 10px;',
               #     attention_message() ),
               
               # 1.1 Export/import board ---------------------------
               #div(class = 'scroller_anchor'),
               #div(class = 'scroller', ) ,
               
               #div( id = "main_body_open_loading",
               #     textOutput("Loading") %>% withSpinner(type=4) 
               #),
               
               shinyjs::hidden(
                  div(
                  id = "main_body_info",
                  h1(paste0("New Zealand trade intelligence for the ", maxYear)) ,
                  fluidRow(
                     valueBoxOutput("ExTotBox"), #%>% withSpinner(type=4),
                     valueBoxOutput("ImTotBox"),
                     valueBoxOutput("BlTotBox")
                  ),
                  
                  h2(paste0("Goods")),
                  fluidRow(
                     valueBoxOutput("ExGBox") ,
                     valueBoxOutput("ImGBox") ,
                     valueBoxOutput("BlGBox")
                  ),
                  
                  h2(paste0("Services")),
                  fluidRow(
                     valueBoxOutput("ExSBox") ,
                     valueBoxOutput("ImSBox") ,
                     valueBoxOutput("BlSBox")
                  ) ,
                  
                  ## 1.2 Time serise plot ----------------------------------------
                  h2(paste0("New Zealand trade over the past 20 years")),
                  fluidRow( column( width = 6,h4("Goods and services trade", align = 'center'), highchartOutput('IEGSLineHc') ),
                            column( width = 6,h4("Trade balance", align = 'center'), highchartOutput('GSTotalBalanceLineHc') )
                  ),
                  
                  
                  
                  
                  ## 1.3 Table shows growth rate ---------------------------------
                  h2(paste0("Short, medium, and long term growth")),
                  p("Compound annual growth rate (CAGR) for the past 1, 5, 10 and 20 years") ,
                  #fluidRow( h2(paste0("Short, medium, and long term growth")),
                  #          p("Compound annual growth rate (CAGR) for the past 1, 5, 10 and 20 years") ),
                  fluidRow( dataTableOutput('GrowthTab')  ),
                  
                  div( id = 'message_to_show_more',
                       tags$hr(),
                       tags$h3( "Click on the 'Show more details' button to display addtional information on free trade agreements, and imports/exports by commodities and markets." ),
                       actionButton("btn_show_more",
                                    div(icon('chevron-circle-right'),icon('chevron-circle-down'),paste0(' Show more details')) #,
                                    #icon = icon('chevron-circle-down')
                       ) 
                       #actionButton("btn_show_more",
                       #            paste0(' Show more details'),
                       #            icon = icon('chevron-circle-down') #,
                       #style='padding-top:3px; padding-bottom:3px;padding-left:5px;padding-right:5px;font-size:150%; color: white !important;background-color:#006272; border:none; border-radius: 5px; box-shadow: 3px 3px 8px 0 #404040;'
                       #) 
                  ),
                  
                  div( id = "show_more_detail" ) 
               )),
               
            
               shinyjs::hidden( div( id = "load_more_message",
                                     tags$hr(),
                                     tags$h1("Loading...", align = "center")  )
                               )

               ),
      
      ## 3.2.1 Export/import commodities/services intelligence ------------------------
      tabItem( tabName = 'ci_exports',
               ## 2.1 Help text first -------------- 
               div(id = 'ci_howto_ex',
                   howto_ci() ),
               
               ## 3... wait message ------
               hidden(
                  div( id = 'wait_message_ci_ex',
                       h2( "I am preparing the report now and only for you ....." ),
                       textOutput("Loading_ci_ex") %>% withSpinner(type=4)  
                  )),
               
               ## divs for pre-defined commodity groups ----------------- 
               tags$div(id = 'body_ex') ,
               tags$div(id = 'body_growth_ex') ,
               shinyjs::hidden( div( id = "body_ci_market_loading_message",
                                     tags$hr(),
                                     tags$h1("Generating reports...", align = "center") ,
                                     textOutput("Loading_ci_ex_intl") %>% withSpinner(type=4)  )
               ),
               tags$div(id = 'body_ci_markets_ex'), 
               
               
               ## divs for self-defined commodity groups ----------------- 
               tags$div(id = 'body_ex_self_defined') ,
               tags$div(id = 'body_growth_ex_self_defined') ,
               shinyjs::hidden( div( id = "body_ci_market_loading_message_self_define",
                                     tags$hr(),
                                     tags$h1("Generating reports...", align = "center"),
                                     textOutput("Loading_ci_ex_intl_self_defined") %>% withSpinner(type=4)  )
               ),
               tags$div(id = 'body_ci_markets_ex_self_defined')
               
            ),
      
      ## 3.2.2 Export/import commodities/services intelligence ------------------------
      tabItem( tabName = 'ci_imports',
               ## 3.1 Help text first ---------------------
               div(id = 'ci_howto_im',
                   howto_ci() ),
               
               ## 3... wait message ------
               hidden(
                  div( id = 'wait_message_ci_im',
                       h2( "I am preparing the report now and only for you ....." ),
                       textOutput("Loading_ci_im") %>% withSpinner(type=4)
                  )),
               
               ## 3.1 div for pre-defined HS group reports ----------------------
               tags$div(id = 'body_im') ,
               tags$div(id = 'body_growth_im') ,
               tags$div(id = 'body_ci_markets_im'),
               
               ## 3.x div for self-defined HS group reports ----------------------
               tags$div(id = 'body_im_self_defined') ,
               tags$div(id = 'body_growth_im_self_defined') ,
               tags$div(id = 'body_ci_markets_im_self_defined')
      ),
             
             
      ## 3.2.3 Quick Intel by HS codes ---------------
      tabItem( tabName = 'ci_intel_by_hs',
                tags$div( id = 'ci_intel_by_hs_hstable' ,
                     fluidRow( h1( "Quick intelligence on export/import by using HS codes" ),
                               h3( "How to:"),
                               howto_hs_finder(),
                               dataTableOutput("HSCodeTable")
                     )
                )#,
               ,div( id = 'clear_table',
                     #tags$hr(),
                     #tags$h3( "Click on the 'Show more details' button to display addtional information on free trade agreements, and imports/exports by commodities and markets." ),
                     actionButton("action_bnt_ClearTable",
                                  #paste0(' Clear all selections'),
                                  #icon = icon('refresh'),
                                  div(icon('rotate-right'), icon('arrows-rotate') ,paste0(' Clear all selections')) #,
                                  #style='padding-top:3px; padding-bottom:3px;padding-left:5px;padding-right:5px;font-size:120% '
                     ) 
               )
               ,shinyjs::hidden( div( id = "ci_intel_hs_loading_message",
                                     tags$hr(),
                                     tags$h1("Generating reports...", align = "center"),
                                     textOutput("Loading_ci_intel_hs") %>% withSpinner(type=4)  )
               )
               ,tags$div( id = "ci_intel_by_hs_toadd" )
               ,shinyjs::hidden( div( id = "ci_intel_hs_loading_message_intl",
                                      tags$hr(),
                                      tags$h1("Generating reports...", align = "center"),
                                      textOutput("Loading_ci_intel_hs_intl") %>% withSpinner(type=4)   )
               )
               ,tags$div( id = "ci_intel_by_hs_toadd_intl" )
      ),
      
      ## 3.3 country intellgence -----------------------------------------------------
      tabItem( tabName = 'country_intel',
               ## 3.3.1 Help text first -------------- 
               div(id = 'country_howto',
                   howto_country() ) ,
               
               ## 3... wait message ------
               hidden(
                  div( id = 'wait_message_country_intel',
                    h2( "I am preparing the report now and only for you ....." ),
                    textOutput("Loading_country_intel") %>% withSpinner(type=4)  
                    )),
               
               ## 3... div to holder created UIs ------
               tags$div( id = 'country_name' ),
               tags$div( id = 'country_info' ),
               tags$div( id = 'country_trade_summary' ),
               tags$div( id = 'country_appendix' ) 
      ),
      
      ## 3.4 HS code finder ------------------------------
      # tabItem( tabName = 'hs_finder',
      #          div( id = 'hs_code_finder_table' ,
      #               fluidRow( h1( "Level 2, 4 and 6 HS code table" ),
      #                         h3( "How to:"),
      #                         howto_hs_finder(),
      #                         dataTableOutput("HSCodeTable")
      #                         ) 
      #               )
      #          ),
      
      ## 3.5 Help and info -------------------------------
      tabItem( tabName = 'help',
               ## 3.5.1 Data sources ---------------
               div( id = 'help_contact',
                    contact() ),
               
               div( id = 'help_data_source',
                    data_source() ),
               
               div( id = 'when_to_update',
                    when_update() ), 
               
               div( id = 'help_hs_code',
                    hs_code_explain() ),
               
               div( id = 'help_trade_term',
                    trade_terms() ),
               
               div( id = 'help_confidential_data',
                    confidential_trade_data() ) #,
               
               #div( id = 'help_urgent_update',
               #     urgent_updates() )
               ) ,
      
      ## 3.6 Monthly update from Stats NZ --------------
      tabItem( tabName = 'monthly_update',
               div( id = 'monthly_update',
                    fluidRow( htmlOutput('MonthlyUpdate') )
               ))
   )
)



## put UI together --------------------
ui <- 
   dashboardPage(header, siderbar, body )

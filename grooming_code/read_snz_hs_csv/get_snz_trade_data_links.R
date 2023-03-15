
## stats nz's new web page for detailed data
Base_URL <- "https://www.stats.govt.nz/large-datasets/csv-files-for-download/overseas-merchandise-trade-datasets#datasets-for-imports"
document <- read_html(Base_URL)

SNZ_URL <- "https://www.stats.govt.nz"

## SNZ made the webpage unable to scrap EASILY --
page_contents <- 
   document %>% 
   html_elements("#pageViewData") %>% 
   html_attr("data-value")

## manipulate a bit 
page_contents <- 
gsub("[\"]|[\\]", "", page_contents)

## split the contents
contents_splited <- unlist(strsplit(page_contents, split = "href="))

contents_csv_files <- 
   grep( "/assets/Large-datasets/OMT-datasets/*", contents_splited, value = T)

contents_zip_files <- 
   grep( "https://www3.stats.govt.nz/HS10_by_Country*", contents_splited, value = T)

## to extract links from them ------
### do .csv files first ------
links_csv <- 
   unlist(
      lapply(contents_csv_files, 
             function( i_file ){
                pos <- unlist(gregexpr( "[>]", i_file ) )[1] - 1
                link <- substr( i_file , 1, pos )
                return(link)
             })
   )
   
links_csv <- paste0(SNZ_URL, links_csv)
   
### do .zip seconc ----
links_zip <-
   unlist(
      lapply(contents_zip_files, 
             function( i_file ){
                pos <- unlist(gregexpr( "[>]", i_file ) )[1] - 1
                link <- substr( i_file , 1, pos )
                return(link)
             })
   )


##
save(links_csv, file = "data_intermediate/links_csv.rda")
save(links_zip, file = "data_intermediate/links_zip.rda")


############################
rm( list = setdiff( ls(), keepers) )
gc()
###############
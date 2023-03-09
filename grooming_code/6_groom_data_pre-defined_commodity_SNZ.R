## group pre-defined commodity names by StatsNZ
list_snz_commodity_ex <- NULL

tmp_gs <- unique( dtf_shiny_commodity_service_ex$Type_gs )

for( i_gs in tmp_gs ){
   list_snz_commodity_ex[[i_gs]] <- 
      unique( dtf_shiny_commodity_service_ex$SNZ_commodity[dtf_shiny_commodity_service_ex$Type_gs == i_gs] )
}

save( list_snz_commodity_ex, file = 'shiny/list_snz_commodity_ex.rda' ) 


## group pre-defined commodity names by StatsNZ
list_snz_commodity_im <- NULL

tmp_gs <- unique( dtf_shiny_commodity_service_im$Type_gs )

for( i_gs in tmp_gs ){
   list_snz_commodity_im[[i_gs]] <- 
      unique( dtf_shiny_commodity_service_im$SNZ_commodity[dtf_shiny_commodity_service_im$Type_gs == i_gs] )
}

save( list_snz_commodity_im, file = 'shiny/list_snz_commodity_im.rda' ) 
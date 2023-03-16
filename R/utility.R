###########################################################################
## Project description:  
##     Export and import dashboard
##   Project root folder:  
##      P:\OTSP\Export_Import_Dashboard
##	Script:	
##		./Export_Import_Dashboard/R/utility.R
## Objective:	
##    utility functions
##	History:
##    30/11/17 by Wei Zhang
###########################################################################



###########################################################################
## Table of Contents ------------------------------------------------------
###########################################################################
YearEnd <- 
   function (TimePeriod, YrEndMthNum = 12) 
   {
      Year <- lubridate::year(TimePeriod)
      Year <- ifelse(lubridate::month(TimePeriod) > YrEndMthNum, 
                     Year + 1, Year)
      return(Year)
   }

rename.levels <- 
   function (x, orig, new)
   {
      if (!is.factor(x))
         stop("x must be a factor")
      if (length(orig) != length(new))
         stop("Number of new labels must equal number of old labels.")
      for (i in 1:length(orig)) {
         levels(x)[levels(x) == orig[i]] <- new[i]
      }
      return(x)
   }


CAGR <- 
   function (ratio, period, digits = 1)
{
   round((exp(log(ratio)/period) - 1) * 100, digits)
   }


## function to fill out missing cells
fill_missing_cell <- 
   function( vec_all ) {
      vec <- as.character(vec_all)
      tmp_unique <- na.omit(unique( vec ))
      tmp_unique <- tmp_unique[ tmp_unique != " " ]
      
      tmp_pos <- which(  vec %in% tmp_unique )
      
      for( i in 1:length(tmp_pos) ){
         if(i==1){
            vec[2:(tmp_pos[i+1]-1)] <- vec[tmp_pos[i]]
         }
         if(i>1&i<length(tmp_pos)){
            vec[ (tmp_pos[i]+1):(tmp_pos[i+1]-1) ] <- vec[tmp_pos[i]]
         }
         if(i==length(tmp_pos)){
            vec[ (tmp_pos[i]+1):length(vec) ] <- vec[tmp_pos[i]]
         }
      }
      
      return(vec)
   }

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## first capital letter
capFirst <- function(s) {
   paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

## unrowwise a df
unrowwise <- function(x) {
   if( any(class(x) == 'rowwise_df') ){
      class(x) <- setdiff( class(x), 'rowwise_df' )
   }
   x
}

## Remove leading and trailing spaces -------------------------------------
rm_space <- function(x){
   gsub("^\\s+|\\s+$", "", x)
}


## excel_col_num ----------------------------------------------------------
excel_col_num <- 
   function(col_label){
      col_num <- 0; len <- nchar(col_label)
      while(len >= 1){
         col_num <- 
            col_num * 26 + (strtoi(charToRaw(substr(col_label, 1, 1)), 16L) - strtoi(charToRaw("A"), 16L) + 1)
         len <- nchar(col_label <- substr(col_label, 2, len))
      }
      return(col_num)
   }
## Examples:
# excel_col_num("A")
# excel_col_num("J")
# excel_col_num("AA")
# excel_col_num("AAA")
# (1*26 +1)*26 +1
## excel_col_num ----------------------------------------------------------






## convert "(blank)" in cells to na value ---------------------------------
blank2na <- function(x){rename.levels(factor(x), orig = "(blank)", new = NA)}
## Examples:
## convert "(blank)" in cells to na value ---------------------------------







## remove data frame levels -----------------------------------------------
remove.levels <- 
   function (dtf) {
      as.data.frame(apply(dtf, 2, as.vector), stringsAsFactors = FALSE)
   }
## remove data frame levels -----------------------------------------------



## reset data frame levels ------------------------------------------------
reset.levels <- 
   function (dtf) {
      as.data.frame(apply(dtf,2,function(x) {if(is.factor(x)) factor(x) else x}))
   }
## reset data frame levels ------------------------------------------------


## fix date format after read from xlsx -----------------------------------
fix.excel.date <- 
   function(date) {
      return(as.Date(as.integer(as.vector(blank2na(date)))-25569, origin = "1970-01-01"))
   }
## Examples:
# fix.excel.date(42034)
## fix date format after read from xlsx -----------------------------------


#### This is for plot rworldmap without the border color for pies
######################################################3
rwmNewMapPlot <- function(mapToPlot=getMap(),
                          oceanCol=NA,
                          mapRegion="world",
                          xlim=NA,                  
                          ylim=NA,
                          aspect=1){
   
   #browser()
   
   ## setting map extents if a mapRegion has been specified
   if (mapRegion != "world"){
      dFwesn <- setMapExtents(mapRegion)
      xlim <- c(dFwesn$we, dFwesn$ea)
      ylim <- c(dFwesn$so, dFwesn$no)
   }
   
   #getting xlim & ylim from bbox of map if they haven't been specified
   if (length(xlim)<2) xlim <- bbox(mapToPlot)['x',]  
   if (length(ylim)<2) ylim <- bbox(mapToPlot)['y',]
   
   
   plot.new()
   
   #replicate behaviour of plot.Spatial in the sp package regarding aspect
   #if the map is unprojected the aspect is set based upon the mean y coord
   #only if region not 'world'
   if (aspect == 'variable' & mapRegion != "world")
      aspect <- ifelse(is.na(proj4string(mapToPlot)) || is.projected(mapToPlot),
                       1, 1/cos((mean(ylim) * pi)/180))
   
   plot.window(xlim=xlim,ylim=ylim,asp=aspect)#,xaxs='i',yaxs='i')#,bg=oceanCol,xpd=NA)
   
   #rect(xlim[1],ylim[1],xlim[2],ylim[2],col=oceanCol,border=oceanCol)
   #making the rectangle as big as the whole map should ensure it fills
   rect(mapToPlot@bbox[1],mapToPlot@bbox[2],mapToPlot@bbox[3],mapToPlot@bbox[4],col=oceanCol,border=oceanCol)
   
   #26/3/13 returning xlim & ylim so it can be used elsewhere
   lims <- data.frame(xlim,ylim)
   invisible(lims)
   
}


###########################################################################################
mapPies2 <- 
   function (dF, nameX = "LON", nameY = "LAT", nameZs = c(names(dF)[3], 
                                                          names(dF)[4]), zColours = c(1:length(nameZs)), ratio = 1, 
             addCatLegend = TRUE, addSizeLegend = TRUE, symbolSize = 1, 
             maxZVal = NA, xlim = NA, ylim = NA, mapRegion = "world", 
             borderCol = "grey", oceanCol = NA, landCol = NA, add = FALSE, 
             main = "", lwd = 0.5, pieBorder = NA, ...) 
   {
      
      
      functionName <- as.character(sys.call()[[1]])
      if (mapRegion == "data") {
         xlim <- c(min(dF[, nameX], na.rm = TRUE), max(dF[, nameX], 
                                                       na.rm = TRUE))
         ylim <- c(min(dF[, nameY], na.rm = TRUE), max(dF[, nameY], 
                                                       na.rm = TRUE))
      }
      if (class(dF) == "SpatialPolygonsDataFrame") {
         centroidCoords <- coordinates(dF)
         dF[["nameX"]] <- centroidCoords[, 1]
         dF[["nameY"]] <- centroidCoords[, 2]
         nameX <- "nameX"
         nameY <- "nameY"
         if (!add) {
            lims <- rwmNewMapPlot(mapToPlot = dF, oceanCol = oceanCol, 
                                  mapRegion = mapRegion, xlim = xlim, ylim = ylim)
            xlim <- lims$xlim
            ylim <- lims$ylim
            
            detach('package:graphics')
            plot(dF, add = TRUE, border = borderCol, col = landCol, 
                 main = main, lwd = lwd)
            library('graphics')   
            
         }
         dF <- dF@data
      }
      else if (!add) {
         lims <- rwmNewMapPlot(mapToPlot = getMap(), oceanCol = oceanCol, 
                               mapRegion = mapRegion, xlim = xlim, ylim = ylim)
         xlim <- lims$xlim
         ylim <- lims$ylim
         
         detach('package:graphics')
         plot(getMap(), add = TRUE, border = borderCol, col = landCol, 
              main = main, lwd = lwd)
         library('graphics')
         
      }
      maxSumValues <- 0
      for (locationNum in 1:length(dF[, nameZs[1]])) {
         sumValues <- sum(dF[locationNum, nameZs], na.rm = TRUE)
         if (sumValues > maxSumValues) 
            maxSumValues <- sumValues
      }
      symbolMaxSize <- 0.02 * max(xlim[2] - xlim[1], (ylim[2] - 
                                                         ylim[1]) * ratio)
      symbolScale <- symbolMaxSize/sqrt(maxSumValues)
      cat("symbolMaxSize=", symbolMaxSize, " maxSumValues=", maxSumValues, 
          " symbolScale=", symbolScale, "\n")
      
      
      
      for (locationNum in 1:length(dF[, nameZs[1]])) {
         sliceValues <- as.numeric(dF[locationNum, nameZs])
         if (sum(sliceValues, na.rm = TRUE) == 0) 
            next
         cumulatProps <- c(0, cumsum(sliceValues)/sum(sliceValues, 
                                                      na.rm = TRUE))
         pointsInCircle = 360
         radius <- sqrt(sum(sliceValues, na.rm = TRUE)) * symbolScale
         radius <- radius * symbolSize
         for (sliceNum in 1:length(sliceValues)) {
            n <- max(2, floor((pointsInCircle * (cumulatProps[sliceNum + 
                                                                 1] - cumulatProps[sliceNum]))))
            P <- list(x = ratio * radius * cos(2 * pi * seq(cumulatProps[sliceNum], 
                                                            cumulatProps[sliceNum + 1], length = n)) + dF[locationNum, 
                                                                                                          nameX], y = radius * sin(2 * pi * seq(cumulatProps[sliceNum], 
                                                                                                                                                cumulatProps[sliceNum + 1], length = n)) + dF[locationNum, 
                                                                                                                                                                                              nameY])
            
            polygon(c(P$x, dF[locationNum, nameX]), c(P$y, dF[locationNum, 
                                                              nameY]), col = zColours[sliceNum],
                    border = pieBorder )
            
         }
      }
      
      
      if (addCatLegend) 
         legend("bottomleft", legend = nameZs, fill = zColours, 
                cex = 0.7, bg = "white")
      radius <- symbolMaxSize * symbolSize
      plotExtents <- par("usr")
      plotS <- plotExtents[3]
      plotW <- plotExtents[1]
      plotN <- plotExtents[4]
      plotE <- plotExtents[2]
      centreE <- plotW + radius * 2 * ratio
      centreN <- plotN - radius * 2
      t <- seq(0, 2 * pi, length = 100)
      P <- list(x = ratio * radius * cos(t) + centreE, y = radius * 
                   sin(t) + centreN)
      str(P)
   }


#### function
FormatDollars <- 
   function (x, endmark = "", ...) {
      x <- paste0("$", format(round(x, ...), big.mark = ","), endmark)
      x <- gsub(" ", "", x)
      return(x)
}












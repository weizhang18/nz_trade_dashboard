# below is for use when outputing cmyk colourspace versions
font <- "Calibri"
ref_font <- "Calibri Light"
light_font <- "Calibri Light"


update_geom_defaults("text", list(family = font))
update_geom_defaults("text",   list(family ="Calibri"))


## update to theme_nothing
theme_nothing <- function (base_size = 12, base_family = font) 
{
    theme_bw(base_size = base_size, base_family = base_family) %+replace% 
        theme(rect              = element_blank(),
              line              = element_blank(), 
              axis.ticks.length = unit(0.01, "cm"),
              # axis.ticks.margin = unit(0, "lines"),   ## depreciated
              axis.text.y         = element_blank(), 
              axis.text.x = element_text( size = 20, face = 'bold' ),
              axis.ticks.y        = element_blank() ,
              #axis.title        = element_blank()
              #panel.margin = unit(2, "lines"),
              plot.margin = unit(c(0,1,0,-1) ,'line')
              )
}


theme_nothing_flip <- function (base_size = 12, base_family = font) 
{
   theme_bw(base_size = base_size, base_family = base_family) %+replace% 
      theme(rect              = element_blank(),
            line              = element_blank(), 
            axis.ticks.length = unit(0.01, "cm"),
            # axis.ticks.margin = unit(0, "lines"),   ## depreciated
            axis.text.x         = element_blank(), 
            axis.text.y = element_text( size = 28 ),
            axis.ticks.y        = element_blank() ,
            #axis.title        = element_blank()
            #panel.margin = unit(2, "lines"),
            plot.margin = unit(c(1,1,0,0) ,'line')
      )
}


## update to theme_nothing
theme_nothing_map <- function (base_size = 12, base_family = font) 
{
   theme_bw(base_size = base_size, base_family = base_family) %+replace% 
      theme(rect              = element_blank(),
            line              = element_blank(), 
            axis.ticks.length = unit(0.01, "cm"),
            # axis.ticks.margin = unit(0, "lines"),   ## depreciated
            axis.text.y         = element_blank(), 
            axis.text.x = element_text( size = 28 ),
            axis.ticks.y        = element_blank() ,
            #axis.title        = element_blank()
            #panel.margin = unit(2, "lines"),
            plot.margin = unit(c(-2,-1,0,-2) ,'line')
      )
}

## update to theme_nothing for wraps
theme_nothing_wrap <- function (base_size = 12, base_family = font) 
{
   theme_bw(base_size = base_size, base_family = base_family) %+replace% 
      theme(rect              = element_blank(),
            line              = element_blank(), 
            axis.ticks.length = unit(0.01, "cm"),
            # axis.ticks.margin = unit(0, "lines"),   ## depreciated
            axis.text.y         = element_blank(), 
            axis.text.x = element_text( size = 20 ),
            axis.ticks.y        = element_blank() ,
            #axis.title        = element_blank()
            panel.margin = unit(2, "lines"),
            plot.margin = unit(c(1,1,0,1) ,'line'),
            strip.text = element_text(size = rel(2.0), hjust = 0.5) 
      )
}

## update to theme_nothing for wraps
theme_nothing_wrap_line <- function (base_size = 12, base_family = font) 
{
   theme_bw(base_size = base_size, base_family = base_family) %+replace% 
      theme(rect              = element_blank(),
            #line              = element_blank(), 
            axis.ticks.length = unit(0.01, "cm"),
            # axis.ticks.margin = unit(0, "lines"),   ## depreciated
            #axis.text.y         = element_blank(), 
            axis.text.y = element_text( size = 15 ),
            axis.text.x = element_text( size = 15 ),
            #axis.ticks.y        = element_blank() ,
            #axis.title        = element_blank()
            panel.margin = unit(2, "lines"),
            plot.margin = unit(c(1,1,0,1) ,'line'),
            strip.text = element_text(size = rel(2.0), hjust = 0.5) 
      )
}

##
theme_heatmap <- function (base_size = 12, base_family = font) 
{
   theme_bw(base_size = base_size, base_family = base_family) %+replace% 
      theme(rect              = element_blank(),
            line              = element_blank(), 
            axis.ticks.length = unit(0.01, "cm"),
            # axis.ticks.margin = unit(0, "lines"),   ## depreciated
            axis.text.y         = element_text(size = 20), 
            axis.text.x = element_text( size = 20, angle = 0 ),
            axis.ticks.y        = element_blank() #,
            #axis.title        = element_blank()
            #panel.margin = unit(2, "lines"),
            #plot.margin = unit(c(1,-0,0,-2) ,'line')
      )
}


#  basic
theme_1 <- function(base_size = 25, base_family=font, ...){
   theme_bw(base_size=base_size, base_family = base_family) %+replace%
      theme(
         # background colours set
         strip.text = element_text( size = 15),
         panel.border = element_blank(),
         strip.background = element_blank(),
         plot.background = element_rect(fill = NA, colour = NA) ,
         panel.background = element_rect(fill = NA, colour = NA),
         legend.background = element_rect(fill = NA, colour = NA),
         legend.key = element_rect(fill = NA, colour = NA),
         # set the panel properties
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_line(colour = "grey80", size = 0.1, linetype = 1),
         panel.grid.minor.y = element_blank(),
         axis.ticks = element_blank(),
         axis.line = element_line(colour = "black", size = 0.2),
         # set the text properties
         axis.text = element_text(family = light_font, size = 15, colour = "black"),
         axis.title = element_text(family = light_font, size = 20),
         legend.key.width = unit(22, "mm"),
         legend.text = element_text(family = light_font, size = 18, colour = "black"),
         legend.title = element_text(face = "plain", size = 18),
         legend.title.align=0.5,
         legend.position = 'bottom',
         plot.title = element_text(family = ref_font, size = 8, hjust = 0.20)
      )
}     

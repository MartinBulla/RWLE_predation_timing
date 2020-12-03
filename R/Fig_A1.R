# LOAD TOOLS and DATA
  require(here)
  source(here::here('R/tools.R'))
  
  nsim <- 5000 # number of simulations for predictions
  day_ = 'lightgrey'
  night_ = 'grey30'

  source(here::here('R/prepare_data.R'))
  a[, md := format(a$date, "%m-%d")]
  a[, ymd := as.POSIXct(paste(2019,md, sep ='-'))]

# PLOT
   gg = 
   ggplot() + 
      geom_tile(aes(x = ymd, y = hr, fill = mean),a) + 
      scale_fill_viridis_c(option = "plasma", name = "C°") +
      facet_wrap(~year, nrow = 4) + 
      scale_y_continuous(expand = c(0, 0), lim = c(0,24), breaks = seq(0,24, by = 6), labels = seq(0,24, by = 6), name = "Time of day [hour]") + 
      xlab("Month")+

      theme_MB +   
      theme( 
            legend.text=element_text(size=5),
            legend.title=element_text(size=6, hjust = 0.5)
            )
   ggsave(file = 'Output/Fig_A1.png', gg, dpi = 300, width = 7, height = 10, units = 'cm')

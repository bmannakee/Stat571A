load_le <- function(){
     le <- read.csv("~/Stat571A/IHME_US_COUNTY_LIFE_EXPECTANCY_1987_2009.csv")	
     colnames(le) <- c('fips','State','County','Year','Male','Female','White.Male',
                'White.Female','Black.Male','Black.Female','Male.Delta','Female.Delta')
     # Econ datat in est09ALL.txt is fixed width	
     col_widths <- c(2,4,9,9,9,5,5,5,9,9,9,5,5,5,9,9,9,5,5,5,7,7,7,8,8,8,5,5,5,46,3,23)
     col_names <- c('State.FIPS','County.FIPS','all.ages.pov.percent','all.ages.pov.lower',
                    'all.ages.pov.upper','median.household.income.est','income.lower','income.upper',
                    'county.name','postal.code','date.time')
     econ_data <- read.fwf('est09ALL.txt',col_widths)
     econ_data_89 <- read.fwf('est89all.txt',col_widths)
     econ_data <- econ_data[,c(1,2,6:8,21:23,30:32)]
     names(econ_data) <- col_names
     state_len <- nchar(econ_data$State.FIPS)	
     county_len <- nchar(econ_data$County.FIPS)
     zeros <- c('','0','00','000')
     econ_data$County.FIPS <- paste(zeros[4-county_len],econ_data$County.FIPS,sep='')
     econ_data$fips <- paste(econ_data$State.FIPS,econ_data$County.FIPS,sep='')
     total_frame <- merge(le,econ_data,by='fips')
     stopifnot(nchar(total_frame$fips)==5 || nchar(total_frame$fips)==4)
     return(total_frame)
 }

plot_historical_le <- function(){
  years <- seq(from=1900,to=2010,by=5)
  male_le <- c(46.3,47.3,48.4,52.5,53.6,57.6,58.1,59.9,60.8,63.6,65.6,66.7,66.6,66.8,67.1,68.8,70.0,71.1,71.8,72.5,74.3,75.2,76.2)
  female_le <- c(48.3,50.2,51.8,56.8,54.6,60.6,61.6,63.9,65.2,67.9,71.1,72.8,73.1,73.8,74.7,76.6,77.4,78.2,78.8,78.9,79.7,80.4,81.1)
  stopifnot(length(male_le)==length(female_le) && length(female_le)==length(years))
  frame <- cbind(year=years,male=male_le,female=female_le)
  frame <- data.frame(frame)
  library('ggplot2')
  p <- ggplot(frame,aes(x=year))
  p <- p + geom_line(aes(y=male,colour='Male')) + geom_line(aes(y=female,colour='Female')) + scale_colour_manual(name='Sex',values=c('red','blue'))
  p <- p + theme_bw() + theme(axis.title.x=element_blank()) + ylab('Average Life Expectancy') + ggtitle('Average US life expectancy at birth:1900-2010')
  return(p)
}


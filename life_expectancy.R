load_le <- function(){
     le <- read.csv("~/Stat571A/IHME_US_COUNTY_LIFE_EXPECTANCY_1987_2009.csv")	
     colnames(le) <- c('fips','State','County','Year','Male',' Female','White.Male',
                'White.Female','Black.Male','Black.Female','Male.Delta','Female.Delta')
     # Econ datat in est09ALL.txt is fixed width	
     col_widths <- c(2,4,9,9,9,5,5,5,9,9,9,5,5,5,9,9,9,5,5,5,7,7,7,8,8,8,5,5,5,46,3,23)
     col_names <- c('State.FIPS','County.FIPS','all.ages.pov.percent','all.ages.pov.lower',
                    'all.ages.pov.upper','median.household.income.est','income.lower','income.upper',
                    'county.name','postal.code','date.time')
     econ_data <- read.fwf('est09ALL.txt',col_widths)
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
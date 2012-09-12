load_le <- function(){
     le <- read.csv("~/Stat571A/IHME_US_COUNTY_LIFE_EXPECTANCY_1987_2009.csv")
3   3	
   colnames(le) <- c('fips','State','County','Year','Male','Female','White.Male',
4	 4	
                'White.Female','Black.Male','Black.Female','Male.Delta','Female.Delta')
5	  	

 	 5	
+  # Econ datat in est09ALL.txt is fixed width
 	 6	
+  col_widths <- c(2,4,9,9,9,5,5,5,9,9,9,5,5,5,9,9,9,5,5,5,7,7,7,8,8,8,5,5,5,46,3,23)
 	 7	
+  col_names <- c('State.FIPS','County.FIPS','all.ages.pov.percent','all.ages.pov.lower',
 	 8	
+                 'all.ages.pov.upper','median.household.income.est','income.lower','income.upper',
 	 9	
+                 'county.name','postal.code','date.time')
 	 10	
+  econ_data <- read.fwf('est09ALL.txt',col_widths)
 	 11	
+  econ_data <- econ_data[,c(1,2,6:8,21:23,30:32)]
 	 12	
+  names(econ_data) <- col_names
 	 13	
+  library(reshape)
 	 14	
+  state_len <- nchar(econ_data$State.FIPS)
 	 15	
+  county_len <- nchar(econ_data$County.FIPS)
 	 16	
+  zeros <- c('0','00','000','0000')
 	 17	
+  econ_data$State.FIPS <- paste(econ_data$State.FIPS,zeros[5-state_len-county_len],sep='')
 	 18	
+  return(list(l=le,e=econ_data))
6	 19	
 }
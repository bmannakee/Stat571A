load_le <- function(){
  le <- read.csv("~/Stat571A/IHME_US_COUNTY_LIFE_EXPECTANCY_1987_2009.csv")
  colnames(le) <- c('fips','State','County','Year','Male','Female','White.Male',
               'White.Female','Black.Male','Black.Female','Male.Delta','Female.Delta')
  return(le)
}


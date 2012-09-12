load_le <- function(){
  le <- read.csv("~/Stat571A/IHME_US_COUNTY_LIFE_EXPECTANCY_1987_2009.csv")
  colnames(le) <- c('FIPS','State','County','Year','Male','Female','White.Male',
                    'White.Female','Black.Male','Black.Female','Male.Delta','Female.Delta')
  
  econ_data <- read.csv("~/Stat571A/est09ALL.csv", header=T,check.names=T,stringsAsFactors=FALSE)
  econ_data <- subset(econ_data,select=c(State.FIPS,County.FIPS,Postal,Name,Median.Household.Income,X90..CI.Lower.Bound.6,X90..CI.Upper.Bound.6))
  econ_data <- na.omit(econ_data)
  econ_data$County.FIPS[nchar(econ_data$County.FIPS)==1] <- paste('000',econ_data$County.FIPS,sep='')
  econ_data$State.FIPS[econ_data$State.FIPS=='01'] <- '1'
  econ_data$State.FIPS[econ_data$State.FIPS=='02'] <- '2'
  econ_data$State.FIPS[econ_data$State.FIPS=='04'] <- '4'
  econ_data$State.FIPS[econ_data$State.FIPS=='05'] <- '5'
  econ_data$State.FIPS[econ_data$State.FIPS=='06'] <- '6'
  econ_data$State.FIPS[econ_data$State.FIPS=='08'] <- '8'
  econ_data$State.FIPS[econ_data$State.FIPS=='09'] <- '9'

  #econ_data$FIPS <- paste(econ_data$State.FIPS,econ_data$County.FIPS,sep='')
  return(econ_data)
}

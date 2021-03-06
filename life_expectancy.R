load_le <- function(){
     le <- read.csv("~/Stat571A/IHME_US_COUNTY_LIFE_EXPECTANCY_1987_2009.csv")	
     colnames(le) <- c('fips','State','County','Year','Male','Female','White.Male',
                'White.Female','Black.Male','Black.Female','Male.Delta','Female.Delta')
     # Econ datat in est09ALL.txt is fixed width	
     col_widths <- c(2,4,9,9,9,5,5,5,9,9,9,5,5,5,9,9,9,5,5,5,7,7,7,8,8,8,5,5,5,46,3,23)
     col_names <- c('State.FIPS','County.FIPS','all.ages.pov.percent.09','all.ages.pov.lower.09',
                    'all.ages.pov.upper.09','income.09','income.lower.09','income.upper.09',
                    'county.name','postal.code','date.time')
     col_names_89 <- c('State.FIPS','County.FIPS','all.ages.pov.percent.89','all.ages.pov.lower.89',
                    'all.ages.pov.upper.89','income.89','income.lower.89','income.upper.89')
     
     econ_data <- read.fwf('est09ALL.txt',col_widths,stringsAsFactors=FALSE)
     econ_data_89 <- read.fwf('est89all.txt',col_widths,stringsAsFactors=FALSE)
     econ_data <- econ_data[,c(1,2,6:8,21:23,30:32)]
     econ_data_89 <- econ_data_89[,c(1,2,6:8,21:23)]
     #econ_data_89[,3:8] <- sapply(econ_data_89[,3:8],as.numeric)
     names(econ_data) <- col_names
     names(econ_data_89) <- col_names_89
     state_len <- nchar(econ_data$State.FIPS)
     state_len_89 <- nchar(econ_data_89$State.FIPS)
     county_len <- nchar(econ_data$County.FIPS)
     county_len_89 <- nchar(econ_data_89$County.FIPS)
     zeros <- c('','0','00','000')
     econ_data$County.FIPS <- paste(zeros[4-county_len],econ_data$County.FIPS,sep='')
     econ_data$fips <- paste(econ_data$State.FIPS,econ_data$County.FIPS,sep='')
     econ_data_89$County.FIPS <- paste(zeros[4-county_len_89],econ_data_89$County.FIPS,sep='')
     econ_data_89$fips <- paste(econ_data_89$State.FIPS,econ_data_89$County.FIPS,sep='')
     econ_data_89 <- econ_data_89[,3:9]
     econ_data <- subset(econ_data,fips!='15005')
     econ_data_89 <- subset(econ_data_89,fips!='15005')
     econ_data[,3:8] <- sapply(econ_data[,3:8],as.numeric)
     econ_data_89[,1:5] <- sapply(econ_data_89[,1:5],as.numeric)
     econ_frame <- merge(econ_data,econ_data_89,by='fips')
     econ_frame$Income.delta <- econ_frame$income.09 - econ_frame$income.89
     total_frame <- merge(le,econ_frame,by='fips')
     total_frame <- merge(total_frame,load_geo(),by='fips')
     stopifnot(nchar(total_frame$fips)==5 || nchar(total_frame$fips)==4)
     return(total_frame)
 }

load_geo <- function(){
  geo <- read.csv('county_geocode.csv')
  names(geo) <- c('State.FIPS','County.FIPS','County','State','Pop','Lat','Long')
  county_len <- nchar(geo$County.FIPS)
  zeros <- c('','0','00','000')
  geo$County.FIPS <- paste(zeros[4-county_len],geo$County.FIPS,sep='')
  geo$fips <- paste(geo$State.FIPS,geo$County.FIPS,sep='')
  return(geo)
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

get_regression_09 <- function(frame,sex){
  require(tseries) || install.packages('tseries')
  library(tseries)
  # First we get the regression coefficients and run tests for normality of the residuals
  model <- lm(frame[,sex]~frame[,'income.09'])
  results <- summary(model)$coef
  intercept <- signif(results[1],2)
  slope <- signif(results[2],3)
  int_p <- ifelse(results[7]<.001,'0.000',signif(results[7],4))
  slope_p <- ifelse(results[8] < .001,'0.000',signif(results[8],4))
  r.squared <- signif(summary(model)$r.squared,2)
  df <- summary(model)$df[2]
  jb <- signif(jarque.bera.test(model$resid)$p.value,3)
  # Now we create a plot of the regression. These will be bare bones.
  library('ggplot2')
  p <- ggplot(frame, aes_string(x='income.09',y=sex))
  p <- p + geom_point(shape=1)
  p <- p + geom_smooth(method=lm)
  p <- p + theme_bw() + xlab('Median Income') + ylab('Life Expectancy')
  vec <- list(intercept,int_p,slope,slope_p,r.squared,df,jb,p,model$residuals)
  names(vec) <- c('int','int_p','slope','slope_p','r.squared','df','jb','plot','residuals')
  return(vec)
}


get_regression_09_log <- function(frame,sex){
  require(tseries) || install.packages('tseries')
  library(tseries)
  # First we get the regression coefficients and run tests for normality of the residuals
  model <- lm(log(frame[,sex])~log(frame[,'income.09']))
  results <- summary(model)$coef
  intercept <- signif(results[1],2)
  slope <- signif(results[2],3)
  int_p <- ifelse(results[7]<.001,'0.000',signif(results[7],4))
  slope_p <- ifelse(results[8] < .001,'0.000',signif(results[8],4))
  r.squared <- signif(summary(model)$r.squared,2)
  df <- summary(model)$df[2]
  jb <- signif(jarque.bera.test(model$resid)$p.value,3)
  # Now we create a plot of the regression. These will be bare bones.
  library('ggplot2')
  p <- ggplot(frame, aes_string(x='income.09',y=sex))
  p <- p + geom_point(shape=1)
  p <- p + geom_smooth(method=lm)
  p <- p + scale_x_log10(breaks=c(2e4,5e4,1e5)) + scale_y_log10(breaks=c(70,75,80,85))
  p <- p + theme_bw() + xlab('Log Median Income') + ylab('Log Life Expectancy')
  vec <- list(intercept,int_p,slope,slope_p,r.squared,df,jb,p,model$residuals)
  names(vec) <- c('int','int_p','slope','slope_p','r.squared','df','jb','plot','residuals')
  return(vec)
}





















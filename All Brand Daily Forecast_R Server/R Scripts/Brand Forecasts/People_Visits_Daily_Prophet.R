#### Install the neccessary packages if not available yet
if (("prophet" %in% rownames(installed.packages())) == FALSE) install.packages("prophet")
if (("forecast" %in% rownames(installed.packages())) == FALSE) install.packages("forecast")
if (("lubridate" %in% rownames(installed.packages())) == FALSE) install.packages("lubridate")

suppressMessages(library(prophet))
suppressMessages(library(forecast))

############################################## INPUT #################################################

#### We expect the following variables passed in from the master script

# brand_name - the name of the brand

# brand_data - historical data frame with two columns (ds, y) for date and the forecasted metric

# metric - the name of the forecasted metric

# actual_end - the last date of the historical data

# fc_periods - the numbers of days we want to forecast ahead

# predicts - the data frame that collects the forecast results from the forecast scripts for each 
#            brand.

######################################## GET TRAINING DATA ############################################
train.raw <- brand_data[brand_data$ds <= actual_end,]

# Remove outliers in the training data using the tsclean function in the package "forecast"
train.ts <- ts(train.raw$y, freq = 365)
train.ts.cleaned <- tsclean(train.ts, replace.missing = TRUE, lambda = NULL)

# create training data after removing outliers
train <- data.frame(ds = train.raw$ds
                    , y = as.numeric(train.ts.cleaned))

############################### BUILD THE HOLIDAY DATA FRAME - IF ANY #################################
suppressMessages(require(lubridate))

dateframe <- generateHolidayDates(start_date = min(brand_data$ds), end_date = actual_end+days(fc_periods))

#--- Specify the significant holidays here
holiday_names = c('Cannes', 'Oscars', 'Grammys', 'METGala', 'LaborDay'
                  , 'ColumbusDay', 'FathersDay', 'MemorialDay', 'MothersDay'
                  , 'GoldenGlobes', 'NewYearEve', 'PresidentDay', 'Valentine'
                  , 'EmancipationDay', 'MartinLutherKingDay', 'Easter'
                  , 'Christmas', 'Emmys', 'Thanksgiving')

# Create an empty data frame to store the holiday windows
holidays = data.frame(holiday = character()
                      , ds = as.Date(character())
                      , lower_window = integer() 
                      , upper_window = integer()
                      , stringsAsFactors = FALSE)

# Generate the holiday dates for the specified holidays
for (h in holiday_names) {
  dates = dateframe[dateframe[h] == 1,'date'] #dates of the specified holiday
  rows = nrow(dates)
  holiday = data.frame(holiday = h  
                       , ds = dates
                       , lower_window = -2
                       , upper_window = 3
                       , stringsAsFactors = FALSE)
  holidays = rbind(holidays, holiday)
}

#--- Now treat the special cases

# Cannes window is 2 weeks from the specified dates
len = nrow(holidays[holidays$holiday == 'Cannes',])
holidays[holidays$holiday == 'Cannes', c('lower_window', 'upper_window')] = rep(c(0, 14), each = len)

# Also Cannes happened on first Sunday in 2018
holidays[holidays$holiday == 'Cannes' & year(holidays$ds) == 2018, 'ds'] = as.Date('2018-05-06')

# New Year Eve window is 3 days before the holiday date
len = nrow(holidays[holidays$holiday == 'NewYearEve',])
holidays[holidays$holiday == 'NewYearEve', c('lower_window', 'upper_window')] = rep(c(-3, 0), each = len)

# Thanksgiving window is a week before the holiday date
len = nrow(holidays[holidays$holiday == 'Thanksgiving',])
holidays[holidays$holiday == 'Thanksgiving', c('lower_window', 'upper_window')] = rep(c(-7, 0), each = len)

########################################### MODELLING #################################################
fit.prophet <- prophet(train
                       , holidays = holidays
                       , growth = 'linear'
                       , changepoint.prior.scale = 1.0
                       , changepoints = c('2018-04-01', '2018-04-10')
)

########################################## PREDICTING #################################################

future <- make_future_dataframe(fit.prophet, fc_periods)

forecast <- predict(fit.prophet, future)

############################ PASSING RESULTS BACK TO MASTER SCRIPT ####################################

#### Put the predicts (both historical and forecast periods) into the data frame "predicts"
brand_predicts <- data.frame(brand = brand_name
                             , activity_date = as.Date(forecast$ds)
                             , metric = metric 
                             , pred = forecast$yhat
                             , stringsAsFactors = FALSE)
predicts <- rbind(predicts, brand_predicts)

# The double arrow is to pass the local predict values to the global predict values 
# in the master script
predicts <<- predicts
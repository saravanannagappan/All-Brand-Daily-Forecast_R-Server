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

# Remove data before 2017-01-01 as the pattern was so different from the later years
train <- train.raw
train <- train[train$ds >= as.Date('2017-01-01'),]

# Remove the spike from 9/9/2017 to 9/26/2017
train[train$ds >= as.Date('2017-09-09') & train$ds <= as.Date('2017-09-26'), 'y'] <- NA


#--- Remove outliers in the training data using the tsclean function in the package "forecast"
# The outliers are identified, and replaced by estimates that are the sum of the linear trend
# on the whole training data and the seasonal components.
train.ts <- ts(train$y, freq = 365)
train.ts.cleaned <- tsclean(train.ts, replace.missing = TRUE, lambda = NULL)

# create training data after removing outliers
train <- data.frame(ds = train$ds
                    , y = as.numeric(train.ts.cleaned))

############################### BUILD THE HOLIDAY DATA FRAME - IF ANY #################################

holidays <- NULL

########################################### MODELLING #################################################
fit.prophet <- prophet(train
                       , holidays = holidays
                       , growth = 'linear'
                       , yearly.seasonality = TRUE
                       , changepoint.prior.scale = 10
                       , changepoints = '2017-11-01'
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
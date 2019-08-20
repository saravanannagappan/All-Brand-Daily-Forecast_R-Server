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

# View(train.raw)

# Remove outliers in the training data using the tsclean function in the package "forecast"
train.ts <- ts(train.raw$y, freq = 365)
train.ts.cleaned <- tsclean(train.ts, replace.missing = TRUE, lambda = NULL)

# create training data after removing outliers
train <- data.frame(ds = train.raw$ds
                    , y = as.numeric(train.ts.cleaned))

# Check out the clean numbers against the original numbers
# plot(x = train.raw$ds, y = train.raw$y, type = "l")
# lines(x = train$ds, y = train$y, col = rgb(1,0,0,alpha=0.5))

# plot(x = train$ds, y = train$y, type = "l")
# lines(x = train.raw$ds, y = train.raw$y, col = rgb(1,0,0,alpha=0.5))

############################### BUILD THE HOLIDAY DATA FRAME - IF ANY #################################

holidays <- NULL
# View(holidays)

########################################### MODELLING #################################################
fit.prophet <- prophet(train
                       , holidays = holidays
                       , growth = 'linear'
                       , yearly.seasonality = TRUE
                       , changepoint.prior.scale = 100
                       , changepoints = '2017-12-25'
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
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

train <- train.raw

############################### BUILD THE HOLIDAY DATA FRAME - IF ANY #################################
suppressMessages(require(lubridate))

dateframe <- generateHolidayDates(start_date = min(brand_data$ds), end_date = actual_end+days(fc_periods))

thanksgivings <- data.frame(
  holiday = 'thanksgiving',
  ds = dateframe[dateframe$Thanksgiving == 1,'date']
  , lower_window = 4
  , upper_window = 0
)

christmas <- data.frame(
  holiday = 'christmas',
  ds = dateframe[dateframe$Christmas == 1, 'date'],
  lower_window = 5,
  upper_window = 0
)

#------- Get the three weekends before the Christmas period (starting on 12/20) automatically -----------------------
# The first weekend before Christmas is counted as the first one outside of the Christmas period (12/20 to 12/25)
# Result: three dataframes sundayWeekBeforeChristmas, sunday2WeekBeforeChristmas, sunday3WeekBeforeChristmas

# If the last date of the processed time range was within 30 days before the Xmas period (12/20), 
# add dates up to the 12/20 for that year to get the correct orders of the Sundays before Xmas period.
# Else, just need to process the specified time range
temp.lastDate <- max(dateframe$date)
temp.xmasStart <- as.Date(ISOdate(year(temp.lastDate), 12, 20))
temp.offset <- as.numeric(temp.xmasStart - temp.lastDate)

if (temp.offset > 0 & temp.offset <= 30) { 
  temp <- data.frame(date = seq(min(dateframe$date), temp.xmasStart, 'day'), stringsAsFactors=FALSE)
} else {
  temp <- data.frame(date = seq(min(dateframe$date), max(dateframe$date), 'day'), stringsAsFactors=FALSE)
} 

# Now get the order of the Sundays before Christmas
temp <- data.frame(temp
                   , YoD = year(temp$date)
                   , DoW = wday(temp$date)
                   , MMDD = format(temp$date, "%m-%d")
                   , stringsAsFactors = FALSE)

# Get the Sundays within 30 days before Christmas period (12/20)
temp <- subset(temp, MMDD >  "11-20" & MMDD < "12-20" & DoW == 1)

# Turn temp into data.table for further processing.
suppressMessages(require(data.table))
temp <- data.table(temp)

# Get the first Sunday before Christmas and create the corresponding weekend frame for it
# Keep only the dates <= the actual last date of the processed time range.
sunday1 <- temp[temp[,j = .I[.N], by = YoD]$V1]$date
sunday1 <- sunday1[sunday1 <= temp.lastDate]

sundayWeekBeforeChristmas <- data.frame(
  holiday = 'SundayWeekBeforeChristmas',
  ds = sunday1,
  lower_window = 1,
  upper_window = 0
)

# Get the second Sunday before Christmas and create the corresponding weekend frame for it
# Keep only the dates <= the actual last date of the processed time range.
sunday2 <- temp[temp[,j = .I[.N-1], by = YoD]$V1]$date
sunday2 <- sunday2[sunday2 <= temp.lastDate]

sunday2WeekBeforeChristmas <- data.frame(
  holiday = 'Sunday2WeekBeforeChristmas',
  ds = sunday2,
  lower_window = 1,
  upper_window = 0
)

# Get the third Sunday before Christmas and create the corresponding weekend frame for it
sunday3 <- temp[temp[,j = .I[.N-2], by = YoD]$V1]$date
sunday3 <- sunday3[sunday3 <= temp.lastDate]

sunday3WeekBeforeChristmas <- data.frame(
  holiday = 'Sunday3WeekBeforeChristmas',
  ds = sunday3,
  lower_window = 1,
  upper_window = 0
)

#-----------------------------------------------------------------------------------------------------------

superbowl <- data.frame(
  holiday = 'SuperBowl',
  ds = dateframe[dateframe$SuperBowl == 1, 'date'],
  lower_window = 0,
  upper_window = 0
)

valentine <- data.frame(
  holiday = 'Valentine',
  ds = dateframe[dateframe$Valentine == 1, 'date'],
  lower_window = 0,
  upper_window = 0
)

easter <- data.frame(
  holiday = 'Easter',
  ds = dateframe[dateframe$Easter == 1, 'date'],
  lower_window = 7,
  upper_window = 0
)

fourthjuly <- data.frame(
  holiday = 'FourthJuly',
  ds = dateframe[dateframe$July4th == 1, 'date'],
  lower_window = 0,
  upper_window = 0
)

newyear <- data.frame(
  holiday = 'NewYear',
  ds = dateframe[dateframe$NewYear == 1, 'date'],
  lower_window = 3,
  upper_window = 0
)

#----------------------- Get the four weekends after New Year (1/1) automatically -------------------------------
# Result: 4 dataframes firstSunAfterNewYear, secondSunAfterNewYear, thirdSunAfterNewYear, forthSunAfterNewYear
temp <- dateframe[,c('date', 'YoD', 'MoY', 'DoW')]
temp <- data.frame(temp, MMDD = format(temp$date,"%m-%d"), stringsAsFactors = FALSE)

# Get the Sundays after New Year
temp <- subset(temp, MoY %in% c("Jan","Feb") & MMDD > "01-01" & DoW == 'Sun')

# Turn temp into data.table for further processing.
suppressMessages(require(data.table))
temp <- data.table(temp)

# Get the first Sunday after New Year and create the corresponding weekend frame for it
sunday1 <- temp[temp[,j = .I[1], by = YoD]$V1]$date

firstSunAfterNewYear <- data.frame(
  holiday = 'FirstSunAfterNewYear',
  ds = sunday1,
  lower_window = 1,
  upper_window = 0
)

# Get the second Sunday after New Year and create the corresponding weekend frame for it
sunday2 <- temp[temp[,j = .I[2], by = YoD]$V1]$date

secondSunAfterNewYear <- data.frame(
  holiday = 'SecondSunAfterNewYear',
  ds = sunday2,
  lower_window = 1,
  upper_window = 0
)

# Get the third Sunday after New Year and create the corresponding weekend frame for it
sunday3 <- temp[temp[,j = .I[3], by = YoD]$V1]$date

thirdSunAfterNewYear <- data.frame(
  holiday = 'ThirdSunAfterNewYear',
  ds = sunday3,
  lower_window = 1,
  upper_window = 0
)

# Get the forth Sunday after New Year and create the corresponding weekend frame for it
sunday4 <- temp[temp[,j = .I[4], by = YoD]$V1]$date

forthSunAfterNewYear <- data.frame(
  holiday = 'ForthSunAfterNewYear',
  ds = sunday4,
  lower_window = 1,
  upper_window = 0
)

#-------------------------------------------------------------------------------------------------------- 

holidays <- generateHolidayRanges(holidays = rbind(thanksgivings
                                                   ,christmas
                                                   , sundayWeekBeforeChristmas
                                                   , sunday2WeekBeforeChristmas
                                                   , sunday3WeekBeforeChristmas
                                                   , superbowl
                                                   , valentine
                                                   , easter
                                                   , fourthjuly
                                                   , newyear
                                                   , firstSunAfterNewYear
                                                   , secondSunAfterNewYear
                                                   , thirdSunAfterNewYear
                                                   , forthSunAfterNewYear
))

########################################### MODELLING #################################################
fit.prophet <- prophet(train
                       , holidays = holidays
                       , growth = 'linear'
                       , changepoint.prior.scale = 1.0
                       , changepoints = c('2017-06-20', '2017-07-01')
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
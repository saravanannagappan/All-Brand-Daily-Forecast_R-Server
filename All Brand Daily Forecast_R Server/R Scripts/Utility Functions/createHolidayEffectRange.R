#### Set options warn = 1 to make the warnings printed as they occur (easier to see the warnings belong to which part of code)
options(warn = 1)

#### Install the neccessary packages if not available yet
if (("zoo" %in% rownames(installed.packages())) == FALSE) install.packages("zoo",repos="http://cran.rstudio.com/")
if (("lubridate" %in% rownames(installed.packages())) == FALSE) install.packages("lubridate",repos="http://cran.rstudio.com/")

######################## FUNCTION TO GENERATE HOLIDAY IMPACTED RANGES OUT OF THE SPECIFIED HOLIDAYS #########################
# INPUT: 
#     holidays - a data frame following Facebook Prophet format with the following columns:
#       holiday (type: character): the name of the holiday
#       ds (type: Date): the dates of the holiday
#       lower_window (type: numeric): the number of impacted days before the holiday
#       upper_window (type: numeric): the number of impacted days after the holiday
# OUTPUT:
#     A data frame that contains both the holidays and their impacted days with the following columns:
#       holiday (type: character): the name of the holiday; 
#                                  its lower window range is named as (holiday_name+lw+#days before the holiday)
#                                  its upper window range is named as (holiday_name+uw+#days after the holiday)
#       ds (type: Date): the dates of the holidays and the impacted periods
generateHolidayRanges <- function(holidays = NULL){
  tryCatch({
    # Get the holiday ranges for each holiday using the createHolidayEffectRange function
    holidayranges <- apply(holidays, 1, FUN = function(x){
      createHolidayEffectRange(holiday_name = x[1], holiday_date = as.Date(x[2]), lower_window = x[3], upper_window = x[4])
    })
    
    # Merge the generated ranges into a data frame
    output <- data.frame(holiday = character(),
                         ds=as.Date(character()),
                         stringsAsFactors=FALSE)
    
    for(i in 1:length(holidayranges)) {
      range <- as.data.frame(holidayranges[i])
      names(range) <- c('holiday', 'ds')    
      output <- rbind(output, range)
    }
    
    rownames(output) <- 1:nrow(output)
    
    return(output)
    
  },error = function(e){
    message(paste0("\n",as.character(Sys.time())," > ","Function generateHolidayRanges did not run. Details:\n"))
    message(paste(e))
    message("-------------------------------------------------------------------------------\n")
    stop()
  }
  ) #### TryCatch() ENDS
}

############################# FUNCTION TO CREATE RANGE OF HOLIDAY EFFECT ######################################

# The purpose of this function is to automatically generate holiday variables for the surrounding dates that 
# are affected by the holiday. The output of this function is intended to be compatible with the required format 
# for the variable holidays of the Facebook Prophet forecasting procedure.
createHolidayEffectRange <- function(holiday_name = NULL,holiday_date = NULL, lower_window = 0, upper_window = 0){
  tryCatch({
      suppressMessages(require('lubridate'))
      # Create an empty holiday range data frame
      holidayrange <- data.frame(holiday = holiday_name,
                                 ds=as.Date(holiday_date),
                                 stringsAsFactors=FALSE)
      
      # Generate the lower window dates
      if (lower_window > 0) {
        for(i in 1:lower_window){
          holidayrange <- rbind(holidayrange, list(holiday = paste(holiday_name,'.lw',i,sep=''),ds = holiday_date-days(i)))  
        }
      }
      
      # Generate the upper window dates
      if (upper_window > 0) {
        for(i in 1:upper_window){
          holidayrange <- rbind(holidayrange, list(holiday = paste(holiday_name,'.uw',i,sep=''),ds = holiday_date+days(i)))  
        }
      }
      
      # The result is the data frame that list the holiday impacted dates
      return(holidayrange)
  
  },error = function(e){
    message(paste0("\n",as.character(Sys.time())," > ","Function createHolidayEffectRange did not run. Details:\n"))
    message(paste(e))
    message("-------------------------------------------------------------------------------\n")
    stop()
  }
  ) #### TryCatch() ENDS
}

# test <- createHolidayEffectRange(holiday_name = 'thanksgiving'
#                                 , holiday_date = as.Date('2012-11-22')
#                                 ,lower_window = 4, upper_window = 2)

############################################## TESTING #####################################################

# #-- Create the testing dataframe 
# thanksgiving <- data.frame(
#   holiday = 'thanksgiving'
#   , ds = as.Date(c('2012-11-22','2013-11-28','2014-11-27','2015-11-26','2016-11-24'))
#   , lower_window = 4
#   , upper_window = 0
# )
# 
# christmas <- data.frame(
#   holiday = 'christmas'
#   , ds = as.Date(c('2012-12-25','2013-12-25','2014-12-25','2015-12-25','2016-12-25'))
#   , lower_window = 7
#   , upper_window = 0
# )

# holidays <- rbind(thanksgiving, christmas)
# 
# View(holidays)
# 
# #-- Generate Holiday Ranges
# test <- generateHolidayRanges(holidays = holidays)
# 
# test
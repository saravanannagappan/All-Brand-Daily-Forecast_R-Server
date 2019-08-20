#### Set options warn = 1 to make the warnings printed as they occur (easier to see the warnings belong to which part of code)
options(warn = 1)

#### Install the neccessary packages if not available yet
if (("lubridate" %in% rownames(installed.packages())) == FALSE) install.packages("lubridate",repos="http://cran.rstudio.com/")
if (("timeDate" %in% rownames(installed.packages())) == FALSE) install.packages("timeDate",repos="http://cran.rstudio.com/")
if (("R.utils" %in% rownames(installed.packages())) == FALSE) install.packages("R.utils",repos="http://cran.rstudio.com/")
if (("data.table" %in% rownames(installed.packages())) == FALSE) install.packages("data.table",repos="http://cran.rstudio.com/")

######################## FUNCTION TO GENERATE HOLIDAY DATES FOR A GIVEN YEAR #########################
# INPUT: 
#     start_date (type: Date) - The start date of the period we want to specify the holidays
#     end_date (type: Date) - The end date of the period we want to specify the holidays
#       
# OUTPUT:
#     A data frame that contains:
#       date (type: Date): the dates of the specified time range
#       holiday (type: character): the name of the holiday of the dates if any; 

generateHolidayDates <- function(start_date, end_date){
    
    tryCatch({
        # Make sure we have not loaded "data.table" package before loading the "lubridate" package
        # as it would mask some functions in the "lubridate" package. 
        
        suppressMessages(require(R.utils))
        if(isPackageLoaded('data.table')) detach("package:data.table", unload = TRUE)
        
        #---- Create a data frame of date range
        # Columns: Dates, Year of Date (YoD), Month of Year(MoY), Week of Year (WoY), and Day of Week (DoW)
        suppressMessages(require(lubridate))
      
        dateframe <- data.frame(date = seq(start_date, end_date, 'day')
                                 , stringsAsFactors=FALSE)
        
        dateframe <- cbind(dateframe
                           , YoD = year(dateframe$date)
                           , MoY = month(dateframe$date, label = TRUE)
                           , WoY = week(dateframe$date)
                           , DoW = wday(dateframe$date,label = TRUE))
        
        #---- Indexing Day of Week in a Month in a Year. 
        # This is to support identifying the holidays marked by Day of Week in a certain month (e.g. Thanksgiving, Super Bowl)
        suppressMessages(require(data.table))
        
        dateframe <- data.table(dateframe)
        
        dateframe[, DoWoMIndex := seq_len(.N), by = c('YoD', 'MoY', 'DoW')]
        
        #---- Identify the holidays of the years. Each of the holiday will be a binary column
        
        # Thanksgivings - the fourth Thursday of November
        dateframe$Thanksgiving <- ifelse(dateframe$MoY == 'Nov' & dateframe$DoW == 'Thu' 
                                          & dateframe$DoWoMIndex == 4, 1, 0)
        
        # Christmas 
        dateframe$Christmas <- ifelse(dateframe$MoY == 'Dec' & day(dateframe$date) == 25, 1, 0)
        
        # Super Bowl
        dateframe$SuperBowl <- ifelse(dateframe$MoY == 'Feb' & dateframe$DoW == 'Sun' & dateframe$DoWoMIndex == 1, 1, 0)
        
        # Valentine
        dateframe$Valentine <- ifelse(dateframe$MoY == 'Feb' & day(dateframe$date) == 14, 1, 0)
        
        # Easter
        suppressMessages(require(timeDate))
        
        easters <- Easter(year(start_date):year(end_date))
        
        easterframe <- data.frame(date = as.Date.timeDate(easters), Easter = 1)
        
        dateframe <- merge(dateframe, easterframe, by = 'date', all.x = TRUE)
        
        dateframe$Easter <- ifelse(is.na(dateframe$Easter),0,1)
        
        # StPatricks
        dateframe$StPatricks <- ifelse(dateframe$MoY == 'Mar' & day(dateframe$date) == 17, 1, 0)
        
        # Cinco_de_Mayo
        dateframe$Cinco_de_Mayo <- ifelse(dateframe$MoY == 'May' & day(dateframe$date) == 05, 1, 0)
        
        # MothersDay - 2nd Sunday of May
        dateframe$MothersDay <- ifelse(dateframe$MoY == 'May' & dateframe$DoW == 'Sun' & dateframe$DoWoMIndex == 2, 1, 0)
        
        #MemorialDay - last Monday of May
        memorialframe <- dateframe[dateframe[, j = .I[DoWoMIndex == max(DoWoMIndex) & MoY == 'May' & DoW == 'Mon']
                                              , by = .(YoD, MoY, DoW)]$V1]
        
        memorialframe <- cbind(memorialframe, MemorialDay = 1)
        
        dateframe <- merge(dateframe, memorialframe[,c('date','MemorialDay'), with = FALSE], by = 'date', all.x = TRUE)
        
        dateframe$MemorialDay <- ifelse(is.na(dateframe$MemorialDay),0,1)
        
        # FathersDay - 3rd Sunday of June
        dateframe$FathersDay <- ifelse(dateframe$MoY == 'Jun' & dateframe$DoW == 'Sun' & dateframe$DoWoMIndex == 3, 1, 0)
        
        # July4th
        dateframe$July4th <- ifelse(dateframe$MoY == 'Jul' & day(dateframe$date) == 4, 1, 0)
        
        # Labor Day - 1st Monday in September
        dateframe$LaborDay <- ifelse(dateframe$MoY == 'Sep' & dateframe$DoW == 'Mon' & dateframe$DoWoMIndex == 1, 1, 0)
        
        # Halloween
        dateframe$Halloween <- ifelse(dateframe$MoY == 'Oct' & day(dateframe$date) == 31, 1, 0)
        
        # NewYear
        dateframe$NewYear <- ifelse(dateframe$MoY == 'Jan' & day(dateframe$date) == 1, 1, 0)
        
        # NewYearEve
        dateframe$NewYearEve <- ifelse(dateframe$MoY == 'Dec' & day(dateframe$date) == 31, 1, 0)
        
        # SAG - last Sunday in January
        sagframe <- dateframe[dateframe[, j = .I[DoWoMIndex == max(DoWoMIndex) & MoY == 'Jan' & DoW == 'Sun']
                                             , by = .(YoD, MoY, DoW)]$V1]
        
        sagframe <- cbind(sagframe, SAG = 1)
        
        dateframe <- merge(dateframe, sagframe[,c('date','SAG'), with = FALSE], by = 'date', all.x = TRUE)
        
        dateframe$SAG <- ifelse(is.na(dateframe$SAG),0,1)
        
        # Cannes - 2nd Sunday in May and last for 2 weeks (middle two weeks in May)
        dateframe$Cannes <- ifelse(dateframe$MoY == 'May' & dateframe$DoW == 'Sun' & dateframe$DoWoMIndex == 2, 1, 0)
        
        # Oscars - last Sunday in February
        oscarsframe <- dateframe[dateframe[, j = .I[DoWoMIndex == max(DoWoMIndex) & MoY == 'Feb' & DoW == 'Sun']
                                        , by = .(YoD, MoY, DoW)]$V1]
        
        oscarsframe <- cbind(oscarsframe, Oscars = 1)
        
        dateframe <- merge(dateframe, oscarsframe[,c('date','Oscars'), with = FALSE], by = 'date', all.x = TRUE)
        
        dateframe$Oscars <- ifelse(is.na(dateframe$Oscars),0,1)
        
        # Grammys - 2nd Sunday in February
        dateframe$Grammys <- ifelse(dateframe$MoY == 'Feb' & dateframe$DoW == 'Sun' & dateframe$DoWoMIndex == 2, 1, 0)
        
        # METGala - 1st Monday in May
        dateframe$METGala <- ifelse(dateframe$MoY == 'May' & dateframe$DoW == 'Mon' & dateframe$DoWoMIndex == 1, 1, 0)
        
        # ColumbusDay - 2nd Monday in October
        dateframe$ColumbusDay <- ifelse(dateframe$MoY == 'Oct' & dateframe$DoW == 'Mon' & dateframe$DoWoMIndex == 2, 1, 0)
        
        # VeteranDay - November 11
        dateframe$VeteranDay <- ifelse(dateframe$MoY == 'Nov' & day(dateframe$date) == 11, 1, 0)
        
        # GoldenGlobes - 1st Sunday in January
        dateframe$GoldenGlobes <- ifelse(dateframe$MoY == 'Jan' & dateframe$DoW == 'Sun' & dateframe$DoWoMIndex == 1, 1, 0)
        
        # PresidentDay - 3rd Monday in February
        dateframe$PresidentDay <- ifelse(dateframe$MoY == 'Feb' & dateframe$DoW == 'Mon' & dateframe$DoWoMIndex == 3, 1, 0)
        
        # EmancipationDay - April 16
        dateframe$EmancipationDay <- ifelse(dateframe$MoY == 'Apr' & day(dateframe$date) == 16, 1, 0)
        
        # MartinLutherKingDay - 3rd Monday in January
        dateframe$MartinLutherKingDay <- ifelse(dateframe$MoY == 'Jan' & dateframe$DoW == 'Mon' & dateframe$DoWoMIndex == 3, 1, 0)
        
        # Emmys - 3rd Sunday in September
        dateframe$Emmys <- ifelse(dateframe$MoY == 'Sep' & dateframe$DoW == 'Sun' & dateframe$DoWoMIndex == 3, 1, 0)
        
        # April Fool - April 1
        dateframe$AprilFool <- ifelse(dateframe$MoY == 'Apr' & day(dateframe$date) == 1, 1, 0)
        
        return(as.data.frame(dateframe))
    
    },error = function(e){
      message(paste0("\n",as.character(Sys.time())," > ","Function generateHolidayDates did not run. Details:\n"))
      message(paste(e))
      message("-------------------------------------------------------------------------------\n")
      stop()
    }
  ) #### TryCatch() ENDS
  
}

################################### END OF FUNCTION DEFINITION #####################################

############################################ EXECUTE ###############################################

# # Specify the start date and end date here
# start = as.Date('2015-01-01')
# 
# end = as.Date('2019-06-30')
# 
# # # Run the function
# dateframe <- generateHolidayDates(start_date = start, end_date = end)
# 
# View(subset(dateframe, Emmys == 1))

# # Export the result to a .csv file
# write.csv(output, 'holiday_dates.csv')
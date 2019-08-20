###################################### DAILY FORECASTING - MASTER SCRIPT #########################################
# The purpose of this script is to:
# 
# (1) Prepare the historical data to run the forecast
# 
# (2) Call the brand forecast scripts corresponding to each brand
# 
# (3) Aggregate the forecast results for all brands into one single file.
#     The forecast results include both the forecasted numbers and model accuracy information.
# 
##################################################################################################################
#### Install the neccessary packages if not available yet
if (("optparse" %in% rownames(installed.packages())) == FALSE) install.packages("optparse")
if (("RJDBC" %in% rownames(installed.packages())) == FALSE) install.packages("RJDBC")
if (("zoo" %in% rownames(installed.packages())) == FALSE) install.packages("zoo")
if (("lubridate" %in% rownames(installed.packages())) == FALSE) install.packages("lubridate")
if (("prophet" %in% rownames(installed.packages())) == FALSE) install.packages("prophet")
if (("forecast" %in% rownames(installed.packages())) == FALSE) install.packages("forecast")
if (("dplyr" %in% rownames(installed.packages())) == FALSE) install.packages("dplyr")

#### ASSUMPTIONS
# For Folder Structure: We are at the parent folder of the R Scripts folder.

############################################ CLEAN THE WORKSPACE #################################################
# Make sure we don't use variable values in the previous processes.
setwd("/home/r-forecasting/R/All Brand Daily Forecast_R Server")
rm(list = ls())

############################################### ERROR LOGGING STARTS #############################################

# Open the connection to the log file
con_log <- file(paste0("./Logging/log_file_", Sys.Date(),".log"), 'a')

# Feed the logging messages (errors, warnings, messages) through that connection
sink(con_log, append=TRUE)
sink(con_log, append=TRUE, type="message")

#### Wrap the whole master script into a try() clause so that we will still close the connection to the log file
#### regardless of whether the script has errors or no.
try({
  
    message(paste0("\n",as.character(Sys.time())," > ","MASTER FORECAST PROCESS STARTS"
                   ,"\n-------------------------------------------------------------------------------"))
    
    ####################################### LOAD THE UTILITY FUNCTIONS ###############################################
    
    
    #---------- Function Definition - sourceEntireFolder ----------------------------
    
    # This function is to find all .R files within a folder and sources them.
    sourceEntireFolder <- function(folderName, verbose=FALSE, showWarnings=TRUE) { 
      files <- list.files(folderName, full.names=TRUE)
      
      # Grab only R files
      files <- files[ grepl("\\.[rR]$", files) ]
      
      if (!length(files) && showWarnings)
        warning("No R files in ", folderName)
      
      for (f in files) {
        if (verbose)
          cat("sourcing: ", f, "\n")
        ## TODO:  add caught whether error or not and return that
        try(source(f, local=FALSE, echo=FALSE), silent=!verbose)
      }
      return(invisible(NULL))
    }
    
    #--------------------------------------------------------------------------------
    
    #### Go to the folder "Utility Functions" to define all the neccessary unitily functions
    # The functions are: rmse, mape, generateHolidayDates, generateHolidayRanges
    
    # Assumption: We are at the parent folder of the R Scripts folder.
    sourceEntireFolder(folderName = "./R Scripts/Utility Functions", verbose = TRUE)
    
    ################################################## DATA INPUT ####################################################
    
    #-------------------------------------------- Connecting to Snowflake --------------------------------------------

    # Load library
    require(RJDBC)

    # Specify driver
    jdbcDriver <- JDBC(driverClass="net.snowflake.client.jdbc.SnowflakeDriver",
                       classPath="./snowflake-jdbc-3.7.0.jar") # <-- this is where I saved the jar file

    # Create a connection
    # This is the most critical part.
    # You have to make sure you enter your SSO path as well as corp username with domain

    #---- This is for single sign-on login
    # con <- dbConnect(jdbcDriver, "jdbc:snowflake://meredith.us-east-1.snowflakecomputing.com/?authenticator=externalbrowser",
    #                            'username@domain.com', 'password')

    #---- This uses service account credentials
    con <- dbConnect(jdbcDriver, "jdbc:snowflake://meredith.us-east-1.snowflakecomputing.com",
                     'SA_DSA_ETL_PROD', 'DSA@Snowflake2019')

    # To query data, at this point, you are good to go. start querying data. - CHANGE MADE IN HERE
    data.raw <- dbGetQuery(con, "Select * From dev.sandbox.dsa_historical_data
                           ")

    # # Close the connection
    # dbDisconnect(con)
    
    #--------------------------------------------------------------------------------------------------------------------- 
    
    data <- data.raw
    
    # View(data)
    
    # lowercase all column names for easier typing
    colnames(data) <- tolower(colnames(data))
    
    # Make sure the activity_date column is of type Date
    data$activity_date <- as.Date(data$activity_date)
    # data$activity_date <- as.Date(data$activity_date, "%m/%d/%Y")
    
    # EXCLUDE the following forecasts
    data <- subset(data, !(brand %in% c('Diabetic Living', 'Fitness', 'FIT Pregnancy') & (metric == 'Visits')))
    
    # View(data)
    
    ############################################ END OF DATA INPUT ###################################################
    
    ################################# RUN THE FORECAST SCRIPTS FOR EACH BRAND #######################################
    # Run the daily forecasts for each brand from the forecast scripts
    # Assumption: The script names contains the brand names & the brand names match those in the input data
    
    #----- Global Forecast Configuration ----------
      
    # The variables below define general forecast requirements and to be passed to individual brand forecast scripts.
      
    # actual_end - the last date of the historical data
    actual_end <- max(data$activity_date)
    # actual_end <- as.Date('2019-02-28')
    
    # fc_periods - the numbers of days we want to forecast ahead
    # NOTE: Replace this variable by fc_end_date to make sure all forecasts end at the same date.
    #       This condition entails that different forecasts can have different fc_periods.
    #       Therefore the values of the fc_periods is defined at the time we call the specific forecasts (for loop below)
    #       instead of being here.
    # fc_periods <- 730
    fc_end_date <- actual_end + 730 
    
    # predicts - the data frame that collects the predicts from the forecast models for individual brands
    #            for both historical time and forecast time. 
    #            This information will be used to evaluate the model performance.
    #            The data frame columns: (brand, activity_date, metric, preds)
    predicts <- data.frame(brand = character()
                           , activity_date = as.Date(character())
                           , metric = character() 
                           , pred = numeric()
                           , stringsAsFactors = FALSE)
    
    #----------------------------------------------
    
    #----- Call the Brand Forecast Scripts --------
    # This procedure will further pass the following variables to the brand forecast scripts
    # brand_name - the name of the brand
    # brand_data - historical data frame with two columns (ds, y) for date and the forecasted metric
    # metric - the name of the forecasted metric
    
    #--- Get the brand forecast files in the folder "Brand Forecasts"
    # NOTE: 
    # - Forecast File Naming Convention = <brand name>_<metric>_<time granularity>_<technique>
    #   E.g. the file name for the People daily visits forecasting script should be People_Visits_Daily_Prophet.R
    # - It doesn't matter if the strings are upper/lower cases.
    model_path <- "./R Scripts/Brand Forecasts/"
    
    files <- list.files(model_path, full.names=TRUE)
    
    #--- Now run the forecasts
    
    # Set options warn = 1 to recognize the warnings belong to which brand scripts.
    options(warn = 1)
    
    # Get the brand names
    brand_names <- unique(data$brand)
    
    # Get the metric names
    metric_names <- unique(data$metric)
    
    require('zoo')
    for (m in metric_names) {
      for (b in brand_names) {
        
        # Extract data for a single brand and a single metric
        brand_data <- subset(data, metric == m & brand == b, select = c('activity_date', 'value'))
        
        # Change column names to ds, y
        colnames(brand_data) <- c("ds", "y")
        
        # Make sure the data has all the dates of the historical period 
        # (if there's no traffic on some days/months, the metric value is NA for the days)
        date_seq <- data.frame(ds = seq(min(brand_data$ds), max(brand_data$ds), by = 'day')
                               , stringsAsFactors = FALSE)
        
        require(dplyr)
        brand_data <- date_seq %>% 
          left_join(brand_data, by = 'ds')
        
        # Make sure the data is sorted by dates
        brand_data <- brand_data[order(brand_data$ds),]
        
        # Trim off any NA values at the beginning and end of the time series
        brand_data <- na.trim(brand_data)
        
        # Get the brand name and the metric name
        metric <- m
        
        brand_name <- b
        
        # Specify the fc_periods (forecast horizon) here
        fc_periods <- fc_end_date - max(brand_data$ds)
        
        message(paste("Begin forecasting for", brand_name, metric, "\n"))
        
        # Grab the file path.
        # Lower all characters, also treat special characters as they are to simplify text processing
        #f <- files[grepl(tolower(paste(model_path, brand_name, "_", metric, "_", sep = ""))
        #                 , tolower(files), fixed = TRUE)]
        f <- paste("./R Scripts/Brand Forecasts/", brand_name,"_", metric ,"_Daily_Prophet.R",  sep = "")
        
        # Source the scripts in new environments to keep the variables in the scripts local to them
        # Wrap the call in the try() to detect if any brand forecast files have problems
        tryCatch(do.call(source, list(file = f, local=TRUE), envir=new.env())
                 ,error = function(e){
                   message(paste("Forecasting for", brand_name, metric,"was NOT SUCCESSFUL. Details:\n"))
                   message(paste(e))
                 }
        )
        
        message(paste("End forecasting for",brand_name
                      ,"\n-------------------------------------------------------------------------------\n"))
      }
    }
    
    # View(predicts)
    
    #----------------------------------------------
    
    ###################################### CALCULATE THE MODEL ACCURACY ######################################
    
    # #---------- Calculate RMSE and MAPE - Test Set ----------
    # test.actuals <- data[data$activity_date > actual_end,]
    # 
    # test.predicts <- predicts[predicts$activity_date > actual_end & predicts$activity_date <= max(data$activity_date),]
    # 
    # # View(test.predicts)
    # 
    # # Merge actuals and predicts by brand names and date
    # test_frame <- merge(test.actuals, test.predicts, all.x = TRUE, by = c('activity_date', 'brand', 'metric'))
    # 
    # # Sort the data a bit
    # test_frame <- with(test_frame, test_frame[order(brand, activity_date),])
    # 
    # # View(test_frame)
    # 
    # # Now get the MAPEs for all brands
    # require(dplyr)
    # mapes <- test_frame %>%
    #   group_by(vertical, brand) %>%
    #   mutate(percent_error = round(abs(pred-value)/value * 100, 2)) %>%
    #   summarize(mape = mean(percent_error)) %>%
    #   arrange(desc(mape))
    # 
    # View(mapes)
    # 
    # #----------  Create data frame with historical data and predicts ----------
    # historical <- subset(data, select = -vertical)
    # historical['type'] = 'Historical'
    # 
    # preds <- subset(predicts, activity_date > actual_end)
    # preds['type'] = 'Forecast'
    # colnames(preds) = colnames(historical)
    # 
    # # View(historical)
    # # View(preds)
    # 
    # result_daily <- rbind(historical, preds)
    # 
    # # View(result_daily)
    # 
    # result_daily[,'value'] = as.integer(as.matrix(round(result_daily[,'value'], 0)))
    # 
    # # View(result_daily)
    # 
    # write.csv(result_daily, '../Output/testing_all_brands_v4.csv', row.names = FALSE)
    
    ############################################ EXPORT THE RESULTS #################################################
    
    #--------- Build the results data for this forecast run ---------
    # which is a data frame with the columns (last_date_actual, vertical, brand, activity_date, metric, pred)
    require(dplyr)
    
    # Get the latest date in the historical data for each metric in each brand
    brand_max_dates <- data %>%
                        group_by(brand, metric) %>%
                        summarize(max_date = max(activity_date)) %>%
                        select(brand, metric, max_date)
    
    # Remove the prediction on the historical period in the forecast results
    predicts <- predicts %>%
                  inner_join(brand_max_dates, by = c('brand', 'metric')) %>%
                  filter(activity_date > max_date) %>%
                  select(-max_date)
    
    # Now get the final results to export
    results <- unique(data[c('vertical', 'brand')]) %>%
                   inner_join(predicts, by = 'brand') %>%
                   # Add in the forecasting date as the last date in the historical data.
                   # Also round the predicted numbers.
                   mutate(last_date_actual = actual_end, pred = round(pred)) %>% 
                   select(last_date_actual, vertical, brand, activity_date, metric, pred)
    
    # If the preds <= 0, make them zero
    results$pred <- ifelse(results$pred <= 0, 0, results$pred)
    
    # # The results will be exported to the folder "Output" of the package "All Brand Daily Forecast"
    # write.csv(results, file = "./Daily Forecasts_All Brands.csv", row.names = FALSE)
    
    #--------- Export the results to Snowflake ---------
    # Process: first insert the results into a staging table, then upsert the output table with the staging data.
    # Reason: Sometime we might need to rerun & reload the forecasting results.
    #         Upsert is better than Insert for this reason.

    # # Load library
    # require(RJDBC)
    #
    # # Specify driver
    # jdbcDriver <- JDBC(driverClass="net.snowflake.client.jdbc.SnowflakeDriver",
    #                    classPath="./snowflake-jdbc-3.7.0.jar") # <-- this is where I saved the jar file
    #
    # # Create a connection
    # # This is the most critical part.
    # # You have to make sure you enter your SSO path as well as corp username with domain
    # #---- This is for single sign-on login
    # # con <- dbConnect(jdbcDriver, "jdbc:snowflake://meredith.us-east-1.snowflakecomputing.com/?authenticator=externalbrowser",
    # #                            'username@domain.com', 'password')
    #
    # #---- This uses service account credentials
    # con <- dbConnect(jdbcDriver, "jdbc:snowflake://meredith.us-east-1.snowflakecomputing.com",
    #                  'SA_DSA_ETL_PROD', 'DSA@Snowflake2019')

    #---- Create a temporary staging table - CHANGE MADE IN HERE
    dbSendStatement(con,"Create or Replace Temporary Table dev.sandbox.dev_staging (
                    last_date_actual DATE
                    , vertical VARCHAR
                    , brand VARCHAR
                    , activity_date DATE
                    , metric VARCHAR
                    , pred NUMBER(38,0)
    )")

    #---- Insert current forecast results to the STAGING table
    # Since it seems we can't send more than 16,000 records in one go, break the data down to smaller chunks to send

    idx <- 0
    data_size <- nrow(results)
    batch_size <- 16000
    while (idx <= data_size) {
      # print(paste0("start: ", idx+1, " end: ", ifelse(idx+batch_size < data_size, idx+batch_size, data_size)))

      # Formatting rows to insert into SQL statement
      rows <- apply(results[(idx+1):ifelse(idx+batch_size < data_size, idx+batch_size, data_size),], 1
                    , function(x){paste0("'", x, "'", collapse = ', ')})
      rows <- paste0('(', rows, ')')

      # SQL statement for inserting data - CHANGE MADE IN HERE
      statement <- paste0(
        "Insert Into dev.sandbox.dev_staging"
        , ' Values ', paste0(rows, collapse = ', ')
      )

      # Now send the Insert statement
      dbSendStatement(con, statement)

      idx <- idx+batch_size
    }

    #---- Upsert the data from the STAGING to the MAIN table - CHANGE MADE IN HERE
    dbSendStatement(con, "
                    Merge Into dev.sandbox.dsa_forecasting_data d Using dev.sandbox.dev_staging s
                    On d.last_date_actual = s.last_date_actual
                    and d.vertical = s.vertical
                    and d.brand = s.brand
                    and d.activity_date = s.activity_date
                    When matched then update set pred = s.pred
                    When not matched Then Insert Values (s.last_date_actual, s.vertical, s.brand, s.activity_date, s.metric, s.pred)
                    ")

    #--- Close the connection to Snowflake
    dbDisconnect(con)
    
    message(paste("-------------------------------------------------------------------------------\n"
                  , Sys.time(), ">", "MASTER FORECAST PROCESS ENDS","\n"))
})

########################################### ERROR LOGGING STOPS ###########################################
# Revert logging output back to the console -- only then access the file!
sink(type = "message")
sink()

# Close the connection to the log file
close(con_log)

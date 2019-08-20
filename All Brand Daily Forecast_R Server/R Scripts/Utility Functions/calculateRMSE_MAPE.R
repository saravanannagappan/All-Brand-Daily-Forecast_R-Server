#### Functions to calculate RMSE and MAPE

#### Install the neccessary packages if not available yet
if (("zoo" %in% rownames(installed.packages())) == FALSE) install.packages("zoo",repos="http://cran.rstudio.com/")

#-- RMSE
rmse <- function(actual,predicted) {
  tryCatch({
    # trim off NA values
    suppressMessages(require(zoo))
    actual <- na.trim(actual)
    predicted <- na.trim(predicted)
    
    #TODO: Make sure actual and predicted are of the same length
    
    #TODO: Make sure actual and predicted are of the same metric
      
    # Now return the RMSE
    return (round((sum((actual-predicted)^2)/length(actual))^.5,2))
  
  },error = function(e){
      message(paste0("\n",as.character(Sys.time())," > ","Function rmse did not run. Details:\n"))
      message(paste(e))
      message("-------------------------------------------------------------------------------\n")
      stop()
  }
  ) #### TryCatch() ENDS
}

#-- MAPE
mape <- function(actual,predicted) { 
  tryCatch({
    # trim off NA values
    suppressMessages(require(zoo))
    actual <- na.trim(actual)
    predicted <- na.trim(predicted)
    
    #TODO: Make sure actual and predicted are of the same length
    
    #TODO: Make sure actual and predicted are of the same metric
    
    # Now return the MAPE
    return (round(mean(100*abs((actual-predicted)/actual)),2))
    
  },error = function(e){
      message(paste0("\n",as.character(Sys.time())," > ","Function mape did not run. Details:\n"))
      message(paste(e))
      message("-------------------------------------------------------------------------------\n")
      stop()
  }
  ) #### TryCatch() ENDS
}
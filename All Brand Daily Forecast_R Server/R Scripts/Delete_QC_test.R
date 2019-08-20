setwd("/home/r-forecasting/R/All Brand Daily Forecast_R Server")

#### Install the neccessary packages if not available yet
if (("RJDBC" %in% rownames(installed.packages())) == FALSE) install.packages("RJDBC",repos="http://cran.rstudio.com/")

#### ASSUMPTIONS
# For Folder Structure: We are at the parent folder of the R Scripts folder.

############################################ CLEAN THE WORKSPACE #################################################
# Make sure we don't use variable values in the previous processes.
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
  
    message(paste0("\n",as.character(Sys.time())," > ","HISTORICAL DATA UPDATE PROCESS STARTS"
                   ,"\n-------------------------------------------------------------------------------"))
  
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
                     'SA_DSA_ETL_PROD', 'DSA@Snowflake20191')
    
    #---- Get the latest date in the historical_data table
    update_date <- dbGetQuery(con,"Select Min(brand_max_date) update_date
                                           From (Select brand, Max(activity_date) brand_max_date
                                                 From dev.sandbox.dsa_historical_data
                                                 Group By brand) max_date_by_brand
                                      ") 
    update_date <- update_date[1,1]
    
    #---- Create SQL statement to get updates on daily visits, which are the data after the latest date in the historical table
    new_data_statement <- paste0("Select Case When brand in ('People', 'Entertainment Weekly', 'People En Espanol') Then 'Entertainment' 
                                 When brand in ('Allrecipes', 'MyRecipes', 'Eating Well', 'Cooking Light', 'Dinner Spinner', 'Diabetic Living', 'Rachael Ray', 'Extra Crispy') Then 'Food'
                                 When brand in ('Parents', 'Health', 'Shape', 'Family Circle', 'FIT Pregnancy', 'Fitness') Then 'Health & Parenting'
                                 When brand in ('Southern Living', 'Martha Stewart Living', 'Better Homes and Garden', 'Real Simple', 'Midwest Living', 'Traditional Home', 'Coastal Living') Then 'Home'
                                 When brand in ('InStyle', 'HelloGiggles', 'Martha Stewart Weddings') Then 'Lifestyle'
                                 When brand in ('Travel & Leisure', 'Food & Wine', 'Departures') Then 'Travel & Luxury'
                                 When brand in ('Parenting', 'More (DC)', 'Wood Magazine', 'All People Quilt', 'Living the Country Life') then 'Other'
                                 End As vertical
                                 , brand, activity_date, 'Visits' metric, visits value
                                 From (Select Case When brand = 'Allrecipes' Then 'Allrecipes'
                                 When brand in ('Better Homes and Garden', 'BHG') Then 'Better Homes and Garden'
                                 When brand = 'Coastal Living' Then 'Coastal Living'
                                 When brand = 'Cooking Light' Then 'Cooking Light'
                                 When brand = 'Departures' Then 'Departures'
                                 When brand in ('Diabetic Living', 'DIABETICLIVING') Then 'Diabetic Living'
                                 When brand in ('Eating Well', 'EATINGWELL') Then 'Eating Well'
                                 When brand = 'Entertainment Weekly' Then 'Entertainment Weekly'
                                 When brand = 'Extra Crispy' Then 'Extra Crispy'
                                 When brand in ('Family Circle', 'FAMILYCIRCLE') Then 'Family Circle'
                                 When brand in ('Fitness', 'FITNESS') Then 'Fitness'
                                 When brand in ('FITPREGNANCY', 'FIT Pregnancy') Then 'FIT Pregnancy'
                                 When brand in ('Food & Wine', 'Food and Wine') Then 'Food & Wine'
                                 When brand = 'Health' Then 'Health'
                                 When brand in ('Hello Giggles', 'HelloGiggles') Then 'HelloGiggles'
                                 When brand in ('InStyle', 'In Style Magazine') Then 'InStyle'
                                 When brand in ('MARTHALIVING', 'Martha Stewart Living') Then 'Martha Stewart Living'
                                 When brand in ('Martha Stewart Weddings', 'MARTHAWEDDINGS') Then 'Martha Stewart Weddings'
                                 When brand in ('Midwest Living', 'MIDWESTLIVING') Then 'Midwest Living'
                                 When brand in ('MyRecipes', 'My Recipes') Then 'MyRecipes'
                                 When brand in ('PARENTS', 'Parents') Then 'Parents'
                                 When brand in ('People.com', 'People Magazine') Then 'People'
                                 When brand = 'People En Espanol' Then 'People En Espanol'
                                 When brand in ('Rachael Ray', 'RACHAELRAY') Then 'Rachael Ray'
                                 When brand = 'Real Simple' Then 'Real Simple'
                                 When brand in ('Shape', 'SHAPE') Then 'Shape'
                                 When brand = 'Southern Living' Then 'Southern Living'
                                 When brand in ('TRADITIONALHOME', 'Traditional Home') Then 'Traditional Home'
                                 When brand in ('Travel & Leisure', 'Travel and Leisure') Then 'Travel & Leisure'                  
                                 When brand = 'Parenting' Then 'Parenting'
                                 When brand = 'More (DC)' Then 'More (DC)'
                                 When brand = 'Wood Magazine' Then 'Wood Magazine'
                                 When brand = 'All People Quilt' Then 'All People Quilt'
                                 When brand = 'Living the Country Life' Then 'Living the Country Life'
                                 When brand = 'Dinner Spinner' Then 'Dinner Spinner'
                                 End As brand
                                 , activity_date, visits              
                                 From (
                                 -- Get data from traffic_summary
                                 Select long_descr brand, activity_date
                                 , Sum(Case 
                                 -- get full visits when the brands are not People and Travel & Leisure
                                 When long_descr not in ('People Magazine', 'Travel & Leisure') Then visits
                                 -- get full visits when the brands are People and Travel & Leisure, and the dates are before Dec 2018
                                 When long_descr in ('People Magazine', 'Travel & Leisure') and activity_date < '2018-12-01' Then visits
                                 When long_descr in ('People Magazine', 'Travel & Leisure')
                                 and activity_date >= '2018-12-01' and (campaign not like '%arbit%' and campaign not like '%arb_%') Then visits                              
                                 End) visits
                                 From prod.digitaldb.traffic_summary t
                                 Join (Select fieldvalue, long_descr
                                 From prod.cdb.DOMAIN_DEFN
                                 Where fieldname like '%Dataset_Id%'
                                 ) dd
                                 On dd.fieldvalue = t.dataset_id  
                                 Where shop_traffic_flag = 'N' -- Exclude Shop traffic
                                 and channel_grouping not in ('Paid', 'Paid Search') -- Exclude Paid traffic
                                 and long_descr in ('People Magazine', 'Allrecipes', 'Entertainment Weekly', 'Parents', 'My Recipes', 'Southern Living', 'Health', 'Martha Stewart Living', 'Better Homes and Garden', 'Real Simple', 'Travel & Leisure', 'In Style Magazine', 'Food & Wine', 'People En Espanol', 'Shape', 'Hello Giggles', 'Eating Well', 'Cooking Light', 'Midwest Living', 'Diabetic Living', 'Rachael Ray', 'Extra Crispy', 'Traditional Home', 'Coastal Living', 'Martha Stewart Weddings', 'Family Circle', 'FIT Pregnancy', 'Fitness', 'Departures'
                                 , 'Parenting', 'More (DC)', 'Wood Magazine', 'All People Quilt', 'Living the Country Life'                                  
                                 )      
                                 and dataset_id != '92561964' -- remove this dataset_id that associates with 'Allrecipes'
                                 and activity_date > '", update_date, "' -- get the data that has not exist in the historical data table yet
                                 Group By 1, 2
                                 
                                 -- Get daily visits for DinnerSpinner
                                 Union All       
                                 Select 'Dinner Spinner' brand, hit_date activity_date, Count(distinct unique_visit_id) visits
                                 From prod.digitaldb.page_impression_remixios
                                 Where hit_date > '", update_date, "' -- get the data that has not exist in the historical data table yet
                                 Group By 1, 2
                                 ) data_raw
                                 ) data_clean_brand_names
                                 ")
    
    #---- Upsert new data to the historical data table
    dbSendStatement(con, paste0("
                      Merge Into dev.sandbox.dsa_historical_data d Using (", new_data_statement,") n       
                       On d.vertical = n.vertical
                         and d.brand = n.brand
                         and d.activity_date = n.activity_date
                      When matched then update set value = n.value
                      When not matched Then Insert Values (n.vertical, n.brand, n.activity_date, n.metric, n.value)
                    "))
    
    # Close the connection
    dbDisconnect(con)
    
    message(paste("-------------------------------------------------------------------------------\n"
                  , Sys.time(), ">", "HISTORICAL DATA UPDATE PROCESS ENDS","\n"))

})

########################################### ERROR LOGGING STOPS ###########################################
# Revert logging output back to the console -- only then access the file!
sink(type = "message")
sink()

# Close the connection to the log file
close(con_log)

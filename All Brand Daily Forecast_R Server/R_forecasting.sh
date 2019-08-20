#!/bin/sh
#!/usr/bin/Rscript


 
#cd /home/r-forecasting/R/All\ Brand\ Daily\ Forecast_R\ Server/R\ Scripts/

sudo Rscript /home/r-forecasting/R/All\ Brand\ Daily\ Forecast_R\ Server/R\ Scripts/Update\ Historical\ Data_Snowflake_Logging.R
sudo Rscript /home/r-forecasting/R/All\ Brand\ Daily\ Forecast_R\ Server/R\ Scripts/Daily\ Forecasting-Master\ Script_Snowflake_Logging.R

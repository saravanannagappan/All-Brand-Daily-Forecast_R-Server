echo off

title Run the Forecast

:: Set up the path to the RScript execution file
set R_Script="C:\Program Files\R\R-3.5.3\bin\RScript.exe"

:: Execute the R Master Script
%R_Script% --vanilla ".\R Scripts\Daily Forecasting-Master Script_Snowflake_Logging.R" 

echo %errorlevel%

:: Check if the process ran successfully.
:: if %errorlevel% <> 0 (echo The process was not successfully completed.)
:: else (echo The process was successfully completed.)

pause

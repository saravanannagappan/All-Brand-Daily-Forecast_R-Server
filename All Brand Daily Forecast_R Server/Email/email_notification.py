#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May 28 16:41:18 2019

@author: nagappas
"""

import qc_log
import send_email
import datetime
import os

os.chdir("/home/r-forecasting/R/All Brand Daily Forecast_R Server/Email/")
d = datetime.datetime.today()
log_file = "../Logging/log_file_" + d.strftime('%Y-%m-%d') + ".log"

#log_file = "./Logging/log_file_2019-05-27.log"

obj = qc_log.qc_log_files(log_file)

if obj.FindFile():
    EmailBody = obj.Words_Search()
    
else:
    EmailBody = "File Not Found in the Path"

if EmailBody != 'No Error':    
    email = send_email.Email(log_file)
    email.send_email(EmailBody)
print("Completed")

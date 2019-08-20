#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May 28 16:32:27 2019

@author: nagappas
"""

import os

class qc_log_files():
    
    def __init__ (self, filePath):
        self.filePath = filePath
    
    def FindFile(self):
        return os.path.exists(self.filePath)
    
    def Words_Search(self):
        HisStart = False
        HisEnd = False
        MasStart = False
        MasEnd = False
        docError = False
        
        with open(self.filePath) as file:
            for line in file.readlines():
                
                if 'HISTORICAL DATA UPDATE PROCESS STARTS' in line: 
                    HisStart = True
                
                if 'HISTORICAL DATA UPDATE PROCESS ENDS' in line:
                    HisEnd = True
                
                if 'MASTER FORECAST PROCESS STARTS' in line:
                    MasStart = True
                if 'MASTER FORECAST PROCESS ENDS' in line:
                    MasEnd = True
                
                if 'Error' in line or 'ERROR' in line:
                    docError = True
                    
        if HisStart == False or HisEnd == False:
            return "HISTORICAL"
        elif MasEnd == False or MasStart == False:
            return "Master"
        elif docError == True:
            return "Processing Error"
        else:
            return "No Error"        
        
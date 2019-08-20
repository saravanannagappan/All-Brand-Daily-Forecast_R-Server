#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri May 31 17:31:49 2019

@author: nagappas
"""



from O365 import Account
import json

class Email():
    
    def __init__ (self, filePath):
        self.filePath = filePath
        
        json_file =  open('credentials.txt', 'r')  
        data = json.load(json_file)
        self.client_id = data['client_id']
        self.client_secret = data['client_secret']
            
    def connect(self):
         credentials = (self.client_id, self.client_secret)
         scopes = ['Mail.Send', 'offline_access', 'Mail.ReadWrite', 'User.Read']
         self.account = Account(credentials)
         if not self.account.is_authenticated:  # will check if there is a token and has not expired
            # ask for a login
            self.account.authenticate(scopes=scopes)
            
    def send_email(self, error):
         self.connect()   
         m = self.account.new_message()
         m.to.add(['John.Gugliotti@meredith.com', 'CuongN@allrecipes.com', 'Avinesh.Vasudevan@meredith.com'])
         #m.to.add(['saravanan.nagappan@meredith.com'])
         m.cc.add(['saravanan.nagappan@meredith.com'])
         m.subject = 'Error in R_Forcasting'
          
         
         if error != 'File Not Found in the Path' :
             m.body = """Hi, <br><br>
                     There is an error while running the forecasting code.<br>
                     For more details Please find the attached log file. <br><br> 
                     <b> """ + error + """  </b> <br><br>Regards, <br> Code""" 
                     
             m.attachments.add(self.filePath)
         else:
             m.body = """Hi, <br><br>
                     There is an error while running the forecasting code.<br>
                     <b> """ + error + """  </b> <br><br>Regards, <br> Code"""
                     
         
         m.send()   
     

  
"""
url = account.connection.get_authorization_url(requested_scopes=[scopes])  # visit url
result_url = input('Paste the result url here...')  # wait for the user input. Just use whatever mechanism you want to retrieve the url from the user.
account.connection.request_token(result_url)
"""    


"""
credentials = ('5024bf5f-15ce-4327-b97e-02923348e619', '2GcwqyQduC0hya+QGlE@+KtvIqrpH79*')
scopes = ['Mail.Send', 'offline_access', 'Mail.ReadWrite', 'User.Read']
account = Account(credentials)

 
if not account.is_authenticated:  # will check if there is a token and has not expired
    # ask for a login
    account.authenticate(scopes=scopes)
        
m = account.new_message()
m.to.add(['saravanan.nagappan@meredith.com'])
m.subject = 'R_Notification'
m.body = "Hi, This the a test email with attachment"
m.attachments.add('Sample.txt')
m.send()   

"""  
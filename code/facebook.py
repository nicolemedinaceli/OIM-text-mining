#pip install facebook-scraper
import os
import numpy as np
import pandas as pd
import csv
from facebook_scraper import get_posts
os.chdir('C:\\Users\\ALVARO\\Documents\\GitHub\\OIM-text-mining\\data')
#########################
#paginas facebook
#########################
bd=pd.DataFrame()
for post in get_posts('paginasiete', pages=2):
    aux=pd.DataFrame([post])    
    bd=pd.concat([bd,aux]) 


bd.to_csv(r'bd.csv', index = False, header=True)
##################################################
#ejemplo para grupo de facebook publico
##################################################
bdg=pd.DataFrame()
for post in get_posts(group='2644801065579479', pages=1):
    aux=pd.DataFrame([post])    
    bdg=pd.concat([bdg,aux]) 

bdg.to_csv(r'bdg.csv', index = False, header=True)
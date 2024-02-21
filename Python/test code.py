# -*- coding: utf-8 -*-
"""
Created on Wed Feb 21 14:31:44 2024

@author: olivia.hebner
"""

## !pip install geopandas

import geopandas as gpd
import matplotlib.pyplot as plt

shapefile = gpd.read_file("C:/Users/olivia.hebner/OneDrive - Summit LLC/GitHub/Du Bois Challenge 2024/Data Input/DuBoisChallenge - Georgia Counties w 1870 & 1880 data.shp")

for col in shapefile.columns:
    print(col)
    
shapefile["data1870 ("].value_counts()

fig,ax = plt.subplots(figsize=(10,10))

duboisPalette = {'2500 - 5000': '#E5A59E',
                 '5000 - 10000': '#D62B3F',
                 '> 1000': '#475C52',
                 '1000 - 5000': '#EEAB25',
                 '10000 - 15000': '#D0B49C',
                 '15000 - 20000': '#634130', 
                 '20000 - 30000': '#201E56'}


#shapefile.plot(ax=ax, column = "data1870 (", label=duboisPalette)
#shapefile.plot(ax=ax, label="data1870 (")
shapefile.plot(ax=ax, column = "data1870 (")







#========================================================================================
# Script to make donut charts for each treatment and sampling height that quantify the 
# amount of carbon used for respiration, changes in non-structural carbon stores, and 
# growth. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')

# read in structural carbon gain
#----------------------------------------------------------------------------------------
strucData <- read_csv (file = 'structuralCarbonData.csv', col_types = cols ())
respData <- read_csv ()
#========================================================================================

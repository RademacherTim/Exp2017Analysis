#==================================================================================================
# Script to check the ring labels on the microcore section for the 2017 experiment at Harvard Forest
#--------------------------------------------------------------------------------------------------

# Select treatment (1 = control, 2 = girdling, 3 = compression, 4 = double compression)
#--------------------------------------------------------------------------------------------------
treatment <- 1

# Set working directory to folder with images and json files
#--------------------------------------------------------------------------------------------------
setwd (paste0 ('../SlideScannerImages/T',treatment,'/'))

# Plot ring measurements from microcore sections
#--------------------------------------------------------------------------------------------------
fileList <- list.files (pattern = '.jpg')

# Get necessary metadata to match each images with json file
#--------------------------------------------------------------------------------------------------
trees   <- as.numeric (substr (unlist (strsplit (fileList, '-')) [seq (1, length (fileList)*3, by = 3)], 2, 3))
heights <- substr (unlist (strsplit (fileList, '-')) [seq (2, length (fileList)*3, by = 3)], 3, 3)


#==================================================================================================
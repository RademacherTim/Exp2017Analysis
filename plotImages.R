#========================================================================================
# Script to plot the microcore images to check the measurements
#---------------------------------------------------------------------------------------- 

# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('raster')
library ('rgdal')
library ('rjson')
library ('RColorBrewer')

# set working directory
#----------------------------------------------------------------------------------------
previousWD <- getwd ()
setwd ('/home/trademacehr/projects/NSF-DB Plant Growth/Exp2017/microcoreImages/')

# set treatment (1 = control; 2 = gridling; 3 = compression; 4 = double compression) 
#----------------------------------------------------------------------------------------
treatment <- 3

# list image files
#----------------------------------------------------------------------------------------
imagesNames <- list.files (path = paste0 ('./HF-Exp2017-LowRes/T',treatment,'/'), 
                           pattern = '_rotated.jpg')

jsonFiles <- list.files (path = paste0 ('./data/T',treatment,'/'), pattern = '.json')

# loop over images
#----------------------------------------------------------------------------------------
for (i in 1:length (imagesNames)) {
  
  # read the image file
  img <- raster (paste0 ('./HF-Exp2017-LowRes/T',treatment,'/',imagesNames [i]))
  
  # extract treeID, sampling height and date from image's name
  treeID <- substr (strsplit (imagesNames [i], split = '-') [[1]] [1], 2, 3)
  samplingHeight <- substr (strsplit (imagesNames [i], split = '-') [[1]] [2], 3, 3)
  samplingDate   <- as.POSIXct (substr (strsplit (imagesNames [i], split = '-') [[1]] [3], 1, 10),
                                format = '%Y.%m.%d')
  print (paste0 ('Found:',treeID,'.',treatment,samplingHeight,' ',samplingDate,'.'))
  
  # find json file with coordinate
  for (f in 1:length (jsonFiles)) {
    temp <- fromJSON (file = paste0 ('./data/T',treatment,'/',jsonFiles [f])) 
    if (substr (temp [['sampleID']], 1, 2) == treeID &
        substr (temp [['sampleID']], 6, 6) == samplingHeight &
        temp [['sampleDate']] == samplingDate) {
      data <- temp
      break  
    }
    print (temp [['sampleDate']])
    print (substr (temp [['sampleID']], 1, 2))
    print (substr (temp [['sampleID']], 6, 6))
  }
  rm (temp)  
  print (paste0 ('Found json file:',substr (data [['sampleID']], 1, 2),'.',treatment,
                 substr (data [['sampleID']], 6, 6),' ',data [['sampleDate']],'.'))
  print (paste0 ('json file name: ',jsonFiles [f]))
  
  # get marker coordinates
  len <- length (data [['ringData']])
  xs <- unlist (data [['ringData']]) [seq (2, len*7, by = 7)] 
  ys <- unlist (data [['ringData']]) [seq (3, len*7, by = 7)]
    
  # open ploting device
  png (paste0 (substr (imagesNames [i], 1, 18),'_check.png'),
       width = dim (img) [2],
       height = dim (img) [1])
  
  # plot that image
  plot (img,
        col = colorRampPalette (brewer.pal (n = 11, name = "RdGy"), bias = 0.2,
                                interpolate = 'spline') (30))
  points (x  = xs,
          y  = ys,
          pch = 19, 
          #cex = 1,
          cex = 20,
          col = 'cornflowerblue')
  
  # close device
  dev.off ()
}

# reset working directory
#----------------------------------------------------------------------------------------
setwd (previousWD)
#========================================================================================
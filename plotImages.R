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
setwd ('/media/TREE/PlantGrowth/data/microcores/images/Exp2017/lowResolutionRotatedForAnalysis/')

# list image files
#----------------------------------------------------------------------------------------
imagesNames <- list.files (path = './', pattern = '.jpg')

jsonFiles <- list.files (path = '../../../woodAnatomy/Exp2017/ringWidthTRIAD/', pattern = '.json')

# loop over images
#----------------------------------------------------------------------------------------
for (i in 1:length (imagesNames)) {
  
  # read the image file
  img <- raster (imagesNames [i])
  
  # extract treeID, sampling height and date from image's name
  if (substr (imagesNames [i], 1, 1) == 'P') {
    treeID <- substr (strsplit (imagesNames [i], split = '-') [[1]] [1], 2, 3)
    samplingHeight <- substr (strsplit (imagesNames [i], split = '-') [[1]] [2], 3, 3)
    samplingDate   <- as.POSIXct (substr (strsplit (imagesNames [i], split = '-') [[1]] [3], 1, 10),
                                  format = '%Y.%m.%d')
    treatment <- as.numeric (substr (imagesNames [i], 5, 6))
  } else if (substr (imagesNames [i], 1, 1) == 'T') {
    treeID <- substr (imagesNames [i], 4, 5)
    samplingHeight <- substr (imagesNames [i], 6, 6)
    samplingDate   <- as.POSIXct (substr (imagesNames [i], 8, 17),
                                  format = '%Y.%m.%d')
    treatment <- as.numeric (substr (imagesNames [i], 2, 2))
  }
  if (as.numeric (treeID) == 1 |
      as.numeric (treeID) == 2 |
      as.numeric (treeID) == 3 |
      as.numeric (treeID) == 4 |
      as.numeric (treeID) == 5 |
      as.numeric (treeID) == 7 |
      as.numeric (treeID) == 8 |
      as.numeric (treeID) == 9 |
      as.numeric (treeID) == 10 |
      as.numeric (treeID) == 11 |
      as.numeric (treeID) == 12 |
      as.numeric (treeID) == 13 |
      as.numeric (treeID) == 14 |
      as.numeric (treeID) == 15 |
      as.numeric (treeID) == 17 |
      as.numeric (treeID) == 19 |
      as.numeric (treeID) == 20 |
      as.numeric (treeID) == 21 |
      as.numeric (treeID) == 22 |
      as.numeric (treeID) == 23 |
      as.numeric (treeID) == 24 |
      as.numeric (treeID) == 25 |
      as.numeric (treeID) == 26 |
      as.numeric (treeID) == 27 |
      as.numeric (treeID) == 28 |
      as.numeric (treeID) == 29 |
      as.numeric (treeID) == 30 |
      as.numeric (treeID) == 31 |
      as.numeric (treeID) == 32 |
      as.numeric (treeID) == 33 |
      as.numeric (treeID) == 34 |
      as.numeric (treeID) == 35 |
      as.numeric (treeID) == 36 |
      as.numeric (treeID) == 38 |
      as.numeric (treeID) == 39 |
      as.numeric (treeID) == 40) {
    next
  } 
  print (imagesNames [i])
  print (paste0 ('Found:',treeID,'.',treatment,samplingHeight,' ',samplingDate,'.'))
  
  # find appropriate json file with coordinates
  for (f in 1:length (jsonFiles)) {
    temp <- fromJSON (file = paste0 (path = '../../../woodAnatomy/Exp2017/ringWidthTRIAD/',jsonFiles [f]))
    if (substr (temp [['sampleID']], 1, 2) == treeID &
        substr (temp [['sampleID']], 6, 6) == samplingHeight &
        temp [['sampleDate']] == samplingDate) {
      data <- temp
      break
    }
    # print (temp [['sampleDate']])
    # print (substr (temp [['sampleID']], 1, 2))
    # print (substr (temp [['sampleID']], 6, 6))
  }
  rm (temp)
  print (paste0 ('Found json file:',substr (data [['sampleID']], 1, 2),'.',substr (data [['sampleID']], 4, 4),
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
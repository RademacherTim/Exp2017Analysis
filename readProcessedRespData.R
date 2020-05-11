# Read all processed respiration data
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')

# define the work station
#----------------------------------------------------------------------------------------
machine <- 'timPersonal'

# define data directory
#----------------------------------------------------------------------------------------
if (machine == 'timNAU') {
  dataDir <- '/media/tim/dataDisk/PlantGrowth/data/respiration/processed/'
} else if (machine == 'timPersonal') {
  dataDir <- '../../data/respiration/processed/'
}

# Create list of all the .rds file with data for each session
#----------------------------------------------------------------------------------------
study <- 'Exp2017' # Eventually, I should loop over all studies
tmp <- list.files (path = paste0 (dataDir,study,'/'), pattern = 'sessionData.rds', recursive = TRUE)
fileList <- tibble (study =  study, fileName =  tmp); rm (tmp)

# Loop over each .rds file with processed data
#----------------------------------------------------------------------------------------
for (i in 1:dim (fileList) [1]) {
  # Read the ith file
  tmp <- readRDS (paste0 (dataDir,'/',fileList [['study']] [i],'/',fileList [['fileName']] [i]))
  if (i == 1) {
    respData <- tmp
  } else {
    # Append the session file to the end of the respData frame
    respData <- rbind (respData, tmp)
  }
}

# plot histogram
#----------------------------------------------------------------------------------------
hist (respData [['fluxRaw']])

# Find outliers
#----------------------------------------------------------------------------------------
respData [which (respData [['fluxRaw']] < 0), ]

# Set small negative values with decent measurement to 0
#----------------------------------------------------------------------------------------
# respData [['flux']] [respData [['file']] %in% c ("G-Exp2018_04pxp2_20180727_135344.csv",
#                                                  "G-Exp2018_03pxp2_20180806_140110.csv",
#                                                  "G-Exp2018_05pxp2_20180817_131839.csv",
#                                                  "G-Exp2018_02pxp1_20180824_133717.csv",
#                                                  "G-Exp2018_03pxp3_20180824_135305.csv",
#                                                  "G-Exp2018_03pxp2_20181004_133713.csv",
#                                                  "G-Exp2018_03pxp3_20181004_134337.csv",
#                                                  "G-Exp2018_11pxp1_20190423_135013.csv",
#                                                  "40p2p1_2017-11-01.txt",
#                                                  "37p4p3_2017-11-01.txt",
#                                                  "40p2p1_2018-04-05.txt")] <- 0.0
# respData [['flux']] [c (1193, 1637)] <- 0.0

# Set bad measurements to NA
#----------------------------------------------------------------------------------------
respData [['flux']] [respData [['file']] %in% c ("G-Exp2018_11pxp2_20180625_080256.csv",
                                                 "G-Exp2018_02pxp3_20180727_133311.csv",
                                                 "G-Exp2018_04pxp3_20180727_135607.csv",
                                                 "G-Exp2018_05pxp2_20180727_141552.csv",
                                                 "G-Exp2018_11pxp2_20180824_142131.csv")] <- NA

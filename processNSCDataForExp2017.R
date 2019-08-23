#========================================================================================
# Read and process NSC data using the NSCProcessR package 
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('dplyr::filter')) library ('tidyverse')
if (!existsFunction ('NSCprocessR::processNSC')) library ('NSCprocessR')
if (!existsFunction ('lubridate::month')) library ('lubridate')

# Read data from the spreadsheets
#----------------------------------------------------------------------------------------
rawData1 <- readRawNSCData (fileDir = '/home/trademacehr/projects/NSF-DB Plant Growth/Exp2017/data/',
                            fileName = 'RCLab_2019_02_09_HF_Leaves_Exp2017.xlsx',
                            IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
rawData2 <- readRawNSCData (fileDir = '/home/trademacehr/projects/NSF-DB Plant Growth/Exp2017/data/',
                            fileName = 'RCLab_2019_01_26_HF_Leaves_Exp2017.xlsx',
                            IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
rawData3 <- readRawNSCData (fileDir = '/home/trademacehr/projects/NSF-DB Plant Growth/Exp2017/data/',
                            fileName = 'RCLab_2019_03_22_HF_Roots_Exp2017.xlsx',
                            IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
rawData4 <- readRawNSCData (fileDir = '/home/trademacehr/projects/NSF-DB Plant Growth/Exp2017/data/',
                            fileName = 'RCLab_2019_04_30_HF_Wood_Exp2017.xlsx',
                            IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))

# Process the raw data
#----------------------------------------------------------------------------------------
processedData1 <- processNSCs (rawData = rawData1,
                               cvLimitSample = 0.25,
                               cvLimitTube = 0.25,
                               LCS = 'Oak')
processedData2 <- processNSCs (rawData = rawData2,
                               cvLimitSample = 0.25,
                               cvLimitTube = 0.25,
                               LCS = 'Oak')
processedData3 <- processNSCs (rawData = rawData3,
                               cvLimitSample = 0.25,
                               cvLimitTube = 0.25,
                               LCS = 'Oak')
processedData4 <- processNSCs (rawData = rawData4,
                               cvLimitSample = 0.25,
                               cvLimitTube = 0.25,
                               LCS = 'Oak')
rm (rawData1, rawData2, rawData3, rawData4)

# Produce pdf file with calibration curves
#----------------------------------------------------------------------------------------
PLOTCAL <- FALSE
if (PLOTCAL) {
  res <- plotCalibrationCurves (data = processedData1)
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
  res <- plotCalibrationCurves (data = processedData2)
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
  res <- plotCalibrationCurves (data = processedData3)
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
  res <- plotCalibrationCurves (data = processedData4)
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
}
rm (PLOTCAL)

# Add all datasets to the lab mastersheet # TTR These need work!!!!! Something is not 
# working correctly. I get a warning pertaining to the distinct () function.
#----------------------------------------------------------------------------------------
res <- addToLabMasterSheet (data = processedData1,
                            fileDir = '/home/trademacehr/projects/NSF-DB Plant Growth/',
                            fileName = 'RCLabNSCMasterSheet.xlsx',
                            IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
if (res != 0) print ('Error: addToLabMasterSheet () did not work.')
res <- addToLabMasterSheet (data = processedData2,
                            fileDir = '/home/trademacehr/projects/NSF-DB Plant Growth/',
                            fileName = 'RCLabNSCMasterSheet.xlsx',
                            IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
if (res != 0) print ('Error: addToLabMasterSheet () did not work.')
res <- addToLabMasterSheet (data = processedData3,
                            fileDir = '/home/trademacehr/projects/NSF-DB Plant Growth/',
                            fileName = 'RCLabNSCMasterSheet.xlsx',
                            IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
if (res != 0) print ('Error: addToLabMasterSheet () did not work.')
res <- addToLabMasterSheet (data = processedData4,
                            fileDir = '/home/trademacehr/projects/NSF-DB Plant Growth/',
                            fileName = 'RCLabNSCMasterSheet.xlsx',
                            IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
if (res != 0) print ('Error: addToLabMasterSheet () did not work.')

# combine all processed data into one tibble
#----------------------------------------------------------------------------------------
processedData <- rbind (processedData1, processedData2, processedData3, processedData4)
rm (processedData1, processedData2, processedData3, processedData4)

# add columns for treeID, and treatment to tibble 
#----------------------------------------------------------------------------------------
processedData [['treeID']]    <- NA
processedData [['treatment']] <- NA
processedData [['sampleHeight']] <- NA

# loop to add treeID and treatment
#----------------------------------------------------------------------------------------
for (i in 1:length (processedData [['SampleID']])) {
  string <- unlist (strsplit (x = processedData [['SampleID']] [i], split = '-')) [2]
  processedData [['treeID']]    [i] <- as.numeric (strsplit (x = string, split = '.', 
                                                             fixed = T) [[1]] [1])
  processedData [['treatment']] [i] <- as.numeric (strsplit (x = string, split = '.', 
                                                             fixed = T) [[1]] [2])
  if (substring (processedData [['Tissue']] [i], 1, 4) == 'Stem' &
      !is.na (processedData [['Tissue']] [i])){
    if (substring (processedData [['Tissue']] [i], 6, 6) == 'B' & 
        (processedData [['treatment']] [i] == 2 |
         processedData [['treatment']] [i] == 3)) {
      processedData [['sampleHeight']] [i] <- 1.0
    } else if (substring (processedData [['Tissue']] [i], 6, 6) == 'B' & 
               processedData [['treatment']] [i] == 4) {
      processedData [['sampleHeight']] [i] <- 0.5
    } else if (substring (processedData [['Tissue']] [i], 6, 6) == 'M') {
      processedData [['sampleHeight']] [i] <- 1.5
    } else if (substring (processedData [['Tissue']] [i], 6, 6) == 'A' & 
               (processedData [['treatment']] [i] == 2 | 
                processedData [['treatment']] [i] == 3)) {
      processedData [['sampleHeight']] [i] <- 2.0
    } else if (substring (processedData [['Tissue']] [i], 6, 6) == 'A' & 
               processedData [['treatment']] [i] == 4) {
      processedData [['sampleHeight']] [i] <- 2.5
    } 
    processedData [['Tissue']] [i] <- 'Stem'
  } 
}

# Get tissue specific data 
#----------------------------------------------------------------------------------------
stemData <- processedData [processedData [['Tissue']] == 'Stem', ]
leafData <- processedData [processedData [['Tissue']] == 'Leaf' | 
                           processedData [['Tissue']] == 'Needle', ]
rootData <- processedData [processedData [['Tissue']] == 'Root', ]
#========================================================================================

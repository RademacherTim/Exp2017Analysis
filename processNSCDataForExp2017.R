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
ADDTOMASTER <-FALSE
if (ADDTOMASTER) {
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
}

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


# Print sugar and starch values to detect outliers
#----------------------------------------------------------------------------------------
# for (n in 1:41) {
#   nS <- sum (stemData [['treeID']] == n)
#   print (c (n, nS))
#   print ('Sugar:')
#   for (i in 1:nS) {
#     print (c (n, nS, month (stemData [['DateOfSampleCollection']] [stemData [['treeID']] == n] [i]),
#               stemData [['ConcentrationSugarPerDW']] [stemData [['treeID']] == n] [i]))
#   }
#   print ('Starch:')
#   for (i in 1:nS) {
#     print (c (n, nS, month (stemData [['DateOfSampleCollection']] [stemData [['treeID']] == n] [i]),
#               stemData [['ConcentrationStarchPerDW']] [stemData [['treeID']] == n] [i]))
#   }
# }

# use the mean for each sampling date only
#----------------------------------------------------------------------------------------
stemData <- ungroup (stemData %>% 
                     group_by (DateOfSampleCollection, treeID, sampleHeight, treatment) %>% 
                     summarise (sugar  = mean (ConcentrationSugarPerDW), 
                                starch = mean (ConcentrationStarchPerDW)))
stemData <- stemData %>% rename (date = DateOfSampleCollection)

# Add NA for 2017-10-09 measurements for tree 1 and 3 for now.
#----------------------------------------------------------------------------------------
stemData <- add_row (stemData, .before = 164, treeID = 1, date = as_date ('2017-10-09'),
                     treatment = 1, sampleHeight = 1.5, sugar = NA, starch = NA)
stemData <- add_row (stemData, .before = 168, treeID = 3, date = as_date ('2017-10-09'),
                     treatment = 1, sampleHeight = 1.5, sugar = NA, starch = NA)
# Add NA for 2017-08-10 measurements of tree 41 at 1.0 m (B) and for 
# 2017-10-09 measurement of tree 27 at 2.5 m (A) for which the sample size was too small 
# with 3.9 mg and 4.3 mg, respectively
#----------------------------------------------------------------------------------------
stemData <- add_row (stemData, .before = 163, treeID = 41, date = as_date ('2017-08-10'),
                     treatment = 2, sampleHeight = 1.0, sugar = NA, starch = NA)
stemData <- add_row (stemData, .before = 219, treeID = 27, date = as_date ('2017-10-09'),
                     treatment = 4, sampleHeight = 2.5, sugar = NA, starch = NA)

#----------------------------------------------------------------------------------------
leafData <- ungroup (leafData %>% group_by (DateOfSampleCollection, treeID) %>% 
                     summarise (sugar  = mean (ConcentrationSugarPerDW), 
                                starch = mean (ConcentrationStarchPerDW)))
leafData <- leafData %>% rename (date = DateOfSampleCollection)

# Add NA for 2017-08-10 measurement of tree 13, 
#        for 2017-10-09 measurement of tree 41,
#    and for 2017-11-01 measurement of tree 04, 16, 31 
#----------------------------------------------------------------------------------------
leafData <- add_row (leafData, .before = 54,  treeID = 13, date = as_date ('2017-08-10'),
                     sugar = NA, starch = NA)
leafData <- add_row (leafData, .before = 123, treeID = 41, date = as_date ('2017-10-09'),
                     sugar = NA, starch = NA)
leafData <- add_row (leafData, .before = 127, treeID = 4,  date = as_date ('2017-11-03'),
                     sugar = NA, starch = NA)
leafData <- add_row (leafData, .before = 139, treeID = 16, date = as_date ('2017-11-03'),
                     sugar = NA, starch = NA)
leafData <- add_row (leafData, .before = 154, treeID = 31, date = as_date ('2017-11-03'),
                     sugar = NA, starch = NA)

rootData <- ungroup (rootData %>% group_by (DateOfSampleCollection, treeID) %>% 
                     summarise (sugar  = mean (ConcentrationSugarPerDW), 
                                starch = mean (ConcentrationStarchPerDW)))
rootData <- rootData %>% rename (date = DateOfSampleCollection)
#========================================================================================

#========================================================================================
# Draw stacked barplots of absolute values for structural carbon gain, change in 
# nonstructural carbon, and respiratory losses. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')
library ('readxl')

# Get colour schemes
#----------------------------------------------------------------------------------------
source ('plotingFunctions.R')

# Read file with response variables 
#----------------------------------------------------------------------------------------
data <- read_csv ('/home/tim/Downloads/2017ExperimentalData.csv', col_types = cols ())

# read file with response variables 
#----------------------------------------------------------------------------------------
allometricData <- read_excel (path = '/media/tim/dataDisk/PlantGrowth/data/allometry/Exp2017/allometricDataExp2017.xlsx',
                              sheet = 'allometricData')
allometricData <- filter (allometricData, tree <= 40)

# calculate change in concentration for NSC data
#----------------------------------------------------------------------------------------
data [['deltaSugarW0_250']] <- NA; data [['deltaStarchW0_250']] <- NA
data [['deltaSugarW0_200']] <- NA; data [['deltaStarchW0_200']] <- NA
data [['deltaSugarW0_150']] <- NA; data [['deltaStarchW0_150']] <- NA
data [['deltaSugarW0_100']] <- NA; data [['deltaStarchW0_100']] <- NA
data [['deltaSugarW0_50']]  <- NA; data [['deltaStarchW0_50']]  <- NA
data [['deltaSugarW1_250']] <- NA; data [['deltaStarchW1_250']] <- NA
data [['deltaSugarW1_200']] <- NA; data [['deltaStarchW1_200']] <- NA
data [['deltaSugarW1_150']] <- NA; data [['deltaStarchW1_150']] <- NA
data [['deltaSugarW1_100']] <- NA; data [['deltaStarchW1_100']] <- NA
data [['deltaSugarW1_50']]  <- NA; data [['deltaStarchW1_50']]  <- NA
for (i in 41:160) {
  data [['deltaSugarW0_250']] [i] <- data [['sugarW0_250']] [i] - data [['sugarW0_250']] [i-40]
  data [['deltaSugarW0_200']] [i] <- data [['sugarW0_200']] [i] - data [['sugarW0_200']] [i-40]
  data [['deltaSugarW0_150']] [i] <- data [['sugarW0_150']] [i] - data [['sugarW0_150']] [i-40]
  data [['deltaSugarW0_100']] [i] <- data [['sugarW0_100']] [i] - data [['sugarW0_100']] [i-40]
  data [['deltaSugarW0_50']]  [i] <- data [['sugarW0_50']]  [i] - data [['sugarW0_50']]  [i-40]
  data [['deltaStarchW0_250']] [i] <- data [['starchW0_250']] [i] - data [['starchW0_250']] [i-40]
  data [['deltaStarchW0_200']] [i] <- data [['starchW0_200']] [i] - data [['starchW0_200']] [i-40]
  data [['deltaStarchW0_150']] [i] <- data [['starchW0_150']] [i] - data [['starchW0_150']] [i-40]
  data [['deltaStarchW0_100']] [i] <- data [['starchW0_100']] [i] - data [['starchW0_100']] [i-40]
  data [['deltaStarchW0_50']]  [i] <- data [['starchW0_50']]  [i] - data [['starchW0_50']]  [i-40]
  if (i >= 121) {
    data [['deltaSugarW1_250']] [i] <- data [['sugarW1_250']] [i] - data [['sugarW1_250']] [i-120]
    data [['deltaSugarW1_200']] [i] <- data [['sugarW1_200']] [i] - data [['sugarW1_200']] [i-120]
    data [['deltaSugarW1_150']] [i] <- data [['sugarW1_150']] [i] - data [['sugarW1_150']] [i-120]
    data [['deltaSugarW1_100']] [i] <- data [['sugarW1_100']] [i] - data [['sugarW1_100']] [i-120]
    data [['deltaSugarW1_50']]  [i] <- data [['sugarW1_50']]  [i] - data [['sugarW1_50']]  [i-120]
    data [['deltaStarchW1_250']] [i] <- data [['starchW1_250']] [i] - data [['starchW1_250']] [i-120]
    data [['deltaStarchW1_200']] [i] <- data [['starchW1_200']] [i] - data [['starchW1_200']] [i-120]
    data [['deltaStarchW1_150']] [i] <- data [['starchW1_150']] [i] - data [['starchW1_150']] [i-120]
    data [['deltaStarchW1_100']] [i] <- data [['starchW1_100']] [i] - data [['starchW1_100']] [i-120]
    data [['deltaStarchW1_50']]  [i] <- data [['starchW1_50']]  [i] - data [['starchW1_50']]  [i-120]
  }
}

# Wrangle data to long format with unique labels
#----------------------------------------------------------------------------------------
respData <- data %>% select (c (2:3, 24:28)) %>% pivot_longer (cols =  c (3:7), 
                                                               names_to = 'height',
                                                               names_prefix = 'resp',
                                                               values_to = 'resp')
respData [['height']] [respData [['height']] == '050'] <- '50'
respData [['resp']] <- -respData [['resp']]
struData <- data %>% select (c (2:3, 29:33)) %>% pivot_longer (cols =  c (3:7), 
                                                               names_to = 'height',
                                                               names_prefix = 'structuralCarbonat',
                                                               values_to = 'SC')
struData [['height']] [struData [['height']] == '050'] <- '50'
nonsData <- data %>% select (c (2:3, seq (34, 42, by = 2))) %>% 
  pivot_longer (cols =  c (3:7), names_to = 'height',
                names_prefix = 'deltaSugarW0_', values_to = 'sugar0')
nonsData <- data %>% select (c (2:3, seq (35, 43, by = 2))) %>% 
  pivot_longer (cols =  c (3:7), names_to = 'height',
                names_prefix = 'deltaStarchW0_', values_to = 'starch0') %>%
  right_join (nonsData, by = c ('month', 'tree', 'height'))
nonsData <- data %>% select (c (2:3, seq (44, 52, by = 2))) %>% 
  pivot_longer (cols =  c (3:7), names_to = 'height',
                names_prefix = 'deltaSugarW1_', values_to = 'sugar1') %>%
  right_join (nonsData, by = c ('month', 'tree', 'height'))
nonsData <- data %>% select (c (2:3, seq (45, 53, by = 2))) %>% 
  pivot_longer (cols =  c (3:7), names_to = 'height',
                names_prefix = 'deltaStarchW1_', values_to = 'starch1') %>%
  right_join (nonsData, by = c ('month', 'tree', 'height'))
nonsData [['total0']] <- nonsData [['sugar0']] + nonsData [['starch0']]
nonsData [['total1']] <- nonsData [['sugar1']] + nonsData [['starch1']]
nonsData [['total']]  <- nonsData [['total0']] + nonsData [['total1']]

# Add all data together
#----------------------------------------------------------------------------------------
allData <- right_join (respData, struData, by = c ('month', 'tree', 'height')) %>% 
           right_join (nonsData, by = c ('month', 'tree', 'height')) %>%
           filter (month != 'july')

# Add treatment to the data
#----------------------------------------------------------------------------------------
allData [['treatment']] <- allometricData [['treatment']] [allData [['tree']]]

# Change height for plotting symbol 
#----------------------------------------------------------------------------------------
allData <- filter (allData, !(height != 150 & treatment == 1))
allData <- filter (allData, !(height %in% c (50, 150, 250) & treatment %in% 2:3))
allData <- filter (allData, !(height %in% c (100, 200) & treatment == 4))
allData [['height']] <- as.numeric (allData[['height']])
allData [['height']] [allData [['height']] == 100 | allData [['height']]    == 50]  <- 'B' # below
allData [['height']] [allData [['height']] == 150 & allData [['treatment']] == 4]   <- 'M' # middle
allData [['height']] [allData [['height']] == 150 & allData [['treatment']] == 1]   <- 'C' # control
allData [['height']] [allData [['height']] == 200 | allData [['height']]    == 250] <- 'A' # above

# Summarise data by group
#----------------------------------------------------------------------------------------
summaryData <- allData %>% group_by (treatment, height, month) %>% 
               summarise (meanResp    = mean (resp,   na.rm = TRUE), 
                          meanSC      = mean (SC,     na.rm = TRUE), 
                          meanNSC0    = mean (total0,  na.rm = TRUE),
                          meanSugar0  = mean (sugar0,  na.rm = TRUE),
                          meanStarch0 = mean (starch0, na.rm = TRUE),
                          meanNSC1    = mean (total1,  na.rm = TRUE),
                          meanSugar1  = mean (sugar1,  na.rm = TRUE),
                          meanStarch1 = mean (starch1, na.rm = TRUE),
                          sdResp    = sd (resp,   na.rm = TRUE), 
                          sdSC      = sd (SC,     na.rm = TRUE), 
                          sdNSC0    = sd (total0,  na.rm = TRUE),
                          sdSugar0  = sd (sugar0,  na.rm = TRUE),
                          sdStarch0 = sd (starch0, na.rm = TRUE), 
                          sdNSC1    = sd (total1,  na.rm = TRUE),
                          sdSugar1  = sd (sugar1,  na.rm = TRUE),
                          sdStarch1 = sd (starch1, na.rm = TRUE))

# Summarise data by group
#----------------------------------------------------------------------------------------
cumulativeData <- allData %>% group_by (tree, height, treatment) %>% 
                  summarise (SC   = sum (SC,     na.rm = TRUE),
                             Resp = sum (resp,   na.rm = TRUE),
                             NSC  = sum (total0, na.rm = TRUE))
cumulativeData <- cumulativeData %>% group_by (treatment, height) %>% 
                  summarise (meanSC   = mean (SC,   na.rm = TRUE),
                             meanNSC  = mean (NSC,  na.rm = TRUE),
                             meanResp = mean (Resp, na.rm = TRUE),
                             sdSC   = sd (SC,   na.rm = TRUE),
                             sdNSC  = sd (NSC,  na.rm = TRUE),
                             sdResp = sd (Resp, na.rm = TRUE))

# Create panel of three barplot for period changes and a fourth cumulative parplot panel  
#----------------------------------------------------------------------------------------
tiff (filename = '../fig/Exp2017carbonDynamicsAlongGradient.tiff', width = 1000, height = 400)
layout (matrix (1:4, nrow = 1, byrow = TRUE), width = c (1.74,1,1,2.48))
for (m in c ('august','october','november','total')) {
  
  # Check whether it is a single period or the cumulative total 
  #----------------------------------------------------------------------------------------
  if (m != 'total') {
    # get two matrices, one for structural carbon gain
    #----------------------------------------------------------------------------------------
    dataPos <- summaryData %>% ungroup %>% filter (month == m) %>% 
      select (meanResp, meanSC, meanNSC0, sdResp, sdSC, sdNSC0)  
  } else {
    dataPos <- cumulativeData %>% ungroup %>% 
      select (meanResp, meanSC, meanNSC, sdResp, sdSC, sdNSC)
  }
  dataPos <- dataPos [c (3, 7, 5, 1, 8, 4, 6, 2), ]
  dataNeg <- dataPos
  dataPos [dataPos < 0] <- 0
  dataNeg [dataNeg > 0] <- 0
  
  # switch order of rows and wrangle for barplot format
  #----------------------------------------------------------------------------------------
  error <- dataPos [, 6:4] / sqrt (10)
  dataPos <- t (as.matrix (dataPos [, 3:1]))
  dataNeg <- t (as.matrix (dataNeg [, 3:1]))
  
  # draw stacked barplot
  #----------------------------------------------------------------------------------------
  if (m == 'august') {
    par (mar = c (5, 10, 2, 1))
  } else if (m != 'total') {
      par (mar = c (5, 1, 2, 1))
  } else {
    par (mar = c (5, 1, 2, 13))
    }
  barplot (height = dataPos, horiz = TRUE, 
           xlab ='', xlim = c (ifelse (m != 'total', -18, -33), ifelse (m!= 'total', 36, 48)), 
           ylim = c (0, 20), axes = F,
           space = c (1, 1, 1, 2, 1, 2, 1, 1),
           border = 0, col = sColours [['colour']] [c (1, 3, 2)],
           cex.axis = 1.5, cex = 1.5)
  arrows (x0 = dataPos [2, ] - error [['sdSC']], 
          y0 = c (1.5, 3.5, 5.5, 8.5, 10.5, 13.5, 15.5, 17.5), 
          x1 = dataPos [2, ] + error [['sdSC']],
          length = 0.05, code = 3, angle = 90, col = '#4e5b31')
  barplot (height = dataNeg, horiz = TRUE, add = TRUE,
           border = 0, col = sColours [['colour']] [c (1, 3, 2)],
           axes = FALSE, space = c (1, 1, 1, 2, 1, 2, 1, 1))
  arrows (x0 = dataNeg [3, ] + dataNeg [1, ] - error [['sdResp']], 
          y0 = c (1.5, 3.5, 5.5, 8.5, 10.5, 13.5, 15.5, 17.5), 
          x1 = dataNeg [3, ] + dataNeg [1, ] + error [['sdResp']],
          length = 0.05, code = 3, angle = 90, col = '#be4d00')
  abline (v = 0, col = '#99999999', lwd = 1, lty = 2)
  axis (side = 1, cex.axis = 1.5)
  mtext (side = 1, line = 3, text = expression (paste (Delta, ' carbon (g)')))
  if (m == 'august') {
    axis (side = 2, at = c (1.5, 3.5, 5.5, 8.5, 10.5, 13.5, 15.5, 17.5), las = 1,
          labels = c ('girdled','double \n compressed','compressed','control','double \n compressed','compressed','double \n compressed','girdled'), cex.axis = 1.5)
    #mtext (side = 2, line = 2.5, at = c(1.5, 5.5, 10.5, 16.5), 
    #       text = c ('control','girdled','compressed','double \n compressed'))
    descriptor <- expression (paste (''))
  } else if (m == 'october') {
    descriptor <- expression (paste (''))
  } else if (m == 'november') {
    descriptor <- expression (paste (''))
    # text (x = -13, y = 19, labels = 'respiratory loss', col = '#b35806', cex = 1.0)
    # text (x =  10, y = 19, labels = 'growth', col = '#1b7837', cex = 1.0)
    # text (x =  -8, y = 17.3, labels = expression (paste ('sugar')), col = '#542788', cex = 1.0)
    # text (x =  -8, y = 15.7, labels = expression (paste ('starch')), col = '#b2abd2', cex = 1.0)
  } else {
    descriptor <- expression (paste (''))
  }
  text (x = -17, y = 20, pos = 4, labels = descriptor, col = '#003e74', cex = 2)
  
  # add line to separate the panels/plots
  if (m != 'total') abline (v = 36, col = '#333333', lwd = 2)
}
dev.off ()
#========================================================================================

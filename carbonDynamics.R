#========================================================================================
# Draw stacked barplots of absolute values for structural carbon gain, change in 
# nonstructural carbon, and respiratory losses. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')
library ('readxl')

# create colours array 
#---------------------------------------------------------------------------------------
colours <- tibble (colour   = c ('#8073ac','#e08214','#5aae61'), 
                   variable = c ('NSC'    ,'resp'   ,'SC'))

# read file with response variables 
#----------------------------------------------------------------------------------------
data <- read_csv ('/home/tim/Downloads/2017ExperimentalData - responseVariables.csv',
                  col_types = cols ())

# read file with response variables 
#----------------------------------------------------------------------------------------
allometricData <- read_excel (path = '/media/tim/dataDisk/PlantGrowth/data/allometry/Exp2017/allometricDataExp2017.xlsx',
                              sheet = 'allometricData')
allometricData <- filter (allometricData, tree <= 40)

# calculate change in concentration for NSC data
#----------------------------------------------------------------------------------------
data [['deltaSugarW250']] <- NA; data [['deltaStarchW250']] <- NA
data [['deltaSugarW200']] <- NA; data [['deltaStarchW200']] <- NA
data [['deltaSugarW150']] <- NA; data [['deltaStarchW150']] <- NA
data [['deltaSugarW100']] <- NA; data [['deltaStarchW100']] <- NA
data [['deltaSugarW50']]  <- NA; data [['deltaStarchW50']]  <- NA
for (i in 41:160) {
  data [['deltaSugarW250']] [i] <- data [['sugarW250']] [i] - data [['sugarW250']] [i-40]
  data [['deltaSugarW200']] [i] <- data [['sugarW200']] [i] - data [['sugarW200']] [i-40]
  data [['deltaSugarW150']] [i] <- data [['sugarW150']] [i] - data [['sugarW150']] [i-40]
  data [['deltaSugarW100']] [i] <- data [['sugarW100']] [i] - data [['sugarW100']] [i-40]
  data [['deltaSugarW50']]  [i] <- data [['sugarW50']]  [i] - data [['sugarW50']]  [i-40]
  data [['deltaStarchW250']] [i] <- data [['starchW250']] [i] - data [['starchW250']] [i-40]
  data [['deltaStarchW200']] [i] <- data [['starchW200']] [i] - data [['starchW200']] [i-40]
  data [['deltaStarchW150']] [i] <- data [['starchW150']] [i] - data [['starchW150']] [i-40]
  data [['deltaStarchW100']] [i] <- data [['starchW100']] [i] - data [['starchW100']] [i-40]
  data [['deltaStarchW50']]  [i] <- data [['starchW50']]  [i] - data [['starchW50']]  [i-40]
}

# wrangle data to long format with unique labels
#----------------------------------------------------------------------------------------
respData <- data %>% select (c (2:3, 18:22)) %>% pivot_longer (cols =  c (3:7), 
                                                               names_to = 'height',
                                                               names_prefix = 'resp',
                                                               values_to = 'resp')
respData [['height']] [respData [['height']] == '050'] <- '50'
respData [['resp']] <- -respData [['resp']]
struData <- data %>% select (c (2:3, 28:32)) %>% pivot_longer (cols =  c (3:7), 
                                                               names_to = 'height',
                                                               names_prefix = 'structuralCarbonat',
                                                               values_to = 'SC')
struData [['height']] [struData [['height']] == '050'] <- '50'
nonsData <- data %>% select (c (2:3, seq (33, 42, by = 2))) %>% 
            pivot_longer (cols =  c (3:7), names_to = 'height',
                          names_prefix = 'deltaSugarW', values_to = 'sugar')
nonsData <- data %>% select (c (2:3, seq (34, 42, by = 2))) %>% 
            pivot_longer (cols =  c (3:7), names_to = 'height',
                          names_prefix = 'deltaStarchW', values_to = 'starch') %>%
            right_join (nonsData, by = c ('month', 'tree', 'height'))
nonsData [['total']] <- nonsData [['sugar']] + nonsData [['starch']]
allData <- right_join (respData, struData, by = c ('month', 'tree', 'height')) %>% 
           right_join (nonsData, by = c ('month', 'tree', 'height')) %>%
           filter (month != 'july')

# add treatment to the data
#----------------------------------------------------------------------------------------
allData [['treatment']] <- allometricData [['treatment']] [allData [['tree']]]

# change height for plotting symbol 
#----------------------------------------------------------------------------------------
allData <- filter (allData, !(height != 150 & treatment == 1))
allData <- filter (allData, !(height %in% c (50, 150, 250) & treatment %in% 2:3))
allData <- filter (allData, !(height %in% c (100, 200) & treatment == 4))
allData [['height']] <- as.numeric (allData[['height']])
allData [['height']] [allData [['height']] == 100 | allData [['height']]    == 50]  <- 'B' # below
allData [['height']] [allData [['height']] == 150 & allData [['treatment']] == 4]   <- 'M' # middle
allData [['height']] [allData [['height']] == 150 & allData [['treatment']] == 1]   <- 'C' # control
allData [['height']] [allData [['height']] == 200 | allData [['height']]    == 250] <- 'A' # above

# summarise data by group
#----------------------------------------------------------------------------------------
summaryData <- allData %>% group_by (treatment, height, month) %>% 
               summarise (meanResp   = mean (resp,   na.rm = TRUE), 
                          meanSC     = mean (SC,     na.rm = TRUE), 
                          meanNSC    = mean (total,  na.rm = TRUE),
                          meanSugar  = mean (sugar,  na.rm = TRUE),
                          meanStarch = mean (starch, na.rm = TRUE))

# create panel of three barplot for period changes 
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017carbonDynamics.png', width = 800, height = 400)
layout (matrix (1:3, nrow = 1, byrow = TRUE), width = c (1.3,1,1))
for (m in c ('august','october','november')) {

  # get two matrices, one for structural carbon gain
  #----------------------------------------------------------------------------------------
  summaryDataPos <- summaryData %>% ungroup %>% filter (month == m) %>% 
                    select (meanResp, meanSC, meanNSC)
  summaryLabels <- summaryData %>% filter (month == m) %>% 
                   select (treatment, height)
  summaryLabels <- summaryLabels [8:1, ]
  summaryDataNeg <- summaryDataPos [8:1, ]; summaryDataPos <- summaryDataPos [8:1, ]
  summaryDataPos [summaryDataPos < 0] <- 0
  summaryDataNeg [summaryDataNeg > 0] <- 0
  
  # Switch order of rows
  #----------------------------------------------------------------------------------------
  summaryDataPos <- summaryDataPos [, 3:1]
  summaryDataNeg <- summaryDataNeg [, 3:1]

  # draw stacked barplot
  #----------------------------------------------------------------------------------------
  if (m == 'august') {par (mar = c (5, 6, 2, 1))} else {par (mar = c (5, 1, 2, 1))}
  barplot (height = t (as.matrix (summaryDataPos)), horiz = TRUE, 
           xlab ='', xlim = c (-20, 25), ylim = c (0, 20.5), axes = F,
           border = 0, col = colours [['colour']] [c (1, 3, 2)],
           space = c (1,2,1,2,1,2,1,1), cex.axis = 1.5, cex = 1.5)
  barplot (height = t (as.matrix (summaryDataNeg)), horiz = TRUE, add = TRUE,
           border = 0, col = colours [['colour']] [c (1, 3, 2)],
           space = c (1,2,1,2,1,2,1,1), axes = FALSE)
  abline (v = 0, col = '#99999999', lwd = 2)
  axis (side = 1, cex.axis = 1.5)
  mtext (side = 1, line = 3, text = 'carbon increment (g)')
  if (m == 'august') {
    axis (side = 2, at = c (1.5, 4.5, 6.5, 9.5, 11.5, 14.5, 16.5, 18.5), las = 1,
          labels = c ('C','B','A','B','A','B','M','A'), cex.axis = 1.5)
    mtext (side = 2, line = 2.5, at = c(1.5, 5.5, 10.5, 16.5), 
           text = c ('control','girdled','compressed','double \n compressed'))
    descriptor <- expression (paste ('1'))
  } else if (m == 'october') {
    descriptor <- expression (paste ('2'))
  } else {
    descriptor <- expression (paste ('3'))
    # text (x = -13, y = 19, labels = 'respiratory loss', col = '#b35806', cex = 1.0)
    # text (x =  10, y = 19, labels = 'growth', col = '#1b7837', cex = 1.0)
    # text (x =  -8, y = 17.3, labels = expression (paste ('sugar')), col = '#542788', cex = 1.0)
    # text (x =  -8, y = 15.7, labels = expression (paste ('starch')), col = '#b2abd2', cex = 1.0)
  }
  text (x = -17, y = 20, pos = 4, labels = descriptor, col = '#003e74', cex = 2)
  
  # add line to separate the panels/plots
  if (m != 'november') abline (v = 25, col = '#333333', lwd = 2)
}
# legend ('right', legend = c ('respiratory loss', expression (paste (delta, ' starch')), 
#                              expression (paste (delta, ' sugar')),'growth'),
#         fill = c ('#b35806','#542788','#b2abd2','#1b7837'), box.lty = 0, border = 0, 
#         bg = 'transparent', cex = 0.8)
dev.off ()
#========================================================================================

#========================================================================================
# Draw stacked barplots of absolute values for structural carbon gain, change in 
# nonstructural carbon, and respiratory losses. 
#----------------------------------------------------------------------------------------

# To-do list:
#----------------------------------------------------------------------------------------
# -TR Add a fourth panel with cumulative changes from baseline in July to November.


# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')
library ('readxl')

source ('plotingFunctions.R')

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

# make graph of respiration and growth for all periods
#----------------------------------------------------------------------------------------
par (mfrow = c (1, 1))
par (mar = c (5, 5, 1, 1))
plot (x = allData [['SC']] [allData [['treatment']] == 1],
      y = allData [['resp']] [allData [['treatment']] == 1],
      xlim = c (0, 60), ylim = c (0, -40), las = 1, 
      col = tColours [['colour']] [1], pch = 21, bg = tColours [['colour']] [1],
      xlab = 'structural growth (g C)', ylab = 'respiratory loss (g C)')
abline (lm (allData [['resp']] [allData [['treatment']] == 1] ~ 
              allData [['SC']] [allData [['treatment']] == 1]),
        col = tColours [['colour']] [1])
points (x = allData [['SC']] [allData [['treatment']] == 2 & allData [['height']] == 'A'],
        y = allData [['resp']] [allData [['treatment']] == 2 & allData [['height']] == 'A'],
        col = tColours [['colour']] [2], pch = 24, bg = tColours [['colour']] [2])
abline (lm (allData [['resp']] [allData [['treatment']] == 2 & allData [['height']] == 'A'] ~ 
              allData [['SC']] [allData [['treatment']] == 2 & allData [['height']] == 'A']),
        col = tColours [['colour']] [2])
points (x = allData [['SC']] [allData [['treatment']] == 2 & allData [['height']] == 'B'],
        y = allData [['resp']] [allData [['treatment']] == 2 & allData [['height']] == 'B'],
        col = tColours [['colour']] [2], pch = 25)
abline (lm (allData [['resp']] [allData [['treatment']] == 2 & allData [['height']] == 'B'] ~ 
              allData [['SC']] [allData [['treatment']] == 2 & allData [['height']] == 'B']),
        col = tColours [['colour']] [2], lty = 2)
points (x = allData [['SC']] [allData [['treatment']] == 3 & allData [['height']] == 'A'],
        y = allData [['resp']] [allData [['treatment']] == 3 & allData [['height']] == 'A'],
        col = tColours [['colour']] [3], pch = 24, bg = tColours [['colour']] [3])
abline (lm (allData [['resp']] [allData [['treatment']] == 3 & allData [['height']] == 'A'] ~ 
              allData [['SC']] [allData [['treatment']] == 3 & allData [['height']] == 'A']),
        col = tColours [['colour']] [3])
points (x = allData [['SC']] [allData [['treatment']] == 3 & allData [['height']] == 'B'],
        y = allData [['resp']] [allData [['treatment']] == 3 & allData [['height']] == 'B'],
        col = tColours [['colour']] [3], pch = 25)
abline (lm (allData [['resp']] [allData [['treatment']] == 3 & allData [['height']] == 'B'] ~ 
              allData [['SC']] [allData [['treatment']] == 3 & allData [['height']] == 'B']),
        col = tColours [['colour']] [3], lty = 2)
points (x = allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'A'],
        y = allData [['resp']] [allData [['treatment']] == 4 & allData [['height']] == 'A'],
        col = tColours [['colour']] [4], pch = 24, bg = tColours [['colour']] [4])
abline (lm (allData [['resp']] [allData [['treatment']] == 4 & allData [['height']] == 'A'] ~ 
              allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'A']),
        col = tColours [['colour']] [4])
points (x = allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'M'],
        y = allData [['resp']] [allData [['treatment']] == 4 & allData [['height']] == 'M'],
        col = tColours [['colour']] [4], pch = 22, bg = tColours [['colour']] [4])
abline (lm (allData [['resp']] [allData [['treatment']] == 4 & allData [['height']] == 'M'] ~ 
              allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'M']),
        col = tColours [['colour']] [4], lty = 3)
points (x = allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'B'],
        y = allData [['resp']] [allData [['treatment']] == 4 & allData [['height']] == 'B'],
        col = tColours [['colour']] [4], pch = 25)
abline (lm (allData [['resp']] [allData [['treatment']] == 4 & allData [['height']] == 'B'] ~ 
              allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'B']),
        col = tColours [['colour']] [4], lty = 2)
abline (lm (allData [['resp']] ~ allData [['SC']]), lwd = 3, col = '#666666')
legend (x = 35, y = -40, 
        legend = c ('control','above girdle','below girdle','above compression','below compression','above double compression','middle double compression','below double compression'), 
        pch = c (21, 24, 25, 24, 25, 24, 22, 25), col = tColours [['colour']] [c (1, 2, 2, 3, 3, 4, 4, 4)],
        bg = 'transparent', box.lty = 0, pt.bg = c (tColours [['colour']] [1:2], 'white',tColours [['colour']] [3],'white', tColours [['colour']] [4], tColours [['colour']] [4],'white'))
legend (x = 30, y = -40, legend = rep ('', 8), col = tColours [['colour']] [c (1, 2, 2, 3, 3, 4, 4, 4)], 
        lty = c (1, 1, 2, 1, 2, 1, 3, 2), box.lty = 0, bg = 'transparent')

# make graph of growth versus respiration by individual period
#----------------------------------------------------------------------------------------
layout (matrix (1:3, nrow = 1), widths = c (1, 1, 1))
for (m in c ('august','october','november')) {
  par (mar = c (5, 5, 1, 1))
  plot (x = allData [['SC']] [allData [['treatment']] == 1 & allData [['month']] == m],
        y = allData [['resp']] [allData [['treatment']] == 1 & allData [['month']] == m],
        xlim = c (0, 25), ylim = c (0, - 25), las = 1, 
        col = tColours [['colour']] [1], pch = 21, bg = tColours [['colour']] [1],
        xlab = 'structural growth (g C)', ylab = 'respiratory loss (g C)')
  abline (lm (allData [['resp']] [allData [['treatment']] == 1 & allData [['month']] == m] ~ 
                allData [['SC']] [allData [['treatment']] == 1 & allData [['month']] == m]),
          col = tColours [['colour']] [1])
  points (x = allData [['SC']] [allData [['treatment']] == 2 & allData [['height']] == 'A' & allData [['month']] == m],
          y = allData [['resp']] [allData [['treatment']] == 2 & allData [['height']] == 'A' & allData [['month']] == m],
          col = tColours [['colour']] [2], pch = 24, bg = tColours [['colour']] [2])
  abline (lm (allData [['resp']] [allData [['treatment']] == 2 & allData [['height']] == 'A' & allData [['month']] == m] ~ 
              allData [['SC']] [allData [['treatment']] == 2 & allData [['height']] == 'A' & allData [['month']] == m]),
          col = tColours [['colour']] [2])
  points (x = allData [['SC']] [allData [['treatment']] == 2 & allData [['height']] == 'B' & allData [['month']] == m],
          y = allData [['resp']] [allData [['treatment']] == 2 & allData [['height']] == 'B' & allData [['month']] == m],
          col = tColours [['colour']] [2], pch = 25)
  abline (lm (allData [['resp']] [allData [['treatment']] == 2 & allData [['height']] == 'B' & allData [['month']] == m] ~ 
                allData [['SC']] [allData [['treatment']] == 2 & allData [['height']] == 'B' & allData [['month']] == m]),
          col = tColours [['colour']] [2], lty = 2)
  points (x = allData [['SC']] [allData [['treatment']] == 3 & allData [['height']] == 'A' & allData [['month']] == m],
          y = allData [['resp']] [allData [['treatment']] == 3 & allData [['height']] == 'A' & allData [['month']] == m],
          col = tColours [['colour']] [3], pch = 24, bg = tColours [['colour']] [3])
  abline (lm (allData [['resp']] [allData [['treatment']] == 3 & allData [['height']] == 'A' & allData [['month']] == m] ~ 
                allData [['SC']] [allData [['treatment']] == 3 & allData [['height']] == 'A' & allData [['month']] == m]),
          col = tColours [['colour']] [3])
  points (x = allData [['SC']] [allData [['treatment']] == 3 & allData [['height']] == 'B' & allData [['month']] == m],
          y = allData [['resp']] [allData [['treatment']] == 3 & allData [['height']] == 'B' & allData [['month']] == m],
          col = tColours [['colour']] [3], pch = 25)
  abline (lm (allData [['resp']] [allData [['treatment']] == 3 & allData [['height']] == 'B' & allData [['month']] == m] ~ 
                allData [['SC']] [allData [['treatment']] == 3 & allData [['height']] == 'B' & allData [['month']] == m]),
          col = tColours [['colour']] [3], lty = 2)
  points (x = allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'A' & allData [['month']] == m],
          y = allData [['resp']] [allData [['treatment']] == 4 & allData [['height']] == 'A' & allData [['month']] == m],
          col = tColours [['colour']] [4], pch = 24, bg = tColours [['colour']] [4])
  abline (lm (allData [['resp']] [allData [['treatment']] == 4 & allData [['height']] == 'A' & allData [['month']] == m] ~ 
                allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'A' & allData [['month']] == m]),
          col = tColours [['colour']] [4])
  points (x = allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'M' & allData [['month']] == m],
          y = allData [['resp']] [allData [['treatment']] == 4 & allData [['height']] == 'M' & allData [['month']] == m],
          col = tColours [['colour']] [4], pch = 22, bg = tColours [['colour']] [4])
  abline (lm (allData [['resp']] [allData [['treatment']] == 4 & allData [['height']] == 'M' & allData [['month']] == m] ~ 
                allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'M' & allData [['month']] == m]),
          col = tColours [['colour']] [4], lty = 3)
  points (x = allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'B' & allData [['month']] == m],
          y = allData [['resp']] [allData [['treatment']] == 4 & allData [['height']] == 'B' & allData [['month']] == m],
          col = tColours [['colour']] [4], pch = 25)
  abline (lm (allData [['resp']] [allData [['treatment']] == 4 & allData [['height']] == 'B' & allData [['month']] == m] ~ 
                allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'B' & allData [['month']] == m]),
          col = tColours [['colour']] [4], lty = 2)
  abline (lm (allData [['resp']] [allData [['month']] == m] ~ allData [['SC']] [allData [['month']] == m]), lwd = 3, col = '#666666')

  if (m == 'november') {
    legend (x = 0, y = -25, 
            legend = c ('control','above girdle','below girdle','above compression','below compression','above double compression','middle double compression','below double compression'), 
            pch = c (21, 24, 25, 24, 25, 24, 22, 25), col = tColours [['colour']] [c (1, 2, 2, 3, 3, 4, 4, 4)],
            bg = 'transparent', box.lty = 0, pt.bg = c (tColours [['colour']] [1:2], 'white',tColours [['colour']] [3],'white', tColours [['colour']] [4], tColours [['colour']] [4],'white'))
  }
  text (x = 10, y = -25, labels = m, cex = 1.5)
}

# Read NSC data
#----------------------------------------------------------------------------------------
source ('../nonstructuralCarbon/processExpNSCData.R')
stemData2017 <- filter (stemData2017, treeID <= 40)

# Plot period growth as a function of nonstructural carbon concentration at end of period
#----------------------------------------------------------------------------------------
par (mfrow = c (1, 1)) 
par (mar = c (5, 5, 1, 1))
plot (y = allData [['SC']] [allData [['treatment']] == 1],
      x = select (filter (stemData2017, treatment == 1, date != as_date ('2017-07-05')), sugar) [[1]],
      xlim = c (0.3, 1.8), ylim = c (0, 45), las = 1, 
      col = tColours [['colour']] [1], pch = 21, bg = tColours [['colour']] [1],
      ylab = 'structural growth (g C)', xlab = 'soluble sugar concentration (% weight DM)')
abline (lm (allData [['SC']] [allData [['treatment']] == 1] ~ 
              select (filter (stemData2017, treatment == 1, date != as_date ('2017-07-05')), sugar) [[1]]), 
        col = tColours [['colour']] [1])
points (y = allData [['SC']] [allData [['treatment']] == 2 & allData [['height']] == 'A'],
        x = select (filter (stemData2017, treatment == 2 & date != as_date ('2017-07-05') & sampleHeight == 2), sugar) [[1]],
        col = tColours [['colour']] [2], pch = 24, bg = tColours [['colour']] [2])
abline (lm (allData [['SC']] [allData [['treatment']] == 2 & allData [['height']] == 'A'] ~ 
              select (filter (stemData2017, treatment == 2, date != as_date ('2017-07-05') & sampleHeight == 2), sugar) [[1]]), 
        col = tColours [['colour']] [2])
points (y = allData [['SC']] [allData [['treatment']] == 2 & allData [['height']] == 'B'],
        x = select (filter (stemData2017, treatment == 2 & date != as_date ('2017-07-05') & sampleHeight == 2), sugar) [[1]],
        col = tColours [['colour']] [2], pch = 25)
abline (lm (allData [['SC']] [allData [['treatment']] == 2 & allData [['height']] == 'B'] ~ 
              select (filter (stemData2017, treatment == 2, date != as_date ('2017-07-05') & sampleHeight == 1), sugar) [[1]]), 
        col = tColours [['colour']] [2], lty = 2)
points (y = allData [['SC']] [allData [['treatment']] == 3 & allData [['height']] == 'A'],
        x = select (filter (stemData2017, treatment == 3 & date != as_date ('2017-07-05') & sampleHeight == 1), sugar) [[1]],
        col = tColours [['colour']] [3], pch = 24, bg = tColours [['colour']] [3])
abline (lm (allData [['SC']] [allData [['treatment']] == 3 & allData [['height']] == 'A'] ~ 
              select (filter (stemData2017, treatment == 3, date != as_date ('2017-07-05') & sampleHeight == 2), sugar) [[1]]), 
        col = tColours [['colour']] [3])
points (y = allData [['SC']] [allData [['treatment']] == 3 & allData [['height']] == 'B'],
        x = select (filter (stemData2017, treatment == 3 & date != as_date ('2017-07-05') & sampleHeight == 1), sugar) [[1]],
        col = tColours [['colour']] [3], pch = 25)
abline (lm (allData [['SC']] [allData [['treatment']] == 3 & allData [['height']] == 'B'] ~ 
              select (filter (stemData2017, treatment == 3, date != as_date ('2017-07-05') & sampleHeight == 1), sugar) [[1]]), 
        col = tColours [['colour']] [3], lty = 2)
points (y = allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'A'],
        x = select (filter (stemData2017, treatment == 4 & date != as_date ('2017-07-05') & sampleHeight == 2.5), sugar) [[1]],
        col = tColours [['colour']] [4], pch = 24, bg = tColours [['colour']] [4])
abline (lm (allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'A'] ~ 
              select (filter (stemData2017, treatment == 4, date != as_date ('2017-07-05') & sampleHeight == 2.5), sugar) [[1]]), 
        col = tColours [['colour']] [4])
points (y = allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'M'],
        x = select (filter (stemData2017, treatment == 4 & date != as_date ('2017-07-05') & sampleHeight == 1.5), sugar) [[1]],
        col = tColours [['colour']] [4], pch = 22, bg = tColours [['colour']] [4])
abline (lm (allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'M'] ~ 
              select (filter (stemData2017, treatment == 4, date != as_date ('2017-07-05') & sampleHeight == 1.5), sugar) [[1]]), 
        col = tColours [['colour']] [4], lty = 3)
points (y = allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'B'],
        x = select (filter (stemData2017, treatment == 4 & date != as_date ('2017-07-05') & sampleHeight == 0.5), sugar) [[1]],
        col = tColours [['colour']] [4], pch = 25)
abline (lm (allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'B'] ~ 
              select (filter (stemData2017, treatment == 4, date != as_date ('2017-07-05') & sampleHeight == 0.5), sugar) [[1]]), 
        col = tColours [['colour']] [4], lty = 2)
abline (lm (allData [['SC']] ~ stemData2017 [['sugar']] [81:320]), lwd = 3, col = '#666666')
legend (x = 1.3, y = 45, 
        legend = c ('control','above girdle','below girdle','above compression','below compression','above double compression','middle double compression','below double compression'), 
        pch = c (21, 24, 25, 24, 25, 24, 22, 25), col = tColours [['colour']] [c (1, 2, 2, 3, 3, 4, 4, 4)],
        bg = 'transparent', box.lty = 0, pt.bg = c (tColours [['colour']] [1:2], 'white',tColours [['colour']] [3],'white', tColours [['colour']] [4], tColours [['colour']] [4],'white'))
legend (x = 1.2, y = 45, legend = rep ('', 8), col = tColours [['colour']] [c (1, 2, 2, 3, 3, 4, 4, 4)], 
        lty = c (1, 1, 2, 1, 2, 1, 3, 2), box.lty = 0, bg = 'transparent')

# plot period growth as a function of nonstructural carbon concentration at beginning of period
#----------------------------------------------------------------------------------------
par (mfrow = c (1, 1)) 
par (mar = c (5, 5, 1, 1))
plot (y = allData [['SC']] [allData [['treatment']] == 1],
      x = select (filter (stemData2017, treatment == 1, date != as_date ('2017-11-03')), sugar) [[1]],
      xlim = c (0.3, 1.8), ylim = c (0, 45), las = 1, 
      col = tColours [['colour']] [1], pch = 21, bg = tColours [['colour']] [1],
      ylab = 'structural growth (g C)', xlab = 'soluble sugar concentration (% weight DM)')
abline (lm (allData [['SC']] [allData [['treatment']] == 1] ~ 
              select (filter (stemData2017, treatment == 1, date != as_date ('2017-11-03')), sugar) [[1]]), 
        col = tColours [['colour']] [1])
points (y = allData [['SC']] [allData [['treatment']] == 2 & allData [['height']] == 'A'],
        x = select (filter (stemData2017, treatment == 2 & date != as_date ('2017-11-03') & sampleHeight == 2), sugar) [[1]],
        col = tColours [['colour']] [2], pch = 24, bg = tColours [['colour']] [2])
abline (lm (allData [['SC']] [allData [['treatment']] == 2 & allData [['height']] == 'A'] ~ 
              select (filter (stemData2017, treatment == 2, date != as_date ('2017-11-03') & sampleHeight == 2), sugar) [[1]]), 
        col = tColours [['colour']] [2])
points (y = allData [['SC']] [allData [['treatment']] == 2 & allData [['height']] == 'B'],
        x = select (filter (stemData2017, treatment == 2 & date != as_date ('2017-11-03') & sampleHeight == 2), sugar) [[1]],
        col = tColours [['colour']] [2], pch = 25)
abline (lm (allData [['SC']] [allData [['treatment']] == 2 & allData [['height']] == 'B'] ~ 
              select (filter (stemData2017, treatment == 2, date != as_date ('2017-11-03') & sampleHeight == 1), sugar) [[1]]), 
        col = tColours [['colour']] [2], lty = 2)
points (y = allData [['SC']] [allData [['treatment']] == 3 & allData [['height']] == 'A'],
        x = select (filter (stemData2017, treatment == 3 & date != as_date ('2017-11-03') & sampleHeight == 1), sugar) [[1]],
        col = tColours [['colour']] [3], pch = 24, bg = tColours [['colour']] [3])
abline (lm (allData [['SC']] [allData [['treatment']] == 3 & allData [['height']] == 'A'] ~ 
              select (filter (stemData2017, treatment == 3, date != as_date ('2017-11-03') & sampleHeight == 2), sugar) [[1]]), 
        col = tColours [['colour']] [3])
points (y = allData [['SC']] [allData [['treatment']] == 3 & allData [['height']] == 'B'],
        x = select (filter (stemData2017, treatment == 3 & date != as_date ('2017-11-03') & sampleHeight == 1), sugar) [[1]],
        col = tColours [['colour']] [3], pch = 25)
abline (lm (allData [['SC']] [allData [['treatment']] == 3 & allData [['height']] == 'B'] ~ 
              select (filter (stemData2017, treatment == 3, date != as_date ('2017-11-03') & sampleHeight == 1), sugar) [[1]]), 
        col = tColours [['colour']] [3], lty = 2)
points (y = allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'A'],
        x = select (filter (stemData2017, treatment == 4 & date != as_date ('2017-11-03') & sampleHeight == 2.5), sugar) [[1]],
        col = tColours [['colour']] [4], pch = 24, bg = tColours [['colour']] [4])
abline (lm (allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'A'] ~ 
              select (filter (stemData2017, treatment == 4, date != as_date ('2017-11-03') & sampleHeight == 2.5), sugar) [[1]]), 
        col = tColours [['colour']] [4])
points (y = allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'M'],
        x = select (filter (stemData2017, treatment == 4 & date != as_date ('2017-11-03') & sampleHeight == 1.5), sugar) [[1]],
        col = tColours [['colour']] [4], pch = 22, bg = tColours [['colour']] [4])
abline (lm (allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'M'] ~ 
              select (filter (stemData2017, treatment == 4, date != as_date ('2017-11-03') & sampleHeight == 1.5), sugar) [[1]]), 
        col = tColours [['colour']] [4], lty = 3)
points (y = allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'B'],
        x = select (filter (stemData2017, treatment == 4 & date != as_date ('2017-11-03') & sampleHeight == 0.5), sugar) [[1]],
        col = tColours [['colour']] [4], pch = 25)
abline (lm (allData [['SC']] [allData [['treatment']] == 4 & allData [['height']] == 'B'] ~ 
              select (filter (stemData2017, treatment == 4, date != as_date ('2017-11-03') & sampleHeight == 0.5), sugar) [[1]]), 
        col = tColours [['colour']] [4], lty = 2)
abline (lm (allData [['SC']] ~ stemData2017 [['sugar']] [81:320]), lwd = 3, col = '#666666')
legend (x = 1.3, y = 45, 
        legend = c ('control','above girdle','below girdle','above compression','below compression','above double compression','middle double compression','below double compression'), 
        pch = c (21, 24, 25, 24, 25, 24, 22, 25), col = tColours [['colour']] [c (1, 2, 2, 3, 3, 4, 4, 4)],
        bg = 'transparent', box.lty = 0, pt.bg = c (tColours [['colour']] [1:2], 'white',tColours [['colour']] [3],'white', tColours [['colour']] [4], tColours [['colour']] [4],'white'))
legend (x = 1.2, y = 45, legend = rep ('', 8), col = tColours [['colour']] [c (1, 2, 2, 3, 3, 4, 4, 4)], 
        lty = c (1, 1, 2, 1, 2, 1, 3, 2), box.lty = 0, bg = 'transparent')


# plot residual of respiratory loss and structural carbon gain against sugar concentration
#----------------------------------------------------------------------------------------
plot (y = residuals (lm (allData [['resp']] ~ allData [['SC']])),
      x = stemData2017 [['sugar']] [81:320], 
      xlab = 'wood sugar concentration (%weight DM)', ylab = 'residuals between growth and respiration')

# summarise data by group
#----------------------------------------------------------------------------------------
summaryData <- allData %>% group_by (treatment, height, month) %>% 
               summarise (meanResp   = mean (resp,   na.rm = TRUE), 
                          meanSC     = mean (SC,     na.rm = TRUE), 
                          meanNSC    = mean (total,  na.rm = TRUE),
                          meanSugar  = mean (sugar,  na.rm = TRUE),
                          meanStarch = mean (starch, na.rm = TRUE),
                          sdResp   = sd (resp,   na.rm = TRUE), 
                          sdSC     = sd (SC,     na.rm = TRUE), 
                          sdNSC    = sd (total,  na.rm = TRUE),
                          sdSugar  = sd (sugar,  na.rm = TRUE),
                          sdStarch = sd (starch, na.rm = TRUE))

# summarise data by group
#----------------------------------------------------------------------------------------
cumulativeData <- allData %>% group_by (tree, height, treatment) %>% 
                  summarise (SC   = sum (SC, na.rm = TRUE),
                             Resp = sum (resp, na.rm = TRUE),
                             NSC  = sum (total, na.rm = TRUE))
cumulativeData <- cumulativeData %>% group_by (treatment, height) %>% 
                  summarise (meanSC   = mean (SC,   na.rm = TRUE),
                             meanNSC  = mean (NSC,  na.rm = TRUE),
                             meanResp = mean (Resp, na.rm = TRUE),
                             sdSC   = sd (SC,   na.rm = TRUE),
                             sdNSC  = sd (NSC,  na.rm = TRUE),
                             sdResp = sd (Resp, na.rm = TRUE))
  
# plot average wood sugar concentration versus cumulative growth
#----------------------------------------------------------------------------------------
datSugar <- data %>% select (c (2:3, seq (6, 14, by = 2))) %>% 
  pivot_longer (cols =  c (3:7), names_to = 'height',
                names_prefix = 'sugar', values_to = 'sugar')
datSugar <- datSugar %>% filter (!is.na (sugar)) %>% group_by (tree, height) %>% 
            summarise (mean = mean (sugar))
cumGrowth <- allData %>% group_by (tree, height, treatment) %>% 
             summarise (SC = sum (SC, na.rm = TRUE))
png ('../fig/Exp2017SolubleSugarConcentrationVsCumulativeGrowth.png', width = 600, height = 400)
plot (cumGrowth [['SC']] [cumGrowth [['treatment']] == 1],
      datSugar [['mean']] [cumGrowth [['treatment']] == 1], pch = 21,
      col = tColours [['colour']] [1], bg = tColours [['colour']] [1], 
      xlab = 'cumulative structural growth (g C)', ylab = 'mean soluble sugar concentration (% dry weight)',
      xlim = c (0, 80), ylim = c (0.5, 2.5), axes = FALSE)
axis (side = 1, cex = 1.3); axis (side = 2, las = 1, cex = 1.3)
for (i in 2:4) {
  heights <- c ('A','B') 
  if (i == 4) heights <- c ('A','B','C') 
  for (h in heights) {
    con <- cumGrowth [['treatment']] == i & cumGrowth [['height']] == h
    points (cumGrowth [['SC']] [con],
            datSugar [['mean']] [con], pch = ifelse (h == 'A', 24, ifelse (h == 'B', 25, 22)),
            col = tColours [['colour']] [i], bg = ifelse (h == 'A', tColours [['colour']] [i], 'white')) 
  }
}
legend (x = 50, y = 2.5, 
        legend = c ('control','above girdle','below girdle','above compression','below compression','above double compression','middle double compression','below double compression'), 
        pch = c (21, 24, 25, 24, 25, 24, 22, 25), col = tColours [['colour']] [c (1, 2, 2, 3, 3, 4, 4, 4)],
        bg = 'transparent', box.lty = 0, pt.bg = c (tColours [['colour']] [1:2], 'white',tColours [['colour']] [3],'white', tColours [['colour']] [4], tColours [['colour']] [4],'white'))
dev.off ()

# create panel of three barplot for period changes 
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017carbonDynamicsWithTotal.png', width = 1000, height = 400)
layout (matrix (1:4, nrow = 1, byrow = TRUE), width = c (1.3,1,1,1.3))
for (m in c ('august','october','november','total')) {

  # Check whether it is a single period or the cumulative total 
  #----------------------------------------------------------------------------------------
  if (m != 'total') {
  # get two matrices, one for structural carbon gain
  #----------------------------------------------------------------------------------------
    dataPos <- summaryData %>% ungroup %>% filter (month == m) %>% 
               select (meanResp, meanSC, meanNSC, sdResp, sdSC, sdNSC)  
  } else {
    dataPos <- cumulativeData %>% ungroup %>% 
               select (meanResp, meanSC, meanNSC, sdResp, sdSC, sdNSC)
  }
  dataPos <- dataPos [c (1,3,2,5,4,7,8,6), ]
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
  if (m == 'august') {par (mar = c (5, 6, 2, 1))} else {par (mar = c (5, 1, 2, 1))}
  barplot (height = dataPos, horiz = TRUE, 
           xlab ='', xlim = c (ifelse (m != 'total', -20, -34), ifelse (m!= 'total', 25, 37)), 
           ylim = c (0, 20.5), axes = F,
           border = 0, col = sColours [['colour']] [c (1, 3, 2)],
           space = c (1,2,1,2,1,2,1,1), cex.axis = 1.5, cex = 1.5)
  arrows (x0 = dataPos [2, ] - error [['sdSC']], 
          y0 = c (1.5, 4.5, 6.5, 9.5, 11.5, 14.5, 16.5, 18.5), 
          x1 = dataPos [2, ] + error [['sdSC']],
          length = 0.05, code = 3, angle = 90, col = '#4e5b31')
  barplot (height = dataNeg, horiz = TRUE, add = TRUE,
           border = 0, col = sColours [['colour']] [c (1, 3, 2)],
           space = c (1,2,1,2,1,2,1,1), axes = FALSE)
  arrows (x0 = dataNeg [3, ] + dataNeg [1, ] - error [['sdResp']], 
          y0 = c (1.5, 4.5, 6.5, 9.5, 11.5, 14.5, 16.5, 18.5), 
          x1 = dataNeg [3, ] + dataNeg [1, ] + error [['sdResp']],
          length = 0.05, code = 3, angle = 90, col = '#be4d00')
  abline (v = 0, col = '#99999999', lwd = 1, lty = 2)
  axis (side = 1, cex.axis = 1.5)
  mtext (side = 1, line = 3, text = expression (paste (Delta, ' carbon (g)')))
  if (m == 'august') {
    axis (side = 2, at = c (1.5, 4.5, 6.5, 9.5, 11.5, 14.5, 16.5, 18.5), las = 1,
          labels = c ('C','B','A','B','A','B','M','A'), cex.axis = 1.5)
    mtext (side = 2, line = 2.5, at = c(1.5, 5.5, 10.5, 16.5), 
           text = c ('control','girdled','compressed','double \n compressed'))
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
  if (m != 'total') abline (v = 25, col = '#333333', lwd = 2)
}
# legend ('right', legend = c ('respiratory loss', expression (paste (delta, ' starch')), 
#                              expression (paste (delta, ' sugar')),'growth'),
#         fill = c ('#b35806','#542788','#b2abd2','#1b7837'), box.lty = 0, border = 0, 
#         bg = 'transparent', cex = 0.8)
dev.off ()
#========================================================================================

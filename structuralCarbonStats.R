#========================================================================================
# This script test for differences between treatments and sampling height in structural 
# carbon related variables, such as ring widths, cell wall thickness, lumen diameter, 
# number of cell per ring and cell size. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lme4')
if (!existsFunction ('add_column')) library ('tidyverse')

# set colour scheme and ploting functions
#----------------------------------------------------------------------------------------
source ('plotingFunctions.R')
source ('processAnatomicalData.R')

# Determine the mean ring width at each point in time
#-----------------------------------------------------------------------------------------
temp1 <- data %>% filter (year == 2017) %>% 
  group_by (treatment, height, tree, period) %>%
  summarise (maxRW = max (RADDISTR.BAND, na.rm = T)) %>% 
  mutate (period = as_date (period))
temp2 <- data %>% filter (year == 2017) %>% 
  group_by (treatment, height, tree) %>%
  summarise (maxRW = max (RADDISTR.BAND, na.rm = T)) %>% 
  add_column (period = as_date ('2017-08-09'), .before = 4)
temp3 <- data %>% filter (year == 2017) %>% 
  group_by (treatment, height, tree) %>%
  summarise (maxRW = max (RADDISTR.BAND, na.rm = T)) %>% 
  add_column (period = as_date ('2017-10-09'), .before = 4)
temp4 <- data %>% filter (year == 2017) %>% 
  group_by (treatment, height, tree) %>%
  summarise (maxRW = max (RADDISTR.BAND, na.rm = T)) %>% 
  add_column (period = as_date ('2017-11-03'), .before = 4)
temp <- dplyr::union_all (temp1, temp2) %>% 
  dplyr::union_all (temp3) %>% dplyr::union_all (temp4) %>% 
  arrange (treatment, height, tree, period) %>% 
  group_by (treatment, height, tree, period) %>% 
  summarise (maxRW = min (maxRW, na.rm = TRUE)) %>% 
  rename (ringWidth = maxRW) %>% ungroup
temp [['height']] [temp [['treatment']] == 1] <- 'C'
temp <- temp %>%  mutate (treatment = factor (treatment, levels = c (4:1)),
          tree = factor (tree, levels = c (1:40)),
          height = factor (height, levels = c ('A', 'M', 'B', 'C')),
          period = factor (period))
# Add a RW increment
temp <- temp %>% group_by (tree) %>% mutate (incRW = ringWidth - min (ringWidth))

M0 <- lmer (formula = incRW ~ (1 | tree) + period + treatment:height:period, 
            data = filter (temp, period != '2017-07-03'),
            REML = TRUE)
summary (M0)

# source ring width and other anatomical data
#----------------------------------------------------------------------------------------
standardisedRW2017 <- read_csv (file = 'standardisedRW2017.csv',
                                col_types = cols ())

# wranlge standardised ring width at the end of the experiment
#----------------------------------------------------------------------------------------
standardisedRW2017 <- add_column (standardisedRW2017, 
                                  treatment = rep (allometricData [['treatment']] [1:40], 4))
RW2017 <- tibble (tree = 1, height = 'C', treatment = 1, 
                  RW = standardisedRW2017 [['RW2017at150']] [1])
for (i in 2:40) {
  if (standardisedRW2017 [['treatment']] [i] == 1) {
    RW2017 <- add_row (RW2017, tree = i, 
                       treatment = standardisedRW2017 [['treatment']] [i],
                       height = 'C', RW = standardisedRW2017 [['RW2017at150']] [i]) 
  } else if (standardisedRW2017 [['treatment']] [i] == 2 |
             standardisedRW2017 [['treatment']] [i] == 3) {
    RW2017 <- add_row (RW2017, tree = i, 
                       treatment = standardisedRW2017 [['treatment']] [i],
                       height = 'A', RW = standardisedRW2017 [['RW2017at200']] [i])
    RW2017 <- add_row (RW2017, tree = i, 
                       treatment = standardisedRW2017 [['treatment']] [i],
                       height = 'B', RW = standardisedRW2017 [['RW2017at100']] [i])
  } else if (standardisedRW2017 [['treatment']] [i] == 4) {
    RW2017 <- add_row (RW2017, tree = i, 
                       treatment = standardisedRW2017 [['treatment']] [i],
                       height = 'A', RW = standardisedRW2017 [['RW2017at250']] [i])
    RW2017 <- add_row (RW2017, tree = i, 
                       treatment = standardisedRW2017 [['treatment']] [i],
                       height = 'M', RW = standardisedRW2017 [['RW2017at150']] [i])
    RW2017 <- add_row (RW2017, tree = i, 
                       treatment = standardisedRW2017 [['treatment']] [i],
                       height = 'B', RW = standardisedRW2017 [['RW2017at050']] [i])
  }
}

# convert height, treatment and tree to factors 
RW2017 [['height']]    <-  factor (RW2017 [['height']], levels = c ('A','M','B','C'))
RW2017 [['treatment']] <- factor (RW2017 [['treatment']], levels = c (4:1))
RW2017 [['tree']]      <- factor (RW2017 [['tree']])

# fit mixed effects model with tree as random effect
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = RW ~ (1 | tree) + treatment:height, 
            data = RW2017,
            REML = TRUE)
summary (M1)
# plot (M1)
# qqnorm (resid (M1))

# extract the model parameters from the lumen diameter model
#----------------------------------------------------------------------------------------
M01Values <- tibble (beta   = getME (M1, 'beta'), 
                     se     = as.numeric (coef (summary (M1)) [, 2]), 
                     tValue = as.numeric (coef (summary (M1)) [, 3]))
M01Values <- add_column (M01Values, .before = 1,
                         treatment = c (1, 4:2, 4, 4:2),
                         height = c ('C','A','A','A','M','B','B','B'))

# add the intercept
#----------------------------------------------------------------------------------------
M01Values [['beta']] [2:length (M01Values [['beta']])] <-
  M01Values [['beta']] [2:length (M01Values [['beta']])] + M01Values [['beta']] [1]

# add y Position in plot
#----------------------------------------------------------------------------------------
M01Values [['yPos']] <- NA
M01Values [['yPos']] [M01Values [['treatment']] == 1] <- yPositions [1]
M01Values [['yPos']] [M01Values [['treatment']] == 2 & M01Values [['height']] == 'B'] <- 
  yPositions [2]
M01Values [['yPos']] [M01Values [['treatment']] == 2 & M01Values [['height']] == 'A'] <- 
  yPositions [3]
M01Values [['yPos']] [M01Values [['treatment']] == 3 & M01Values [['height']] == 'B'] <- 
  yPositions [4]
M01Values [['yPos']] [M01Values [['treatment']] == 3 & M01Values [['height']] == 'A'] <- 
  yPositions [5]
M01Values [['yPos']] [M01Values [['treatment']] == 4 & M01Values [['height']] == 'B'] <- 
  yPositions [6]
M01Values [['yPos']] [M01Values [['treatment']] == 4 & M01Values [['height']] == 'M'] <- 
  yPositions [7]
M01Values [['yPos']] [M01Values [['treatment']] == 4 & M01Values [['height']] == 'A'] <- 
  yPositions [8]

# add symbols column
#----------------------------------------------------------------------------------------
M01Values [['height']] [M01Values [['height']] == 'C'] <- 21
M01Values [['height']] [M01Values [['height']] == 'A'] <- 24
M01Values [['height']] [M01Values [['height']] == 'M'] <- 22
M01Values [['height']] [M01Values [['height']] == 'B'] <- 25

png (filename = '../fig/Exp2017RingWidth.png', width = 600) 
  # plot the cummulative cell wall area for each period
  #--------------------------------------------------------------------------------------
  par (mar = c (5, 5, 1, 0))
  plot (x = M01Values [['beta']] [M01Values [['date']] == iDate],
        y = M01Values [['yPos']] [M01Values [['date']] == iDate], 
        las = 1, xlab = 'ring width (mm)', ylab = '', axes = FALSE,
        xlim = c (0.3, 1.2), ylim = c (0, 6.6), 
        col = tColours [['colour']] [M01Values [['treatment']] [M01Values [['date']] == iDate]], 
        bg = ifelse (abs (M01Values [['tValue']] [M01Values [['date']] == iDate]) >= 2, 
                     tColours [['colour']] [M01Values[['treatment']] [M01Values [['date']] == iDate]], 
                     'white'), lwd = 2, 
        cex = 2, #abs (M01Values [['tValue']] [M01Values [['date']] == iDate]),
        pch = as.numeric (M01Values [['height']] [M01Values [['date']] == iDate]))
  
  
  # add x-axis
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = seq (0.4, 1.2, by = 0.2))
  
  # add y-axis
  #------------------------------------------------------------------------------------
  axis (side = 2, at = yPositions, labels = c ('C','B','A','B','A','B','M','A'), 
        las = 1)
  
  # add rectangle for control standard error
  #--------------------------------------------------------------------------------------
  rect (xleft = M01Values [['beta']] [M01Values [['treatment']] == 1] - 
                M01Values [['se']]   [M01Values [['treatment']] == 1],
        xright = M01Values [['beta']] [M01Values [['treatment']] == 1] + 
                 M01Values [['se']]   [M01Values [['treatment']] == 1],
        ybottom = 0.6, ytop = 6, lty = 0, col = '#aaaaaa55')
  
  # add standard error
  #--------------------------------------------------------------------------------------
  arrows (x0 = M01Values [['beta']] - M01Values [['se']],
          x1 = M01Values [['beta']] + M01Values [['se']],
          y0 = M01Values [['yPos']], 
          code = 3, length = 0.05, angle = 90, lwd = 2, col = '#333333')
  
  # plot means
  #--------------------------------------------------------------------------------------
  points (x = M01Values [['beta']],
          y = M01Values [['yPos']],
          col = tColours [['colour']] [M01Values [['treatment']]], 
          bg = ifelse (abs (M01Values [['tValue']]) >= 2, 
                       tColours [['colour']] [M01Values[['treatment']]], 
                       'white'), 
          lwd = 2, cex = 2, #abs (M01Values [['tValue']] [M01Values [['date']] == iDate]),
          pch = as.numeric (M01Values [['height']]))
  
  # add treatments
  #------------------------------------------------------------------------------------
  mtext (side = 2, line = 2, text = 'control',    at = yPositions [1])
  mtext (side = 2, line = 2, text = 'girdled',    at = mean (yPositions [c(2,3)]))
  mtext (side = 2, line = 2, text = 'compressed', at = mean (yPositions [c(4,5)]))
  mtext (side = 2, line = 3, text = 'double',     at = mean (yPositions [c(6,7,8)]))
  mtext (side = 2, line = 2, text = 'compressed', at = mean (yPositions [c(6,7,8)]))
      
  # add title
  #------------------------------------------------------------------------------------
  mtext (side = 2, line = 6, text = 'ring width', cex = 1.5)
  
  # add typical between tree variation to plot
  #----------------------------------------------------------------------------------------
  betweenTreeVar <- as_tibble (VarCorr (M1)) [1, 5] [[1]]
  text (labels = expression (paste (sigma [tree])), x = 1.2, y = 0.4, 
        cex = 1.5, pos = 2, col = '#777777')
  segments (x0 = 1.2 - betweenTreeVar, 
            x1 = 1.2, 
            y0 = 0.2, lwd = 4,
            col = '#777777')
dev.off ()

# create tibble with cell numbers per ring
#----------------------------------------------------------------------------------------
cellNumber <- tibble (tree = NA, treatment = NA, height = NA, nTotal = NA, nJul = NA, 
                      nAug = NA, nOct = NA, nNov = NA)

# determine the approximate number of cells in each ring
#----------------------------------------------------------------------------------------
for (iTree in 1:40) { # loop over trees
  
  # get treatment and determine heights
  #----------------------------------------------------------------------------------------
  treatment <- unique (data [['treatment']] [data [['tree']] == iTree])
  heights   <- unique (data [['height']] [data [['tree']] == iTree])
  
  # loop over sampling heights
  #----------------------------------------------------------------------------------------
  for (iHeight in heights) {

    # determine average number of cells in sector and sum them
    #----------------------------------------------------------------------------------------
    con <- data [['year']] == 2017 & 
           data [['tree']] == iTree & 
           data [['height']] == iHeight
    conJul <- con & data [['period']] == as_date ('2017-07-03')
    conAug <- con & data [['period']] == as_date ('2017-08-09') 
    conOct <- con & data [['period']] == as_date ('2017-10-09')
    conNov <- con & data [['period']] == as_date ('2017-11-03')
    nJul <- floor (sum (20 / data [['cellRadWidth']] [conJul], na.rm = TRUE))
    nAug <- floor (sum (20 / data [['cellRadWidth']] [conAug], na.rm = TRUE))
    nOct <- floor (sum (20 / data [['cellRadWidth']] [conOct], na.rm = TRUE))
    nNov <- floor (sum (20 / data [['cellRadWidth']] [conNov], na.rm = TRUE))
    nCells <- floor (sum (20 / data [['cellRadWidth']] [con], na.rm = TRUE))
    iH <- iHeight
    if (treatment == 1) iH <- 'C'
    cellNumber <- add_row (cellNumber, tree = iTree, treatment = treatment, height = iH, 
                           nTotal = nCells, nJul = nJul, nAug = nAug, nOct = nOct, 
                           nNov = nNov)
    
  } # end height loop
} # end tree loop

# delete first row, which is empty and 06.1M because data is no good
#----------------------------------------------------------------------------------------
cellNumber <- cellNumber [-1, ]

# Convert data to long format and add date column
#----------------------------------------------------------------------------------------
cellNumber <- pivot_longer (cellNumber, cols = c (nTotal, nJul, nAug, nOct, nNov), 
                            values_to = 'n')
cellNumber <- add_row (cellNumber, period = NA)
cellNumber [['period']] [cellNumber [['name']] == 'nJul'] <- as_date ('2017-07-03')
cellNumber [['period']] [cellNumber [['name']] == 'nAug'] <- as_date ('2017-08-09')
cellNumber [['period']] [cellNumber [['name']] == 'nOct'] <- as_date ('2017-10-09')
cellNumber [['period']] [cellNumber [['name']] == 'nNov'] <- as_date ('2017-11-03')
cellNumber [['period']] [cellNumber [['name']] == 'nTotal'] <- as_date ('2017-11-03') 

# wrangle cell number in the ring
#----------------------------------------------------------------------------------------
cellNumber [['tree']]      <- factor (cellNumber [['tree']])
cellNumber [['treatment']] <- factor (cellNumber [['treatment']], levels = c (4:1))
cellNumber [['height']]    <- factor (cellNumber [['height']], levels = c ('A','M','B','C'))
cellNumber [['period']]    <- factor (cellNumber [['period']])

# fit mixed effect model with tree as random effect
#----------------------------------------------------------------------------------------
M2 <- lmer (formula = n ~ (1 | tree) + period + period:treatment:height,
            data = filter (cellNumber, name != 'nTotal'),
            REML = TRUE)
summary (M2)
# plot (M2)
# qqnorm (resid (M2))

# extract the model parameters from the cell number model
#----------------------------------------------------------------------------------------
M02Values <- tibble (beta   = getME (M2, 'beta'), 
                     se     = as.numeric (coef (summary (M2)) [, 2]), 
                     tValue = as.numeric (coef (summary (M2)) [, 3]))
M02Values <- add_column (M02Values, .before = 1,
                         treatment = c (1, 4:2, 4, 4:2),
                         height = c ('C','A','A','A','M','B','B','B'))

# add the intercept
#----------------------------------------------------------------------------------------
M02Values [['beta']] [2:length (M02Values [['beta']])] <-
  M02Values [['beta']] [2:length (M02Values [['beta']])] + M02Values [['beta']] [1]

# add y Position in plot
#----------------------------------------------------------------------------------------
M02Values [['yPos']] <- NA
M02Values [['yPos']] [M02Values [['treatment']] == 1] <- yPositions [1]
M02Values [['yPos']] [M02Values [['treatment']] == 2 & M02Values [['height']] == 'B'] <- 
  yPositions [2]
M02Values [['yPos']] [M02Values [['treatment']] == 2 & M02Values [['height']] == 'A'] <- 
  yPositions [3]
M02Values [['yPos']] [M02Values [['treatment']] == 3 & M02Values [['height']] == 'B'] <- 
  yPositions [4]
M02Values [['yPos']] [M02Values [['treatment']] == 3 & M02Values [['height']] == 'A'] <- 
  yPositions [5]
M02Values [['yPos']] [M02Values [['treatment']] == 4 & M02Values [['height']] == 'B'] <- 
  yPositions [6]
M02Values [['yPos']] [M02Values [['treatment']] == 4 & M02Values [['height']] == 'M'] <- 
  yPositions [7]
M02Values [['yPos']] [M02Values [['treatment']] == 4 & M02Values [['height']] == 'A'] <- 
  yPositions [8]

# add symbols column
#----------------------------------------------------------------------------------------
M02Values [['height']] [M02Values [['height']] == 'C'] <- 21
M02Values [['height']] [M02Values [['height']] == 'A'] <- 24
M02Values [['height']] [M02Values [['height']] == 'M'] <- 22
M02Values [['height']] [M02Values [['height']] == 'B'] <- 25

png (filename = '../fig/Exp2017NumberOfCells.png', width = 600) 
  # plot the number of cell in final ring
  #--------------------------------------------------------------------------------------
  par (mar = c (5, 5, 1, 0))
  plot (x = M02Values [['beta']] [M02Values [['date']] == iDate],
        y = M02Values [['yPos']] [M02Values [['date']] == iDate], 
        las = 1, xlab = 'number of cells (n)', ylab = '', axes = FALSE,
        xlim = c (10, 80), ylim = c (0, 6.6), 
        col = tColours [['colour']] [M02Values [['treatment']] [M02Values [['date']] == iDate]], 
        bg = ifelse (abs (M02Values [['tValue']] [M02Values [['date']] == iDate]) >= 2, 
                     tColours [['colour']] [M02Values[['treatment']] [M02Values [['date']] == iDate]], 
                     'white'), lwd = 2, 
        cex = 2, #abs (M02Values [['tValue']] [M02Values [['date']] == iDate]),
        pch = as.numeric (M02Values [['height']] [M02Values [['date']] == iDate]))
  
  
  # add x-axis
  #--------------------------------------------------------------------------------------
  axis (side = 1, at = seq (10, 80, by = 10))
  
  # add y-axis
  #------------------------------------------------------------------------------------
  axis (side = 2, at = yPositions, labels = c ('C','B','A','B','A','B','M','A'), 
        las = 1)
  
  # add rectangle for control standard error
  #--------------------------------------------------------------------------------------
  rect (xleft = M02Values [['beta']] [M02Values [['treatment']] == 1] - 
          M02Values [['se']]   [M02Values [['treatment']] == 1],
        xright = M02Values [['beta']] [M02Values [['treatment']] == 1] + 
          M02Values [['se']]   [M02Values [['treatment']] == 1],
        ybottom = 0.6, ytop = 6, lty = 0, col = '#aaaaaa55')
  
  # add standard error
  #--------------------------------------------------------------------------------------
  arrows (x0 = M02Values [['beta']] - M02Values [['se']],
          x1 = M02Values [['beta']] + M02Values [['se']],
          y0 = M02Values [['yPos']], 
          code = 3, length = 0.05, angle = 90, lwd = 2, col = '#333333')
  
  # plot means
  #--------------------------------------------------------------------------------------
  points (x = M02Values [['beta']],
          y = M02Values [['yPos']],
          col = tColours [['colour']] [M02Values [['treatment']]], 
          bg = ifelse (abs (M02Values [['tValue']]) >= 2, 
                       tColours [['colour']] [M02Values[['treatment']]], 
                       'white'), 
          lwd = 2, cex = 2, #abs (M02Values [['tValue']] [M02Values [['date']] == iDate]),
          pch = as.numeric (M02Values [['height']]))
  
  # add treatments
  #------------------------------------------------------------------------------------
  mtext (side = 2, line = 2, text = 'control',    at = yPositions [1])
  mtext (side = 2, line = 2, text = 'girdled',    at = mean (yPositions [c(2,3)]))
  mtext (side = 2, line = 2, text = 'compressed', at = mean (yPositions [c(4,5)]))
  mtext (side = 2, line = 3, text = 'double',     at = mean (yPositions [c(6,7,8)]))
  mtext (side = 2, line = 2, text = 'compressed', at = mean (yPositions [c(6,7,8)]))
  
  # add title
  #------------------------------------------------------------------------------------
  mtext (side = 2, line = 6, text = 'ring width', cex = 1.5)
  
  # add typical between tree variation to plot
  #----------------------------------------------------------------------------------------
  betweenTreeVar <- as_tibble (VarCorr (M2)) [1, 5] [[1]]
  text (labels = expression (paste (sigma [tree])), x = 80, y = 0.4, 
        cex = 1.5, pos = 2, col = '#777777')
  segments (x0 = 80 - betweenTreeVar, 
            x1 = 80, 
            y0 = 0.2, lwd = 4,
            col = '#777777')
dev.off ()

# wrangle wood anatomy data
#----------------------------------------------------------------------------------------
woodAnatomy <- data [data [['year']] == 2017, ]
woodAnatomy [['tree']]      <- factor (woodAnatomy [['tree']]) 
woodAnatomy [['treatment']] <- factor (woodAnatomy [['treatment']], levels = c (4:1))
woodAnatomy [['height']] [woodAnatomy [['treatment']] == 1] <- 'C'
woodAnatomy [['height']] <- factor (woodAnatomy [['height']], levels = c ('A','M','B','C'))
woodAnatomy <- select (woodAnatomy, MRW, LA, DRAD, DTAN, CWA, CWAACC, CWTTAN, CWTALL, period, cellRadWidth, cellTanWidth, tree, treatment, height)

# create factor stating if it was formed before or after the experiment
#----------------------------------------------------------------------------------------
woodAnatomy <- add_column (woodAnatomy, 
                           afterOnset = factor (ifelse (woodAnatomy [['period']] > as_date ('2017-07-03'), 
                                                        TRUE, FALSE), levels = c (FALSE, TRUE)))

# fit mixed effects model for radial cell size before the experiment
#----------------------------------------------------------------------------------------
M4 <- lmer (formula = cellRadWidth ~ (1 | tree) + treatment:height, 
            data = woodAnatomy [woodAnatomy [['period']] == as_date ('2017-07-03'), ], 
            REML = TRUE)
summary (M4)

# fit mixed effects model for radial cell size over the proportion that formed after the start of the experimental 
M5 <- lmer (formula = cellRadWidth ~ (1 | tree) + treatment:height, 
            data = woodAnatomy [woodAnatomy [['period']] != as_date ('2017-07-03'), ], 
            REML = TRUE)
summary (M5)

# fit mixed effects model to radial cell size including time of formation
# M6 <- lmer (formula = cellRadWidth ~ (1 | tree) + treatment:height:factor (afterOnset) + factor (afterOnset), 
#             data = woodAnatomy, 
#             REML = FALSE)
# summary (M6)
M6.1 <- lmer (formula = cellRadWidth ~ (1 | tree) + treatment:height:factor (period) + factor (period), 
            data = woodAnatomy, 
            REML = FALSE)
summary (M6.1)

# fit a mixed effects model to look at cell lumen diameter
# M7.0 <- lmer (formula = DRAD ~ (1 | tree) + treatment:height:factor (afterOnset) + factor (afterOnset), 
#               data = woodAnatomy, 
#               REML = FALSE)
# summary (M7.0)
M7.1 <- lmer (formula = DRAD ~ (1 | tree) + treatment:height:factor (period) + factor (period), 
              data = woodAnatomy, 
              REML = TRUE)
summary (M7.1)
#anova (M7.0, M7.1)

# wrangle individual variable data to calculate standard deviations for effect size 
# determination later 
#----------------------------------------------------------------------------------------
indVar <- woodAnatomy %>% group_by (treatment, height) %>% select (DRAD, treatment, height)

# extract the model parameters from the lumen diameter model
#----------------------------------------------------------------------------------------
M07Values <- tibble (beta   = getME (M7.1, 'beta'), 
                     se     = as.numeric (coef (summary (M7.1)) [, 2]), 
                     tValue = as.numeric (coef (summary (M7.1)) [, 3]))
M07Values <- add_column (M07Values, .before = 1,
                         date = c (c ('jul', 'aug', 'oct', 'nov'), rep ('jul', 7), 
                                   rep ('aug', 7), rep ('oct', 7), rep ('nov', 7)),
                         treatment = c (rep (1, 4), rep (c (4:2, 4, 4:2), 4)),
                         height = c (rep ('C', 4), rep (c ('A','A','A','M','B','B','B'), 4)))

# add the date effect
#----------------------------------------------------------------------------------------
M07Values [['beta']] [M07Values [['date']] == 'aug'] [2:length (M07Values [['beta']] [M07Values [['date']] == 'aug'])] <- 
  M07Values [['beta']] [M07Values [['date']] == 'aug'] [2:length (M07Values [['beta']] [M07Values [['date']] == 'aug'])] + 
  M07Values [['beta']] [M07Values [['date']] == 'aug'] [1]
M07Values [['beta']] [M07Values [['date']] == 'oct'] [2:length (M07Values [['beta']] [M07Values [['date']] == 'oct'])] <- 
  M07Values [['beta']] [M07Values [['date']] == 'oct'] [2:length (M07Values [['beta']] [M07Values [['date']] == 'oct'])] + 
  M07Values [['beta']] [M07Values [['date']] == 'oct'] [1]
M07Values [['beta']] [M07Values [['date']] == 'nov'] [2:length (M07Values [['beta']] [M07Values [['date']] == 'nov'])] <- 
  M07Values [['beta']] [M07Values [['date']] == 'nov'] [2:length (M07Values [['beta']] [M07Values [['date']] == 'nov'])] + 
  M07Values [['beta']] [M07Values [['date']] == 'nov'] [1]

# add the intercept, aka July baseline effect
#----------------------------------------------------------------------------------------
M07Values [['beta']] [2:length (M07Values [['beta']])] <-
  M07Values [['beta']] [2:length (M07Values [['beta']])] + M07Values [['beta']] [1]

# add y Position in plot
#----------------------------------------------------------------------------------------
M07Values [['yPos']] <- NA
M07Values [['yPos']] [M07Values [['treatment']] == 1] <- yPositions [1]
M07Values [['yPos']] [M07Values [['treatment']] == 2 & M07Values [['height']] == 'B'] <- 
  yPositions [2]
M07Values [['yPos']] [M07Values [['treatment']] == 2 & M07Values [['height']] == 'A'] <- 
  yPositions [3]
M07Values [['yPos']] [M07Values [['treatment']] == 3 & M07Values [['height']] == 'B'] <- 
  yPositions [4]
M07Values [['yPos']] [M07Values [['treatment']] == 3 & M07Values [['height']] == 'A'] <- 
  yPositions [5]
M07Values [['yPos']] [M07Values [['treatment']] == 4 & M07Values [['height']] == 'B'] <- 
  yPositions [6]
M07Values [['yPos']] [M07Values [['treatment']] == 4 & M07Values [['height']] == 'M'] <- 
  yPositions [7]
M07Values [['yPos']] [M07Values [['treatment']] == 4 & M07Values [['height']] == 'A'] <- 
  yPositions [8]

# add symbols column
#----------------------------------------------------------------------------------------
M07Values [['height']] [M07Values [['height']] == 'C'] <- 21
M07Values [['height']] [M07Values [['height']] == 'A'] <- 24
M07Values [['height']] [M07Values [['height']] == 'M'] <- 22
M07Values [['height']] [M07Values [['height']] == 'B'] <- 25

# create layout for the mixed model-based cummulative cell wall area plot 
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017LumenDiameter.png', width = 600, height = 450)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths = c (1.4, 1, 1, 1.1))
  
  # loop over dates to create on plot of fractional cumulative cell wall area for each period
  #----------------------------------------------------------------------------------------
  for (iDate in c ('jul','aug','oct','nov')) {
    
    # choose appropriate plot margins 
    #--------------------------------------------------------------------------------------
    if (iDate == 'jul') {
      par (mar = c (7, 7, 1, 0))
    } else if (iDate == 'aug' | iDate == 'oct') {
      par (mar = c (7, 0, 1, 0))
    } else {
      par (mar = c (7, 0, 1, 1))
    }
    
    # check effect size and colour symbols accordingly
    #--------------------------------------------------------------------------------------
    deltaMu <- M07Values [['beta']] [M07Values [['date']] == iDate] - 
      M07Values [['beta']] [M07Values [['date']] == iDate] [1]
    d <- 0.5
    for (i in 2:8) {
      # determine treatment and sampling height
      if (i >= 3) {
        if (i == 2) {
          t <- 2; h <- 'B'
        } else {
          t <- 2; h <- 'A'
        }
      } else if (i <= 5) {
        if (i == 4) {
          t <- 3; h <- 'B'
        } else {
          t <- 3; h <- 'A'
        }
      } else {
        if (i == 6) {
          t <- 4; h <- 'B'
        } else if (i == 7) {
          t <- 4; h <- 'M'
        } else {
          t <- 4; h <- 'A'
        }
      }
      d <- c (d, deltaMu [i] / 
                 (sd (indVar [['DRAD']] [indVar [['treatment']] %in% c (1, t) & 
                                         indVar [['height']] %in% c ('C',h)], na.rm = TRUE)))
    }
    
    # plot the cummulative cell wall area for each period
    #--------------------------------------------------------------------------------------
    plot (x = M07Values [['beta']] [M07Values [['date']] == iDate],
          y = M07Values [['yPos']] [M07Values [['date']] == iDate], 
          las = 1, xlab = '', ylab = '', axes = FALSE,
          xlim = c (10, 40), ylim = c (0, 6.6), 
          col = tColours [['colour']] [M07Values [['treatment']] [M07Values [['date']] == iDate]], 
          bg = ifelse (d >= 0.5, tColours [['colour']] [M07Values [['treatment']] [M07Values [['date']] == iDate]], 
                       'white'), 
          lwd = 2, cex = 2+d,
          pch = as.numeric (M07Values [['height']] [M07Values [['date']] == iDate]))
    
    
    # add rectangle for control standard error
    #--------------------------------------------------------------------------------------
    rect (xleft = M07Values [['beta']] [M07Values [['date']] == iDate] [1] - 
            M07Values [['se']]   [M07Values [['date']] == iDate] [1],
          xright = M07Values [['beta']] [M07Values [['date']] == iDate] [1] + 
            M07Values [['se']]   [M07Values [['date']] == iDate] [1],
          ybottom = 0.6, ytop = 6, lty = 0, col = '#aaaaaa55')
    
    # add standard error
    #--------------------------------------------------------------------------------------
    arrows (x0 = M07Values [['beta']] [M07Values [['date']] == iDate] - 
              M07Values [['se']]   [M07Values [['date']] == iDate],
            x1 = M07Values [['beta']] [M07Values [['date']] == iDate] + 
              M07Values [['se']]   [M07Values [['date']] == iDate],
            y0 = M07Values [['yPos']] [M07Values [['date']] == iDate], 
            code = 3, length = 0.05, angle = 90, lwd = 2, col = '#333333')
    
    # plot means
    #--------------------------------------------------------------------------------------
    points (x = M07Values [['beta']] [M07Values [['date']] == iDate],
            y = M07Values [['yPos']] [M07Values [['date']] == iDate],
            col = tColours [['colour']] [M07Values [['treatment']] [M07Values [['date']] == iDate]], 
            bg = ifelse (d >= 0.5, tColours [['colour']] [M07Values [['treatment']] [M07Values [['date']] == iDate]], 
                         'white'), 
            lwd = 2, cex = 2+d, 
            pch = as.numeric (M07Values [['height']] [M07Values [['date']] == iDate]))
    
    # add x-axis
    #--------------------------------------------------------------------------------------
    axis (side = 1, at = seq (12, 40.0, by = 12), cex.axis = 2, mgp = c (3, 2, 0))
    
    # add panel labels and axis 
    #--------------------------------------------------------------------------------------
    if (iDate == 'jul') {
      
      # add y-axis
      #------------------------------------------------------------------------------------
      axis (side = 2, at = yPositions, labels = c ('C','B','A','B','A','B','M','A'), 
            las = 1, cex.axis = 2)
      
      # add treatments
      #------------------------------------------------------------------------------------
      mtext (side = 2, line = 3,   text = 'control',    cex = 1.4, at = yPositions [1])
      mtext (side = 2, line = 3,   text = 'girdled',    cex = 1.4, at = mean (yPositions [c(2,3)]))
      mtext (side = 2, line = 3,   text = 'compressed', cex = 1.4, at = mean (yPositions [c(4,5)]))
      mtext (side = 2, line = 4.5, text = 'double',     cex = 1.4, at = mean (yPositions [c(6,7,8)]))
      mtext (side = 2, line = 3,   text = 'compressed', cex = 1.4, at = mean (yPositions [c(6,7,8)]))
      
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = 10, y = 6.6, pos = 4, labels = 'july', cex = 3)
    } else if (iDate == 'aug') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = 10, y = 6.6, pos = 4, labels = 'august', cex = 3)
      
      # add x-axis label
      #------------------------------------------------------------------------------------
      mtext (side = 1, line = 5.5, at = 40, cex =2,
             text = 'radial lumen diameter (microns)')
    } else if (iDate == 'oct') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = 10, y = 6.6, pos = 4, labels = 'ocotber', cex = 3)
    } else if (iDate == 'nov') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = 10, y = 6.6, pos = 4, labels = 'november', cex = 3)
    }
    
    # make a line separating the panels
    #--------------------------------------------------------------------------------------
    if (iDate != 'nov') abline (v = 41, col = '#666666')
  }
  
  # add typical between tree variation to plot
  #----------------------------------------------------------------------------------------
  betweenTreeVar <- as_tibble (VarCorr (M7.1)) [1, 5] [[1]]
  text (labels = expression (paste (sigma [tree])), x = 36, y = 0.4, 
        cex = 2.5, pos = 2, col = '#777777')
  segments (x0 = 36 - betweenTreeVar, 
            x1 = 36, 
            y0 = 0.2, lwd = 4,
            col = '#777777')
dev.off ()

# fit a mixed effects model to look at cell wall thickness
# M8.0 <- lmer (formula = CWTTAN ~ (1 | tree) + treatment:height:factor (afterOnset) + factor (afterOnset), 
#               data = woodAnatomy, 
#               REML = FALSE)
# summary (M8.0)
M8.1 <- lmer (formula = CWTTAN ~ (1 | tree) + treatment:height:factor (period) + factor (period), 
              data = woodAnatomy, 
              REML = FALSE)
summary (M8.1)
plot (M8.1)
qqnorm (resid (M8.1))
#anova (M8.0, M8.1)

# fit a mixed effects model to look at cell wall area
# M9.0 <- lmer (formula = CWA ~ (1 | tree) + treatment:height:factor (afterOnset) + factor (afterOnset), 
#               data = woodAnatomy, 
#               REML = FALSE)
# summary (M9.0)
M9.1 <- lmer (formula = CWA ~ (1 | tree) + factor (period) + factor (period):treatment:height, 
              data = woodAnatomy, 
              REML = TRUE)
summary (M9.1)

# Change date, tree, treatment, and height to factors
treeCWA [['date']]      <- factor (treeCWA [['date']])
treeCWA [['tree']]      <- factor (treeCWA [['tree']])
treeCWA [['treatment']] <- factor (treeCWA [['treatment']])
treeCWA [['height']]    <- factor (treeCWA [['height']], levels = c ('A','M','B','C'))

# fit a mixed effects model to look at the cummulative cell wall area for each period
M10 <- lmer (formula = CCWA ~ (1 | tree) + treatment:height:date + date, 
             data = treeCWA, 
             REML = TRUE)
summary (M10)

# extract the model parameters
#----------------------------------------------------------------------------------------
M10Values <- tibble (beta   = getME (M10, 'beta'), 
                     se     = as.numeric (coef (summary (M10)) [, 2]), 
                     tValue = as.numeric (coef (summary (M10)) [, 3]))
M10Values <- add_column (M10Values, .before = 1,
                         date = c (c ('jul', 'aug', 'oct', 'nov'), rep ('jul', 7), 
                                   rep ('aug', 7), rep ('oct', 7), rep ('nov', 7)),
                         treatment = c (rep (1, 4), rep (c (2:4, 4, 2:4), 4)),
                         height = c (rep ('C', 4), rep (c ('A','A','A','M','B','B','B'), 4)))

# add the date effect
#----------------------------------------------------------------------------------------
M10Values [['beta']] [M10Values [['date']] == 'aug'] [2:length (M10Values [['beta']] [M10Values [['date']] == 'aug'])] <- 
  M10Values [['beta']] [M10Values [['date']] == 'aug'] [2:length (M10Values [['beta']] [M10Values [['date']] == 'aug'])] + 
  M10Values [['beta']] [M10Values [['date']] == 'aug'] [1]
M10Values [['beta']] [M10Values [['date']] == 'oct'] [2:length (M10Values [['beta']] [M10Values [['date']] == 'oct'])] <- 
  M10Values [['beta']] [M10Values [['date']] == 'oct'] [2:length (M10Values [['beta']] [M10Values [['date']] == 'oct'])] + 
  M10Values [['beta']] [M10Values [['date']] == 'oct'] [1]
M10Values [['beta']] [M10Values [['date']] == 'nov'] [2:length (M10Values [['beta']] [M10Values [['date']] == 'nov'])] <- 
  M10Values [['beta']] [M10Values [['date']] == 'nov'] [2:length (M10Values [['beta']] [M10Values [['date']] == 'nov'])] + 
  M10Values [['beta']] [M10Values [['date']] == 'nov'] [1]

# add the intercept, aka July baseline effect
#----------------------------------------------------------------------------------------
M10Values [['beta']] [2:length (M10Values [['beta']])] <-
  M10Values [['beta']] [2:length (M10Values [['beta']])] + M10Values [['beta']] [1]

# add y Position in plot
#----------------------------------------------------------------------------------------
M10Values [['yPos']] <- NA
M10Values [['yPos']] [M10Values [['treatment']] == 1] <- yPositions [1]
M10Values [['yPos']] [M10Values [['treatment']] == 2 & M10Values [['height']] == 'B'] <- 
  yPositions [2]
M10Values [['yPos']] [M10Values [['treatment']] == 2 & M10Values [['height']] == 'A'] <- 
  yPositions [3]
M10Values [['yPos']] [M10Values [['treatment']] == 3 & M10Values [['height']] == 'B'] <- 
  yPositions [4]
M10Values [['yPos']] [M10Values [['treatment']] == 3 & M10Values [['height']] == 'A'] <- 
  yPositions [5]
M10Values [['yPos']] [M10Values [['treatment']] == 4 & M10Values [['height']] == 'B'] <- 
  yPositions [6]
M10Values [['yPos']] [M10Values [['treatment']] == 4 & M10Values [['height']] == 'M'] <- 
  yPositions [7]
M10Values [['yPos']] [M10Values [['treatment']] == 4 & M10Values [['height']] == 'A'] <- 
  yPositions [8]

# add symbols column
#----------------------------------------------------------------------------------------
M10Values [['height']] [M10Values [['height']] == 'C'] <- 21
M10Values [['height']] [M10Values [['height']] == 'A'] <- 24
M10Values [['height']] [M10Values [['height']] == 'M'] <- 22
M10Values [['height']] [M10Values [['height']] == 'B'] <- 25

# create layout for the mixed model-based cummulative cell wall area plot 
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017CumulativeCellWallArea.png', width = 600, height = 450) 
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths = c (1.4, 1, 1, 1.1))
  
  # loop over dates to create on plot of fractional cumulative cell wall area for each period
  #----------------------------------------------------------------------------------------
  for (iDate in c ('jul','aug','oct','nov')) {
    
    # choose appropriate plot margins 
    #--------------------------------------------------------------------------------------
    if (iDate == 'jul') {
      par (mar = c (7, 7, 1, 0))
    } else if (iDate == 'aug' | iDate == 'oct') {
      par (mar = c (7, 0, 1, 0))
    } else {
      par (mar = c (7, 0, 1, 1))
    }
    
    # check effect size and colour symbols accordingly
    #--------------------------------------------------------------------------------------
    deltaMu <- M10Values [['beta']] [M10Values [['date']] == iDate] - 
      M10Values [['beta']] [M10Values [['date']] == iDate] [1]
    d <- 0.5
    for (i in 2:8) {
      # determine treatment and sampling height
      if (i >= 3) {
        if (i == 2) {
          t <- 2; h <- 'B'
        } else {
          t <- 2; h <- 'A'
        }
      } else if (i <= 5) {
        if (i == 4) {
          t <- 3; h <- 'B'
        } else {
          t <- 3; h <- 'A'
        }
      } else {
        if (i == 6) {
          t <- 4; h <- 'B'
        } else if (i == 7) {
          t <- 4; h <- 'M'
        } else {
          t <- 4; h <- 'A'
        }
      }
      d <- c (d, abs (deltaMu [i]) / 
                (sd (treeCWA [['CCWA']] [treeCWA [['treatment']] %in% c (1, t) & 
                                         treeCWA [['height']] %in% c ('C',h)], na.rm = TRUE)))
    }
    
    # plot the cummulative cell wall area for each period
    #--------------------------------------------------------------------------------------
    plot (x = M10Values [['beta']] [M10Values [['date']] == iDate],
          y = M10Values [['yPos']] [M10Values [['date']] == iDate], 
          las = 1, xlab = '', ylab = '', axes = FALSE,
          xlim = c (0, 1.1), ylim = c (0, 6.6), 
          col = tColours [['colour']] [M10Values [['treatment']] [M10Values [['date']] == iDate]], 
          bg = ifelse (abs (M10Values [['tValue']] [M10Values [['date']] == iDate]) >= 2, 
                       tColours [['colour']] [M10Values[['treatment']] [M10Values [['date']] == iDate]], 
                       'white'), lwd = 2, 
          cex = 2+d, #abs (M10Values [['tValue']] [M10Values [['date']] == iDate]),
          pch = as.numeric (M10Values [['height']] [M10Values [['date']] == iDate]))
    
    # add rectangle for control standard error
    #--------------------------------------------------------------------------------------
    rect (xleft = M10Values [['beta']] [M10Values [['date']] == iDate] [1] - 
                  M10Values [['se']]   [M10Values [['date']] == iDate] [1],
          xright = M10Values [['beta']] [M10Values [['date']] == iDate] [1] + 
                   M10Values [['se']]   [M10Values [['date']] == iDate] [1],
          ybottom = 0.6, ytop = 6, lty = 0, col = '#aaaaaa55')
    
    # add standard error
    #--------------------------------------------------------------------------------------
    arrows (x0 = M10Values [['beta']] [M10Values [['date']] == iDate] - 
                 M10Values [['se']]   [M10Values [['date']] == iDate],
            x1 = M10Values [['beta']] [M10Values [['date']] == iDate] + 
                 M10Values [['se']]   [M10Values [['date']] == iDate],
            y0 = M10Values [['yPos']] [M10Values [['date']] == iDate], 
            code = 3, length = 0.05, angle = 90, lwd = 2, col = '#333333')
    
    # plot means
    #--------------------------------------------------------------------------------------
    points (x = M10Values [['beta']] [M10Values [['date']] == iDate],
            y = M10Values [['yPos']] [M10Values [['date']] == iDate],
            col = tColours [['colour']] [M10Values [['treatment']] [M10Values [['date']] == iDate]], 
            bg = ifelse (d >= 0.5, 
                         tColours [['colour']] [M10Values[['treatment']] [M10Values [['date']] == iDate]], 
                         'white'), 
            lwd = 2, cex = 2+d,
            pch = as.numeric (M10Values [['height']] [M10Values [['date']] == iDate]))
    
    # add x-axis
    #--------------------------------------------------------------------------------------
    axis (side = 1, at = seq (0, 0.8, by = 0.4), cex.axis = 2, mgp = c (3, 2, 0))
    
    # add panel labels and axis 
    #--------------------------------------------------------------------------------------
    if (iDate == 'jul') {
      
      # add y-axis
      #------------------------------------------------------------------------------------
      axis (side = 2, at = yPositions, labels = c ('C','B','A','B','A','B','M','A'), 
            las = 1, cex.axis = 2)
      
      # add treatments
      #------------------------------------------------------------------------------------
      mtext (side = 2, line = 3,   text = 'control',    cex = 1.4, at = yPositions [1])
      mtext (side = 2, line = 3,   text = 'girdled',    cex = 1.4, at = mean (yPositions [c(2,3)]))
      mtext (side = 2, line = 3,   text = 'compressed', cex = 1.4, at = mean (yPositions [c(4,5)]))
      mtext (side = 2, line = 4.5, text = 'double',     cex = 1.4, at = mean (yPositions [c(6,7,8)]))
      mtext (side = 2, line = 3,   text = 'compressed', cex = 1.4, at = mean (yPositions [c(6,7,8)]))

      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = 0, y = 6.6, pos = 4, labels = 'july', cex = 3)
    } else if (iDate == 'aug') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = 0, y = 6.6, pos = 4, labels = 'august', cex = 3)
      
      # add x-axis label
      #------------------------------------------------------------------------------------
      mtext (side = 1, line = 5, at = 1.1, cex = 2.0,
             text = 'fraction of cummulative cell wall area')
    } else if (iDate == 'oct') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = 0, y = 6.6, pos = 4, labels = 'ocotber', cex = 3)
    } else if (iDate == 'nov') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = 0, y = 6.6, pos = 4, labels = 'november', cex = 3)
    }
    
    # make a line separating the panels
    #--------------------------------------------------------------------------------------
    if (iDate != 'nov') abline (v = 1.1, col = '#666666')
  }
  
  # add typical between tree variation to plot
  #----------------------------------------------------------------------------------------
  betweenTreeVar <- as_tibble (VarCorr (M10)) [1, 5] [[1]]
  text (labels = expression (paste (sigma [tree])), x = 1.1, y = 0.4, 
        cex = 2.5, pos = 2, col = '#777777')
  segments (x0 = 1.1 - betweenTreeVar,x1 = 1.1, y0 = 0.2, 
            lwd = 4, col = '#777777')
dev.off ()
#========================================================================================

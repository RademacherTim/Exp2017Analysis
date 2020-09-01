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
cellNumber <- add_column (cellNumber, period = NA)
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
#========================================================================================

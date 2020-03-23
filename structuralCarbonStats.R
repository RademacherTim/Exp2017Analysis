#========================================================================================
# This script test for differences between treatments and sampling height in structural 
# carbon related variables, such as ring widths, cell wall thickness, lumen diameter, 
# number of cell per ring and cell size. 
#----------------------------------------------------------------------------------------

# load dependencies
library ('lme4')
if (!existsFunction ('add_column')) library ('tidyverse')

# source ring width and other anatomical data
standardisedRW2017 <- read_csv (file = 'standardisedRW2017.csv',
                                col_types = cols ())

# wranlge standardised ring width at the end of the experiment
standardisedRW2017 <- add_column (standardisedRW2017, 
                                  treatment = allometricData [['treatment']] [1:40])
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
        col = colours [M01Values [['treatment']] [M01Values [['date']] == iDate]], 
        bg = ifelse (abs (M01Values [['tValue']] [M01Values [['date']] == iDate]) >= 2, 
                     colours [M01Values[['treatment']] [M01Values [['date']] == iDate]], 
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
          col = colours [M01Values [['treatment']]], 
          bg = ifelse (abs (M01Values [['tValue']]) >= 2, 
                       colours [M01Values[['treatment']]], 
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

# wrangle cell number in the ring
#----------------------------------------------------------------------------------------
cellNumber [['tree']]      <- factor (cellNumber [['tree']])
cellNumber [['treatment']] <- factor (cellNumber [['treatment']], levels = c (4:1))
cellNumber [['height']]    <- factor (cellNumber [['height']], levels = c ('A','M','B','C'))

# fit mixed effect model with tree as random effect
#----------------------------------------------------------------------------------------
M2 <- lmer (formula = n ~ (1 | tree) + treatment:height,
            data = cellNumber,
            REML = TRUE)
summary (M2)
# plot (M2)
# qqnorm (resid (M2))

# extract the model parameters from the lumen diameter model
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
  # plot the cummulative cell wall area for each period
  #--------------------------------------------------------------------------------------
  par (mar = c (5, 5, 1, 0))
  plot (x = M02Values [['beta']] [M02Values [['date']] == iDate],
        y = M02Values [['yPos']] [M02Values [['date']] == iDate], 
        las = 1, xlab = 'number of cells (n)', ylab = '', axes = FALSE,
        xlim = c (10, 80), ylim = c (0, 6.6), 
        col = colours [M02Values [['treatment']] [M02Values [['date']] == iDate]], 
        bg = ifelse (abs (M02Values [['tValue']] [M02Values [['date']] == iDate]) >= 2, 
                     colours [M02Values[['treatment']] [M02Values [['date']] == iDate]], 
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
          col = colours [M02Values [['treatment']]], 
          bg = ifelse (abs (M02Values [['tValue']]) >= 2, 
                       colours [M02Values[['treatment']]], 
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
woodAnatomy <- data [data [['YEAR']] == 2017, ]
woodAnatomy [['tree']]      <- factor (substr (woodAnatomy [['TREE']], 1, 2)) 
woodAnatomy [['treatment']] <- factor (substr (woodAnatomy [['PLOT']], 2, 2), levels = c (4:1))
woodAnatomy [['POS']] [woodAnatomy [['treatment']] == 1] <- 'C'
woodAnatomy [['height']] <- factor (woodAnatomy [['POS']], levels = c ('A','M','B','C'))
woodAnatomy <- select (woodAnatomy, c (MRW, LA, DRAD, DTAN, CWA, CWAACC, CWTTAN, CWTALL, period, cellRadWidth, cellTanWidth, zonalCWA, tree, treatment, height))

# create factor stating if it was formed before or after the experiment
woodAnatomy <- add_column (woodAnatomy, 
                           afterOnset = factor (ifelse (woodAnatomy [['period']] > as_date ('2017-07-03'), 
                                                        TRUE, FALSE), levels = c (FALSE, TRUE)))

# fit mixed effects model for radial cell size before the experiment
M4 <- lmer (formula = cellRadWidth ~ (1 | tree) + treatment:height, 
            data = woodAnatomy [woodAnatomy [['period']] == as_date ('2017-07-03'), ], 
            REML = TRUE)
summary (M4)
plot (M4)
qqnorm (resid (M4))

# fit mixed effects model for radial cell size over the proportion that formed after the start of the experimental 
M5 <- lmer (formula = cellRadWidth ~ (1 | tree) + treatment:height, 
            data = woodAnatomy [woodAnatomy [['period']] != as_date ('2017-07-03'), ], 
            REML = TRUE)
summary (M5)
plot (M5)
qqnorm (resid (M5))

# fit mixed effects model to radial cell size including time of formation
# M6 <- lmer (formula = cellRadWidth ~ (1 | tree) + treatment:height:factor (afterOnset) + factor (afterOnset), 
#             data = woodAnatomy, 
#             REML = FALSE)
# summary (M6)
M6.1 <- lmer (formula = cellRadWidth ~ (1 | tree) + treatment:height:factor (period) + factor (period), 
            data = woodAnatomy, 
            REML = FALSE)
summary (M6.1)
plot (M6.1)
qqnorm (resid (M6.1))
#anova (M6, M6.1)


# fit a mixed effects model to look at cell lumen diameter
# M7.0 <- lmer (formula = DRAD ~ (1 | tree) + treatment:height:factor (afterOnset) + factor (afterOnset), 
#               data = woodAnatomy, 
#               REML = FALSE)
# summary (M7.0)
M7.1 <- lmer (formula = DRAD ~ (1 | tree) + treatment:height:factor (period) + factor (period), 
              data = woodAnatomy, 
              REML = TRUE)
summary (M7.1)
plot (M7.1)
qqnorm (resid (M7.1))
#anova (M7.0, M7.1)

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
png (filename = '../fig/Exp2017LumenDiameter.png', width = 600)
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths = c (1.4, 1, 1, 1.1))
  
  # loop over dates to create on plot of fractional cumulative cell wall area for each period
  #----------------------------------------------------------------------------------------
  for (iDate in c ('jul','aug','oct','nov')) {
    
    # choose appropriate plot margins 
    #--------------------------------------------------------------------------------------
    if (iDate == 'jul') {
      par (mar = c (5, 5, 1, 0))
    } else if (iDate == 'aug' | iDate == 'oct') {
      par (mar = c (5, 0, 1, 0))
    } else {
      par (mar = c (5, 0, 1, 1))
    }
    
    # plot the cummulative cell wall area for each period
    #--------------------------------------------------------------------------------------
    plot (x = M07Values [['beta']] [M07Values [['date']] == iDate],
          y = M07Values [['yPos']] [M07Values [['date']] == iDate], 
          las = 1, xlab = '', ylab = '', axes = FALSE,
          xlim = c (10, 40), ylim = c (0, 6.6), 
          col = colours [M07Values [['treatment']] [M07Values [['date']] == iDate]], 
          bg = ifelse (abs (M07Values [['tValue']] [M07Values [['date']] == iDate]) >= 2, 
                       colours [M07Values[['treatment']] [M07Values [['date']] == iDate]], 
                       'white'), lwd = 2, 
          cex = 2, #abs (M07Values [['tValue']] [M07Values [['date']] == iDate]),
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
            col = colours [M07Values [['treatment']] [M07Values [['date']] == iDate]], 
            bg = ifelse (abs (M07Values [['tValue']] [M07Values [['date']] == iDate]) >= 2, 
                         colours [M07Values[['treatment']] [M07Values [['date']] == iDate]], 
                         'white'), 
            lwd = 2, cex = 2, #abs (M07Values [['tValue']] [M07Values [['date']] == iDate]),
            pch = as.numeric (M07Values [['height']] [M07Values [['date']] == iDate]))
    
    # add x-axis
    #--------------------------------------------------------------------------------------
    axis (side = 1, at = seq (12, 40.0, by = 12))
    
    # add panel labels and axis 
    #--------------------------------------------------------------------------------------
    if (iDate == 'jul') {
      
      # add y-axis
      #------------------------------------------------------------------------------------
      axis (side = 2, at = yPositions, labels = c ('C','B','A','B','A','B','M','A'), 
            las = 1)
      
      # add treatments
      #------------------------------------------------------------------------------------
      mtext (side = 2, line = 2, text = 'control',    at = yPositions [1])
      mtext (side = 2, line = 2, text = 'girdled',    at = mean (yPositions [c(2,3)]))
      mtext (side = 2, line = 2, text = 'compressed', at = mean (yPositions [c(4,5)]))
      mtext (side = 2, line = 3, text = 'double',     at = mean (yPositions [c(6,7,8)]))
      mtext (side = 2, line = 2, text = 'compressed', at = mean (yPositions [c(6,7,8)]))
      
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = 10, y = 6.6, pos = 4, labels = 'july', cex = 1.3)
    } else if (iDate == 'aug') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = 10, y = 6.6, pos = 4, labels = 'august', cex = 1.3)
      
      # add x-axis label
      #------------------------------------------------------------------------------------
      mtext (side = 1, line = 3, at = 40, cex = 1.0,
             text = 'radial lumen diameter (microns)')
    } else if (iDate == 'oct') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = 10, y = 6.6, pos = 4, labels = 'ocotber', cex = 1.3)
    } else if (iDate == 'nov') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = 10, y = 6.6, pos = 4, labels = 'november', cex = 1.3)
    }
    
    # make a line separating the panels
    #--------------------------------------------------------------------------------------
    if (iDate != 'nov') abline (v = 41, col = '#666666')
  }
  
  # add typical between tree variation to plot
  #----------------------------------------------------------------------------------------
  betweenTreeVar <- as_tibble (VarCorr (M7.1)) [1, 5] [[1]]
  text (labels = expression (paste (sigma [tree])), x = 36, y = 0.4, 
        cex = 1.5, pos = 2, col = '#777777')
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
M9.1 <- lmer (formula = CWA ~ (1 | tree) + treatment:height:factor (period) + factor (period), 
              data = woodAnatomy, 
              REML = TRUE)
summary (M9.1)
plot (M9.1)
qqnorm (resid (M9.1))
#anova (M9.0, M9.1)

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
plot (M10)
qqnorm (resid (M10))

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
png (filename = '../fig/Exp2017CumulativeCellWallArea_2.png', width = 600, height = 400) 
  layout (matrix (1:4, nrow = 1, byrow = TRUE), widths = c (1.4, 1, 1, 1.1))
  
  # loop over dates to create on plot of fractional cumulative cell wall area for each period
  #----------------------------------------------------------------------------------------
  for (iDate in c ('jul','aug','oct','nov')) {
    
    # choose appropriate plot margins 
    #--------------------------------------------------------------------------------------
    if (iDate == 'jul') {
      par (mar = c (5, 5, 1, 0))
    } else if (iDate == 'aug' | iDate == 'oct') {
      par (mar = c (5, 0, 1, 0))
    } else {
      par (mar = c (5, 0, 1, 1))
    }
    
    # plot the cummulative cell wall area for each period
    #--------------------------------------------------------------------------------------
    plot (x = M10Values [['beta']] [M10Values [['date']] == iDate],
          y = M10Values [['yPos']] [M10Values [['date']] == iDate], 
          las = 1, xlab = '', ylab = '', axes = FALSE,
          xlim = c (0, 1.1), ylim = c (0, 6.6), 
          col = colours [M10Values [['treatment']] [M10Values [['date']] == iDate]], 
          bg = ifelse (abs (M10Values [['tValue']] [M10Values [['date']] == iDate]) >= 2, 
                       colours [M10Values[['treatment']] [M10Values [['date']] == iDate]], 
                       'white'), lwd = 2, 
          cex = 2, #abs (M10Values [['tValue']] [M10Values [['date']] == iDate]),
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
            col = colours [M10Values [['treatment']] [M10Values [['date']] == iDate]], 
            bg = ifelse (abs (M10Values [['tValue']] [M10Values [['date']] == iDate]) >= 2, 
                         colours [M10Values[['treatment']] [M10Values [['date']] == iDate]], 
                         'white'), 
            lwd = 2, cex = 2, #abs (M10Values [['tValue']] [M10Values [['date']] == iDate]),
            pch = as.numeric (M10Values [['height']] [M10Values [['date']] == iDate]))
    
    # add x-axis
    #--------------------------------------------------------------------------------------
    axis (side = 1, at = seq (0, 1.0, by = 0.5))
    
    # add panel labels and axis 
    #--------------------------------------------------------------------------------------
    if (iDate == 'jul') {
      
      # add y-axis
      #------------------------------------------------------------------------------------
      axis (side = 2, at = yPositions, labels = c ('C','B','A','B','A','B','M','A'), 
            las = 1)
      
      # add treatments
      #------------------------------------------------------------------------------------
      mtext (side = 2, line = 2, text = 'control',    at = yPositions [1])
      mtext (side = 2, line = 2, text = 'girdled',    at = mean (yPositions [c(2,3)]))
      mtext (side = 2, line = 2, text = 'compressed', at = mean (yPositions [c(4,5)]))
      mtext (side = 2, line = 3, text = 'double',     at = mean (yPositions [c(6,7,8)]))
      mtext (side = 2, line = 2, text = 'compressed', at = mean (yPositions [c(6,7,8)]))

      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = 0, y = 6.6, pos = 4, labels = 'july', cex = 1.3)
    } else if (iDate == 'aug') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = 0, y = 6.6, pos = 4, labels = 'august', cex = 1.3)
      
      # add x-axis label
      #------------------------------------------------------------------------------------
      mtext (side = 1, line = 3, at = 1.1, cex = 1.0,
             text = 'fraction of cummulative cell wall area')
    } else if (iDate == 'oct') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = 0, y = 6.6, pos = 4, labels = 'ocotber', cex = 1.3)
    } else if (iDate == 'nov') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = 0, y = 6.6, pos = 4, labels = 'november', cex = 1.3)
    }
    
    # make a line separating the panels
    #--------------------------------------------------------------------------------------
    if (iDate != 'nov') abline (v = 1.1, col = '#666666')
  }
  
  # add typical between tree variation to plot
  #----------------------------------------------------------------------------------------
  betweenTreeVar <- as_tibble (VarCorr (M10)) [1, 5] [[1]]
  text (labels = expression (paste (sigma [tree])), x = 1.1, y = 0.4, 
        cex = 1.5, pos = 2, col = '#777777')
  segments (x0 = 1.1 - betweenTreeVar,x1 = 1.1, y0 = 0.2, 
            lwd = 4, col = '#777777')
dev.off ()
#========================================================================================

#========================================================================================
# This script test for differences between treatments and sampling height in 
# nonstructural carbon related variables, such as soluble sugar and strach concentrations. 
#----------------------------------------------------------------------------------------

# load dependencies
library ('lme4')
library ('nlme')

# Read the sugar and starch concentration (means for needles and roots and first centimeter for wood sections)
#----------------------------------------------------------------------------------------
suppressMessages (source ('/home/tim/projects/PlantGrowth/nonstructuralCarbon/processExpNSCData.R'))

# Convert tree, date, treatment and sampleHeight to factors
#----------------------------------------------------------------------------------------
stemData2017 [['tree']]      <- factor (stemData2017 [['treeID']])
stemData2017 [['date']]      <- factor (stemData2017 [['date']])
stemData2017 [['treatment']] <- factor (stemData2017 [['treatment']], levels = c (4:1))
for (i in 1:dim (stemData2017) [1]) {
  if (stemData2017 [['treatment']] [i] == 1) {
    stemData2017 [['height']] [i] <- 'C'
  } else if (stemData2017 [['treatment']] [i] == 2 | 
             stemData2017 [['treatment']] [i] == 3) {
    if (stemData2017 [['sampleHeight']] [i] == 1.0) {
      stemData2017 [['height']] [i] <- 'B'  
    } else if (stemData2017 [['sampleHeight']] [i] == 2.0) {
      stemData2017 [['height']] [i] <- 'A'  
    }
  } else if (stemData2017 [['treatment']] [i] == 4) {
    if (stemData2017 [['sampleHeight']] [i] == 0.5) {
      stemData2017 [['height']] [i] <- 'B'  
    } else if (stemData2017 [['sampleHeight']] [i] == 1.5) {
      stemData2017 [['height']] [i] <- 'M'  
    } else if (stemData2017 [['sampleHeight']] [i] == 2.5) {
      stemData2017 [['height']] [i] <- 'A'  
    }
  }
}
stemData2017 [['height']] <- factor (stemData2017 [['height']], levels = c ('A','M','B','C'))
stemData2017 <- filter (stemData2017, tree != 41)

# Convert tree, date, treatment and sampleHeight to factors for leaf data
#----------------------------------------------------------------------------------------
leafData2017 [['tree']]      <- factor (leafData2017 [['treeID']])
leafData2017 [['date']]      <- factor (leafData2017 [['date']])
leafData2017 [['treatment']] <- factor (leafData2017 [['treatment']], levels = c (1:4))
leafData2017 <- filter (leafData2017, tree != 41)

# Convert tree, date, treatment and sampleHeight to factors for root data
#----------------------------------------------------------------------------------------
rootData2017 [['tree']]      <- factor (rootData2017 [['treeID']]) 
rootData2017 [['date']]      <- factor (rootData2017 [['date']]) 
rootData2017 [['treatment']] <- factor (rootData2017 [['treatment']], levels = c (1:4)) 
rootData2017 <- filter (rootData2017, tree != 41)

# Get differences from baseline for the NSC concentrations
#----------------------------------------------------------------------------------------
leafData2017 [['deltaSugar']] <- leafData2017 [['sugar']] - 
                                 leafData2017 [['sugar']] [1:40]
leafData2017 [['deltaStarch']] <- leafData2017 [['starch']] - 
                                  leafData2017 [['starch']] [1:40]
stemData2017 [['deltaSugar']] <- stemData2017 [['sugar']] - 
                                 stemData2017 [['sugar']] [1:80]
stemData2017 [['deltaStarch']] <- stemData2017 [['starch']] - 
                                  stemData2017 [['starch']] [1:80]
rootData2017 [['deltaSugar']] <- rootData2017 [['sugar']] - 
                                 rootData2017 [['sugar']] [1:40]
rootData2017 [['deltaStarch']] <- rootData2017 [['starch']] - 
                                  rootData2017 [['starch']] [1:40]


# Fit mixed effects model with tree as random effect to analyse stem soluble sugar for 1cm of wood,
# Fit model to the difference from the July baseline
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = deltaSugar ~ (1 | tree) + date + date:treatment:height,
            data = stemData2017 [81:320, ],
            REML = TRUE)
summary (M1)

# TR I got to organise everything below to actually plot my figure.
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

# create layout for the mixed model-based figure of change in sugar 
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017StemDeltaSugar.png', width = 600, height = 400) 
layout (matrix (1:3, nrow = 1, byrow = TRUE), widths = c (1.4, 1, 1.1))

# loop over dates to create on plot of fractional cumulative cell wall area for each period
#----------------------------------------------------------------------------------------
for (iDate in c ('aug','oct','nov')) {
  
  # choose appropriate plot margins 
  #--------------------------------------------------------------------------------------
  if (iDate == 'aug') {
    par (mar = c (5, 5, 1, 0))
  } else if (iDate == 'oct') {
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

# Difference in stem starch concentrations
#----------------------------------------------------------------------------------------
M2 <- lmer (formula = deltaStarch ~ (1 | tree) + date + date:treatment:height,
            data = stemData2017 [81:320, ],
            REML = TRUE)
summary (M2)

# Fit mixed effects model to the leaf soluble sugar concentration difference from baseline 
#----------------------------------------------------------------------------------------
M3 <- lmer (formula = deltaSugar ~ (1 | tree) + date + treatment:date,
            data = leafData2017 [41:160, ],
            REML = TRUE)
summary (M3)

# Fit mixed effects model to the leaf starch concentration difference from baseline 
#----------------------------------------------------------------------------------------
M4 <- lmer (formula = deltaStarch ~ (1 | tree) + date + treatment:date,
            data = leafData2017 [41:160, ],
            REML = TRUE)
summary (M4)

# Fit mixed effects model to the root soluble sugar concentration difference from baseline 
#----------------------------------------------------------------------------------------
M5 <- lmer (formula = deltaSugar ~ (1 | tree) + date + treatment:date,
            data = rootData2017 [41:160, ],
            REML = TRUE)
summary (M5)

# Fit mixed effects model to the root starch concentration difference from baseline 
#----------------------------------------------------------------------------------------
M6 <- lmer (formula = deltaStarch ~ (1 | tree) + date + treatment:date,
            data = rootData2017 [41:160, ],
            REML = TRUE)
summary (M6)

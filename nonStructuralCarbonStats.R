#========================================================================================
# This script test for differences between treatments and sampling height in 
# nonstructural carbon related variables, such as soluble sugar and strach concentrations. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lme4')
library ('tidyverse')

# read the sugar and starch concentration for needles, roots and first cm of wood
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

# extract the model parameters from the stem sugar model
#----------------------------------------------------------------------------------------
M01Values <- tibble (beta   = getME (M1, 'beta'), 
                     se     = as.numeric (coef (summary (M1)) [, 2]), 
                     tValue = as.numeric (coef (summary (M1)) [, 3]))
M01Values <- add_column (M01Values, .before = 1,
                         treatment = c (rep (1, 3), c (4, 4, 4, 3, 3, 3, 2, 2, 2, 4, 4, 4, 
                                                       4, 4, 4, 3, 3, 3, 2, 2, 2)),
                         height = c (rep ('C', 3), rep ('A', 9), rep ('M', 3), rep ('B', 9)),
                         date = rep (c ('aug', 'oct', 'nov'), 8))

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
  layout (matrix (1:3, nrow = 1, byrow = TRUE), widths = c (1.6, 1, 1.1))
  
  # loop over dates to create on plot of fractional cumulative cell wall area for each period
  #----------------------------------------------------------------------------------------
  for (iDate in c ('aug','oct','nov')) {
    
    # choose appropriate plot margins 
    #--------------------------------------------------------------------------------------
    if (iDate == 'aug') {
      par (mar = c (5, 10, 1, 0))
    } else if (iDate == 'oct') {
      par (mar = c (5, 0, 1, 0))
    } else {
      par (mar = c (5, 0, 1, 1))
    }
    
    # plot the cummulative cell wall area for each period
    #--------------------------------------------------------------------------------------
    plot (x = M01Values [['beta']] [M01Values [['date']] == iDate],
          y = M01Values [['yPos']] [M01Values [['date']] == iDate], 
          las = 1, xlab = '', ylab = '', axes = FALSE,
          xlim = c (-0.4, 0.6), ylim = c (0, 6.6), 
          col = colours [M01Values [['treatment']] [M01Values [['date']] == iDate]], 
          bg = ifelse (abs (M01Values [['tValue']] [M01Values [['date']] == iDate]) >= 2, 
                       colours [M01Values[['treatment']] [M01Values [['date']] == iDate]], 
                       'white'), lwd = 2, 
          cex = 2, #abs (M01Values [['tValue']] [M01Values [['date']] == iDate]),
          pch = as.numeric (M01Values [['height']] [M01Values [['date']] == iDate]))
    
    # add zero line
    #------------------------------------------------------------------------------------
    segments (y0 = -0.5, y1 = 6.0, x0 = 0, col = '#999999', lwd = 1, lty = 2)
    
    # add rectangle for control standard error
    #--------------------------------------------------------------------------------------
    rect (xleft = M01Values [['beta']] [M01Values [['date']] == iDate] [1] - 
            M01Values [['se']]   [M01Values [['date']] == iDate] [1],
          xright = M01Values [['beta']] [M01Values [['date']] == iDate] [1] + 
            M01Values [['se']]   [M01Values [['date']] == iDate] [1],
          ybottom = 0.6, ytop = 6, lty = 0, col = '#aaaaaa55')
    
    # add standard error
    #--------------------------------------------------------------------------------------
    arrows (x0 = M01Values [['beta']] [M01Values [['date']] == iDate] - 
              M01Values [['se']]   [M01Values [['date']] == iDate],
            x1 = M01Values [['beta']] [M01Values [['date']] == iDate] + 
              M01Values [['se']]   [M01Values [['date']] == iDate],
            y0 = M01Values [['yPos']] [M01Values [['date']] == iDate], 
            code = 3, length = 0.05, angle = 90, lwd = 2, col = '#333333')
    
    # plot means
    #--------------------------------------------------------------------------------------
    points (x = M01Values [['beta']] [M01Values [['date']] == iDate],
            y = M01Values [['yPos']] [M01Values [['date']] == iDate],
            col = colours [M01Values [['treatment']] [M01Values [['date']] == iDate]], 
            bg = ifelse (abs (M01Values [['tValue']] [M01Values [['date']] == iDate]) >= 2, 
                         colours [M01Values[['treatment']] [M01Values [['date']] == iDate]], 
                         'white'), 
            lwd = 2, cex = 2, #abs (M01Values [['tValue']] [M01Values [['date']] == iDate]),
            pch = as.numeric (M01Values [['height']] [M01Values [['date']] == iDate]))
    
    # add x-axis
    #--------------------------------------------------------------------------------------
    axis (side = 1, at = seq (-0.3, 0.6, by = 0.3), cex.axis = 1.5)
    
    # add panel labels and axis 
    #--------------------------------------------------------------------------------------
    if (iDate == 'aug') {
      
      # add y-axis
      #------------------------------------------------------------------------------------
      axis (side = 2, at = yPositions, labels = c ('C','B','A','B','A','B','M','A'), 
            las = 1, cex.axis = 1.5)
      
      # add treatments
      #------------------------------------------------------------------------------------
      mtext (side = 2, line = 3,   text = 'control',    cex = 1.2, at = yPositions [1])
      mtext (side = 2, line = 3,   text = 'girdled',    cex = 1.2, at = mean (yPositions [c(2,3)]))
      mtext (side = 2, line = 3,   text = 'compressed', cex = 1.2, at = mean (yPositions [c(4,5)]))
      mtext (side = 2, line = 4.5, text = 'double',     cex = 1.2, at = mean (yPositions [c(6,7,8)]))
      mtext (side = 2, line = 3,   text = 'compressed', cex = 1.2, at = mean (yPositions [c(6,7,8)]))
      mtext (side = 2, line = 7,   text = 'wood', cex = 3, at = 3.3)

      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -0.4, y = 6.6, pos = 4, labels = 'august', cex = 2)
    } else if (iDate == 'oct') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -0.4, y = 6.6, pos = 4, labels = 'october', cex = 2)
    
      # add x-axis label
      #------------------------------------------------------------------------------------
      mtext (side = 1, line = 4, at = 0.1, cex = 1.5,
             text = expression (paste ('concentration (% dry weight)')))
    } else if (iDate == 'nov') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -0.4, y = 6.6, pos = 4, labels = 'november', cex = 2)
    }
    
    # make a line separating the panels
    #--------------------------------------------------------------------------------------
    if (iDate != 'nov') abline (v = 0.64, col = '#666666')
  }
  
  # add typical between tree variation to plot
  #----------------------------------------------------------------------------------------
  betweenTreeVar <- as_tibble (VarCorr (M1)) [1, 5] [[1]]
  text (labels = expression (paste (sigma [tree])), x = 0.6, y = 0.4, 
        cex = 2, pos = 2, col = '#777777')
  segments (x0 = 0.6 - betweenTreeVar, x1 = 0.6, y0 = 0.2, 
            lwd = 4, col = '#777777')
dev.off ()

# Difference in stem starch concentrations
#----------------------------------------------------------------------------------------
M2 <- lmer (formula = deltaStarch ~ (1 | tree) + date + date:treatment:height,
            data = stemData2017 [81:320, ],
            REML = TRUE)
summary (M2)


# extract the model parameters from stem starch model
#----------------------------------------------------------------------------------------
M02Values <- tibble (beta   = getME (M2, 'beta'), 
                     se     = as.numeric (coef (summary (M2)) [, 2]), 
                     tValue = as.numeric (coef (summary (M2)) [, 3]))
M02Values <- add_column (M02Values, .before = 1,
                         treatment = c (rep (1, 3), c (4, 4, 4, 3, 3, 3, 2, 2, 2, 4, 4, 4, 
                                                       4, 4, 4, 3, 3, 3, 2, 2, 2)),
                         height = c (rep ('C', 3), rep ('A', 9), rep ('M', 3), rep ('B', 9)),
                         date = rep (c ('aug', 'oct', 'nov'), 8))

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

# create layout for the mixed model-based figure of change in starch
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017StemDeltaStarch.png', width = 600, height = 400) 
  layout (matrix (1:3, nrow = 1, byrow = TRUE), widths = c (1.4, 1, 1.1))
  
  # loop over dates to create on plot of stem starch concentration for each period
  #----------------------------------------------------------------------------------------
  for (iDate in c ('aug','oct','nov')) {
    
    # choose appropriate plot margins 
    #--------------------------------------------------------------------------------------
    if (iDate == 'aug') {
      par (mar = c (5, 6, 1, 0))
    } else if (iDate == 'oct') {
      par (mar = c (5, 0, 1, 0))
    } else {
      par (mar = c (5, 0, 1, 1))
    }
    
    # plot the cummulative cell wall area for each period
    #--------------------------------------------------------------------------------------
    plot (x = M02Values [['beta']] [M02Values [['date']] == iDate],
          y = M02Values [['yPos']] [M02Values [['date']] == iDate], 
          las = 1, xlab = '', ylab = '', axes = FALSE,
          xlim = c (-0.6, 0.3), ylim = c (0, 6.6), 
          col = colours [M02Values [['treatment']] [M02Values [['date']] == iDate]], 
          bg = ifelse (abs (M02Values [['tValue']] [M02Values [['date']] == iDate]) >= 2, 
                       colours [M02Values[['treatment']] [M02Values [['date']] == iDate]], 
                       'white'), lwd = 2, 
          cex = 2, #abs (M02Values [['tValue']] [M02Values [['date']] == iDate]),
          pch = as.numeric (M02Values [['height']] [M02Values [['date']] == iDate]))
    
    # add zero line
    #------------------------------------------------------------------------------------
    segments (y0 = -0.5, y1 = 6.0, x0 = 0, col = '#999999', lwd = 1, lty = 2)
    
    # add rectangle for control standard error
    #--------------------------------------------------------------------------------------
    rect (xleft = M02Values [['beta']] [M02Values [['date']] == iDate] [1] - 
            M02Values [['se']]   [M02Values [['date']] == iDate] [1],
          xright = M02Values [['beta']] [M02Values [['date']] == iDate] [1] + 
            M02Values [['se']]   [M02Values [['date']] == iDate] [1],
          ybottom = 0.6, ytop = 6, lty = 0, col = '#aaaaaa55')
    
    # add standard error
    #--------------------------------------------------------------------------------------
    arrows (x0 = M02Values [['beta']] [M02Values [['date']] == iDate] - 
              M02Values [['se']]   [M02Values [['date']] == iDate],
            x1 = M02Values [['beta']] [M02Values [['date']] == iDate] + 
              M02Values [['se']]   [M02Values [['date']] == iDate],
            y0 = M02Values [['yPos']] [M02Values [['date']] == iDate], 
            code = 3, length = 0.05, angle = 90, lwd = 2, col = '#333333')
    
    # plot means
    #--------------------------------------------------------------------------------------
    points (x = M02Values [['beta']] [M02Values [['date']] == iDate],
            y = M02Values [['yPos']] [M02Values [['date']] == iDate],
            col = colours [M02Values [['treatment']] [M02Values [['date']] == iDate]], 
            bg = ifelse (abs (M02Values [['tValue']] [M02Values [['date']] == iDate]) >= 2, 
                         colours [M02Values[['treatment']] [M02Values [['date']] == iDate]], 
                         'white'), 
            lwd = 2, cex = 2, #abs (M02Values [['tValue']] [M02Values [['date']] == iDate]),
            pch = as.numeric (M02Values [['height']] [M02Values [['date']] == iDate]))
    
    # add x-axis
    #--------------------------------------------------------------------------------------
    axis (side = 1, at = seq (-0.4, 0.2, by = 0.2), cex.axis = 1.5)
    
    # add panel labels and axis 
    #--------------------------------------------------------------------------------------
    if (iDate == 'aug') {
      
      # add y-axis
      #------------------------------------------------------------------------------------
      axis (side = 2, at = yPositions, labels = c ('C','B','A','B','A','B','M','A'), 
            las = 1, cex.axis = 1.5)
      
      # add treatments
      #------------------------------------------------------------------------------------
      mtext (side = 2, line = 3,   text = 'control',    cex = 1.2, at = yPositions [1])
      mtext (side = 2, line = 3,   text = 'girdled',    cex = 1.2, at = mean (yPositions [c(2,3)]))
      mtext (side = 2, line = 3,   text = 'compressed', cex = 1.2, at = mean (yPositions [c(4,5)]))
      mtext (side = 2, line = 4.5, text = 'double',     cex = 1.2, at = mean (yPositions [c(6,7,8)]))
      mtext (side = 2, line = 3,   text = 'compressed', cex = 1.2, at = mean (yPositions [c(6,7,8)]))
      
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -0.5, y = 6.6, pos = 4, labels = 'august', cex = 2)
    } else if (iDate == 'oct') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -0.6, y = 6.6, pos = 4, labels = 'october', cex = 2)
      
      # add x-axis label
      #------------------------------------------------------------------------------------
      mtext (side = 1, line = 4, at = -0.1, cex = 1.5, 
             text = 'concentration (% dry weight)')
    } else if (iDate == 'nov') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -0.6, y = 6.6, pos = 4, labels = 'november', cex = 2)
    }
    
    # make a line separating the panels
    #--------------------------------------------------------------------------------------
    if (iDate != 'nov') abline (v = 0.33, col = '#666666')
  }
  
  # add typical between tree variation to plot
  #----------------------------------------------------------------------------------------
  betweenTreeVar <- as_tibble (VarCorr (M1)) [1, 5] [[1]]
  text (labels = expression (paste (sigma [tree])), x = 0.3, y = 0.4, 
        cex = 2, pos = 2, col = '#777777')
  segments (x0 = 0.3 - betweenTreeVar, x1 = 0.3, y0 = 0.2, 
            lwd = 4, col = '#777777')
dev.off ()

# Fit mixed effects model to the leaf soluble sugar concentration difference from baseline 
#----------------------------------------------------------------------------------------
M3 <- lmer (formula = deltaSugar ~ (1 | tree) + date + treatment:date,
            data = leafData2017 [41:160, ],
            REML = TRUE)
summary (M3)

# extract the model parameters from the lumen diameter model
#----------------------------------------------------------------------------------------
M03Values <- tibble (beta   = getME (M3, 'beta'), 
                     se     = as.numeric (coef (summary (M3)) [, 2]), 
                     tValue = as.numeric (coef (summary (M3)) [, 3]))
M03Values <- add_column (M03Values, .before = 1,
                         treatment = c (rep (1, 3), c (2, 2, 2, 3, 3, 3, 4, 4, 4)),
                         date = rep (c ('aug', 'oct', 'nov'), 4))

# create layout for the mixed model-based figure of change in sugar 
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017LeafDeltaSugar.png', width = 600, height = 500) 
  layout (matrix (1:3, nrow = 1, byrow = TRUE), widths = c (1.6, 1, 1.1))
  
  # loop over dates to create on plot of fractional cumulative cell wall area for each period
  #----------------------------------------------------------------------------------------
  for (iDate in c ('aug','oct','nov')) {
    
    # choose appropriate plot margins 
    #--------------------------------------------------------------------------------------
    if (iDate == 'aug') {
      par (mar = c (5, 10, 6, 0))
    } else if (iDate == 'oct') {
      par (mar = c (5, 0, 6, 0))
    } else {
      par (mar = c (5, 0, 6, 1))
    }
    
    # plot the cummulative cell wall area for each period
    #--------------------------------------------------------------------------------------
    plot (x = M03Values [['beta']] [M03Values [['date']] == iDate],
          y = M03Values [['treatment']] [M03Values [['date']] == iDate], 
          las = 1, xlab = '', ylab = '', axes = FALSE,
          xlim = c (-2.4, 4.5), ylim = c (0.3, 4.6), 
          col = colours [M03Values [['treatment']] [M03Values [['date']] == iDate]], 
          bg = ifelse (abs (M03Values [['tValue']] [M03Values [['date']] == iDate]) >= 2, 
                       colours [M03Values[['treatment']] [M03Values [['date']] == iDate]], 
                       'white'), lwd = 2, 
          cex = 2, #abs (M03Values [['tValue']] [M03Values [['date']] == iDate]),
          pch = ifelse (M03Values [['treatment']] [M03Values [['date']] == iDate] == 1, 
                        21, 24))
    
    # add zero line
    #------------------------------------------------------------------------------------
    segments (y0 = -0.5, y1 = 4.3, x0 = 0, col = '#999999', lwd = 1, lty = 2)
    
    # add rectangle for control standard error
    #--------------------------------------------------------------------------------------
    rect (xleft = M03Values [['beta']] [M03Values [['date']] == iDate] [1] - 
            M03Values [['se']]   [M03Values [['date']] == iDate] [1],
          xright = M03Values [['beta']] [M03Values [['date']] == iDate] [1] + 
            M03Values [['se']]   [M03Values [['date']] == iDate] [1],
          ybottom = 0.7, ytop = 4.2, lty = 0, col = '#aaaaaa55')
    
    # add standard error
    #--------------------------------------------------------------------------------------
    arrows (x0 = M03Values [['beta']] [M03Values [['date']] == iDate] - 
              M03Values [['se']]   [M03Values [['date']] == iDate],
            x1 = M03Values [['beta']] [M03Values [['date']] == iDate] + 
              M03Values [['se']]   [M03Values [['date']] == iDate],
            y0 = M03Values [['treatment']] [M03Values [['date']] == iDate], 
            code = 3, length = 0.05, angle = 90, lwd = 2, col = '#333333')
    
    # plot means
    #--------------------------------------------------------------------------------------
    points (x = M03Values [['beta']] [M03Values [['date']] == iDate],
            y = M03Values [['treatment']] [M03Values [['date']] == iDate],
            col = colours [M03Values [['treatment']] [M03Values [['date']] == iDate]], 
            bg = ifelse (abs (M03Values [['tValue']] [M03Values [['date']] == iDate]) >= 2, 
                         colours [M03Values[['treatment']] [M03Values [['date']] == iDate]], 
                         'white'), 
            lwd = 2, cex = 2, #abs (M03Values [['tValue']] [M03Values [['date']] == iDate]),
            pch = ifelse (M03Values [['treatment']] [M03Values [['date']] == iDate] == 1, 
                          21, 24))
    
    # add x-axis
    #--------------------------------------------------------------------------------------
    axis (side = 1, at = seq (-2, 4, by = 2), cex.axis = 1.5)
    
    # add panel labels and axis 
    #------------------------------------------------------------------------------------
    if (iDate == 'aug') {
      
      # add treatments
      #----------------------------------------------------------------------------------
      mtext (side = 2, line = 2,   text = 'control',    at = 1, cex = 1.2)
      mtext (side = 2, line = 2,   text = 'girdled',    at = 2, cex = 1.2)
      mtext (side = 2, line = 2,   text = 'compressed', at = 3, cex = 1.2)
      mtext (side = 2, line = 3.5, text = 'double',     at = 4, cex = 1.2)
      mtext (side = 2, line = 2,   text = 'compressed', at = 4, cex = 1.2)
      
      # add titles
      #----------------------------------------------------------------------------------
      mtext (side = 2, line = 7, at = 3.1, text = 'leaves', cex = 3)
      
      # add y-axis
      #--------------------------------------------------------------------------------------
      axis (side = 2, at = 1:4, labels = rep ('', 4))
      
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -2, y = 4.6, pos = 4, labels = 'august', cex = 2)
    } else if (iDate == 'oct') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -2, y = 4.6, pos = 4, labels = 'october', cex = 2)
      
      # add x-axis label
      #------------------------------------------------------------------------------------
      mtext (side = 1, line = 4, at = 0.75, cex = 1.5,
             text = expression (paste ('concentration (% dry weight)')))
      
      # add title
      #----------------------------------------------------------------------------------
      mtext (side = 3, line = 3, at = 0.5, text = 'sugar', cex = 3)
    } else if (iDate == 'nov') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -2, y = 4.6, pos = 4, labels = 'november', cex = 2)
    }
    
    # make a line separating the panels
    #--------------------------------------------------------------------------------------
    if (iDate != 'nov') abline (v = 4.7, col = '#666666')
  }
  
  # add typical between tree variation to plot
  #----------------------------------------------------------------------------------------
  betweenTreeVar <- as_tibble (VarCorr (M3)) [1, 5] [[1]]
  text (labels = expression (paste (sigma [tree])), x = 4.5, y = 0.5, 
        cex = 2, pos = 2, col = '#777777')
  segments (x0 = 4.5 - betweenTreeVar, x1 = 4.5, y0 = 0.3, 
            lwd = 4, col = '#777777')
dev.off ()

# Fit mixed effects model to the leaf starch concentration difference from baseline 
#----------------------------------------------------------------------------------------
M4 <- lmer (formula = deltaStarch ~ (1 | tree) + date + treatment:date,
            data = leafData2017 [41:160, ],
            REML = TRUE)
summary (M4)

# extract the model parameters from the lumen diameter model
#----------------------------------------------------------------------------------------
M04Values <- tibble (beta   = getME (M4, 'beta'), 
                     se     = as.numeric (coef (summary (M4)) [, 2]), 
                     tValue = as.numeric (coef (summary (M4)) [, 3]))
M04Values <- add_column (M04Values, .before = 1,
                         treatment = c (rep (1, 3), c (2, 2, 2, 3, 3, 3, 4, 4, 4)),
                         date = rep (c ('aug', 'oct', 'nov'), 4))

# create layout for the mixed model-based figure of change in starch 
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017LeafDeltaStarch.png', width = 600, height = 500) 
  layout (matrix (1:3, nrow = 1, byrow = TRUE), widths = c (1.4, 1, 1.1))
  
  # loop over dates to create on plot of change in leaf starch for each period
  #----------------------------------------------------------------------------------------
  for (iDate in c ('aug','oct','nov')) {
    
    # choose appropriate plot margins 
    #--------------------------------------------------------------------------------------
    if (iDate == 'aug') {
      par (mar = c (5, 5, 6, 0))
    } else if (iDate == 'oct') {
      par (mar = c (5, 0, 6, 0))
    } else {
      par (mar = c (5, 0, 6, 1))
    }
    
    # plot the cummulative cell wall area for each period
    #--------------------------------------------------------------------------------------
    plot (x = M04Values [['beta']] [M04Values [['date']] == iDate],
          y = M04Values [['treatment']] [M04Values [['date']] == iDate], 
          las = 1, xlab = '', ylab = '', axes = FALSE,
          xlim = c (-2.4, 2.0), ylim = c (0.3, 4.6), 
          col = colours [M04Values [['treatment']] [M04Values [['date']] == iDate]], 
          bg = ifelse (abs (M04Values [['tValue']] [M04Values [['date']] == iDate]) >= 2, 
                       colours [M04Values[['treatment']] [M04Values [['date']] == iDate]], 
                       'white'), lwd = 2, 
          cex = 2, #abs (M04Values [['tValue']] [M04Values [['date']] == iDate]),
          pch = ifelse (M04Values [['treatment']] [M04Values [['date']] == iDate] == 1, 
                        21, 24))
    
    # add zero line
    #------------------------------------------------------------------------------------
    segments (y0 = -0.5, y1 = 4.3, x0 = 0, col = '#999999', lwd = 1, lty = 2)
    
    # add rectangle for control standard error
    #--------------------------------------------------------------------------------------
    rect (xleft = M04Values [['beta']] [M04Values [['date']] == iDate] [1] - 
            M04Values [['se']]   [M04Values [['date']] == iDate] [1],
          xright = M04Values [['beta']] [M04Values [['date']] == iDate] [1] + 
            M04Values [['se']]   [M04Values [['date']] == iDate] [1],
          ybottom = 0.7, ytop = 4.2, lty = 0, col = '#aaaaaa55')
    
    # add standard error
    #--------------------------------------------------------------------------------------
    arrows (x0 = M04Values [['beta']] [M04Values [['date']] == iDate] - 
              M04Values [['se']]   [M04Values [['date']] == iDate],
            x1 = M04Values [['beta']] [M04Values [['date']] == iDate] + 
              M04Values [['se']]   [M04Values [['date']] == iDate],
            y0 = M04Values [['treatment']] [M04Values [['date']] == iDate], 
            code = 3, length = 0.05, angle = 90, lwd = 2, col = '#333333')
    
    # plot means
    #--------------------------------------------------------------------------------------
    points (x = M04Values [['beta']] [M04Values [['date']] == iDate],
            y = M04Values [['treatment']] [M04Values [['date']] == iDate],
            col = colours [M04Values [['treatment']] [M04Values [['date']] == iDate]], 
            bg = ifelse (abs (M04Values [['tValue']] [M04Values [['date']] == iDate]) >= 2, 
                         colours [M04Values[['treatment']] [M04Values [['date']] == iDate]], 
                         'white'), 
            lwd = 2, cex = 2, #abs (M04Values [['tValue']] [M04Values [['date']] == iDate]),
            pch = ifelse (M04Values [['treatment']] [M04Values [['date']] == iDate] == 1, 
                          21, 24))
    
    # add x-axis
    #--------------------------------------------------------------------------------------
    axis (side = 1, at = seq (-2, 2, by = 2), cex.axis = 1.5)
    
    # add panel labels and axis 
    #--------------------------------------------------------------------------------------
    if (iDate == 'aug') {
      
      # add treatments
      #------------------------------------------------------------------------------------
      mtext (side = 2, line = 2,   text = 'control',    cex = 1.2, at = 1)
      mtext (side = 2, line = 2,   text = 'girdled',    cex = 1.2, at = 2)
      mtext (side = 2, line = 2,   text = 'compressed', cex = 1.2, at = 3)
      mtext (side = 2, line = 3.5, text = 'double',     cex = 1.2, at = 4)
      mtext (side = 2, line = 2,   text = 'compressed', cex = 1.2, at = 4)
      
      # add y-axis
      #--------------------------------------------------------------------------------------
      axis (side = 2, at = 1:4, labels = rep ('', 4))
      
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -2.4, y = 4.7, pos = 4, labels = 'august', cex = 2)
    } else if (iDate == 'oct') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -2.4, y = 4.7, pos = 4, labels = 'october', cex = 2)
      
      # add title
      #----------------------------------------------------------------------------------
      mtext (side = 3, line = 3, at = -0.4, text = 'starch', cex = 3)
      
      # add x-axis label
      #------------------------------------------------------------------------------------
      mtext (side = 1, line = 4, at = -0.2, cex = 1.5,
             text = 'concentration (% dry weight)')
    } else if (iDate == 'nov') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -2.4, y = 4.7, pos = 4, labels = 'november', cex = 2)
    }
    
    # make a line separating the panels
    #--------------------------------------------------------------------------------------
    if (iDate != 'nov') abline (v = 2.1, col = '#666666')
  }
  
  # add typical between tree variation to plot
  #----------------------------------------------------------------------------------------
  betweenTreeVar <- as_tibble (VarCorr (M4)) [1, 5] [[1]]
  text (labels = expression (paste (sigma [tree])), x = 2, y = 0.5, 
        cex = 2, pos = 2, col = '#777777')
  segments (x0 = 2 - betweenTreeVar, x1 = 2, y0 = 0.3, 
            lwd = 4, col = '#777777')
dev.off ()

# Fit mixed effects model to the root soluble sugar concentration difference from baseline 
#----------------------------------------------------------------------------------------
M5 <- lmer (formula = deltaSugar ~ (1 | tree) + date + treatment:date,
            data = rootData2017 [41:160, ],
            REML = TRUE)
summary (M5)

# extract the model parameters from the root sugar model
#----------------------------------------------------------------------------------------
M05Values <- tibble (beta   = getME (M5, 'beta'), 
                     se     = as.numeric (coef (summary (M5)) [, 2]), 
                     tValue = as.numeric (coef (summary (M5)) [, 3]))
M05Values <- add_column (M05Values, .before = 1,
                         treatment = c (rep (1, 3), c (2, 2, 2, 3, 3, 3, 4, 4, 4)),
                         date = rep (c ('aug', 'oct', 'nov'), 4))

# create layout for the mixed model-based figure of change in starch 
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017RootDeltaSugar.png', width = 600, height = 450) 
  layout (matrix (1:3, nrow = 1, byrow = TRUE), widths = c (1.6, 1, 1.1))
  
  # loop over dates to create on plot of change in leaf starch for each period
  #----------------------------------------------------------------------------------------
  for (iDate in c ('aug','oct','nov')) {
    
    # choose appropriate plot margins 
    #--------------------------------------------------------------------------------------
    if (iDate == 'aug') {
      par (mar = c (5, 10, 1, 0))
    } else if (iDate == 'oct') {
      par (mar = c (5, 0, 1, 0))
    } else {
      par (mar = c (5, 0, 1, 1))
    }
    
    # plot the cummulative cell wall area for each period
    #--------------------------------------------------------------------------------------
    plot (x = M05Values [['beta']] [M05Values [['date']] == iDate],
          y = M05Values [['treatment']] [M05Values [['date']] == iDate], 
          las = 1, xlab = '', ylab = '', axes = FALSE,
          xlim = c (-0.8, 0.8), ylim = c (0.3, 4.6), 
          col = colours [M05Values [['treatment']] [M05Values [['date']] == iDate]], 
          bg = ifelse (abs (M05Values [['tValue']] [M05Values [['date']] == iDate]) >= 2, 
                       colours [M05Values[['treatment']] [M05Values [['date']] == iDate]], 
                       'white'), lwd = 2, 
          cex = 2, #abs (M05Values [['tValue']] [M05Values [['date']] == iDate]),
          pch = ifelse (M05Values [['treatment']] [M05Values [['date']] == iDate] == 1, 
                        21, 24))
    
    # add zero line
    #------------------------------------------------------------------------------------
    segments (y0 = -0.5, y1 = 4.3, x0 = 0, col = '#999999', lwd = 1, lty = 2)
    
    # add rectangle for control standard error
    #--------------------------------------------------------------------------------------
    rect (xleft = M05Values [['beta']] [M05Values [['date']] == iDate] [1] - 
            M05Values [['se']]   [M05Values [['date']] == iDate] [1],
          xright = M05Values [['beta']] [M05Values [['date']] == iDate] [1] + 
            M05Values [['se']]   [M05Values [['date']] == iDate] [1],
          ybottom = 0.7, ytop = 4.2, lty = 0, col = '#aaaaaa55')
    
    # add standard error
    #--------------------------------------------------------------------------------------
    arrows (x0 = M05Values [['beta']] [M05Values [['date']] == iDate] - 
              M05Values [['se']]   [M05Values [['date']] == iDate],
            x1 = M05Values [['beta']] [M05Values [['date']] == iDate] + 
              M05Values [['se']]   [M05Values [['date']] == iDate],
            y0 = M05Values [['treatment']] [M05Values [['date']] == iDate], 
            code = 3, length = 0.05, angle = 90, lwd = 2, col = '#333333')
    
    # plot means
    #--------------------------------------------------------------------------------------
    points (x = M05Values [['beta']] [M05Values [['date']] == iDate],
            y = M05Values [['treatment']] [M05Values [['date']] == iDate],
            col = colours [M05Values [['treatment']] [M05Values [['date']] == iDate]], 
            bg = ifelse (abs (M05Values [['tValue']] [M05Values [['date']] == iDate]) >= 2, 
                         colours [M05Values[['treatment']] [M05Values [['date']] == iDate]], 
                         'white'), 
            lwd = 2, cex = 2, #abs (M05Values [['tValue']] [M05Values [['date']] == iDate]),
            pch = ifelse (M05Values [['treatment']] [M05Values [['date']] == iDate] == 1, 
                          21, 24))
    
    # add x-axis
    #--------------------------------------------------------------------------------------
    axis (side = 1, at = seq (-0.5, 0.5, by = 0.5), cex.axis = 1.5)
    
    # add panel labels and axis 
    #--------------------------------------------------------------------------------------
    if (iDate == 'aug') {
      
      # add treatments
      #------------------------------------------------------------------------------------
      mtext (side = 2, line = 2,   text = 'control',    cex = 1.2, at = 1)
      mtext (side = 2, line = 2,   text = 'girdled',    cex = 1.2, at = 2)
      mtext (side = 2, line = 2,   text = 'compressed', cex = 1.2, at = 3)
      mtext (side = 2, line = 3.5, text = 'double',     cex = 1.2, at = 4)
      mtext (side = 2, line = 2,   text = 'compressed', cex = 1.2, at = 4)
      mtext (side = 2, line = 7,   text = 'roots',      cex = 3,   at = 2.5)
      
      # add y-axis
      #--------------------------------------------------------------------------------------
      axis (side = 2, at = 1:4, labels = rep ('', 4))
      
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -0.8, y = 4.6, pos = 4, labels = 'august', cex = 2)
    } else if (iDate == 'oct') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -0.8, y = 4.6, pos = 4, labels = 'october', cex = 2)
      
      # add x-axis label
      #------------------------------------------------------------------------------------
      mtext (side = 1, line = 4, at = 0, cex = 1.5,
             text = expression (paste ('concentration (% dry weight)')))
    } else if (iDate == 'nov') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -0.8, y = 4.6, pos = 4, labels = 'november', cex = 2)
    }
    
    # make a line separating the panels
    #--------------------------------------------------------------------------------------
    if (iDate != 'nov') abline (v = 0.85, col = '#666666')
  }
  
  # add typical between tree variation to plot
  #----------------------------------------------------------------------------------------
  betweenTreeVar <- as_tibble (VarCorr (M5)) [1, 5] [[1]]
  text (labels = expression (paste (sigma [tree])), x = 0.8, y = 0.5, 
        cex = 2, pos = 2, col = '#777777')
  segments (x0 = 0.8 - betweenTreeVar, x1 = 0.8, y0 = 0.3, 
            lwd = 4, col = '#777777')
dev.off ()

# Fit mixed effects model to the root starch concentration difference from baseline 
#----------------------------------------------------------------------------------------
M6 <- lmer (formula = deltaStarch ~ (1 | tree) + date + treatment:date,
            data = rootData2017 [41:160, ],
            REML = TRUE)
summary (M6)

# extract the model parameters from the root starch model
#----------------------------------------------------------------------------------------
M06Values <- tibble (beta   = getME (M6, 'beta'), 
                     se     = as.numeric (coef (summary (M6)) [, 2]), 
                     tValue = as.numeric (coef (summary (M6)) [, 3]))
M06Values <- add_column (M06Values, .before = 1,
                         treatment = c (rep (1, 3), c (2, 2, 2, 3, 3, 3, 4, 4, 4)),
                         date = rep (c ('aug', 'oct', 'nov'), 4))

# create layout for the mixed model-based figure of change in starch 
#----------------------------------------------------------------------------------------
png (filename = '../fig/Exp2017RootDeltaStarch.png', width = 600, height = 450) 
  layout (matrix (1:3, nrow = 1, byrow = TRUE), widths = c (1.3, 1, 1.1))
  
  # loop over dates to create on plot of change in leaf starch for each period
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
    plot (x = M06Values [['beta']] [M06Values [['date']] == iDate],
          y = M06Values [['treatment']] [M06Values [['date']] == iDate], 
          las = 1, xlab = '', ylab = '', axes = FALSE,
          xlim = c (-0.8, 0.6), ylim = c (0.3, 4.6), 
          col = colours [M06Values [['treatment']] [M06Values [['date']] == iDate]], 
          bg = ifelse (abs (M06Values [['tValue']] [M06Values [['date']] == iDate]) >= 2, 
                       colours [M06Values[['treatment']] [M06Values [['date']] == iDate]], 
                       'white'), lwd = 2, 
          cex = 2, #abs (M06Values [['tValue']] [M06Values [['date']] == iDate]),
          pch = ifelse (M06Values [['treatment']] [M06Values [['date']] == iDate] == 1, 
                        21, 24))
    
    # add zero line
    #------------------------------------------------------------------------------------
    segments (y0 = -0.5, y1 = 4.4, x0 = 0, col = '#999999', lwd = 1, lty = 2)
    
    # add rectangle for control standard error
    #--------------------------------------------------------------------------------------
    rect (xleft = M06Values [['beta']] [M06Values [['date']] == iDate] [1] - 
            M06Values [['se']]   [M06Values [['date']] == iDate] [1],
          xright = M06Values [['beta']] [M06Values [['date']] == iDate] [1] + 
            M06Values [['se']]   [M06Values [['date']] == iDate] [1],
          ybottom = 0.7, ytop = 4.3, lty = 0, col = '#aaaaaa55')
    
    # add standard error
    #--------------------------------------------------------------------------------------
    arrows (x0 = M06Values [['beta']] [M06Values [['date']] == iDate] - 
              M06Values [['se']]   [M06Values [['date']] == iDate],
            x1 = M06Values [['beta']] [M06Values [['date']] == iDate] + 
              M06Values [['se']]   [M06Values [['date']] == iDate],
            y0 = M06Values [['treatment']] [M06Values [['date']] == iDate], 
            code = 3, length = 0.05, angle = 90, lwd = 2, col = '#333333')
    
    # plot means
    #--------------------------------------------------------------------------------------
    points (x = M06Values [['beta']] [M06Values [['date']] == iDate],
            y = M06Values [['treatment']] [M06Values [['date']] == iDate],
            col = colours [M06Values [['treatment']] [M06Values [['date']] == iDate]], 
            bg = ifelse (abs (M06Values [['tValue']] [M06Values [['date']] == iDate]) >= 2, 
                         colours [M06Values[['treatment']] [M06Values [['date']] == iDate]], 
                         'white'), 
            lwd = 2, cex = 2, #abs (M06Values [['tValue']] [M06Values [['date']] == iDate]),
            pch = ifelse (M06Values [['treatment']] [M06Values [['date']] == iDate] == 1, 
                          21, 24))
    
    # add x-axis
    #--------------------------------------------------------------------------------------
    axis (side = 1, at = seq (-0.8, 0.6, by = 0.4), cex.axis = 1.5)
    
    # add panel labels and axis 
    #--------------------------------------------------------------------------------------
    if (iDate == 'aug') {
      
      # add treatments
      #------------------------------------------------------------------------------------
      mtext (side = 2, line = 2,   text = 'control',    at = 1, cex = 1.2)
      mtext (side = 2, line = 2,   text = 'girdled',    at = 2, cex = 1.2)
      mtext (side = 2, line = 2,   text = 'compressed', at = 3, cex = 1.2)
      mtext (side = 2, line = 3.5, text = 'double',     at = 4, cex = 1.2)
      mtext (side = 2, line = 2,   text = 'compressed', at = 4, cex = 1.2)
      
      # add y-axis
      #--------------------------------------------------------------------------------------
      axis (side = 2, at = 1:4, labels = rep ('', 4))
      
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -0.8, y = 4.6, pos = 4, labels = 'august', cex = 2)
    } else if (iDate == 'oct') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -0.8, y = 4.6, pos = 4, labels = 'october', cex = 2)
      
      # add x-axis label
      #------------------------------------------------------------------------------------
      mtext (side = 1, line = 4, at = -0.1, cex = 1.5,
             text = expression (paste ('concentration (% dry weight)')))
    } else if (iDate == 'nov') {
      # add panel descriptor
      #------------------------------------------------------------------------------------
      text (x = -0.8, y = 4.6, pos = 4, labels = 'november', cex = 2)
    }
    
    # make a line separating the panels
    #--------------------------------------------------------------------------------------
    if (iDate != 'nov') abline (v = 0.65, col = '#666666')
  }
  
  # add typical between tree variation to plot
  #----------------------------------------------------------------------------------------
  betweenTreeVar <- as_tibble (VarCorr (M6)) [1, 5] [[1]]
  text (labels = expression (paste (sigma [tree])), x = 0.6, y = 0.5, 
        cex = 2, pos = 2, col = '#777777')
  segments (x0 = 0.6 - betweenTreeVar, x1 = 0.6, y0 = 0.3, 
            lwd = 4, col = '#777777')
dev.off ()

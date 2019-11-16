#========================================================================================
# Script to make Figure X
# 
# Manuscript: 
#
#
#
# Figure caption:
#   Wood anatomical properties of the 2017 and 2015 growing season. (a) Radial cell wall 
#   thickness, (b) cell lumen diameter and (c) number of cells are show for each sector 
#   and treatment (colour) of the growth ring for 2017 (solid lines) and 2015 (dashed 
#   lines). Each line represents the treatment mean (n = 10) with the standard deviation 
#   displayed as shading. 
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
require ('readxl')
require ('plyr')
require ('dplyr')
require ('ggplot2')
source ("../data/HF_2017_T1_T4_Rox/Functions/1-read.roxas.R")
source ("../data/HF_2017_T1_T4_Rox/Functions/2-assign.to.R")
source ('colourPalette.R')

#----------------------------------------------------------------------------------------
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  ## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
  ##   data: a data frame.
  ##   measurevar: the name of a column that contains the variable to be summariezed
  ##   groupvars: a vector containing names of columns that contain grouping variables
  ##   na.rm: a boolean that indicates whether to ignore NA's
  ##   conf.interval: the percent range of the confidence interval (default is 95%)
  
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Read ROXAS outputs for cell level data
#----------------------------------------------------------------------------------------
T2017 <- read.roxas (WD = '../data/HF_2017_T1_T4_Rox/', 
                     from = 2015, to = 2017, 
                     PLOT = FALSE)

# Assign cell level data to sectors
#----------------------------------------------------------------------------------------
cellularData <- as_tibble (assign.to (T2017 [['cell']], method = "SECTOR", NSECTOR = 10))
cellularData [['Treatment']]    <- as.numeric (substr (cellularData [['TREE.ID']], 2, 2))
cellularData [['sampleHeight']] <- substr (cellularData [['TREE.ID']], 6, 6)
cellularData [['TREE.ID']]      <- as.numeric (substr (cellularData [['TREE.ID']], 4, 5))
cellularData [['NBRNO']]        <- as.numeric (cellularData [['NBRNO']])
cellularData [['TO.SECTORS']]   <- as.factor (cellularData [['TO.SECTORS']])

sectorAssociatedData <- cellularData %>% 
                        filter (YEAR >= 2015, CWTrad != -9999, CWTrad != -999, !is.na (TO.SECTORS)) %>%
                        group_by (SITE, TREE.ID, as.factor (YEAR), Treatment, TO.SECTORS)

sectorSummary <- ddply (sectorAssociatedData, 
                        c ("SITE", "TREE.ID", "YEAR", "Treatment", "sampleHeight", "TO.SECTORS"), 
                        summarise,
                        LDtanmean = mean (LDtan),
                        CWTradmean = mean (CWTrad, na.rm = TRUE))

B.CWT <- summarySE (sectorSummary, 
                    measurevar = "CWTradmean", 
                    groupvars = c ("Treatment", "YEAR", "sampleHeight", "TO.SECTORS"))
B.LD <- summarySE (sectorSummary, 
                   measurevar = "LDtanmean", 
                   groupvars = c ("Treatment", "YEAR", "sampleHeight", "TO.SECTORS"))

# Make cell wall thickness plot
var0 <- aggregate (B.CWT [['CWTradmean']] [B.CWT [['YEAR']] == 2017], mean, 
                   by = list (B.CWT [['TO.SECTORS']]   [B.CWT [['YEAR']] == 2017], 
                              B.CWT [['Treatment']]    [B.CWT [['YEAR']] == 2017],
                              B.CWT [['sampleHeight']] [B.CWT [['YEAR']] == 2017]))
var1 <- aggregate (B.CWT [['ci']] [B.CWT [['YEAR']] == 2017], mean, 
                   by = list (B.CWT [['TO.SECTORS']]   [B.CWT [['YEAR']] == 2017], 
                              B.CWT [['Treatment']]    [B.CWT [['YEAR']] == 2017],
                              B.CWT [['sampleHeight']] [B.CWT [['YEAR']] == 2017]))
names (var0) <- c ('sector','treatment','sampleHeight','CWT')
names (var1) <- c ('sector','treatment','sampleHeight','CWTci')
var0 [['sector']] <- as.numeric (var0 [['sector']]); var1 [['sector']] <- as.numeric (var1 [['sector']])
par  (mar = c (5,5,1,1))
plot (x = var0 [['sector']] [var0 [['treatment']] == 1],
      y = var0 [['CWT']] [var0 [['treatment']] == 1],
      typ = 'l',
      las = 1, lwd = 2,
      col = colours [1],
      xlab = 'percent ring width (%)',
      ylab = expression (paste ('radial cell wall thickness (',mu,'m)'), sep = ''),
      ylim = c (0, 6),
      xaxt = 'n')
axis (side = 1, at = 1:10, labels = seq (10, 100, by = 10))
polygon (x = c (1:10, 10:1),
         y = c (var0 [['CWT']] [var0 [['treatment']] == 1] - var1 [['CWTci']] [var1 [['treatment']] == 1],
                rev (var0 [['CWT']] [var0 [['treatment']] == 1] + var1 [['CWTci']] [var1 [['treatment']] == 1])),
         col = coloursShade [1],
         lty = 0)
lines (x = var0 [['sector']] [var0 [['treatment']] == 2 & var0 [['sampleHeight']] == 'A'],
       y = var0 [['CWT']] [var0 [['treatment']] == 2 & var0 [['sampleHeight']] == 'A'],
       lwd = 2,
       col = colours [2])
polygon (x = c (1:10, 10:1),
         y = c (var0 [['CWT']] [var0 [['treatment']] == 2 & var0 [['sampleHeight']] == 'A'] - 
                var1 [['CWTci']] [var1 [['treatment']] == 2 & var1 [['sampleHeight']] == 'A'],
                rev (var0 [['CWT']] [var0 [['treatment']] == 2 & var0 [['sampleHeight']] == 'A'] + 
                     var1 [['CWTci']] [var1 [['treatment']] == 2 & var1 [['sampleHeight']] == 'A'])),
         col = coloursShade [2],
         lty = 0)
lines (x = var0 [['sector']] [var0 [['treatment']] == 2 & var0 [['sampleHeight']] == 'B'],
       y = var0 [['CWT']] [var0 [['treatment']] == 2 & var0 [['sampleHeight']] == 'B'],
       lwd = 2,
       col = colours [2])
polygon (x = c (1:10, 10:1),
         y = c (var0 [['CWT']] [var0 [['treatment']] == 2 & var0 [['sampleHeight']] == 'B'] - 
                  var1 [['CWTci']] [var1 [['treatment']] == 2 & var1 [['sampleHeight']] == 'B'],
                rev (var0 [['CWT']] [var0 [['treatment']] == 2 & var0 [['sampleHeight']] == 'B'] + 
                       var1 [['CWTci']] [var1 [['treatment']] == 2 & var1 [['sampleHeight']] == 'B'])),
         col = coloursShade [2],
         lty = 0)
#png (filename = 'figX.png')
#dev.off()
#========================================================================================

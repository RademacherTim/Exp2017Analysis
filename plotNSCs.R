#========================================================================================
# Script to plot the sugar data for the 2017 experiment at Harvard Forest
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
source ('processNSCDataForExp2017.R')

# Make boxplot of treatments and height
#----------------------------------------------------------------------------------------
NSC = 'Sugar'
data = rootData
if (NSC == 'Sugar' & data [['Tissue']] [1] == 'Wood') {
  yLimit <- c (0, 2.4) 
} else if (NSC == 'Starch' & data [['Tissue']] [1] == 'Wood') {
  yLimit <- c (0, 1.0)
} else if (NSC == 'Sugar' & data [['Tissue']] [1] == 'Root') {
  yLimit <- c (0, 3.5)
  NOTWOOD <- TRUE
} else if (NSC == 'Starch' & data [['Tissue']] [1] == 'Root') {
  yLimit <- c (0, 1.6)
  NOTWOOD <- TRUE
} else if (NSC == 'Sugar' & data [['Tissue']] [1] == 'Leaf') {
  yLimit <- c (0, 13)
  NOTWOOD <- TRUE
} else if (NSC == 'Starch' & data [['Tissue']] [1] == 'Leaf') {
  yLimit <- c (0, 9)
  NOTWOOD <- TRUE
}
colours <- c ('#8dd3c799','#ffffb399','#bebada99','#fb807299')
par (mar = c (5, 5, 1, 1))
boxplot (data [[paste ('Concentration',NSC,'PerDW', sep = '')]] [data [['treatment']] == 1 &
                                                                 month (data [['DateOfSampleCollection']]) == 7],
         xlim = c (0, 38.5),
         ylim = yLimit,
         col  = 'white',
         lty = 0,
         las = 1,
         ylab = 'sugar concentration (% dry weight)')
addBoxplot <- function (data, iTreat, iMon, position, iH = 1.5, NSC = 'Sugar') {
  months <- c (7,8,10,11)
  if (is.na (iH)) {
    additional <- 0.0
  } else if (iTreat == 2 & iH == 1.0) {
    additional <- 0.5
  } else if (iTreat == 2 & iH == 2.0) {
    additional <- 1.0
  } else if (iTreat == 3 & iH == 1.0) {
    additional <- 1.5
  } else if (iTreat == 3 & iH == 2.0) {
    additional <- 2.0
  } else if (iTreat == 4 & iH == 0.5) {
    additional <- 2.5
  } else if (iTreat == 4 & iH == 1.5) {
    additional <- 3.0
  } else if (iTreat == 4 & iH == 2.5) {
    additional <- 3.5
  } else {
    additional <- 0.0
  }
  
  if (is.na (iH)) {
    boxplot (data [[paste ('Concentration',NSC,'PerDW', sep = '')]] [data [['treatment']] == iTreat & 
                                                                     month (data [['DateOfSampleCollection']]) == iMon], 
             col = colours [iTreat],
             at  = (which (iMon == months) - 1) * 10 + iTreat + additional,
             add = TRUE,
             axes = FALSE)
    
  } else {
    boxplot (data [[paste ('Concentration',NSC,'PerDW', sep = '')]] [data [['treatment']] == iTreat & 
                                                                     data [['sampleHeight']] == iH &
                                                 month (data [['DateOfSampleCollection']]) == iMon], 
             col = colours [iTreat],
             at  = (which (iMon == months) - 1) * 10 + iTreat + additional,
             add = TRUE,
             axes = FALSE)
  }
}
if (NOTWOOD) {
  res <- sapply (c (7, 8, 10, 11), addBoxplot, data = data, iTreat = 1, NSC = NSC, iH = NA) 
} else {
  res <- sapply (c (7, 8, 10, 11), addBoxplot, data = data, iTreat = 1, NSC = NSC, iH = 1.5)
}

# Treatment 2 - Girdling
#----------------------------------------------------------------------------------------
if (NOTWOOD) {
  res <- sapply (c (7, 8, 10, 11), addBoxplot, data = data, iTreat = 2, iH = NA, NSC = NSC) 
} else {
  res <- sapply (c (7, 8, 10, 11), addBoxplot, data = data, iTreat = 2, iH = 1.0, NSC = NSC)
  res <- sapply (c (7, 8, 10, 11), addBoxplot, data = data, iTreat = 2, iH = 2.0, NSC = NSC) 
}
    
# Treatment 3 - Single compression
#----------------------------------------------------------------------------------------
if (NOTWOOD) {
  res <- sapply (c (7, 8, 10, 11), addBoxplot, data = data, iTreat = 3, iH = NA, NSC = NSC) 
} else {
  res <- sapply (c (7, 8, 10, 11), addBoxplot, data = data, iTreat = 3, iH = 1.0, NSC = NSC) 
  res <- sapply (c (7, 8, 10, 11), addBoxplot, data = data, iTreat = 3, iH = 2.0, NSC = NSC) 
}
# Treatment 4 - Double compression
#----------------------------------------------------------------------------------------
if (NOTWOOD) {
  res <- sapply (c (7, 8, 10, 11), addBoxplot, data = data, iTreat = 4, iH = NA, NSC = NSC) 
} else {
  res <- sapply (c (7, 8, 10, 11), addBoxplot, data = data, iTreat = 4, iH = 0.5, NSC = NSC)
  res <- sapply (c (7, 8, 10, 11), addBoxplot, data = data, iTreat = 4, iH = 1.5, NSC = NSC) 
  res <- sapply (c (7, 8, 10, 11), addBoxplot, data = data, iTreat = 4, iH = 2.5, NSC = NSC) 
}
# Add a reasonable x axis
#----------------------------------------------------------------------------------------
axis (side = 1, at = seq (4.6125, 34.6125, by = 10), 
      label = c ('July', 'August', 'October', 'November'))
abline (v = c (9.25,19.25,29.25), col = '#99999999')
#========================================================================================


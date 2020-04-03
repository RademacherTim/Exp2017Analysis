#----------------------------------------------------------------------------------------
# draw stacked barplots of absolute values for structural carbon gain, change in 
# nonstructural carbon, and respiratory losses. 
#----------------------------------------------------------------------------------------

# summarise data by group
#----------------------------------------------------------------------------------------
summaryData <- allData %>% group_by (treatment, height, month) %>% 
               summarise (meanResp = mean (resp, na.rm = TRUE), 
                          meanSC   = mean (SC, na.rm = TRUE), 
                          meanNSC  = mean (total, na.rm = TRUE),
                          meanSugar = mean (sugar, na.rm = TRUE),
                          meanStarch = mean (starch, na.rm = TRUE))

# create panel of three barplot for period changes 
#----------------------------------------------------------------------------------------
par (mfrow = c (1, 3))
for (m in c ('august','october','november')) {

  # get two matrices, one for structural carbon gain
  #----------------------------------------------------------------------------------------
  summaryDataPos <- summaryData %>% ungroup %>% filter (month == m) %>% 
                    select (meanResp, meanSC, meanSugar, meanStarch)
  summaryLabels <- summaryData %>% filter (month == m) %>% 
                   select (treatment, height)
  summaryLabels <- summaryLabels [8:1, ]
  summaryDataNeg <- summaryDataPos [8:1, ]; summaryDataPos <- summaryDataPos [8:1, ]
  summaryDataPos [summaryDataPos < 0] <- 0
  summaryDataNeg [summaryDataNeg > 0] <- 0
  
  # Switch order of rows
  #----------------------------------------------------------------------------------------
  summaryDataPos <- summaryDataPos [, c (4, 3, 2, 1)]
  summaryDataNeg <- summaryDataNeg [, c (3, 4, 1, 2)]

  # draw stacked barplot
  #----------------------------------------------------------------------------------------
  if (m == 'august') {par (mar = c (5, 5, 2, 1))} else {par (mar = c (5, 1, 2, 1))}
  barplot (height = t (as.matrix (summaryDataPos)), horiz = TRUE, 
           xlab ='change (g Carbon)', xlim = c (-20, 25), 
           border = 0, col = c ('#542788','#b2abd2','#1b7837','#b35806'),
           space = c (1,2,1,2,1,2,1,1))
  barplot (height = t (as.matrix (summaryDataNeg)), horiz = TRUE, add = TRUE,
           border = 0, col = c ('#542788','#b2abd2','#b35806','#1b7837'),
           space = c (1,2,1,2,1,2,1,1))
  if (m == 'august') {
    axis (side = 2, at = c (1.5, 4.5, 6.5, 9.5, 11.5, 14.5, 16.5, 18.5), las = 1,
          labels = c ('C','B','A','B','A','B','M','A'))
    mtext (side = 2, line = 2, at = c(1.5, 5.5, 10.5, 16.5), 
           text = c ('control','girdled','compressed','double \n compressed'))
  }
  abline (v = 0, col = '#666666')
  if (m == 'august') {
    descriptor <- expression (paste (1^st,' month'))
  } else if (m == 'october') {
    descriptor <- expression (paste (2^nd, ' & ', 3^rd,' month'))
  } else {
    descriptor <- expression (paste (4^th,' month'))
  }
  mtext (side = 3, at = -12, text = descriptor, col = '#333333')
}
legend ('right', legend = c ('respiratory loss', expression (paste (delta, ' starch')), 
                             expression (paste (delta, ' sugar')),'growth'),
        fill = c ('#b35806','#542788','#b2abd2','#1b7837'), box.lty = 0, border = 0, 
        bg = 'transparent', cex = 0.8)
#========================================================================================


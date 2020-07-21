#========================================================================================
# Script to plot shallow wood versus deeper wood tissues
#----------------------------------------------------------------------------------------


# Plot scatterplot of sugar in 1cm versus sugar in second cm for July
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (x = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 1) [['sugar']],
      y = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 2) [['sugar']],
      las = 1, xlab = 'soluble sugar in 0-1 cm (% dry weight)', xlim = c (0, 1.6), ylim = c (0, 1.6),
      ylab = 'soluble sugar in 1-2 cm (% dry weight)', col = '#91b9a4aa', pch = 19)
abline (a = 0, b = 1, col = '#666666', lty = 2)

# Plot scatterplot of starch in 1cm versus starch in second cm for July
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (x = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 1) [['starch']],
      y = filter (stemData2017, date == as_datetime ('2017-07-05') & sampleDepth == 2) [['starch']],
      las = 1, xlab = 'starch in 0-1 cm (% dry weight)',  xlim = c (0, 0.8), ylim = c (0, 0.8),
      ylab = 'starch in 1-2 cm (% dry weight)', col = '#91b9a4aa', pch = 19)
abline (a = 0, b = 1, col = '#666666', lty = 2)

# Plot scatterplot of sugar in 1cm versus sugar in second cm for November
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (x = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 1) [['sugar']],
      y = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 2) [['sugar']],
      las = 1, xlab = 'soluble sugar in 0-1 cm (% dry weight)', xlim = c (0, 1.6), ylim = c (0, 1.6),
      ylab = 'soluble sugar in 1-2 cm (% dry weight)', col = '#91b9a4aa', pch = 19)
abline (a = 0, b = 1, col = '#666666', lty = 2)

# Plot scatterplot of starch in 1cm versus starch in second cm for November
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
plot (x = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 1) [['starch']],
      y = filter (stemData2017, date == as_datetime ('2017-11-03') & sampleDepth == 2) [['starch']],
      las = 1, xlab = 'starch in 0-1 cm (% dry weight)', xlim = c (0, 0.8), ylim = c (0, 0.8),
      ylab = 'starch in 1-2 cm (% dry weight)', col = '#91b9a4aa', pch = 19)
abline (a = 0, b = 1, col = '#666666', lty = 2)
#========================================================================================

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

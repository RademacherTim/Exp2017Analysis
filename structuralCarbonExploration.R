# Compare mean 2016 growth 
# mean (colMeans (ringWidthsJul [9:13], na.rm = TRUE))
# mean (colMeans (ringWidthsAug [9:13], na.rm = TRUE))
# mean (colMeans (ringWidthsOct [9:13], na.rm = TRUE))
# mean (colMeans (ringWidthsNov [9:13], na.rm = TRUE))
# dJul <- density (unlist (ringWidthsJul [9:13]), na.rm = T)
# dAug <- density (unlist (ringWidthsAug [9:13]), na.rm = T)
# dOct <- density (unlist (ringWidthsOct [9:13]), na.rm = T)
# dNov <- density (unlist (ringWidthsNov [9:13]), na.rm = T)

# Get mean 2017 growth for each month for all treatments
# mean (colMeans (ringWidthsJul [4:8], na.rm = TRUE)) # 0.914724
# mean (colMeans (ringWidthsAug [4:8], na.rm = TRUE)) # 1.437826
# mean (colMeans (ringWidthsOct [4:8], na.rm = TRUE)) # 1.677127
# mean (colMeans (ringWidthsNov [4:8], na.rm = TRUE)) # 1.7327

# Get mean growth per treatment for each month
# colMeans (ringWidthsJul [ringWidthsJul [['treatment']] == 1, 4:8], na.rm = TRUE)
# colMeans (ringWidthsAug [ringWidthsAug [['treatment']] == 1, 4:8], na.rm = TRUE)
# colMeans (ringWidthsOct [ringWidthsOct [['treatment']] == 1, 4:8], na.rm = TRUE)
# colMeans (ringWidthsNov [ringWidthsNov [['treatment']] == 1, 4:8], na.rm = TRUE)


# colMeans (ringWidthsJul [ringWidthsJul [['treatment']] == 2, 4:8], na.rm = TRUE)
# colMeans (ringWidthsAug [ringWidthsAug [['treatment']] == 2, 4:8], na.rm = TRUE)
# colMeans (ringWidthsOct [ringWidthsOct [['treatment']] == 2, 4:8], na.rm = TRUE)
# colMeans (ringWidthsNov [ringWidthsNov [['treatment']] == 2, 4:8], na.rm = TRUE)

# colMeans (ringWidthsJul [ringWidthsJul [['treatment']] == 3, 4:8], na.rm = TRUE)
# colMeans (ringWidthsAug [ringWidthsAug [['treatment']] == 3, 4:8], na.rm = TRUE)
# colMeans (ringWidthsOct [ringWidthsOct [['treatment']] == 3, 4:8], na.rm = TRUE)
# colMeans (ringWidthsNov [ringWidthsNov [['treatment']] == 3, 4:8], na.rm = TRUE)

# colMeans (ringWidthsJul [ringWidthsJul [['treatment']] == 4, 4:8], na.rm = TRUE)
# colMeans (ringWidthsAug [ringWidthsAug [['treatment']] == 4, 4:8], na.rm = TRUE)
# colMeans (ringWidthsOct [ringWidthsOct [['treatment']] == 4, 4:8], na.rm = TRUE)
# colMeans (ringWidthsNov [ringWidthsNov [['treatment']] == 4, 4:8], na.rm = TRUE)

# plot (dJul, xlab = 'ring width (mm)', ylab = 'density',
#       xlim = c (0, 5.5), ylim = c (0, 0.5), main = '2016', 
#       col = '#018571', lwd = 2, las = 1)
# lines (dAug, col = '#80cdc1', lwd = 2)
# lines (dOct, col = '#dfc27d', lwd = 2)
# lines (dNov, col = '#a6611a', lwd = 2)
# legend (x = 2.5, y = 0.51, legend = c ('July','August','October','November'),
#         col = c ('#018571', '#80cdc1', '#dfc27d', '#a6611a'),
#         lwd = 2, box.lty = 0, bg = 'transparent')

# Compare mean 2015 growth 
# mean (colMeans (ringWidthsJul [14:18], na.rm = TRUE))
# mean (colMeans (ringWidthsAug [14:18], na.rm = TRUE))
# mean (colMeans (ringWidthsOct [14:18], na.rm = TRUE))
# mean (colMeans (ringWidthsNov [14:18], na.rm = TRUE))
# dJul <- density (unlist (ringWidthsJul [14:18]), na.rm = T)
# dAug <- density (unlist (ringWidthsAug [14:18]), na.rm = T)
# dOct <- density (unlist (ringWidthsOct [14:18]), na.rm = T)
# dNov <- density (unlist (ringWidthsNov [14:18]), na.rm = T)
# plot (dJul, xlab = 'ring width (mm)', ylab = 'density',
#       xlim = c (0, 5.5), ylim = c (0, 0.5), main = '2015', 
#       col = '#018571', lwd = 2, las = 1)
# lines (dAug, col = '#80cdc1', lwd = 2)
# lines (dOct, col = '#dfc27d', lwd = 2)
# lines (dNov, col = '#a6611a', lwd = 2)
# legend (x = 2.5, y = 0.51, legend = c ('July','August','October','November'),
#         col = c ('#018571', '#80cdc1', '#dfc27d', '#a6611a'),
#         lwd = 2, box.lty = 0, bg = 'transparent')

# boxplot (at = 6, x = ringWidthsNov [ringWidthsNov [['treatment']] == 1, 6] / 
#                            ringWidthsNov [ringWidthsNov [['treatment']] == 1, 16], 
#          col = colours [1], xlim = c (0, 7), ylim = c (0, 3),
#          axes = FALSE, horizontal = TRUE)
# boxplot (at = 5, x = ringWidthsNov [ringWidthsNov [['treatment']] == 2, 5] / 
#                            ringWidthsNov [ringWidthsNov [['treatment']] == 2, 15], 
#          col = colours [2], add = T, 
#          axes = FALSE, horizontal = TRUE)
# boxplot (at = 4.5, x = ringWidthsNov [ringWidthsNov [['treatment']] == 2, 7] / 
#                            ringWidthsNov [ringWidthsNov [['treatment']] == 2, 17], 
#          col = colours [2], add = T, 
#          axes = FALSE, horizontal = TRUE)
# boxplot (at = 3.5, x = ringWidthsNov [ringWidthsNov [['treatment']] == 3, 5] / 
#                            ringWidthsNov [ringWidthsNov [['treatment']] == 3, 15], 
#          col = colours [3], add = T, 
#          axes = FALSE, horizontal = TRUE)
# boxplot (at = 3, x = ringWidthsNov [ringWidthsNov [['treatment']] == 3, 7] / 
#                            ringWidthsNov [ringWidthsNov [['treatment']] == 3, 17], 
#          col = colours [3], add = T, 
#          axes = FALSE, horizontal = TRUE)
# boxplot (at = 2, x = ringWidthsNov [ringWidthsNov [['treatment']] == 4, 4] / 
#                            ringWidthsNov [ringWidthsNov [['treatment']] == 4, 14], 
#          col = colours [4], add = T, 
#          axes = FALSE, horizontal = TRUE)
# boxplot (at = 1.5, x = ringWidthsNov [ringWidthsNov [['treatment']] == 4, 6] / 
#                            ringWidthsNov [ringWidthsNov [['treatment']] == 4, 16], 
#          col = colours [4], add = T, 
#          axes = FALSE, horizontal = TRUE)
# boxplot (at = 1, x = ringWidthsNov [ringWidthsNov [['treatment']] == 4, 8] / 
#                            ringWidthsNov [ringWidthsNov [['treatment']] == 4, 18], 
#          col = colours [4], add = T, 
#          axes = FALSE, horizontal = TRUE)

# Add a plot for 2015 
# dNov1M <- density (unlist (ringWidthsNov [ringWidthsNov [['treatment']] == 1, 16]), na.rm = T)
# dNov2A <- density (unlist (ringWidthsNov [ringWidthsNov [['treatment']] == 2, 15]), na.rm = T)
# dNov2B <- density (unlist (ringWidthsNov [ringWidthsNov [['treatment']] == 2, 17]), na.rm = T)
# dNov3A <- density (unlist (ringWidthsNov [ringWidthsNov [['treatment']] == 3, 15]), na.rm = T)
# dNov3B <- density (unlist (ringWidthsNov [ringWidthsNov [['treatment']] == 3, 17]), na.rm = T)
# dNov4A <- density (unlist (ringWidthsNov [ringWidthsNov [['treatment']] == 4, 14]), na.rm = T)
# dNov4M <- density (unlist (ringWidthsNov [ringWidthsNov [['treatment']] == 4, 16]), na.rm = T)
# dNov4B <- density (unlist (ringWidthsNov [ringWidthsNov [['treatment']] == 4, 18]), na.rm = T)
# par (mar = c (5,5,3,1))
# plot (dNov1M, col = colours [1], lwd = 2, ylim = c (0, 1.2), las = 1,
#       xlab = 'ring width (mm)',
#       ylab = 'density', main = '2015',
#       xlim = c (0, 6))
# lines (dNov2A, col = colours [2], lwd = 2)
# lines (dNov2B, col = colours [2], lwd = 2, lty = 2)
# lines (dNov3A, col = colours [3], lwd = 2)
# lines (dNov3B, col = colours [3], lwd = 2, lty = 2)
# lines (dNov4A, col = colours [4], lwd = 2)
# lines (dNov4B, col = colours [4], lwd = 2, lty = 2)
# lines (dNov4M, col = colours [4], lwd = 2, lty = 3)
# boxplot (at = 6, x = ringWidthsNov [ringWidthsNov [['treatment']] == 1, 16], 
#          col = colours [1], xlim = c (0, 7), ylim = c (0, 6),
#          axes = FALSE, horizontal = TRUE)
# boxplot (at = 5, x = ringWidthsNov [ringWidthsNov [['treatment']] == 2, 15], 
#          col = colours [2], add = T, 
#          axes = FALSE, horizontal = TRUE)
# boxplot (at = 4.5, x = ringWidthsNov [ringWidthsNov [['treatment']] == 2, 17], 
#          col = colours [2], add = T, 
#          axes = FALSE, horizontal = TRUE)
# boxplot (at = 3.5, x = ringWidthsNov [ringWidthsNov [['treatment']] == 3, 15], 
#          col = colours [3], add = T, 
#          axes = FALSE, horizontal = TRUE)
# boxplot (at = 3, x = ringWidthsNov [ringWidthsNov [['treatment']] == 3, 17], 
#          col = colours [3], add = T, 
#          axes = FALSE, horizontal = TRUE)
# boxplot (at = 2, x = ringWidthsNov [ringWidthsNov [['treatment']] == 4, 14], 
#          col = colours [4], add = T, 
#          axes = FALSE, horizontal = TRUE)
# boxplot (at = 1.5, x = ringWidthsNov [ringWidthsNov [['treatment']] == 4, 16], 
#          col = colours [4], add = T, 
#          axes = FALSE, horizontal = TRUE)
# boxplot (at = 1, x = ringWidthsNov [ringWidthsNov [['treatment']] == 4, 18], 
#          col = colours [4], add = T, 
#          axes = FALSE, horizontal = TRUE)

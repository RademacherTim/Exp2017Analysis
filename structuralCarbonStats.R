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

# plot ring width data
boxplot (RW ~ height * treatment, data = RW2017, las = 1, 
         xlab = 'treatment and sampling height', ylab = 'adjusted ring width', xaxt = 'n')
plot (x = RW2017 [['tree']],
      y = RW2017 [['RW']],
      col = colours [1], pch = 19)
plot (x = RW2017 [['treatment']],
      y = RW2017 [['RW']])

# fit mixed effects model with tree as random effect
M1 <- lmer (formula = RW ~ (1 | tree) + treatment:height, 
            data = RW2017,
            REML = TRUE)
summary (M1)
plot (M1)
qqnorm (resid (M1))

# wrangle cell number in the ring
cellNumber [['tree']]      <- factor (cellNumber [['tree']])
cellNumber [['treatment']] <- factor (cellNumber [['treatment']], levels = c (4:1))
cellNumber [['height']]    <- factor (cellNumber [['height']], levels = c ('A','M','B','C'))

# fit mixed effect model with tree as random effect
M2 <- lmer (formula = n ~ (1 | tree) + treatment:height,
            data = cellNumber,
            REML = TRUE)
summary (M2)
plot (M2)
qqnorm (resid (M2))

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
              REML = FALSE)
summary (M7.1)
plot (M7.1)
qqnorm (resid (M7.1))
anova (M7.0, M7.1)

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

# fit a mixed effects model to look at cell wall thickness
# M9.0 <- lmer (formula = CWA ~ (1 | tree) + treatment:height:factor (afterOnset) + factor (afterOnset), 
#               data = woodAnatomy, 
#               REML = FALSE)
# summary (M9.0)
M9.1 <- lmer (formula = CWA~ (1 | tree) + treatment:height:factor (period) + factor (period), 
              data = woodAnatomy, 
              REML = TRUE)
summary (M9.1)
plot (M9.1)
qqnorm (resid (M9.1))
#anova (M9.0, M9.1)
#========================================================================================

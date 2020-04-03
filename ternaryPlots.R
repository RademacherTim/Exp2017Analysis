#========================================================================================
# Create ternary plots of NSC, respiration and fraction of cell wall area
#----------------------------------------------------------------------------------------


# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')
library ('ggplot2')
library ('ggtern')
library ('readxl')

# set colour scheme for control, girdled, compressed, double compressed and chilled
#----------------------------------------------------------------------------------------
colours <- c ('#91b9a4','#C0334D','#F18904','#5C4A72')

# read file with response variables 
#----------------------------------------------------------------------------------------
data <- read_csv ('/home/tim/Downloads/2017ExperimentalData - responseVariables.csv',
                  col_types = cols ())

# read file with response variables 
#----------------------------------------------------------------------------------------
allometricData <- read_excel (path = '/media/tim/dataDisk/PlantGrowth/data/allometry/Exp2017/allometricDataExp2017.xlsx',
                              sheet = 'allometricData')
allometricData <- filter (allometricData, tree <= 40)

# calculate change in concentration for NSC data
#----------------------------------------------------------------------------------------
data [['deltaSugarW250']] <- NA; data [['deltaStarchW250']] <- NA
data [['deltaSugarW200']] <- NA; data [['deltaStarchW200']] <- NA
data [['deltaSugarW150']] <- NA; data [['deltaStarchW150']] <- NA
data [['deltaSugarW100']] <- NA; data [['deltaStarchW100']] <- NA
data [['deltaSugarW50']]  <- NA; data [['deltaStarchW50']]  <- NA
for (i in 41:160) {
  data [['deltaSugarW250']] [i] <- data [['sugarW250']] [i] - data [['sugarW250']] [i-40]
  data [['deltaSugarW200']] [i] <- data [['sugarW200']] [i] - data [['sugarW200']] [i-40]
  data [['deltaSugarW150']] [i] <- data [['sugarW150']] [i] - data [['sugarW150']] [i-40]
  data [['deltaSugarW100']] [i] <- data [['sugarW100']] [i] - data [['sugarW100']] [i-40]
  data [['deltaSugarW50']]  [i] <- data [['sugarW50']]  [i] - data [['sugarW50']]  [i-40]
  data [['deltaStarchW250']] [i] <- data [['starchW250']] [i] - data [['starchW250']] [i-40]
  data [['deltaStarchW200']] [i] <- data [['starchW200']] [i] - data [['starchW200']] [i-40]
  data [['deltaStarchW150']] [i] <- data [['starchW150']] [i] - data [['starchW150']] [i-40]
  data [['deltaStarchW100']] [i] <- data [['starchW100']] [i] - data [['starchW100']] [i-40]
  data [['deltaStarchW50']]  [i] <- data [['starchW50']]  [i] - data [['starchW50']]  [i-40]
}

# wrangle data to long format with unique labels
#----------------------------------------------------------------------------------------
respData <- data %>% select (c (2:3, 18:22)) %>% pivot_longer (cols =  c (3:7), 
                                                               names_to = 'height',
                                                               names_prefix = 'resp',
                                                               values_to = 'resp')
struData <- data %>% select (c (2:3, 28:32)) %>% pivot_longer (cols =  c (3:7), 
                                                               names_to = 'height',
                                                               names_prefix = 'structuralCarbonat',
                                                               values_to = 'SC')
nonsData <- data %>% select (c (2:3, seq (33, 42, by = 2))) %>% 
                     pivot_longer (cols =  c (3:7), names_to = 'height',
                                   names_prefix = 'deltaSugarW', values_to = 'sugar')
nonsData <- data %>% select (c (2:3, seq (34, 42, by = 2))) %>% 
                     pivot_longer (cols =  c (3:7), names_to = 'height',
                     names_prefix = 'deltaStarchW', values_to = 'starch') %>%
                     right_join (nonsData, by = c ('month', 'tree', 'height'))
nonsData [['total']] <- nonsData [['sugar']] + nonsData [['starch']]
allData <- right_join (respData, struData, by = c ('month', 'tree', 'height')) %>% 
           right_join (select (nonsData, -starch, -sugar), 
                       by = c ('month', 'tree', 'height')) %>%
           filter (month != 'july')

# add treatment to the data
#----------------------------------------------------------------------------------------
allData [['treatment']] <- allometricData [['treatment']] [allData [['tree']]]

# change height for plotting symbol 
#----------------------------------------------------------------------------------------
allData <- filter (allData, !(height != 150 & treatment == 1))
allData <- filter (allData, !(height %in% c (50, 150, 250) & treatment %in% 2:3))
allData <- filter (allData, !(height %in% c (100, 200) & treatment == 4))
allData [['height']] <- as.numeric (allData[['height']])
allData [['height']] [allData [['height']] == 100 | allData [['height']]    == 50]  <- 'B' # below
allData [['height']] [allData [['height']] == 150 & allData [['treatment']] == 4]   <- 'M' # middle
allData [['height']] [allData [['height']] == 150 & allData [['treatment']] == 1]   <- 'C' # control
allData [['height']] [allData [['height']] == 200 | allData [['height']]    == 250] <- 'A' # above
  
# add unique label
#----------------------------------------------------------------------------------------
allData [['label']] <- paste (allData [['month']], 
                              as.character (allData [['tree']]),
                              as.character (allData [['treatment']]), 
                              as.character (allData [['height']]))

# draw ternary plot for respiratory loss, structural carbon gain, and nonstructural 
# carbon concentration
#----------------------------------------------------------------------------------------
# example of usage
# f5a <- ggtern (data = allData, mapping = aes (x = SC, y = resp, z = total)) +
#        geom_point (mapping = aes (fill = factor (treatment), shape = height)) + 
#        scale_fill_discrete (aes = colours [1:4]) +
#        scale_shape_manual (values = c (24, 25, 21, 22)) +
#        theme_minimal () + theme_showsecondary() +
#        theme_showarrows () + tern_limits (T = 40/72.5, L = 30/75.2, R = 2.5/72.5)
# print(f5a)
# 
#    geom_polygon (mapping = aes (fill = Label),
#                alpha = 0.75, size = 0.5, color = "black")
# +
#   geom_text(data = dfLabels, mapping = aes(label = Label, angle = Angle),
#             size = 2.5) + theme_rgbw() + theme_showsecondary() +
#   theme_showarrows() + custom_percent("Percent") +
#   guides(color = "none", fill = "none") +
#   labs(title = "USDA Textural Classification Chart",
#        fill = "Textural Class", color = "Textural Class")

#========================================================================================

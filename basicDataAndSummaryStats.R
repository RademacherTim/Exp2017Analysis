#========================================================================================
# Script to test for differences between the trees (i.e., dbh, height)
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
library ('ggpubr')

# Get the tree measurements of circumference from spreadsheet
#----------------------------------------------------------------------------------------
allometricData <- read_excel (path = '/media/TREE/PlantGrowth/data/allometry/allometricDataExp2017.xlsx', 
                              sheet = 'allometricData', 
                              na = "NA")

# Convert treatment to a factor
#----------------------------------------------------------------------------------------
allometricData [['group']] <- ordered (allometricData [['treatment']])

# Compute summary statistics
#----------------------------------------------------------------------------------------
group_by (allometricData [allometricData [['tree']] != 41, ], group) %>% summarise (
  count = n (),
  meanDBH = mean (dbh150, na.rm = TRUE),
  sdDBH = sd (dbh150, na.rm = TRUE),
  meanH17 = mean (treeHeight2017, na.rm = TRUE),
  sdH17 = sd (treeHeight2017, na.rm = TRUE),
  meanH19 = mean (treeHeight2019, na.rm = TRUE),
  sdH19 = sd (treeHeight2019, na.rm = TRUE)
)

# Plot diameter by group
#----------------------------------------------------------------------------------------
ggboxplot (allometricData [allometricData [['tree']] != 41, ], 
           x = 'group', y = 'dbh150', color = 'group',
           palette = colours, ylab  = 'diameter at breast height (cm)', xlab ='treatment')
ggline (allometricData [allometricData [['tree']] != 41, ], 
        x = 'group', y = 'dbh150', color = 'group',
        add = c ('mean_se', 'jitter'), palette = colours,
        ylab  = 'diameter at breast height (cm)', xlab ='treatment')
# Plot height by group
#----------------------------------------------------------------------------------------
ggboxplot (allometricData [allometricData [['tree']] != 41, ], 
           x = 'group', y = 'treeHeight2017', color = 'group',
           palette = colours, ylab  = 'tree height (m)', xlab ='treatment')
ggline (allometricData [allometricData [['tree']] != 41, ], 
        x = 'group', y = 'treeHeight2017', color = 'group',
        add = c ('mean_se', 'jitter'), palette = colours,
        ylab  = 'tree height (m)', xlab ='treatment')

# Check for difference in dbh between treatment groups
#----------------------------------------------------------------------------------------
res.aov <- aov (dbh150 ~ group, data = allometricData [allometricData [['tree']] != 41, ])
summary (res.aov)

# Check for difference in dbh between treatment groups
#----------------------------------------------------------------------------------------
res.aov <- aov (treeHeight2017 ~ group, data = allometricData [allometricData [['tree']] != 41, ])
summary (res.aov)

# get average dbh
#----------------------------------------------------------------------------------------
mean (allometricData [['dbh150']] [allometricData [['tree']] != 41])
sd (allometricData [['dbh150']] [allometricData [['tree']] != 41])

# get average height in 2017
#----------------------------------------------------------------------------------------
mean (allometricData [['treeHeight2017']] [allometricData [['tree']] != 41])
sd (allometricData [['treeHeight2017']] [allometricData [['tree']] != 41])

# switch to directory with increment core measuremnts
#----------------------------------------------------------------------------------------
setwd ('/home/trademacehr/projects/PlantGrowth/data/incrementCores/Exp2017/')

# make list of files in directory
#----------------------------------------------------------------------------------------
listOfFiles <- list.files ('./', pattern = 'PinusStrobus')

# create tibble with growth data
#----------------------------------------------------------------------------------------
annIncGrowth <- tibble (year = 1995:2019)
k <- 2 # profile counter

# create tibble with all measurements
#----------------------------------------------------------------------------------------
annRadGroInc <- tibble (year = NA, tree = NA, treatment = NA, profile = NA, growth = NA)

# loop over each file, read the containing growth data and add it to the tibble
#----------------------------------------------------------------------------------------
for (i in 1:40) {
  # append '0' to i, if smaller than 10
  if (i < 10) {
    treeID <- paste0 ('0',as.character (i))
  } else {
    treeID <- as.character (i)
  }
  # determine treatment group
  treatment <- allometricData [['treatment']] [allometricData [['tree']] == i]
  # read data in csv file
  temp <- read_csv (paste0 ('PinusStrobus',treeID,'p',treatment,'.csv'),
                    col_types = cols ())
  # delete all linker rows
  temp <- filter (temp, type != 'Linker')
  
  # set profile counter to 1
  p <- 1
  
  for (r in 1:dim (temp) [1]) {
    # Increase profile counter if there already is a measurement for this tree and year
    if (length (annRadGroInc [['growth']] [which (annRadGroInc [['tree']] == i &
                                                  annRadGroInc [['year']] == temp [['year']] [r])]) > 0) {
      p <- 2
    }
    annRadGroInc <- add_row (annRadGroInc, 
                             year = temp [['year']] [r], 
                             tree = i,
                             treatment = treatment,
                             profile = p,
                             growth = temp [['growth']] [r])
  }
}
# delete first row, which was just a dummy
annRadGroInc <- annRadGroInc [-1, ]

# delete rows where growth equals 0
annRadGroInc <- filter (annRadGroInc, growth != 0)

# calculate mean five-year growth
mean (annRadGroInc [['growth']] [annRadGroInc [['year']] > 2012 & 
                                 annRadGroInc [['year']] <= 2017], na.rm = TRUE)
sd (annRadGroInc [['growth']] [annRadGroInc [['year']] > 2012 & 
                               annRadGroInc [['year']] <= 2017], na.rm = TRUE)


# spaghetti plot with all trees
par (mar = c (5, 5, 1, 1), mfrow = c (1, 1))
plot (x = annRadGroInc [['year']] [annRadGroInc [['profile']] == 1 & 
                                     annRadGroInc [['tree']] == 1],
      y = annRadGroInc [['growth']] [annRadGroInc [['profile']] == 1 & 
                                     annRadGroInc [['tree']] == 1],
      typ = 'l', xlab = 'year', ylab = 'annual radial growth increment (mm)',
      las = 1, xlim = c (1995, 2019), ylim = c (0, 10), col = 'white')
i = 40
lines (x = annRadGroInc [['year']] [annRadGroInc [['profile']] == 1 & 
                                    annRadGroInc [['tree']]    == i],
       y = annRadGroInc [['growth']] [annRadGroInc [['profile']] == 1 & 
                                      annRadGroInc [['tree']]    == i], lty = 1)
lines (x = annRadGroInc [['year']] [annRadGroInc [['profile']] == 2 & 
                                    annRadGroInc [['tree']]    == i],
       y = annRadGroInc [['growth']] [annRadGroInc [['profile']] == 2 & 
                                      annRadGroInc [['tree']]    == i], lty = 2)
#========================================================================================
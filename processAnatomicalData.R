#========================================================================================
# Script to read and process the cellular anatomy data for the 2017 experiment at Harvard 
# Forest.
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')
library ('readxl')
library ('rjson')

# Get original working directory
#----------------------------------------------------------------------------------------
originalDir <- getwd ()

# Change working directory to read anatomical data
#----------------------------------------------------------------------------------------
setwd ('/media/tim/dataDisk/PlantGrowth/data/microcores/woodAnatomy/Exp2017/')

# Read the data per 20 micron slices
#----------------------------------------------------------------------------------------
data <- read_excel (path = './20muband_ALL.xlsx', sheet = '20muband_ALL', na = "NA")

# Change working directory
#----------------------------------------------------------------------------------------
setwd ('/media/tim/dataDisk/PlantGrowth/data/allometry/Exp2017/')

# Read the allometric data
#----------------------------------------------------------------------------------------
allometricData <- read_excel (path = paste0 (getwd (),'/allometricDataExp2017.xlsx'), 
                              sheet = 'allometricData', 
                              na = "NA")

# Set working directory to read json files
#----------------------------------------------------------------------------------------
setwd ('/media/tim/dataDisk/PlantGrowth/data/microcores/woodAnatomy/Exp2017/ringWidthTRIAD/')

# Build data frame with ring width measurements for each of the four thinsections
#----------------------------------------------------------------------------------------
ringWidths <- tibble (tree = rep (1:40, 4),
                      treatment = rep (allometricData [['treatment']] [1:40], 4),
                      month = c (rep (7, 40), rep (8, 40), rep (10, 40), rep (11, 40)),
                      RW2017at250 = NA, RW2017at200 = NA, RW2017at150 = NA, 
                      RW2017at100 = NA, RW2017at050 = NA, RW2016at250 = NA, 
                      RW2016at200 = NA, RW2016at150 = NA, RW2016at100 = NA, 
                      RW2016at050 = NA, RW2015at250 = NA, RW2015at200 = NA, 
                      RW2015at150 = NA, RW2015at100 = NA, RW2015at050 = NA,
                      RW2014at250 = NA, RW2014at200 = NA, RW2014at150 = NA, 
                      RW2014at100 = NA, RW2014at050 = NA, RW2013at250 = NA, 
                      RW2013at200 = NA, RW2013at150 = NA, RW2013at100 = NA, 
                      RW2013at050 = NA, RW2012at250 = NA, RW2012at200 = NA, 
                      RW2012at150 = NA, RW2012at100 = NA, RW2012at050 = NA,
                      RW2011at250 = NA, RW2011at200 = NA, RW2011at150 = NA, 
                      RW2011at100 = NA, RW2011at050 = NA, RW2010at250 = NA, 
                      RW2010at200 = NA, RW2010at150 = NA, RW2010at100 = NA,
                      RW2010at050 = NA, RW2009at250 = NA, RW2009at200 = NA, 
                      RW2009at150 = NA, RW2009at100 = NA, RW2009at050 = NA,
                      RW2008at250 = NA, RW2008at200 = NA, RW2008at150 = NA, 
                      RW2008at100 = NA, RW2008at050 = NA)

# List all json output file from TRIAD
#----------------------------------------------------------------------------------------
jsonFiles <- list.files (path = './', pattern = '.json')

# Set loop variable to count files with 2018 year growth
#----------------------------------------------------------------------------------------
k <- 0

# Loop over json files and read them
#----------------------------------------------------------------------------------------
for (j in 1: length (jsonFiles)) {
  
  # Read in TRIAD outputs
  #--------------------------------------------------------------------------------------
  temp <- fromJSON (file = jsonFiles [j]) 
  treeID <- as.numeric (substr (temp [['sampleID']], 1, 2))
  sampleDate <- lubridate::month (temp [['sampleDate']])
  len <- length (temp [['growth']])
  
  # Extract growth measurement, associated years and types of markers
  #--------------------------------------------------------------------------------------
  for (i in 1:len) {
    if (i  == 1) {
      types  <- unlist (temp [['growth']] [i]) [6]
      years  <- unlist (temp [['growth']] [i]) [7] 
      growth <- unlist (temp [['growth']] [i]) [9]
    } else {
      types  <- c (types,  unlist (temp [['growth']] [i]) [6])
      years  <- c (years,  unlist (temp [['growth']] [i]) [7])
      growth <- c (growth, unlist (temp [['growth']] [i]) [9])
    }
  }
  # Double check that years are associated correctly
  #--------------------------------------------------------------------------------------
  if (max (years) == 2017 & growth [years == '2017' & types == 'Normal'] == '0') {
    print (paste0 ('Error with the year number 2017 for:', jsonFiles [j]))
  } else if (max (years) >= 2017 & growth [years == '2018' & types == 'Normal'] != '0') {
    k <- k + 1
    print (paste0 (jsonFiles [j],' has a 2018, CHECK IT!!!', max (years),' file number ', k))
  }
  
  # Wrangle data
  #--------------------------------------------------------------------------------------
  growth <- as.numeric (growth [types == 'Normal' & years <= 2017])
  years  <- as.numeric (years  [types == 'Normal' & years <= 2017])
  growth <- c (growth, rep (NA, 10-length (growth)))
  years  <- c (years,  seq (years [length (years)]-1, 2008))
  
  # Extract treatment
  #--------------------------------------------------------------------------------------
  t <- as.numeric (substr (temp [['sampleID']], 4, 4))
  
  # Create condition for the right sampling date and tree
  #--------------------------------------------------------------------------------------
  con <- ringWidths [['tree']] == treeID & ringWidths [['month']] == sampleDate
  
  # Wrangle into new format
  #--------------------------------------------------------------------------------------
  if (t == 1) {
    if (!is.na (growth [years == 2017])) {
      ringWidths [['RW2017at150']] [con] <- growth [years == 2017]
    }
    if (!is.na (growth [years == 2016])) {
      ringWidths [['RW2016at150']] [con] <- growth [years == 2016]
    }
    if (!is.na (growth [years == 2015])) {
      ringWidths [['RW2015at150']] [con] <- growth [years == 2015]
    }
    if (!is.na (growth [years == 2014])) {
      ringWidths [['RW2014at150']] [con] <- growth [years == 2014]
    }
    if (!is.na (growth [years == 2013])) {
      ringWidths [['RW2013at150']] [con] <- growth [years == 2013]
    }
    if (!is.na (growth [years == 2012])) {
      ringWidths [['RW2012at150']] [con] <- growth [years == 2012]
    }
    if (!is.na (growth [years == 2011])) {
      ringWidths [['RW2011at150']] [con] <- growth [years == 2011]
    }
    if (!is.na (growth [years == 2010])) {
      ringWidths [['RW2010at150']] [con] <- growth [years == 2010]
    }
    if (!is.na (growth [years == 2009])) {
      ringWidths [['RW2009at150']] [con] <- growth [years == 2009]
    }
    if (!is.na (growth [years == 2008])) {
      ringWidths [['RW2008at150']] [con] <- growth [years == 2008]
    }
  } else if (t == 2 | t == 3) {
    sampleHeight <- substr (temp [['sampleID']], 6, 6)
    if (sampleHeight == 'A') {
      if (!is.na (growth [years == 2017])) {
        ringWidths [['RW2017at200']] [con] <- growth [years == 2017]
      }
      if (!is.na (growth [years == 2016])) {
        ringWidths [['RW2016at200']] [con] <- growth [years == 2016]
      }
      if (!is.na (growth [years == 2015])) {
        ringWidths [['RW2015at200']] [con] <- growth [years == 2015]
      }
      if (!is.na (growth [years == 2014])) {
        ringWidths [['RW2014at200']] [con] <- growth [years == 2014]
      }
      if (!is.na (growth [years == 2013])) {
        ringWidths [['RW2013at200']] [con] <- growth [years == 2013]
      }
      if (!is.na (growth [years == 2012])) {
        ringWidths [['RW2012at200']] [con] <- growth [years == 2012]
      }
      if (!is.na (growth [years == 2011])) {
        ringWidths [['RW2011at200']] [con] <- growth [years == 2011]
      }
      if (!is.na (growth [years == 2010])) {
        ringWidths [['RW2010at200']] [con] <- growth [years == 2010]
      }
      if (!is.na (growth [years == 2009])) {
        ringWidths [['RW2009at200']] [con] <- growth [years == 2009]
      }
    } else if (sampleHeight == 'B') {
      if (!is.na (growth [years == 2017])) {
        ringWidths [['RW2017at100']] [con] <- growth [years == 2017]
      }
      if (!is.na (growth [years == 2016])) {
        ringWidths [['RW2016at100']] [con] <- growth [years == 2016]
      }
      if (!is.na (growth [years == 2015])) {
        ringWidths [['RW2015at100']] [con] <- growth [years == 2015]
      }
      if (!is.na (growth [years == 2014])) {
        ringWidths [['RW2014at100']] [con] <- growth [years == 2014]
      }
      if (!is.na (growth [years == 2013])) {
        ringWidths [['RW2013at100']] [con] <- growth [years == 2013]
      }
      if (!is.na (growth [years == 2012])) {
        ringWidths [['RW2012at100']] [con] <- growth [years == 2012]
      }
      if (!is.na (growth [years == 2011])) {
        ringWidths [['RW2011at100']] [con] <- growth [years == 2011]
      }
      if (!is.na (growth [years == 2010])) {
        ringWidths [['RW2010at100']] [con] <- growth [years == 2010]
      }
      if (!is.na (growth [years == 2009])) {
        ringWidths [['RW2009at100']] [con] <- growth [years == 2009]
      }
    }
  } else if (t == 4) {
    sampleHeight <- substr (temp [['sampleID']], 6, 6)
    if (sampleHeight == 'A') {
      if (!is.na (growth [years == 2017])) {
        ringWidths [['RW2017at250']] [con] <- growth [years == 2017]
      }
      if (!is.na (growth [years == 2016])) {
        ringWidths [['RW2016at250']] [con] <- growth [years == 2016]
      }
      if (!is.na (growth [years == 2015])) {
        ringWidths [['RW2015at250']] [con] <- growth [years == 2015]
      }  
      if (!is.na (growth [years == 2014])) {
        ringWidths [['RW2014at250']] [con] <- growth [years == 2014]
      }
      if (!is.na (growth [years == 2013])) {
        ringWidths [['RW2013at250']] [con] <- growth [years == 2013]
      }
      if (!is.na (growth [years == 2012])) {
        ringWidths [['RW2012at250']] [con] <- growth [years == 2012]
      }
      if (!is.na (growth [years == 2011])) {
        ringWidths [['RW2011at250']] [con] <- growth [years == 2011]
      }
      if (!is.na (growth [years == 2010])) {
        ringWidths [['RW2010at250']] [con] <- growth [years == 2010]
      }
      if (!is.na (growth [years == 2009])) {
        ringWidths [['RW2009at250']] [con] <- growth [years == 2009]
      }
    } else if (sampleHeight == 'M') {
      if (!is.na (growth [years == 2017])) {
        ringWidths [['RW2017at150']] [con] <- growth [years == 2017]
      }
      if (!is.na (growth [years == 2016])) {
        ringWidths [['RW2016at150']] [con] <- growth [years == 2016]
      }
      if (!is.na (growth [years == 2015])) {
        ringWidths [['RW2015at150']] [con] <- growth [years == 2015]
      }
      if (!is.na (growth [years == 2014])) {
        ringWidths [['RW2014at150']] [con] <- growth [years == 2014]
      }
      if (!is.na (growth [years == 2013])) {
        ringWidths [['RW2013at150']] [con] <- growth [years == 2013]
      }
      if (!is.na (growth [years == 2012])) {
        ringWidths [['RW2012at150']] [con] <- growth [years == 2012]
      }
      if (!is.na (growth [years == 2011])) {
        ringWidths [['RW2011at150']] [con] <- growth [years == 2011]
      }
      if (!is.na (growth [years == 2010])) {
        ringWidths [['RW2010at150']] [con] <- growth [years == 2010]
      }
      if (!is.na (growth [years == 2009])) {
        ringWidths [['RW2009at150']] [con] <- growth [years == 2009]
      }
    } else if (sampleHeight == 'B') {
      if (!is.na (growth [years == 2017])) {
        ringWidths [['RW2017at050']] [con] <- growth [years == 2017]
      }
      if (!is.na (growth [years == 2016])) {
        ringWidths [['RW2016at050']] [con] <- growth [years == 2016]
      }
      if (!is.na (growth [years == 2015])) {
        ringWidths [['RW2015at050']] [con] <- growth [years == 2015]
      }
      if (!is.na (growth [years == 2014])) {
        ringWidths [['RW2014at050']] [con] <- growth [years == 2014]
      }
      if (!is.na (growth [years == 2013])) {
        ringWidths [['RW2013at050']] [con] <- growth [years == 2013]
      }
      if (!is.na (growth [years == 2012])) {
        ringWidths [['RW2012at050']] [con] <- growth [years == 2012]
      }
      if (!is.na (growth [years == 2011])) {
        ringWidths [['RW2011at050']] [con] <- growth [years == 2011]
      }
      if (!is.na (growth [years == 2010])) {
        ringWidths [['RW2010at050']] [con] <- growth [years == 2010]
      }
      if (!is.na (growth [years == 2009])) {
        ringWidths [['RW2009at050']] [con] <- growth [years == 2009]
      }
    }
  }
}  # end json file loop

# Clean unnecessary variables from loop
#----------------------------------------------------------------------------------------
rm (temp, treeID, t, i, j, k, jsonFiles, sampleDate, sampleHeight, growth, types, years, 
    con, len)

# Add November ring width to the ringWidths tibble
#----------------------------------------------------------------------------------------
for (i in 1:dim (data) [1]) {
  treeID       <- as.numeric (substr (data [['TREE']] [i], 1, 2))
  treatment    <- as.numeric (substr (data [['PLOT']] [i], 2, 2))
  sampleHeight <- data [['POS']] [i]
  year <- data [['YEAR']] [i]
  if (sampleHeight == 'A' & treatment == 4) {
    height <- '250' 
  } else if (sampleHeight == 'A' & (treatment == 2 | treatment == 3)) {
    height <- '200' 
  } else if (sampleHeight == 'M') {
    height <- '150'
  } else if (sampleHeight == 'B' & (treatment == 2 | treatment == 3)) {
    height <- '100'
  } else if (sampleHeight == 'B' & treatment == 4) {
    height <- '050'
  }
  xTemp <- paste0 ('RW',year,'at',height)
  con <- ringWidths [['tree']] == treeID & ringWidths [['month']] == 11
  ringWidths [[xTemp]] [con] <- as.numeric (data [['MRW']] [i]) * 1.0e-3
}

# Clean unnecessary variables from loop
#----------------------------------------------------------------------------------------
rm (xTemp, height, treeID, treatment, i, sampleHeight, year,con)

# Reset working directory
#----------------------------------------------------------------------------------------
setwd ('/home/tim/projects/PlantGrowth/Exp2017Analysis/')

# Divide into tibble for each sampling date
#----------------------------------------------------------------------------------------
ringWidthsJul <- ringWidths [ringWidths [['month']] == 7, ]
ringWidthsAug <- ringWidths [ringWidths [['month']] == 8, ]
ringWidthsOct <- ringWidths [ringWidths [['month']] == 10, ]
ringWidthsNov <- ringWidths [ringWidths [['month']] == 11, ]
# Samples 24.4A and 24.4B only have a partial measurement of the 2015 ring in the November
# slide. This means that I cannot generate fraction ring width for these sections.

# Calculate the fractions of ring formed
f_Jul <- (ringWidthsJul [4:8] / ringWidthsJul [14:18]) * ringWidthsNov [14:18] / ringWidthsNov [4:8] 
f_Aug <- (ringWidthsAug [4:8] / ringWidthsAug [14:18]) * ringWidthsNov [14:18] / ringWidthsNov [4:8] 
f_Oct <- (ringWidthsOct [4:8] / ringWidthsOct [14:18]) * ringWidthsNov [14:18] / ringWidthsNov [4:8] 

# Standardised ring width at the end of the season
#----------------------------------------------------------------------------------------
standardisedRW2017 <- rbind (ringWidthsNov [4:8] / ringWidthsNov [14:18],
                             ringWidthsOct [4:8] / ringWidthsOct [14:18],
                             ringWidthsAug [4:8] / ringWidthsAug [14:18],
                             ringWidthsJul [4:8] / ringWidthsJul [14:18])
standardisedRW2017 <- add_column (standardisedRW2017, 
                                  month = c (rep (11, 40), rep (10, 40), rep (8, 40), 
                                             rep (7, 40)))
WRITE <- FALSE
if (WRITE) write_csv (standardisedRW2017,  path = 'standardisedRW2017.csv'); rm (WRITE)

# Add new column to data
#----------------------------------------------------------------------------------------
data <- add_column (data, period = NA, formationDate = NA)

# Loop over for each row in the anatomical data to associate it with a growth period
#----------------------------------------------------------------------------------------
for (i in 1:dim (data) [1]) {
  
  # If not 2017, skip
  #--------------------------------------------------------------------------------------
  if (data [['YEAR']] [i] != 2017) next
  
  # Get tree ID, treatment and sample height of the sample
  #--------------------------------------------------------------------------------------
  treeID       <- as.numeric (substr (data [['TREE']] [i], 1, 2))
  sampleHeight <- substr (data [['TREE']] [i], 3, 3)
  treatment    <- as.numeric (substr (data [['PLOT']] [i], 2, 2))
  
  # Get fractional boundaries for the particular sample
  #--------------------------------------------------------------------------------------
  if (sampleHeight == 'M') {
    fJul <- f_Jul [treeID, 3]
    fAug <- f_Aug [treeID, 3]
    fOct <- f_Oct [treeID, 3]
  } else if (treatment == 2 | treatment == 3) {
    if (sampleHeight == 'A') {
      fJul <- f_Jul [treeID, 2]
      fAug <- f_Aug [treeID, 2]
      fOct <- f_Oct [treeID, 2]
    } else if (sampleHeight == 'B') {
      fJul <- f_Jul [treeID, 4]
      fAug <- f_Aug [treeID, 4]
      fOct <- f_Oct [treeID, 4]
    }
  } else if (treatment == 4) {
    if (sampleHeight == 'A') {
      fJul <- f_Jul [treeID, 1]
      fAug <- f_Aug [treeID, 1]
      fOct <- f_Oct [treeID, 1]
    } else if (sampleHeight == 'B') {
      fJul <- f_Jul [treeID, 5]
      fAug <- f_Aug [treeID, 5]
      fOct <- f_Oct [treeID, 5]
    }
  }
  
  # associate fractional position with a period
  #--------------------------------------------------------------------------------------
  if (!is.na (data [['RRADDISTR']] [i])) {
    if (!is.na (fJul) & !is.na (fAug) & !is.na (fOct)) {
      if (data [['RRADDISTR']] [i] / 100 <= fJul) {
        periodDate <- as_date ('2017-07-03')
        lowerFraction <- 0
        upperFraction <- fJul
      } else if (data [['RRADDISTR']] [i] / 100 <= fAug) {
        periodDate <- as_date ('2017-08-09')
        lowerFraction <- fJul
        upperFraction <- fAug
      } else if (data [['RRADDISTR']] [i] / 100 <= fOct) {
        periodDate <- as_date ('2017-10-09')
        lowerFraction <- fAug
        upperFraction <- fOct
      } else {
        periodDate <- as_date ('2017-11-03')
        lowerFraction <- fOct
        upperFraction <- 1
      }
    } else if (treeID == 16 & sampleHeight == 'B') {
      if (data [['RRADDISTR']] [i] / 100 <= fJul) {
        periodDate <- as_date ('2017-07-03')
      } else if (data [['RRADDISTR']] [i] / 100 <= fAug) {
        periodDate <- as_date ('2017-08-09')
      }
    }
  } else {
    fraction <- data [['RADDISTR.BAND']] [i] / data [['MRW']] [i]
    if (!is.na (fJul) & !is.na (fAug) & !is.na (fOct)) {
      if (fraction <= fJul) {
        periodDate <- as_date ('2017-07-03')
      } else if (fraction  <= fAug) {
        periodDate <- as_date ('2017-08-09')
      } else if (fraction  <= fOct) {
        periodDate <- as_date ('2017-10-09')
      } else {
        periodDate <- as_date ('2017-11-03')
      }
    }
  }
  # Fill in period date
  #--------------------------------------------------------------------------------------
  data [['period']] [i] <- periodDate
  
  # Determine start date of the period
  #--------------------------------------------------------------------------------------
  if (periodDate == as_date ('2017-07-03')) {
    startDate <- as_date ('2017-03-01') 
  } else if (periodDate == as_date ('2017-08-09')) {
    startDate <- as_date ('2017-03-01') 
  } else if (periodDate == as_date ('2017-10-09')) {
    startDate <- as_date ('2017-08-09') 
  } else if (periodDate == as_date ('2017-08-09')) {
    startDate <- as_date ('2017-03-01') 
  }
  
  # Determine the date time increment
  #--------------------------------------------------------------------------------------
  inc <- periodDate - startDate
  
  # Add a linear estimate of the date of formation
  #--------------------------------------------------------------------------------------
  data [['formationDate']] [i] <- startDate + (data [['RRADDISTR']] [i] / 100.0 ) / (upperFraction - lowerFraction) * inc
}

# Clean unnecessary variables from loop
#----------------------------------------------------------------------------------------
rm (i, fAug, fJul, fOct, fraction, sampleHeight, treatment, treeID)

# Add cell width column to data
#----------------------------------------------------------------------------------------
data <- add_column (data, cellRadWidth = NA)
data <- add_column (data, cellTanWidth = NA)

# Calculate zonal average tangential and radial cell width (microns)
#----------------------------------------------------------------------------------------
data [['cellRadWidth']] <- data [['DRAD']] + 2 * data [['CWTTAN']]
data [['cellTanWidth']] <- data [['DTAN']] + 2 * data [['CWTRAD']]

# Rename column POS to height
#----------------------------------------------------------------------------------------
data <- rename (data, height = POS, treatment = PLOT, year = YEAR)
data [['tree']] <- as.numeric (substr (data [['TREE']], 1, 2))
data [['treatment']] <- as.numeric (substr (data [['treatment']], 2, 2)) 
data <- select (data, -TREE)

# Estimate the number of cell in each sector
#----------------------------------------------------------------------------------------
data <- add_column (data, nCells = 20.0 / data [['cellRadWidth']])

# Provide column with cumulative cell wall arrange_all
#----------------------------------------------------------------------------------------
data <- data %>% group_by (tree, height, year) %>% mutate (cumCWA = cumsum (CWA)) 

# Switch back to original working directory
#----------------------------------------------------------------------------------------
setwd (originalDir); rm (originalDir)
#========================================================================================
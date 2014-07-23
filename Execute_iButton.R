###############################################################
# R code to execute batch analysis of iButtons
#                                                             
# Written by Bill Peterman                                    
# July 2013                                                     
#
##############################################################
# Note: This code will not make folders/directories for you. Please make sure all
# specified folders already exist prior to running the code

# Where is the "iButton_SourceCode.R" located?
    iButton.Source <- "C:/iButton/iButton_SourceCode.R"

# What folder are your data files located? END THIS DIRECTORY WITH "/"
    FOLDER<- "C:/iButton/iButtonFiles/"

# Where do you want to export ".csv" files with iButton header removed? END THIS DIRECTORY WITH "/"
  # THIS FOLDER **MUST** NOT BE THE SAME AS WHERE YOUR RAW .csv FILES ARE STORED
    OUT.DIR <- "C:/iButton/results/"

# Where do you want to export the plots of daily variance? END THIS DIRECTORY WITH "/"
    PLOTS <- "C:/iButton/results/plots/"

# What was the first full day that iButtons were deployed? (Enter as "mm/dd/yy")
    Day.Out <- "02/01/13"

# What was the last full day that iButtons were deployed? (Enter as "mm/dd/yy")
    Day.In <- "07/20/13"

# What threshold of daily variance do you want to set as the cutoff between filled and dried? 
# This is the average variance over the days included in the moving window
    VAR <- 20

# What method do you want to use to assess the daily variance within the specified moving window ("mean" = mean or "var" = variance)?
    METHOD <- "var"

# What size do you want your moving window to be to assess daily variance? (MUST BE GREATER THAN 1)
    WINDOW <- 10

# How many continous days does a dry-fill event have to occur to count it? (MUST BE GREATER THAN 0)
	  c.Days <- 2

# After modifying and running the lines of code above, run the line of code below
    source(iButton.Source)

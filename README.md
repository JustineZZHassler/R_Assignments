# R_Assignments
R Programming Assignments

by Zhen Zhong

For:

R Programming 
by Johns Hopkins University

Platform: COURSERA

Programming Assignment 2: Air Pollution

Three functions that are meant to interact with dataset that accompanies this assignment. 

The data file contains 332 comma-separated-value (CSV) files containing pollution monitoring data for fine particulate matter (PM) air pollution at 332 locations in the United States. 
Each file contains data from a single monitor and the ID number for each monitor is contained in the file name. 
Each file contains three variables:

Date: the date of the observation in YYYY-MM-DD format (year-month-day)
sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)
nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)

'pollutantmean' : a function that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors, taking three arguments: 'directory', 'pollutant', and 'id'.
Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of 
the pollutant across all of the monitors, ignoring any missing values coded as NA. 


 'complete' : a function that reads a directory full of files and reports the number of completely observed cases in each data file. 
 The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases. 


'corr' :a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. 
The function returns a vector of correlations for the monitors that meet the threshold requirement. 
If no monitors meet the threshold requirement, then the function returns a numeric vector of length 0. 


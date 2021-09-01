################################################################################
##############   Small script as tutorial on how to install the   ##############
##############   Continuous Glucose Data Analysis (CGDA) package  ##############
##############   When the package is downloaded                   ##############
##############                                                    ##############
##############   Note that everything behind a hashtag (#)        ##############
##############   is a comment for guidance.                       ##############
##############                                                    ##############
##############   To run each line within this script              ##############
##############   press "Ctrl + Enter"                             ##############
################################################################################

#The first step is to make your work-space empty
rm(list=ls())

#Devtools needs to be installed
install.packages("devtools")

#Next, you need to set your path to where the CGDA_0.7.tar.gz is located
#notice that the exclamation marks are needed when defining your path
#Depending on your operating system, you need different type of dashes ("\", "\\", or "/")
path <- "path/to/CGDA-package"

setwd(path) # Changes your current working directory to that specified in path
# ########## NOTE #############
# if you get the following error: "Error in setwd(path) : cannot change working
# directory" -> This means that your path does not exist.
# Maybe there is a typo in the path (line)

getwd() # Small check to see your working directory in the console

list.files() # Check the files that are present in your working directory
#CGDA_0.7.tar.gz should be present here.

#This step is for installing the package -> Only need to be done once.
devtools::install_local("CGDA_0.7.tar.gz")

#After installing the package, activate the package
library(CGDA)

#Some information on how to use the package
?cganalysis

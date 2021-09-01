################################################################################
##############   Small script as tutorial on how to install the   ##############
##############   Continuous Glucose Data Analysis (CGDA) package  ##############
##############   directly from GitHub                             ##############
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

#This step is for installing the package -> Only need to be done once.
devtools::install_github("EvdVossen/CGDA/CGDA")

#After installing the package, activate the package
library(CGDA)

#Some information on how to use the package
?cganalysis

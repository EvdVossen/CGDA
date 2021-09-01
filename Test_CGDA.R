rm(list=ls())

library(CGDA) #activating the CGDA package
# If this gives an error: Error in library(CGDA) : there is no package called "CGDA"
# The package is not installed. -> Either read the Guide to use the CGDA package
# on github, or use the "install_package_guide.R" script to install the package.

?cganalysis #to see the documentation on this function

getwd()

### mock data Freestyle-libre (FSL)
#Note that dependent on your Operating system, you might need to use other dashes (/ or \ or \\)
input_path <- ("/path/to/the/data/fsl/input_raw") 
output_path <- ("/path/to/the/data/fsl/output")

list.files(input_path) #check whether your files and only your raw data files are present in this map

cganalysis(inputdirectory = input_path,
           outputdirectory = output_path,
           outputname = "output_mock_data_fsl",
           metric_option = "mmol/l",
           fsl_source = TRUE,
           magedef = "1sd",
           congan = 1,
           interpolate = FALSE,
           interpolate_function = 'linear',
           maximum_gap = 120,
           group_names = c("Group 1", "Group 2", "Group 3"),
           subject_group_1 = c("Subject1", "Subject3"),
           subject_group_2 = c("Subject2"),
           subject_group_3 = c("Subject4"),
           groups_legend_title = 'Group Legend Title')

### mock data mmol/l
input_path <- ("/path/to/the/data/mmol_l/input_raw") 
output_path <- ("/path/to/the/data/mmol_l/output")

list.files(input_path)

cganalysis(inputdirectory = input_path,
           outputdirectory = output_path,
           outputname = "output_mock_data",
           metric_option = "mmol/l",
           fsl_source = FALSE,
           magedef = "1sd",
           congan = 1,
           interpolate = TRUE,
           interpolate_function = 'linear',
           maximum_gap = 120,
           group_names = c("Group 1", "Group 2", "Group 3"),
           subject_group_1 = c("Subject 1"),
           subject_group_2 = c("Subject 2"),
           subject_group_3 = c("Subject 3"),
           groups_legend_title = 'Group Legend Title')

### mock data mg/dl
input_path <- ("/path/to/the/data/mg_dl/input_raw") 
output_path <- ("/path/to/the/data/mg_dl/output")

list.files(input_path)

cganalysis(inputdirectory = input_path,
           outputdirectory = output_path,
           outputname = "output_mock_data_fsl",
           aboveexcursionlength = 35,
           metric_option = "mg/dl",
           fsl_source = FALSE,
           magedef = "1sd",
           congan = 1,
           interpolate = FALSE,
           interpolate_function = 'linear',
           maximum_gap = 120,
           group_names = c("Group 1", "Group 2"),
           subject_group_1 = c("Subject 1", "Subject 3"),
           subject_group_2 = c("Subject 2"),
           groups_legend_title = 'Group Legend Title')

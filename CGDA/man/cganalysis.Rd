% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cganalysis.R
\name{cganalysis}
\alias{cganalysis}
\title{Calculate CGM Variables and makes multiple plots}
\usage{
cganalysis(
inputdirectory = getwd(),
outputdirectory = tempdir(),
outputname = "Output",
metric_option = "mmol/l",
fsl_source = FALSE,
date_parse_option = 1,
aboveexcursionlength = 35,
belowexcursionlength = 10,
magedef = "1sd",
congan = 1,
interpolate = FALSE,
interpolate_function = 'linear',
maximum_gap = 120,
group_names = NULL,
subject_group_1 = NULL,
subject_group_2 = NULL,
subject_group_3 = NULL,
subject_group_4 = NULL,
groups_legend_title = 'Group Legend Title')
}
\arguments{
\item{inputdirectory}{The directory containing either the raw FSL txt files
or cleaned CSV files for analysis. Note that this map may ONLY contain the
files used for analysis.}

\item{outputdirectory}{The directory where you would like the results to be
written.}

\item{outputname}{The name of the csv file containing the final output
CGM variables (without the file extension).}

\item{metric_option}{The type of metric system is included in the data.
Choose either "mmol/l" or "mg/dl".}

\item{fsl_source}{Whether your data is from a Freestyle-Libre device. Choose
"TRUE" or "FALSE". \cr
if "TRUE", the data files are with ".txt" extension. \cr}

\item{aboveexcursionlength}{The number of minutes blood sugar must be above
threshold to count an excursion. By default 35 minutes.}

\item{belowexcursionlength}{The number of minutes blood sugar must be below
threshold to count an excursion. By default 10 minutes.}

\item{magedef}{How large an excursion needs to be in order to count in the
MAGE calculation (e.g. greater than 1 standard deviation).}

\item{congan}{CONGA interval in hours.}

\item{interpolate}{Do you want to interpolate gaps in the data? \cr
Choose either "TRUE" or "FALSE".}

\item{interpolate_function}{Which interpolation function to use. Options are
"linear" or "spline".}

\item{maximum_gap}{Maximum gap size to interpolate. By default 120 minutes.}

\item{group_names}{Names of the two groups. By default NULL.}

\item{date_parse_option}{The way the time is written in the data (default = 1); \cr
1: Month/Day/Year Hour:Minutes:Seconds (for example: 1/14/20 12:00) \cr
2: Year/Month/day Hour:Minutes:Seconds (for example: 2020/01/14 12:00) \cr
2: Year-Month-day Hour:Minutes:Seconds (for example: 2020-01-14 12:00) \cr}

\item{subject_group_1}{Names of the subjects in group 1. By Default NULL.}

\item{subject_group_2}{Names of the subjects in group 2. By Default NULL.}

\item{subject_group_3}{Names of the subjects in group 3. By Default NULL.}

\item{subject_group_4}{Names of the subjects in group 4. By Default NULL.}

\item{groups_legend_title}{Legend title of the aggregate daily overlay
(LOESS Smoothing curve for the two groups)}
}
\value{
A data frame containing calculated CGM variables, with each column
representing one CGM file and multiple plots.
}
\description{
This function takes cleaned CGM data and returns clinically relevant measures
(e.g. percent time spent over different glucose thresholds, MAGE, MODD, etc.).
}
\details{
The package processes multiple CGM device outputs.
For Freestyle-libre data; the raw txt files can be processed using the option
fsl_source = TRUE. All other device outputs be saved as a csv, and must have
three columns, the first of which contains the subject ID in the first cell
and date of CGM placement in the second (see example files). The names of the
columns must be "subjectid", "timestamp" and "sensorglucose" (without quotes)
respectively.
}
\examples{
cganalysis(inputdirectory, outputdirectory, outputname =
"output_mock_data_fsl", metric_option = "mmol/l", fsl_source = TRUE,
magedef = "1sd", congan = 1, interpolate = FALSE, maximum_gap = 120,
groupnames = c("Group 1", "Group 2"), subject_group_1 = c("Subject1",
"Subject3"), subject_group_2 = c("Subject2", "Subject4"),
groups_legend_title = "Groups" )
}

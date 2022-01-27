#' Calculate CGM Variables and makes multiple plots
#'
#' This function takes cleaned CGM data and returns clinically relevant measures
#' (e.g. percent time spent over different glucose thresholds, MAGE, MODD, etc.).
#'
#' The package processes multiple CGM device outputs.
#' For Freestyle-libre data; the raw txt files can be processed using the option
#' fsl_source = TRUE. All other device outputs be saved as a csv, and must have
#' three columns, the first of which contains the subject ID in the first cell
#' and date of CGM placement in the second (see example files). The names of the
#' columns must be "subjectid", "timestamp" and "sensorglucose" (without quotes)
#' respectively.
#'
#' @param inputdirectory The directory containing either the raw FSL txt files
#' or cleaned CSV files for analysis. Note that this map may ONLY contain the
#' files used for analysis.
#' @param outputdirectory The directory where you would like the results to be
#' written.
#' @param outputname The name of the csv file containing the final output
#' CGM variables (without the file extension).
#' @param metric_option The type of metric system is included in the data.
#' Choose either "mmol/l" or "mg/dl".
#' @param fsl_source Whether your data is from a Freestyle-Libre device. Choose
#'    "TRUE" or "FALSE". \cr
#'    if "TRUE", the data files are with ".txt" extension. \cr
#' @param date_parse_option The way the time is written in the data (default = 1); \cr
#'    1: Month/Day/Year Hour:Minutes:Seconds (for example: 1/14/20 12:00) \cr
#'    2: Year/Month/day Hour:Minutes:Seconds (for example: 2020/01/14 12:00) \cr
#'    2: Year-Month-day Hour:Minutes:Seconds (for example: 2020-01-14 12:00) \cr
#' @param aboveexcursionlength The number of minutes blood sugar must be above
#' threshold to count an excursion. By default 35 minutes.
#' @param belowexcursionlength The number of minutes blood sugar must be below
#' threshold to count an excursion. By default 10 minutes.
#' @param magedef How large an excursion needs to be in order to count in the
#' MAGE calculation (e.g. greater than 1 standard deviation).
#' @param congan CONGA interval in hours.
#' @param interpolate Do you want to interpolate gaps in the data? \cr
#' Choose either "TRUE" or "FALSE".
#' @param interpolate_function Which interpolation function to use. Options are
#' "linear" or "spline".
#' @param maximum_gap Maximum gap size to interpolate. By default 120 minutes.
#' @param group_names Names of the two groups. By default NULL.
#' @param subject_group_1 Names of the subjects in group 1. By Default NULL.
#' @param subject_group_2 Names of the subjects in group 2. By Default NULL.
#' @param subject_group_3 Names of the subjects in group 3. By Default NULL.
#' @param subject_group_4 Names of the subjects in group 4. By Default NULL.
#' @param groups_legend_title Legend title of the aggregate daily overlay
#' (LOESS Smoothing curve for the two groups)
#' @usage cganalysis(
#' inputdirectory = getwd(),
#' outputdirectory = tempdir(),
#' outputname = "Output",
#' metric_option = "mmol/l",
#' fsl_source = FALSE,
#' date_parse_option = 1,
#' aboveexcursionlength = 35,
#' belowexcursionlength = 10,
#' magedef = "1sd",
#' congan = 1,
#' interpolate = FALSE,
#' interpolate_function = 'linear',
#' maximum_gap = 120,
#' group_names = NULL,
#' subject_group_1 = NULL,
#' subject_group_2 = NULL,
#' subject_group_3 = NULL,
#' subject_group_4 = NULL,
#' groups_legend_title = 'Group Legend Title')
#' @examples cganalysis(inputdirectory, outputdirectory, outputname =
#' "output_mock_data_fsl", metric_option = "mmol/l", fsl_source = TRUE,
#' magedef = "1sd", congan = 1, interpolate = FALSE, maximum_gap = 120,
#' groupnames = c("Group 1", "Group 2"), subject_group_1 = c("Subject1",
#' "Subject3"), subject_group_2 = c("Subject2", "Subject4"),
#' groups_legend_title = "Groups" )
#' @return A data frame containing calculated CGM variables, with each column
#' representing one CGM file and multiple plots.
#' @export

cganalysis <- function(inputdirectory,
                       outputdirectory = tempdir(),
                       outputname = "Output",
                       metric_option = "mmol/l",
                       fsl_source = FALSE,
                       aboveexcursionlength = 35,
                       belowexcursionlength = 10,
                       magedef = "1sd",
                       congan = 1,
                       interpolate = FALSE,
                       interpolate_function = 'linear',
                       maximum_gap = 120,
                       group_names = NULL,
                       date_parse_option = 1,
                       subject_group_1 = NULL,
                       subject_group_2 = NULL,
                       subject_group_3 = NULL,
                       subject_group_4 = NULL,
                       groups_legend_title = 'Group Legend Title') {

  inputdirectory = input_path
  outputdirectory = output_path
  metric_option = "mg/dl"
  fsl_source = FALSE
  aboveexcursionlength = 35
  belowexcursionlength = 10
  magedef = "1sd"
  congan = 1
  interpolate = FALSE
  interpolate_function = 'linear'
  maximum_gap = 120
  group_names = NULL
  date_parse_option = 2
  subject_group_1 = NULL
  subject_group_2 = NULL
  subject_group_3 = NULL
  subject_group_4 = NULL
  groups_legend_title = 'Group Legend Title'


  if (interpolate){
    plot_interpolation <- TRUE
  } else {
    plot_interpolation <- FALSE
  }

  metrics = NULL
  subject_groups_mode = F
  num_groups = 0

  if(!base::is.null(subject_group_1) | !base::is.null(subject_group_2) |
     !base::is.null(subject_group_3) | !base::is.null(subject_group_4) | !base::is.null(group_names)){
    if(base::length(subject_group_1) >= 1 && base::length(subject_group_2) >= 1 &&
            base::is.null(subject_group_3) && base::is.null(subject_group_4) &&
            base::length(group_names) == 2){
      subject_groups_mode = T
      num_groups = 2
      } else if(base::length(subject_group_1) >= 1 && base::length(subject_group_2) >= 1 &&
                base::length(subject_group_3) >= 1 && base::is.null(subject_group_4) &&
                base::length(group_names) == 3){
        subject_groups_mode = T
        num_groups = 3
      } else if(base::length(subject_group_1) >= 1 && base::length(subject_group_2) >= 1 &&
                base::length(subject_group_3) >= 1 && base::length(subject_group_4) >= 1 &&
                base::length(group_names) == 4){
          subject_groups_mode = T
          num_groups = 4
      } else{
        base::cat(base::paste0('Something goes wrong; please check the following things when calling :\n',
                               '- Number of group_names does not match the number of subject_groups (subject_group_1 to subject_group_4) \n',
                               '- Not the first names are assigned of subject_group, but later ones \n',
                               "  example: subject_group_1 = NULL, subject_group_2 = c('subject 1'', 'subject 2''), subject_group_3 = c('subject 3'', 'subject 4'').",
                               "Note that subject_group_1 should be assigned instead of subject_group_3.",
                               "\n",
                               '- The number of group_names is more than 5',
                               "\n",
                               "Note that a maximum of 4 subject groups are supported.",
                               "\n",
                               "for now, the run will continue, but ",
                               "Files will be processed as if NO GROUPS were provided.", "\n"))
      }
  }

  if(metric_option == metric_mmol_per_l$string){
    metrics = metric_mmol_per_l
  } else if(metric_option == metric_mg_per_dl$string){
    metrics = metric_mg_per_dl
  } else{
    base::print('No valid metric selected (either "mmol/l" or "mg/dl"). mmol/L will be used as a default.')
    metrics = metric_mmol_per_l
  }

  # tz = "UTC"
  files <- base::list.files(path = inputdirectory, full.names = TRUE)
  cgmupload <- base::as.data.frame(base::matrix(nrow = 0, ncol = base::length(files)))
  base::colnames(cgmupload) <- base::rep("Record", base::length(files))

  # Create data frame to store all glucose values from all files (for aggregate AGP).
  aggregateAGPdata <- base::data.frame(matrix(ncol = 4, nrow = 0))
  base::colnames(aggregateAGPdata) <- c("subjectid", "time", "sensorglucose", "isinterpol")

  # Iterate through the input directory and calculate CGM variables for each file.
   for (f in 1:base::length(files)) {
     if(fsl_source){
      base::print('Reading file...')
      dateparseorder <- c("Ymd HM", "dmY HM")
      free_libre_raw <- utils::read.csv(file = files[f], sep = '\t', header = FALSE, na.strings=c(""))
      skip <- base::ifelse(base::is.na(free_libre_raw[2, 2]), 3, 2)

      free_libre_raw <- utils::read.csv(file = files[f], sep = "\t",
                                        header = FALSE, skip = skip, na.strings = c(""))
      base::colnames(free_libre_raw) <- c("ID", "time", "type", "scan", "scanmanual")

      con <- file(files[f], "r")
      subject_id <- stringr::str_split(base::readLines(con, n = 1), "\t")[[1]][1]
      close(con)
      base::print(base::paste('Read file with subject:', subject_id))

      datetime_to_write <- base::as.POSIXct(lubridate::parse_date_time(free_libre_raw$time,
                                                                       dateparseorder), tz = "UTC")

      if (base::length(base::unique(free_libre_raw$scanmanual)) == 1 && base::is.na(base::unique(free_libre_raw$scanmanual)[1])){
        free_libre_raw$scan_new = free_libre_raw$scan
      } else {
        if (base::length(free_libre_raw$scanmanual) < 10) {
          base::print(paste0("Subject '", subject_id, "' has less than 10 observations. This file will be skipped."))
          next
        }
        free_libre_raw$scan_new <- dplyr::coalesce(free_libre_raw$scan, free_libre_raw$scanmanual)
      }

      subjectid = c("")
      scan_merged = stringr::str_replace(free_libre_raw$scan_new, ",", ".")
      table <- data.frame(subjectid, timestamp = datetime_to_write, sensorglucose = scan_merged, stringsAsFactors = FALSE)
      table[1, 1] = subject_id
      cgmupload["subject_id", f] <- subject_id
    } else{
      # Basic variables
      table <- utils::read.csv(files[f], stringsAsFactors = FALSE, na.strings = c("NA", ""))
      base::print('Reading file...')
      base::colnames(table)[1] <- "subjectid"
      base::colnames(table)[3] <- "sensorglucose"
      subject_id <- table$subject[1]
      cgmupload["subject_id", f] <- subject_id
      # Format columns.
     if (date_parse_option==1){
        dateparseorder <- c("mdy HM")
        table$timestamp <-
          base::as.POSIXct(lubridate::parse_date_time(table$timestamp,
                                                      orders = dateparseorder, tz = "UTC"))
      } else if (date_parse_option==2){
        table$timestamp <-
          base::as.POSIXct(table$timestamp,
                           format = "%Y/%m/%d %H:%M", tz = "UTC")
        } else if (date_parse_option==3){
        table$timestamp <-
          base::as.POSIXct(table$timestamp,
                           format = "%Y-%m-%d %H:%M", tz = "UTC")
        } else { stop('Currently only the following time formats are :\n',
             'option 1: Month/Day/Year Hour:Minutes:Seconds \n',
             'option 2: Year/Month/Day Hour:Minutes:Seconds \n',
             'option 3: Year-Month-Day Hour:Minutes:Seconds \n',
             'Please set date_parse_option = 1 or 2')
      }
      base::print(paste('Read file with subject:', table$subject[1]))
    }

    if (NA %in% table$timestamp) {
      table <- table[-c(base::which(base::is.na(table$timestamp))),
      ]
    }
    table <- dplyr::arrange(table, timestamp)
    interval <- base::abs(pracma::Mode(base::diff(base::as.numeric(table$timestamp))))

    if(nrow(table)==0){
      stop('Something might have gone wrong by choosing the date_parse_option. Choose 1, 2 or 3: \n',
           'option 1: Month/Day/Year Hour:Minutes:Seconds \n',
           'option 2: Year/Month/Day Hour:Minutes:Seconds \n',
           'option 3: Year-Month-Day Hour:Minutes:Seconds \n',
           'Please specify date_parse_option = 1 or date_parse_option = 2 or date_parse_option = 3.')
    }

    table$subjectid <- subject_id

    #part for the summary statistics
    cgmupload["subject_id", f] <- subject_id
    cgmupload["date_cgm_placement", f] <- base::as.character(base::min(table$timestamp, na.rm = T))
    table$sensorglucose <- base::suppressWarnings(base::as.numeric(table$sensorglucose))

    #Make 24 hour chunks starting from 00:00 to 23:59
    table <- Formatfirstlastdays(table)

    if(nrow(table)==0){
      base::print('This subject does not have any good days and will therefore be skipped.')
      next
    }
    totaltime <- base::as.numeric(base::difftime(base::max(table$timestamp, na.rm = T),
                                                 base::min(table$timestamp, na.rm = T),
                                                 units = "secs"))
    percent_wear = base::floor(((base::length(base::which(!base::is.na(table$sensorglucose)))/(totaltime/interval)) * 100))

    if (percent_wear > 100) {
      percent_wear = 100
    }
    cgmupload["percent_cgm_wear", f] <- percent_wear


    if (interpolate) {
      table <- interpolateGlucoseValues(table, maximum_gap, interpolate_function, interval)
    }

    if(base::length(table$sensorglucose) < 10){
      base::print(paste0("Subject '", subject_id, "' has less than 10 observations. This file will be skipped."))
      next
    }

    ## check data for each day; if it contains less than 80% of the time, remove the day.
    table <- format80percent(table, interval)

    if (base::length(table$timestamp) == 0) {
      base::print(paste0("Subject '", subject_id, "' does not have enough data and cannot be processed with the current settings. This file will be skipped.",
                      sep = ""))
      next
    }


    if (interpolate) {
      aggregateAGPdata <- base::rbind(table[, c("subjectid", "timestamp", "sensorglucose", "isinterpol")], aggregateAGPdata)
    } else {
      aggregateAGPdata <- base::rbind(table[, c("subjectid", "timestamp", "sensorglucose")], aggregateAGPdata)
    }

    # Column names to lower case
    base::colnames(table) = base::tolower(base::colnames(table))

    #Format columns
    cgmupload["num_days_good_data", f] <- base::round(base::length(table$sensorglucose)/(86400/interval))
    table <- table[!base::is.na(table$timestamp) & !base::is.na(table$sensorglucose), ]
    ac1_percent <- metrics$get_a1c_percent(table$sensorglucose)

    cgmupload["total_sensor_readings", f] <- base::as.numeric(base::length(base::which(!base::is.na(table$sensorglucose))))
    average_glucose <- base::mean(table$sensorglucose[base::which(!base::is.na(table$sensorglucose))],
                                  na.rm = T)
    cgmupload["average_sensor_glucose", f] <- average_glucose
    cgmupload["estimated_a1c_percent", f] <- base::round(ac1_percent,
                                                         digits = 1)
    cgmupload["estimated_a1c", f] <- base::round(metrics$get_a1c(ac1_percent), digits = 1)
    cgmupload["estimated_gmi_percent", f] <- base::round(metrics$get_gmi_percent(table$sensorglucose), digits = 1)
    cgmupload["estimated_gmi", f] <- base::round(metrics$get_gmi(table$sensorglucose), digits = 1)


    cgmupload["q1_sensor_glucose", f] <- base::as.numeric(base::summary(table$sensorglucose[
      base::which(!base::is.na(table$sensorglucose))])[2])
    cgmupload["median_sensor_glucose", f] <- base::as.numeric(base::summary(table$sensorglucose[
      base::which(!base::is.na(table$sensorglucose))])[3])
    cgmupload["q3_sensor_glucose", f] <- base::as.numeric(base::summary(table$sensorglucose[
      base::which(!base::is.na(table$sensorglucose))])[5])
    std_glucose <- stats::sd(table$sensorglucose[base::which(!base::is.na(table$sensorglucose))])
    cgmupload["standard_deviation_glucose", f] <- std_glucose
    cgmupload["cv_glucose", f] <- (stats::sd(table$sensorglucose[base::which(!base::is.na(table$sensorglucose))]))/
      base::mean(table$sensorglucose[base::which(!base::is.na(table$sensorglucose))])
    cgmupload["min_sensor_glucose", f] <- base::min(table$sensorglucose[base::which(!base::is.na(table$sensorglucose))])
    cgmupload["max_sensor_glucose", f] <- base::max(table$sensorglucose[base::which(!base::is.na(table$sensorglucose))])

    # Over 13.9 mmol/l, 250 mg/dl
    BGover_max_threshold <-
      base::as.numeric(table$sensorglucose[base::which(!base::is.na(
        table$sensorglucose))], length = 1)
    BGover_max_threshold[BGover_max_threshold < metrics$glucose_thresholds[5]] <- 0
    BGover_max_threshold[BGover_max_threshold >= metrics$glucose_thresholds[5]] <- 1

    BGover_max_threshold.rle <- base::rle(BGover_max_threshold)
    excursions_over_max_threshold <-
      base::as.numeric(BGover_max_threshold.rle$lengths[base::which(BGover_max_threshold.rle$values == 1)])

    cgmupload[base::sprintf("excursions_over_%s", metrics$glucose_thresholds[5]), f] <-
      base::length(base::which(excursions_over_max_threshold >
                                 ((aboveexcursionlength * 60)/interval)))
    cgmupload[base::sprintf("min_spent_over_%s", metrics$glucose_thresholds[5]), f] <- base::sum(BGover_max_threshold) * (interval/60)
    cgmupload[base::sprintf("percent_time_over_%s", metrics$glucose_thresholds[5]), f] <-
      ((base::sum(BGover_max_threshold) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100

    # Over 13 mmol/l, 234.23 mg/dl
    BGover_threshold_4 <-
      base::as.numeric(table$sensorglucose[base::which(!base::is.na(
        table$sensorglucose))], length = 1)
    BGover_threshold_4[BGover_threshold_4 < metrics$glucose_thresholds[4]] <- 1
    BGover_threshold_4[BGover_threshold_4 >= metrics$glucose_thresholds[4]] <- 0
    BGover_threshold_4.rle <- base::rle(BGover_threshold_4)
    excursions_over_threshold_4 <- base::as.numeric(BGover_threshold_4.rle$lengths[base::which(BGover_threshold_4.rle$values ==
                                                                                                 1)])
    cgmupload[base::sprintf("excursions_over_%s", metrics$glucose_thresholds[4]), f] <- base::length(base::which(excursions_over_threshold_4 >
                                                                                                                   ((aboveexcursionlength * 60)/interval)))

    # Interval: [10, 13.9] mmol/l, [180, 250] mg/dl
    BGover_threshold_3 <-
      base::as.numeric(table$sensorglucose[base::which(!base::is.na(
        table$sensorglucose))], length = 1)
    BGover_threshold_3[BGover_threshold_3 >= metrics$glucose_thresholds[5]] <- 0
    BGover_threshold_3[BGover_threshold_3 < metrics$glucose_thresholds[3]] <- 0
    BGover_threshold_3[BGover_threshold_3 != 0] <- 1

    BGover_threshold_3.rle <- base::rle(BGover_threshold_3)
    excursions_over_threshold_3 <-
      base::as.numeric(BGover_threshold_3.rle$lengths[base::which(BGover_threshold_3.rle$values == 1)])

    cgmupload[base::sprintf("excursions_over_%s_under_%s",
                            metrics$glucose_thresholds[3],
                            metrics$glucose_thresholds[5]), f] <-
      base::length(base::which(excursions_over_threshold_3 >
                                 ((aboveexcursionlength * 60)/interval)))
    cgmupload[base::sprintf("min_spent_over_%s_under_%s",
                            metrics$glucose_thresholds[3],
                            metrics$glucose_thresholds[5]), f] <- base::sum(BGover_threshold_3) * (interval/60)
    cgmupload[base::sprintf("percent_time_over_%s_under_%s",
                            metrics$glucose_thresholds[3],
                            metrics$glucose_thresholds[5]), f] <-
      ((base::sum(BGover_threshold_3) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100

    # Interval: [3.9, 10] mmol/l, [70, 180] mg/dl
    BGover_threshold_2 <-
      base::as.numeric(table$sensorglucose[base::which(!base::is.na(
        table$sensorglucose))], length = 1)
    BGover_threshold_2[BGover_threshold_2 >= metrics$glucose_thresholds[3]] <- 0
    BGover_threshold_2[BGover_threshold_2 < metrics$glucose_thresholds[2]] <- 0
    BGover_threshold_2[BGover_threshold_2 != 0] <- 1

    cgmupload[base::sprintf("min_spent_over_%s_under_%s",
                            metrics$glucose_thresholds[2],
                            metrics$glucose_thresholds[3]), f] <- base::sum(BGover_threshold_2) * (interval/60)
    cgmupload[base::sprintf("percent_time_over_%s_under_%s",
                            metrics$glucose_thresholds[2],
                            metrics$glucose_thresholds[3]), f] <-
      ((base::sum(BGover_threshold_2) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100

    # Interval: [3, 3.9] mmol/l, [54, 70] mg/dl
    BGover_threshold_1 <-
      base::as.numeric(table$sensorglucose[base::which(!base::is.na(
        table$sensorglucose))], length = 1)
    BGover_threshold_1[BGover_threshold_1 >= metrics$glucose_thresholds[2]] <- 0
    BGover_threshold_1[BGover_threshold_1 < metrics$glucose_thresholds[1]] <- 0
    BGover_threshold_1[BGover_threshold_1 != 0] <- 1

    cgmupload[base::sprintf("min_spent_over_%s_under_%s",
                            metrics$glucose_thresholds[1],
                            metrics$glucose_thresholds[2]), f] <- base::sum(BGover_threshold_1) * (interval/60)
    cgmupload[base::sprintf("percent_time_over_%s_under_%s",
                            metrics$glucose_thresholds[1],
                            metrics$glucose_thresholds[2]), f] <-
      ((base::sum(BGover_threshold_1) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100

    # Under 3.9 mmol/l, 70 mg/dl
    BGunder_threshold_2 <-
      base::as.numeric(table$sensorglucose[base::which(!base::is.na(
        table$sensorglucose))], length = 1)
    BGunder_threshold_2[BGunder_threshold_2 < metrics$glucose_thresholds[2]] <- 1
    BGunder_threshold_2[BGunder_threshold_2 >= metrics$glucose_thresholds[2]] <- 0

    BGunder_threshold_2.rle <- base::rle(BGunder_threshold_2)
    excursions_under_threshold_2 <- base::as.numeric(BGunder_threshold_2.rle$lengths[
      base::which(BGunder_threshold_2.rle$values == 1)])
    cgmupload[base::sprintf("excursions_under_%s", metrics$glucose_thresholds[2]),f] <-
      base::length(base::which(excursions_under_threshold_2 > ((belowexcursionlength * 60)/interval)))
    cgmupload[base::sprintf("min_spent_under_%s", metrics$glucose_thresholds[2]), f] <-
      base::sum(BGunder_threshold_2) * (interval/60)
    cgmupload[base::sprintf("percent_time_under_%s", metrics$glucose_thresholds[2]), f] <-
      ((base::sum(BGunder_threshold_2) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100

    # Under 3 mmol/l, 54 mg/dl
    BGunder_threshold_1 <-
      base::as.numeric(table$sensorglucose[base::which(!base::is.na(
        table$sensorglucose))], length = 1)
    BGunder_threshold_1[BGunder_threshold_1 < 3] <- 1
    BGunder_threshold_1[BGunder_threshold_1 >= 3] <- 0

    BGunder_threshold_1.rle <- base::rle(BGunder_threshold_1)
    excursions_under_threshold_1 <- base::as.numeric(BGunder_threshold_1.rle$lengths[
      base::which(BGunder_threshold_1.rle$values == 1)])
    cgmupload[base::sprintf("excursions_under_%s", metrics$glucose_thresholds[1]),f] <-
      base::length(base::which(excursions_under_threshold_1 > ((belowexcursionlength * 60)/interval)))
    cgmupload[base::sprintf("min_spent_under_%s", metrics$glucose_thresholds[1]), f] <-
      base::sum(BGunder_threshold_1) * (interval/60)
    cgmupload[base::sprintf("percent_time_under_%s", metrics$glucose_thresholds[1]), f] <-
      ((base::sum(BGunder_threshold_1) * (interval/60))/
         (base::length(table$sensorglucose) * (interval/60))) * 100

    #total AUC
    sensorBG <- base::as.numeric(table$sensorglucose, length = 1)
    xaxis <- base::seq(from = 0, length.out = base::length(sensorBG),
                       by = (interval/60))

    #remove NAs if they are present
    xaxis[base::which(base::is.na(sensorBG))] <- NA
    xaxis <- xaxis[!base::is.na(xaxis)]
    sensorBG <- sensorBG[!base::is.na(sensorBG)]

    aucs <- pracma::cumtrapz(xaxis, sensorBG)
    cgmupload["total_auc", f] <- aucs[base::length(sensorBG)]

    cgmupload["average_auc_per_day", f] <-
      base::as.numeric(cgmupload["total_auc", f])/
      base::as.numeric(cgmupload["num_days_good_data", f])

    # AYC over Mean + 2x STD
    threshold <- average_glucose + 2 * std_glucose
    sensoroversd <- table$sensorglucose
    sensoroversd <- sensoroversd[sensoroversd >= threshold]
    sensoroversd <- sensoroversd[!base::is.na(sensoroversd)]
    xaxis <- base::seq(from = 0, length.out = base::length(sensoroversd),
                       by = (interval/60))

    # Calculate cumulative AUC, and subtract recatangle where length = 10 & width = minutes
    if (base::length(sensoroversd) > 1) {
      aucs <- pracma::cumtrapz(xaxis, sensoroversd)
      aucs <- (aucs[base::length(sensoroversd)]) - (xaxis[base::length(xaxis)] *
                                                      threshold)
      cgmupload["auc_over_2sd", f] <- aucs
    } else {
      cgmupload["auc_over_2sd", f] <- 0
    }
    cgmupload["average_auc_2sd", f] <-
      base::as.numeric(cgmupload["auc_over_2sd", f])/
      base::as.numeric(cgmupload["num_days_good_data", f])

    # AUC over 10 mmol/l, 180 mg/dl.
    sensorover_threshold_3 <- table$sensorglucose
    sensorover_threshold_3 <- sensorover_threshold_3[sensorover_threshold_3 >= metrics$glucose_thresholds[3]]
    sensorover_threshold_3 <- sensorover_threshold_3[!base::is.na(sensorover_threshold_3)]
    xaxis <- base::seq(from = 0, length.out = base::length(sensorover_threshold_3),
                       by = (interval/60))

    # Calculate cumulative AUC, and subtract recatangle where length = 10 & width = minutes
    if (base::length(sensorover_threshold_3) > 1) {
      aucs <- pracma::cumtrapz(xaxis, sensorover_threshold_3)
      aucs <-
        (aucs[base::length(sensorover_threshold_3)]) - (xaxis[base::length(xaxis)] * metrics$glucose_thresholds[3])
      cgmupload[base::sprintf("auc_over_%s", metrics$glucose_thresholds[3]), f] <- aucs
    } else {
      cgmupload[base::sprintf("auc_over_%s", metrics$glucose_thresholds[3]), f] <- 0
    }
    cgmupload[base::sprintf("average_auc_%s", metrics$glucose_thresholds[3]), f] <-
      base::as.numeric(cgmupload[base::sprintf("auc_over_%s", metrics$glucose_thresholds[3]), f]) /
      base::as.numeric(cgmupload["num_days_good_data", f])

    #auc over 13 mmol/l, 234 mg/dl
    sensorover_threshold_4 <- table$sensorglucose
    sensorover_threshold_4 <- sensorover_threshold_4[sensorover_threshold_4 >= metrics$glucose_thresholds[4]]
    sensorover_threshold_4 <- sensorover_threshold_4[!base::is.na(sensorover_threshold_4)]
    xaxis <- base::seq(from = 0, length.out = base::length(sensorover_threshold_4), by =
                         (interval / 60))

    # Calculate cumulative AUC, and subtract recatangle where length = 10 & width = minutes
    if (base::length(sensorover_threshold_4) > 1) {
      aucs <- pracma::cumtrapz(xaxis, sensorover_threshold_4)
      aucs <-
        (aucs[base::length(sensorover_threshold_4)]) - (xaxis[base::length(xaxis)] * metrics$glucose_thresholds[4])
      cgmupload[base::sprintf("auc_over_%s", metrics$glucose_thresholds[4]), f] <- aucs
    } else {
      cgmupload[base::sprintf("auc_over_%s", metrics$glucose_thresholds[4]), f] <- 0
    }
    cgmupload[base::sprintf("average_auc_%s", metrics$glucose_thresholds[4]), f] <-
      base::as.numeric(cgmupload[base::sprintf("auc_over_%s", metrics$glucose_thresholds[4]), f]) /
      base::as.numeric(cgmupload["num_days_good_data", f])

    #auc over 13.9mmol/L,
    sensorover_threshold_5 <- table$sensorglucose
    sensorover_threshold_5 <- sensorover_threshold_5[sensorover_threshold_5 >= metrics$glucose_thresholds[5]]
    sensorover_threshold_5 <- sensorover_threshold_5[!base::is.na(sensorover_threshold_5)]
    xaxis <-
      base::seq(from = 0, length.out = base::length(sensorover_threshold_5), by =
                  (interval / 60))

    # Calculate cumulative AUC, and subtract recatangle where length = 10 & width = minutes
    if (base::length(sensorover_threshold_5) > 1) {
      aucs <- pracma::cumtrapz(xaxis, sensorover_threshold_5)
      aucs <-
        (aucs[base::length(sensorover_threshold_5)]) - (xaxis[base::length(xaxis)] * metrics$glucose_thresholds[5])
      cgmupload[base::sprintf("auc_over_%s", metrics$glucose_thresholds[5]), f] <- aucs
    } else {
      cgmupload[base::sprintf("auc_over_%s", metrics$glucose_thresholds[5]), f] <- 0
    }
    cgmupload[base::sprintf("average_auc_%s", metrics$glucose_thresholds[5]), f] <-
      base::as.numeric(cgmupload[base::sprintf("auc_over_%s", metrics$glucose_thresholds[5]), f]) /
      base::as.numeric(cgmupload["num_days_good_data", f])

    # CONGA
    n <- (congan * 3600)
    conga.times <- table$timestamp + n
    conga.times <- conga.times[!base::is.na(conga.times)]
    conga.times <- conga.times[base::order(conga.times)]
    conga.times <- conga.times[base::which(conga.times %in% table$timestamp)]
    begin.times <- conga.times - n
    base::suppressWarnings(congas <- table$sensorglucose[base::which(table$timestamp %in% conga.times)] -
                             table$sensorglucose[base::which(table$timestamp %in% begin.times)])
    cgmupload[base::paste0("conga_", congan), f] <- stats::sd(congas, na.rm = T)

    #MODD
    table$time <- lubridate::round_date(table$timestamp, "5 minutes")
    table$time <- base::strftime(table$time, format = "%H:%M", tz = "UTC")
    moddtable <- base::data.frame(base::matrix(ncol = 2, nrow = base::length(unique(table$time))))
    base::colnames(moddtable) <- c("time", "mean_differences")
    moddtable$time <- base::unique(table$time)

    #For each time, calculate differences (absolute values) and average them
    for (r in 1:nrow(moddtable)) {
      moddtable$mean_differences[r] <-
        base::mean(base::abs(base::diff(table$sensorglucose[
          base::which(table$time == moddtable$time[r])])))
    }
    #Average the averages
    cgmupload["modd", f] <- base::mean(stats::na.omit(moddtable$mean_differences))
    if (base::length(table$sensorglucose) < 4) {
      next
    }

    # Calculate MAGE; Smooth data using an exponentially weighted moving average, calculate SD of non-smoothed data
    table$smoothed <- base::as.numeric(zoo::rollapply(zoo::zoo(table$sensorglucose),
                                                      9, function(x) c(1, 2, 4, 8, 16, 8, 4, 2, 1) %*%
                                                        (x/46), fill = NA))
    table$smoothed[1:4] <- base::mean(stats::na.omit(table$sensorglucose[1:4]))
    table$smoothed[(base::length(table$smoothed) - 3):base::length(table$smoothed)] <- base::mean(
      table$sensorglucose[(base::length(table$sensorglucose) - 3):base::length(table$sensorglucose)])
    sd <- stats::sd(table$sensorglucose)

    #Identify turning points, peaks, and nadirs
    tpoints <- pastecs::turnpoints(table$smoothed)
    peaks <- base::which(tpoints[["peaks"]] == TRUE)
    pits <- base::which(tpoints[["pits"]] == TRUE)

    # Calculate the difference between each nadir and its following peak. If the data starts on a peak, remove it.
    #Otherwise remove the final pit to create an even number of pits and peaks.
    if (!base::is.na(tpoints[["firstispeak"]]) && tpoints[["firstispeak"]] ==
        TRUE && base::length(peaks) != base::length(pits)) {
      peaks <- peaks[2:base::length(peaks)]
    } else if (!base::is.na(tpoints[["firstispeak"]]) && tpoints[["firstispeak"]] ==
               FALSE && base::length(peaks) != base::length(pits)) {
      pits <- pits[1:(base::length(pits) - 1)]
    }
    differences <- table$sensorglucose[peaks] - table$sensorglucose[pits]

    # Calculate the average of the differences greater than the entire dataset; SD, 2SD, etc.
    if (magedef == "1sd") {
      cgmupload["r_mage", f] <- base::mean(stats::na.omit(differences[base::which(differences >
                                                                                    sd)]))
    } else if (magedef == "1.5sd") {
      cgmupload["r_mage", f] <- base::mean(stats::na.omit(differences[base::which(differences >
                                                                                    (sd * 1.5))]))
    } else if (magedef == "2sd") {
      cgmupload["r_mage", f] <- base::mean(stats::na.omit(differences[base::which(differences >
                                                                                    (sd * 2))]))
    } else {
      cgmupload["r_mage", f] <- base::mean(stats::na.omit(differences[base::which(differences >
                                                                                    magedef)]))
    }
    # End of for cycle to process the input files
   }

  #Format cgmupload
  cgmupload <- base::cbind(`Variable / Field Name` = rownames(cgmupload), cgmupload)
  cgmupload <- base::as.data.frame(base::t(cgmupload))
  cgmupload <- cgmupload[-1, ]
  cgmupload <- cgmupload[base::order(cgmupload$subject_id), ]

  #Remove rows that were not further processed due to lack of good data (more than 80% daily data availability)
  cgmupload <- cgmupload[!is.na(cgmupload$num_days_good_data),]

  #Write file
  filename <- base::paste(outputdirectory, "/", outputname, ".csv", sep = "")
  utils::write.csv(cgmupload, file = filename, row.names = FALSE, na = "")

  aggregateAGPdata$sensorglucose <- base::as.numeric(aggregateAGPdata$sensorglucose)

  if(subject_groups_mode){
    groups = c()
    for (row in 1:nrow(aggregateAGPdata)){
      subject_id = aggregateAGPdata[row, "subjectid"]
      if(subject_id %in% subject_group_1){
        groups <- c(groups, group_names[1])
      } else if(subject_id %in% subject_group_2){
        groups <- c(groups, group_names[2])
      } else if(subject_id %in% subject_group_3){
        groups <- c(groups, group_names[3])
      } else if(subject_id %in% subject_group_4){
        groups <- c(groups, group_names[4])
      } else{
        groups <- c(groups, "Other")
      }
    }

    aggregateAGPdata$group <- groups

    groups = c()
    for (row in 1:nrow(cgmupload)){
      subject_id = cgmupload[row, "subject_id"]
      if(subject_id %in% subject_group_1){
        groups <- c(groups, group_names[1])
      } else if(subject_id %in% subject_group_2){
        groups <- c(groups, group_names[2])
      } else if(subject_id %in% subject_group_3){
        groups <- c(groups, group_names[3])
      } else if(subject_id %in% subject_group_4){
        groups <- c(groups, group_names[4])
      } else{
        groups <- c(groups, "Other")
      }
    }
    cgmupload$group <- groups
  }

  aggregateAGPdata$time <-
    base::as.POSIXct(base::strftime(aggregateAGPdata$timestamp, format = "%H:%M", tz="GMT"),
                     format = "%H:%M", tz="GMT")

  xaxis_label = "Time (hour)"
  plot_title = "Aggregate Daily Overlay (%s Smoothing)"
  plot_title_aggregate = "Aggregate Hourly Readings"

  glucose_interval_levels = c(base::sprintf("under %s %s", metrics$glucose_thresholds[1], metrics$string),
                              base::sprintf("over %s under %s %s", metrics$glucose_thresholds[1], metrics$glucose_thresholds[2], metrics$string),
                              base::sprintf("over %s under %s %s", metrics$glucose_thresholds[2], metrics$glucose_thresholds[3], metrics$string),
                              base::sprintf("over %s under %s %s", metrics$glucose_thresholds[3], metrics$glucose_thresholds[5], metrics$string),
                              base::sprintf("over %s %s", metrics$glucose_thresholds[5], metrics$string))
  glucose_interval_names = base::rep(glucose_interval_levels, num_groups)

  file_name_string = c('subjects', 'groups')

  if(plot_interpolation){
    percent_of_interpolation = base::length(aggregateAGPdata$isinterpol[aggregateAGPdata$isinterpol == 1])/base::length(aggregateAGPdata$isinterpol)
    percent_of_interpolation = base::round(percent_of_interpolation*100, digits = 2)
    interpol_labels =  c("No", base::paste("Yes: ", percent_of_interpolation, "%"))

    aggAGPloess1 <-
      ggplot2::ggplot(aggregateAGPdata, ggplot2::aes(x = time, y = sensorglucose)) +
      ggplot2::geom_smooth(ggplot2::aes(y = sensorglucose), se = FALSE, na.rm=TRUE) +
      ggplot2::geom_point(ggplot2::aes(y = sensorglucose, colour = isinterpol, shape = isinterpol), size = 0.1, na.rm=TRUE) +
      ggplot2::ggtitle(plot_title_aggregate) +
      ggplot2::ylab(metrics$yaxis_label) +
      ggplot2::xlab(xaxis_label) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::scale_color_manual(name = 'Interpolated', values = c('black', 'red'), labels = interpol_labels) +
      ggplot2::scale_shape_manual(name = "Interpolated", labels = interpol_labels, values = c(19, 17)) +
      ggplot2::scale_x_datetime(labels = function(x) base::format(x, format = "%H:%M")) +
      ggplot2::ylim(metrics$yaxis_range_for_plotting[1], metrics$yaxis_range_for_plotting[2]) +
      ggplot2::theme_light()

    grDevices::pdf(base::paste(outputdirectory, "/", "Aggregate_AGP_Loess_interpolated.pdf", sep = ""),
                   width = 11, height = 8.5)
    graphics::plot(aggAGPloess1)
    while (!base::is.null(grDevices::dev.list()))
      grDevices::dev.off()
  }


  if(subject_groups_mode){
    if(plot_interpolation){

      interpol_labels = c()
      unique_groups = base::unique(aggregateAGPdata$group)
      for (index in 1:base::length(unique_groups)){

        unique_group_data <- aggregateAGPdata[aggregateAGPdata$group == unique_groups[index], ]
        length_of_data <- base::nrow(unique_group_data)
        length_of_interpolated_data <- base::nrow(unique_group_data[unique_group_data$isinterpol == T, ])
        percent_of_interpolation <- length_of_interpolated_data/length_of_data*100
        percent_of_interpolation <- base::round(percent_of_interpolation, digits = 2)
        interpol_labels =  base::rbind(interpol_labels, base::paste(unique_groups[index], " | interp: ", percent_of_interpolation, "%", sep=""))
      }

      color_group_factor = base::factor(aggregateAGPdata$group, levels=base::unique(aggregateAGPdata$group))


      percent_of_interpolation = base::length(aggregateAGPdata$isinterpol[aggregateAGPdata$isinterpol == 1])/base::length(aggregateAGPdata$isinterpol)
      percent_of_interpolation = base::round(percent_of_interpolation*100, digits = 2)
      interpol_labels =  c("No", base::paste("Yes: ", percent_of_interpolation, "%"))

      aggAGPloess1 <-
        ggplot2::ggplot(aggregateAGPdata, ggplot2::aes(x = time, y = sensorglucose)) +
        ggplot2::geom_smooth(ggplot2::aes(y = sensorglucose), se = FALSE, na.rm=TRUE) +
        ggplot2::geom_point(ggplot2::aes(y = sensorglucose, colour = isinterpol, shape = isinterpol), size = 0.1, na.rm=TRUE) +
        ggplot2::ggtitle(plot_title_aggregate) +
        ggplot2::ylab(metrics$yaxis_label) +
        ggplot2::xlab(xaxis_label) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::scale_color_manual(name = 'Interpolated', values = c('black', 'red'), labels = interpol_labels) +
        ggplot2::scale_shape_manual(name = "Interpolated", labels = interpol_labels, values = c(19, 17)) +
        ggplot2::scale_x_datetime(labels = function(x) base::format(x, format = "%H:%M")) +
        ggplot2::ylim(metrics$yaxis_range_for_plotting[1], metrics$yaxis_range_for_plotting[2]) +
        ggplot2::theme_light()

      grDevices::pdf(base::paste(outputdirectory, "/", "Aggregate_AGP_Loess_interpolated.pdf", sep = ""),
                     width = 11, height = 8.5)
      graphics::plot(aggAGPloess1)
      while (!base::is.null(grDevices::dev.list()))
        grDevices::dev.off()
    }


    AGPloess <-
      ggplot2::ggplot(aggregateAGPdata, ggplot2::aes(x = time, y = sensorglucose)) +
      ggplot2::geom_smooth(ggplot2::aes(y = sensorglucose, color = group), se = TRUE, na.rm=TRUE) +
      ggplot2::geom_point(ggplot2::aes(y = sensorglucose, color = group), shape = ".", na.rm=TRUE) +
      ggplot2::ggtitle(base::sprintf(plot_title, "LOESS")) +
      ggplot2::ylab(metrics$yaxis_label) +
      ggplot2::xlab(xaxis_label) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::labs(colour = groups_legend_title) +
      ggplot2::scale_x_datetime(labels = function(x) base::format(x, format = "%H:%M")) +
      ggplot2::ylim(metrics$yaxis_range_for_plotting[1], metrics$yaxis_range_for_plotting[2]) +
      ggplot2::theme_light()

    grDevices::pdf(base::paste(outputdirectory, "/", base::sprintf("AGP_Loess_%s.pdf", file_name_string[2]), sep = ""),
                   width = 11, height = 8.5)
    graphics::plot(AGPloess)
    grDevices::dev.off()

    if("Other" %in% groups){
    aggregateAGPdatanoother <- aggregateAGPdata[!aggregateAGPdata$group=="Other",]
    AGPloess2 <-
      ggplot2::ggplot(aggregateAGPdatanoother, ggplot2::aes(x = time, y = sensorglucose)) +
      ggplot2::geom_smooth(ggplot2::aes(y = sensorglucose, color = group), se = TRUE, na.rm=TRUE) +
      ggplot2::geom_point(ggplot2::aes(y = sensorglucose, color = group), shape = ".", na.rm=TRUE) +
      ggplot2::ggtitle(base::sprintf(plot_title, "LOESS")) +
      ggplot2::ylab(metrics$yaxis_label) +
      ggplot2::xlab(xaxis_label) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::labs(colour = groups_legend_title) +
      ggplot2::scale_x_datetime(labels = function(x) base::format(x, format = "%H:%M")) +
      ggplot2::ylim(metrics$yaxis_range_for_plotting[1], metrics$yaxis_range_for_plotting[2]) +
      ggplot2::theme_light()

    grDevices::pdf(base::paste(outputdirectory, "/", base::sprintf("AGP_Loess_%s_without_other_group.pdf", file_name_string[2]), sep = ""),
                   width = 11, height = 8.5)
    graphics::plot(AGPloess2)
    grDevices::dev.off()
    }


  if(num_groups==2){
    aggregateAGPdata_g1 = cgmupload[cgmupload$group == group_names[1], ]
    aggregateAGPdata_g2 = cgmupload[cgmupload$group == group_names[2], ]

    thres_1_string = base::sprintf("percent_time_under_%s", metrics$glucose_thresholds[1])
    thres_2_string = base::sprintf("percent_time_over_%s_under_%s",
                                   metrics$glucose_thresholds[1],
                                   metrics$glucose_thresholds[2])
    thres_3_string = base::sprintf("percent_time_over_%s_under_%s",
                                   metrics$glucose_thresholds[2],
                                   metrics$glucose_thresholds[3])
    thres_4_string = base::sprintf("percent_time_over_%s_under_%s",
                                   metrics$glucose_thresholds[3],
                                   metrics$glucose_thresholds[5])
    thres_5_string = base::sprintf("percent_time_over_%s", metrics$glucose_thresholds[5])

    aggregateAGPdata_g1_mean_thres_1 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g1[, thres_1_string]))), digits = 1)
    aggregateAGPdata_g1_mean_thres_2 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g1[, thres_2_string]))), digits = 1)
    aggregateAGPdata_g1_mean_thres_3 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g1[, thres_3_string]))), digits = 1)
    aggregateAGPdata_g1_mean_thres_4 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g1[, thres_4_string]))), digits = 1)
    aggregateAGPdata_g1_mean_thres_5 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g1[, thres_5_string]))), digits = 1)

    aggregateAGPdata_g2_mean_thres_1 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g2[, thres_1_string]))), digits = 1)
    aggregateAGPdata_g2_mean_thres_2 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g2[, thres_2_string]))), digits = 1)
    aggregateAGPdata_g2_mean_thres_3 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g2[, thres_3_string]))), digits = 1)
    aggregateAGPdata_g2_mean_thres_4 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g2[, thres_4_string]))), digits = 1)
    aggregateAGPdata_g2_mean_thres_5 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g2[, thres_5_string]))), digits = 1)

    mean_percentages = c(aggregateAGPdata_g1_mean_thres_1,
                         aggregateAGPdata_g1_mean_thres_2,
                         aggregateAGPdata_g1_mean_thres_3,
                         aggregateAGPdata_g1_mean_thres_4,
                         aggregateAGPdata_g1_mean_thres_5,
                         aggregateAGPdata_g2_mean_thres_1,
                         aggregateAGPdata_g2_mean_thres_2,
                         aggregateAGPdata_g2_mean_thres_3,
                         aggregateAGPdata_g2_mean_thres_4,
                         aggregateAGPdata_g2_mean_thres_5)
    percentage_labels = c()

    for(i in 1:base::length(mean_percentages)){
      if(mean_percentages[i] < 3){
        percentage_labels <- base::rbind(percentage_labels, '')
      }
      else{
        percentage_labels <- base::rbind(percentage_labels, paste(mean_percentages[i], "%"))
      }
    }

    data_for_stacked <- data.frame(
      groups = base::rep(group_names, each = 5),
      intervals = base::factor(glucose_interval_names, levels = base::rev(glucose_interval_levels)),
      percentage = mean_percentages,
      percentage_labels = percentage_labels
    )

    data_for_stacked <- plyr::ddply(data_for_stacked, plyr::.(groups),
                                    base::transform, pos = base::cumsum(percentage) - (0.5 * percentage))

    # Stacked + percent
    aggStackedTimeInThresholds <- ggplot2::ggplot(data_for_stacked, ggplot2::aes(fill=intervals, y=percentage, x=groups)) +
      ggplot2::geom_bar(stat="identity", colour="black", size=0.3) +
      ggplot2::scale_fill_manual(values=base::rev(metrics$glucose_threshold_colors)) +
      ggplot2::geom_text(data=data_for_stacked, ggplot2::aes(x = groups, y = pos, label = percentage_labels),
                         size=4) +
      ggplot2::theme_light()

    grDevices::pdf(base::paste(outputdirectory, "/", "Aggregate_time_spent_intervals.pdf", sep = ""),
                   width = 11, height = 8.5)
    graphics::plot(aggStackedTimeInThresholds)
    grDevices::dev.off()
  } else if(num_groups==3){
      aggregateAGPdata_g1 = cgmupload[cgmupload$group == group_names[1], ]
      aggregateAGPdata_g2 = cgmupload[cgmupload$group == group_names[2], ]
      aggregateAGPdata_g3 = cgmupload[cgmupload$group == group_names[3], ]

      thres_1_string = base::sprintf("percent_time_under_%s", metrics$glucose_thresholds[1])
      thres_2_string = base::sprintf("percent_time_over_%s_under_%s",
                                     metrics$glucose_thresholds[1],
                                     metrics$glucose_thresholds[2])
      thres_3_string = base::sprintf("percent_time_over_%s_under_%s",
                                     metrics$glucose_thresholds[2],
                                     metrics$glucose_thresholds[3])
      thres_4_string = base::sprintf("percent_time_over_%s_under_%s",
                                     metrics$glucose_thresholds[3],
                                     metrics$glucose_thresholds[5])
      thres_5_string = base::sprintf("percent_time_over_%s", metrics$glucose_thresholds[5])

      aggregateAGPdata_g1_mean_thres_1 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g1[, thres_1_string]))), digits = 1)
      aggregateAGPdata_g1_mean_thres_2 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g1[, thres_2_string]))), digits = 1)
      aggregateAGPdata_g1_mean_thres_3 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g1[, thres_3_string]))), digits = 1)
      aggregateAGPdata_g1_mean_thres_4 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g1[, thres_4_string]))), digits = 1)
      aggregateAGPdata_g1_mean_thres_5 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g1[, thres_5_string]))), digits = 1)

      aggregateAGPdata_g2_mean_thres_1 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g2[, thres_1_string]))), digits = 1)
      aggregateAGPdata_g2_mean_thres_2 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g2[, thres_2_string]))), digits = 1)
      aggregateAGPdata_g2_mean_thres_3 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g2[, thres_3_string]))), digits = 1)
      aggregateAGPdata_g2_mean_thres_4 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g2[, thres_4_string]))), digits = 1)
      aggregateAGPdata_g2_mean_thres_5 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g2[, thres_5_string]))), digits = 1)

      aggregateAGPdata_g3_mean_thres_1 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g3[, thres_1_string]))), digits = 1)
      aggregateAGPdata_g3_mean_thres_2 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g3[, thres_2_string]))), digits = 1)
      aggregateAGPdata_g3_mean_thres_3 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g3[, thres_3_string]))), digits = 1)
      aggregateAGPdata_g3_mean_thres_4 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g3[, thres_4_string]))), digits = 1)
      aggregateAGPdata_g3_mean_thres_5 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g3[, thres_5_string]))), digits = 1)

      mean_percentages = c(aggregateAGPdata_g1_mean_thres_1,
                           aggregateAGPdata_g1_mean_thres_2,
                           aggregateAGPdata_g1_mean_thres_3,
                           aggregateAGPdata_g1_mean_thres_4,
                           aggregateAGPdata_g1_mean_thres_5,
                           aggregateAGPdata_g2_mean_thres_1,
                           aggregateAGPdata_g2_mean_thres_2,
                           aggregateAGPdata_g2_mean_thres_3,
                           aggregateAGPdata_g2_mean_thres_4,
                           aggregateAGPdata_g2_mean_thres_5,
                           aggregateAGPdata_g3_mean_thres_1,
                           aggregateAGPdata_g3_mean_thres_2,
                           aggregateAGPdata_g3_mean_thres_3,
                           aggregateAGPdata_g3_mean_thres_4,
                           aggregateAGPdata_g3_mean_thres_5)
      percentage_labels = c()

      for(i in 1:base::length(mean_percentages)){
        if(mean_percentages[i] < 3){
          percentage_labels <- base::rbind(percentage_labels, '')
        }
        else{
          percentage_labels <- base::rbind(percentage_labels, paste(mean_percentages[i], "%"))
        }
      }

      data_for_stacked <- data.frame(
        groups = base::rep(group_names, each = 5),
        intervals = base::factor(glucose_interval_names, levels = base::rev(glucose_interval_levels)),
        percentage = mean_percentages,
        percentage_labels = percentage_labels
      )

      data_for_stacked <- plyr::ddply(data_for_stacked, plyr::.(groups),
                                      base::transform, pos = base::cumsum(percentage) - (0.5 * percentage))

      # Stacked + percent
      aggStackedTimeInThresholds <- ggplot2::ggplot(data_for_stacked, ggplot2::aes(fill=intervals, y=percentage, x=groups)) +
        ggplot2::geom_bar(stat="identity", colour="black", size=0.3) +
        ggplot2::scale_fill_manual(values=base::rev(metrics$glucose_threshold_colors)) +
        ggplot2::geom_text(data=data_for_stacked, ggplot2::aes(x = groups, y = pos, label = percentage_labels),
                           size=4) +
        ggplot2::theme_light()

      grDevices::pdf(base::paste(outputdirectory, "/", "Aggregate_time_spent_intervals.pdf", sep = ""),
                     width = 11, height = 8.5)
      graphics::plot(aggStackedTimeInThresholds)
      grDevices::dev.off()
    } else if(num_groups==4){
        aggregateAGPdata_g1 = cgmupload[cgmupload$group == group_names[1], ]
        aggregateAGPdata_g2 = cgmupload[cgmupload$group == group_names[2], ]
        aggregateAGPdata_g3 = cgmupload[cgmupload$group == group_names[3], ]
        aggregateAGPdata_g4 = cgmupload[cgmupload$group == group_names[4], ]

        thres_1_string = base::sprintf("percent_time_under_%s", metrics$glucose_thresholds[1])
        thres_2_string = base::sprintf("percent_time_over_%s_under_%s",
                                       metrics$glucose_thresholds[1],
                                       metrics$glucose_thresholds[2])
        thres_3_string = base::sprintf("percent_time_over_%s_under_%s",
                                       metrics$glucose_thresholds[2],
                                       metrics$glucose_thresholds[3])
        thres_4_string = base::sprintf("percent_time_over_%s_under_%s",
                                       metrics$glucose_thresholds[3],
                                       metrics$glucose_thresholds[5])
        thres_5_string = base::sprintf("percent_time_over_%s", metrics$glucose_thresholds[5])

        aggregateAGPdata_g1_mean_thres_1 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g1[, thres_1_string]))), digits = 1)
        aggregateAGPdata_g1_mean_thres_2 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g1[, thres_2_string]))), digits = 1)
        aggregateAGPdata_g1_mean_thres_3 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g1[, thres_3_string]))), digits = 1)
        aggregateAGPdata_g1_mean_thres_4 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g1[, thres_4_string]))), digits = 1)
        aggregateAGPdata_g1_mean_thres_5 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g1[, thres_5_string]))), digits = 1)

        aggregateAGPdata_g2_mean_thres_1 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g2[, thres_1_string]))), digits = 1)
        aggregateAGPdata_g2_mean_thres_2 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g2[, thres_2_string]))), digits = 1)
        aggregateAGPdata_g2_mean_thres_3 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g2[, thres_3_string]))), digits = 1)
        aggregateAGPdata_g2_mean_thres_4 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g2[, thres_4_string]))), digits = 1)
        aggregateAGPdata_g2_mean_thres_5 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g2[, thres_5_string]))), digits = 1)

        aggregateAGPdata_g3_mean_thres_1 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g3[, thres_1_string]))), digits = 1)
        aggregateAGPdata_g3_mean_thres_2 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g3[, thres_2_string]))), digits = 1)
        aggregateAGPdata_g3_mean_thres_3 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g3[, thres_3_string]))), digits = 1)
        aggregateAGPdata_g3_mean_thres_4 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g3[, thres_4_string]))), digits = 1)
        aggregateAGPdata_g3_mean_thres_5 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g3[, thres_5_string]))), digits = 1)

        aggregateAGPdata_g4_mean_thres_1 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g4[, thres_1_string]))), digits = 1)
        aggregateAGPdata_g4_mean_thres_2 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g4[, thres_2_string]))), digits = 1)
        aggregateAGPdata_g4_mean_thres_3 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g4[, thres_3_string]))), digits = 1)
        aggregateAGPdata_g4_mean_thres_4 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g4[, thres_4_string]))), digits = 1)
        aggregateAGPdata_g4_mean_thres_5 = base::round(base::mean(base::as.numeric(base::as.character(aggregateAGPdata_g4[, thres_5_string]))), digits = 1)


        mean_percentages = c(aggregateAGPdata_g1_mean_thres_1,
                             aggregateAGPdata_g1_mean_thres_2,
                             aggregateAGPdata_g1_mean_thres_3,
                             aggregateAGPdata_g1_mean_thres_4,
                             aggregateAGPdata_g1_mean_thres_5,
                             aggregateAGPdata_g2_mean_thres_1,
                             aggregateAGPdata_g2_mean_thres_2,
                             aggregateAGPdata_g2_mean_thres_3,
                             aggregateAGPdata_g2_mean_thres_4,
                             aggregateAGPdata_g2_mean_thres_5,
                             aggregateAGPdata_g3_mean_thres_1,
                             aggregateAGPdata_g3_mean_thres_2,
                             aggregateAGPdata_g3_mean_thres_3,
                             aggregateAGPdata_g3_mean_thres_4,
                             aggregateAGPdata_g3_mean_thres_5,
                             aggregateAGPdata_g4_mean_thres_1,
                             aggregateAGPdata_g4_mean_thres_2,
                             aggregateAGPdata_g4_mean_thres_3,
                             aggregateAGPdata_g4_mean_thres_4,
                             aggregateAGPdata_g4_mean_thres_5)
        percentage_labels = c()

        for(i in 1:base::length(mean_percentages)){
          if(mean_percentages[i] < 3){
            percentage_labels <- base::rbind(percentage_labels, '')
          }
          else{
            percentage_labels <- base::rbind(percentage_labels, paste(mean_percentages[i], "%"))
          }
        }

        data_for_stacked <- data.frame(
          groups = base::rep(group_names, each = 5),
          intervals = base::factor(glucose_interval_names, levels = base::rev(glucose_interval_levels)),
          percentage = mean_percentages,
          percentage_labels = percentage_labels
        )

        data_for_stacked <- plyr::ddply(data_for_stacked, plyr::.(groups),
                                        base::transform, pos = base::cumsum(percentage) - (0.5 * percentage))

        # Stacked + percent
        aggStackedTimeInThresholds <- ggplot2::ggplot(data_for_stacked, ggplot2::aes(fill=intervals, y=percentage, x=groups)) +
          ggplot2::geom_bar(stat="identity", colour="black", size=0.3) +
          ggplot2::scale_fill_manual(values=base::rev(metrics$glucose_threshold_colors)) +
          ggplot2::geom_text(data=data_for_stacked, ggplot2::aes(x = groups, y = pos, label = percentage_labels),
                             size=4) +
          ggplot2::theme_light()

        grDevices::pdf(base::paste(outputdirectory, "/", "Aggregate_time_spent_intervals.pdf", sep = ""),
                       width = 11, height = 8.5)
        graphics::plot(aggStackedTimeInThresholds)
        grDevices::dev.off()
      }
    }

  #Plot for every subject including LOESS smoothing curve (only if there are less than 20 files; otherwise, the plot is too crowded)
  if(nrow(cgmupload)<=20){
  AGPloessSubject <-
    ggplot2::ggplot(aggregateAGPdata, ggplot2::aes(x = time, y = sensorglucose)) +
    ggplot2::geom_smooth(ggplot2::aes(y = sensorglucose, color = subjectid), se = FALSE, na.rm=TRUE) +
    ggplot2::geom_point(ggplot2::aes(y = sensorglucose, color = subjectid), shape = ".", na.rm=TRUE) +
    ggplot2::ggtitle(base::sprintf(plot_title, "LOESS")) +
    ggplot2::ylab(metrics$yaxis_label) +
    ggplot2::xlab(xaxis_label) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(colour = "Subject ID") +
    ggplot2::scale_x_datetime(labels = function(x) base::format(x, format = "%H:%M")) +
    ggplot2::ylim(metrics$yaxis_range_for_plotting[1], metrics$yaxis_range_for_plotting[2]) +
    ggplot2::theme_light()

  grDevices::pdf(base::paste(outputdirectory, "/", base::sprintf("AGP_Loess_%s.pdf", file_name_string[1]), sep = ""),
                 width = 11, height = 8.5)
  graphics::plot(AGPloessSubject)
  grDevices::dev.off()
  }
  #Aggregate basic plot with LOESS smoothing curve
  aggAGPloessBasic <-
    ggplot2::ggplot(aggregateAGPdata, ggplot2::aes(x = time, y = sensorglucose)) +
    ggplot2::geom_smooth(ggplot2::aes(y = sensorglucose), se = FALSE, na.rm=TRUE) +
    ggplot2::geom_point(ggplot2::aes(y = sensorglucose), shape = ".", na.rm=TRUE) +
    ggplot2::ggtitle(base::sprintf(plot_title, "LOESS")) +
    ggplot2::ylab(metrics$yaxis_label) +
    ggplot2::xlab(xaxis_label) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_x_datetime(labels = function(x) base::format(x, format = "%H:%M")) +
    ggplot2::ylim(metrics$yaxis_range_for_plotting[1], metrics$yaxis_range_for_plotting[2])

  grDevices::pdf(base::paste(outputdirectory, "/", "Aggregate_AGP_Loess_Basic.pdf", sep = ""),
                 width = 11, height = 8.5)
  graphics::plot(aggAGPloessBasic)
  grDevices::dev.off()

  #Aggregate with data points colored within the respective glucose_thresholds including LOESS smoothing curve
  aggregateAGPdata$color<-
    base::cut(aggregateAGPdata$sensorglucose, breaks = metrics$glucose_thresholds_plot,
              include.lowest = T)

  if(metric_option=="mmol/l"){
    aggregateAGPdata$label<-
      base::ifelse(aggregateAGPdata$color=="[-Inf,3]", glucose_interval_levels[1],
                   base::ifelse(aggregateAGPdata$color=="(3,3.9]",glucose_interval_levels[2],
                                base::ifelse(aggregateAGPdata$color=="(3.9,10]",glucose_interval_levels[3],
                                             base::ifelse(aggregateAGPdata$color=="(10,13.9]",glucose_interval_levels[4],
                                                          glucose_interval_levels[5]))))
    PallettePlot <- c("under 3 mmol/l"  = "#A71B29", "over 3 under 3.9 mmol/l" = "#ED1C24",
                 "over 3.9 under 10 mmol/l" = "#5ABC68", "over 10 under 13.9 mmol/l" = "#FFF200",
                 "over 13.9 mmol/l" = "#FDB813")
  } else {
    aggregateAGPdata$label<-
      base::ifelse(aggregateAGPdata$color=="[-Inf,54]", glucose_interval_levels[1],
                   base::ifelse(aggregateAGPdata$color=="(54,70]",glucose_interval_levels[2],
                                base::ifelse(aggregateAGPdata$color=="(70,180]",glucose_interval_levels[3],
                                             base::ifelse(aggregateAGPdata$color=="(180,250]",glucose_interval_levels[4],
                                                          glucose_interval_levels[5]))))
    PallettePlot <- c("under 54 mg/dl"  = "#A71B29", "over 54 under 70 mg/dl" = "#ED1C24",
                      "over 70 under 180 mg/dl" = "#5ABC68", "over 180 under 250 mg/dl" = "#FFF200",
                      "over 250 mg/dl" = "#FDB813")

  }

  aggregateAGPdata <- aggregateAGPdata[!base::is.na(aggregateAGPdata$sensorglucose),]
  aggAGPloessThresCol <-
    ggplot2::ggplot(aggregateAGPdata, ggplot2::aes(x = time, y = sensorglucose)) +
    ggplot2::geom_smooth(ggplot2::aes(y = sensorglucose), se = FALSE, na.rm=TRUE) +
    ggplot2::geom_point(ggplot2::aes(y = sensorglucose, colour = label), size = 0.2, na.rm=TRUE) +
    ggplot2::ggtitle(plot_title_aggregate) +
    ggplot2::ylab(metrics$yaxis_label) +
    ggplot2::xlab(xaxis_label) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_color_manual(name = 'Intervals', values = PallettePlot) +
    ggplot2::scale_x_datetime(labels = function(x) base::format(x, format = "%H:%M")) +
    ggplot2::ylim(metrics$yaxis_range_for_plotting[1], metrics$yaxis_range_for_plotting[2]) +
    ggplot2::theme_light()

  grDevices::pdf(base::paste(outputdirectory, "/", "AGP_Loess_ThresCol.pdf", sep = ""),
                 width = 11, height = 8.5)
  graphics::plot(aggAGPloessThresCol)
  while (!base::is.null(grDevices::dev.list()))
    grDevices::dev.off()
}


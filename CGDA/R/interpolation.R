interpolateGlucoseValues <- function(dataframe, maximum_gap, interpolate_function, interval){

  dataframe <- addNaNValsToDfForImputation(dataframe, maximum_gap)

  dataframe$isinterpol <- base::ifelse(base::is.na(dataframe$sensorglucose), T, F)

  if(interpolate_function == 'linear'){
    dataframe$sensorglucose <- zoo::na.approx(base::as.numeric(dataframe$sensorglucose), na.rm = FALSE, maxgap = (maximum_gap * 60)/interval)
  } else if(interpolate_function == 'spline'){
    dataframe$sensorglucose <- zoo::na.spline(base::as.numeric(dataframe$sensorglucose), na.rm = FALSE, maxgap = (maximum_gap * 60)/interval)
  }
  return (dataframe)
}

addNaNValsToDfForImputation <- function(dataFrame, maximum_gap) {

  dataFrame2 =  data.table::copy(dataFrame)
  num_interpolated = 0
  for(i in 1:nrow(dataFrame)) {
    row <- dataFrame[i,]
    nextRow <- dataFrame[i+1,]
    time_difference_lubridate <- nextRow['timestamp'][1] - row['timestamp'][1]

    if(!base::is.na(time_difference_lubridate)){

      time_difference <- lubridate::time_length(time_difference_lubridate$timestamp[1], unit = "minute")

      if(!base::is.na(nextRow['timestamp'][1]) &&
         !base::is.na(row['timestamp'][1]) &&
         time_difference > 15 &&
         time_difference < maximum_gap){

        index = i + num_interpolated

        prevRow <- data.table::copy(row)
        while(time_difference > 30 && time_difference < maximum_gap){
          newRow <- prevRow
          newRow['timestamp'] = newRow['timestamp']+15*60
          newRow['sensorglucose'] = NA_character_
          dataFrame2 <- insertRowInIndex(dataFrame2, newRow, index)
          time_difference_lubridate <- nextRow['timestamp'][1] - newRow['timestamp'][1]
          time_difference <- lubridate::time_length(time_difference_lubridate$timestamp[1], unit = "minute")
          prevRow <- data.table::copy(newRow)
          index <- index + 1
          num_interpolated <- num_interpolated + 1
        }
      }
    }


  }
  return (dataFrame2)
}

insertRowInIndex <- function(existingDF, newrow, r) {
  return (base::rbind(existingDF[1:r, ], newrow, existingDF[-(1:r), ]))
}

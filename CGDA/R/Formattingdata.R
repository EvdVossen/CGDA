Formatfirstlastdays <- function(table) {
  table1 <- base::split(table, base::cut(base::strptime(table$timestamp, format="%F %R"),"1 days"))
  firstday <- base::do.call(base::rbind.data.frame, table1[1])
  firstday <- base::format(firstday$timestamp,"%H:%M:%S")
  rmfirstday <- base::ifelse(firstday[1]>="02:00:00", T, F)
  lastday <- base::do.call(base::rbind.data.frame, table1[base::length(table1)])
  lastday <- base::format(lastday$timestamp, "%H:%M:%S")
  rmlastday <- base::ifelse(dplyr::last(lastday)<="22:00:00", T, F)

  if (rmfirstday == TRUE){
    table1 <- table1[-1]
  }
  if (rmlastday == TRUE){
    table1 <- table1[-base::length(table1)]
  }
  table1 <- base::do.call(base::rbind.data.frame, table1)
  base::row.names(table1) <- NULL
  return(table1)
}

format80percent <- function(table, interval){
  table1 <- table[!base::is.na(table$sensorglucose), ]
  table1 <- base::split(table1, base::cut(base::strptime(table1$timestamp, format="%F %R"),"1 days"))

  lessthan80 <- base::vector(mode = "logical", length = length(table1))
  for (i in 1:base::length(table1)) {
    lessthan80[i] <- base::ifelse((base::length(table1[[i]]$sensorglucose))/(86400/interval) <= 0.8, T, F)
  }

  lessthan80 <- base::which(lessthan80 == TRUE, arr.ind = TRUE)
  if (base::length(lessthan80) == 0){
    table1 <- table1
  } else {
    table1 <- table1[-lessthan80]
  }
  table1 <- base::do.call(base::rbind.data.frame, table1)
  base::row.names(table1) <- NULL
  return(table1)
}

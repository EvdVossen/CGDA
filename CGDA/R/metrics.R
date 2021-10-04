hb1ac_dcct_to_ifcc <- function(dcct) {
  return (10.929 * (dcct - 2.15))
}

glucose_threshold_colors <- c('#A71B29', '#ED1C24', '#5ABC68', '#FFF200', '#FDB813')

metric_mmol_per_l <- NULL
metric_mmol_per_l$glucose_thresholds <- c(3.0, 3.9, 10, 13, 13.9)
metric_mmol_per_l$glucose_thresholds_plot <- c(-Inf, 3.0, 3.9, 10, 13.9, Inf)
metric_mmol_per_l$glucose_threshold_colors <- glucose_threshold_colors
metric_mmol_per_l$string <- 'mmol/l'

metric_mmol_per_l$get_gmi_percent <- function(glucose_table) {
  return (base::round(3.31 + (0.02392 * base::mean(glucose_table[
    base::which(!base::is.na(glucose_table))])*18.015), digits = 2))
}

metric_mmol_per_l$get_gmi <- function(glucose_table) {
  return (base::round(12.71 + (4.70587 * base::mean(glucose_table[
    base::which(!base::is.na(glucose_table))])), digits = 1))
}

metric_mmol_per_l$get_a1c_percent <- function(glucose_table) {
  return (((base::mean(glucose_table[base::which(!base::is.na(glucose_table))])*18.015)+46.7) / 28.7)
}

metric_mmol_per_l$get_a1c <- hb1ac_dcct_to_ifcc
metric_mmol_per_l$yaxis_range_for_plotting <- c(0,16)
metric_mmol_per_l$yaxis_label = "Blood Glucose (mmol/L)"

metric_mg_per_dl <- NULL
metric_mg_per_dl$glucose_thresholds <- c(54, 70, 180, 234, 250)
metric_mg_per_dl$glucose_thresholds_plot <- c(-Inf, 54, 70, 180, 250, Inf)
metric_mg_per_dl$glucose_threshold_colors <- glucose_threshold_colors
metric_mg_per_dl$string <- 'mg/dl'

metric_mg_per_dl$get_gmi_percent <- function(glucose_table) {
  return (base::round((3.31 + (0.02392 * base::mean(glucose_table[
    base::which(!base::is.na(glucose_table))]))), digits = 2))
}

metric_mg_per_dl$get_gmi <- function(glucose_table) {
  return (base::round(12.71 + (4.70587 * base::mean(glucose_table[
    base::which(!base::is.na(glucose_table))])/18.015), digits = 2))
}

metric_mg_per_dl$get_a1c_percent <- function(glucose_table) {
  return ((46.7 + (base::mean(glucose_table[base::which(!base::is.na(glucose_table))])))/28.7)
}

metric_mg_per_dl$get_a1c <- hb1ac_dcct_to_ifcc
metric_mg_per_dl$yaxis_range_for_plotting <- c(0,400)
metric_mg_per_dl$yaxis_label = "Blood Glucose (mg/dL)"

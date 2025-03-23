library(dplyr)
library(RJSONIO)
setwd("//ProteowiseNAS2/Run Data/Beta Run Data")
# setwd("C://Users//Chetan Hire//Documents//GitHub//json-to-dataframe")
fi <- list.files(pattern = "ReagentLog.json",
  recursive = TRUE, full.names = TRUE
)

summary_data <- data.frame(carrierID = character(), ctg_pkg_info = character(),
  ctg_prep_notes = character(), data_type = character(),
  cycle_end_date = integer(),
  index = integer(), run_num = integer(), label = character(),
  experiment_name = character(), script_name = character(),
  avg_final_wash_thru_dp = numeric(), rig_Name = character(),
  run_name = character(), additional_notes = character(),
  no_of_lanes = integer(),
  sample_1 = character(), concentration_1 = double(),
  sample_2 = character(), concentration_2 = double(),
  sample_3 = character(), concentration_3 = double(),
  sample_4 = character(), concentration_4 = double(),
  sample_5 = character(), concentration_5 = double(),
  sample_6 = character(), concentration_6 = double(),
  sample_7 = character(), concentration_7 = double(),
  sample_8 = character(), concentration_8 = double(),
  sample_9 = character(), concentration_9 = double(),
  sample_10 = character(), concentration_10 = double(),
  sample_11 = character(), concentration_11 = double(),
  sample_12 = character(), concentration_12 = double(),
  sample_13 = character(), concentration_13 = double(),
  sample_14 = character(), concentration_14 = double(),
  sample_15 = character(), concentration_15 = double(),
  sample_16 = character(), concentration_16 = double(),
  sample_17 = character(), concentration_17 = double(),
  sample_18 = character(), concentration_18 = double(),
  sample_19 = character(), concentration_19 = double(),
  sample_20 = character(), concentration_20 = double()
)

for (i in 2: length(fi)) {
  json_data <- fromJSON(fi[i], nullValue = NA)
  
  # cID <- NA
  # for (i in seq_len(length(fi))) {
  #   cID[i] <- strsplit(fi[i], "/")[[1]][length(strsplit(fi[i], "/")[[1]]) -1 ]
  # }
  
  carr_id <- strsplit(fi[i], "/")[[1]][length(strsplit(fi[i], "/")[[1]]) -1 ]

  # if (length(json_data) == 18) {
  #   summ_data <- summary_data[0, ]
  #   if ("Carrier ID" %in% names(json_data)) {
  #     carr_id <- json_data$`Carrier ID`
  #   } else if ("Carrier Number" %in% names(json_data)) {
  #     carr_id <- json_data$`Carrier Number`
  #   } else {
  #     carr_id <- NA
  #   }

    # for (j in 1: length(json_data$`Run Details`)) { # nolint
    for (j in seq_len(length(json_data$`Run Details`))) {
      a <- nrow(summ_data) + 1
      summ_data[a, ] <- NA
      summ_data$data_type[a] <- "Run Details"
      summ_data$rig_Name[a] <- json_data$`Run Details`[[j]]$`Rig Name`
      summ_data$run_num[a] <- json_data$`Run Details`[[j]]$`Run Num`
      summ_data$run_name[a] <- json_data$`Run Details`[[j]]$`Run Name`
      summ_data$additional_notes[a] <-
        as.character(json_data$`Run Details`[[j]]$`Run Completion Details`$`Additional Notes`)
    }

    if (length(json_data$`Cycle Data`) > 0) {

      # for (k in 1: length(json_data$`Cycle Data`)) { # nolint
      for (k in seq_len(length(json_data$`Cycle Data`))) {

        b <- nrow(summ_data) + 1
        summ_data[b, ] <- NA
        summ_data$data_type[b] <- "Cycle Data"
        summ_data$index[b] <- json_data$`Cycle Data`[[k]]$index
        summ_data$run_num[b] <- json_data$`Cycle Data`[[k]]$run_num
        summ_data$label[b] <- json_data$`Cycle Data`[[k]]$label
        summ_data$experiment_name[b] <-
          json_data$`Cycle Data`[[k]]$experiment_name
        summ_data$script_name[b] <- json_data$`Cycle Data`[[k]]$script_name
        summ_data$avg_final_wash_thru_dp[b] <-
          json_data$`Cycle Data`[[k]]$cycle_metrics[[1]]

        if ("cycle_end_date" %in% names(json_data$`Cycle Data`[[k]])) {
          summ_data$cycle_end_date[b] <-
            as.integer(json_data$`Cycle Data`[[k]]$cycle_end_date)
        }

        if (length(json_data$`Run Details`) >=
              json_data$`Cycle Data`[[k]]$run_num) {
          parent_run_num <- json_data$`Cycle Data`[[k]]$run_num
          summ_data$rig_Name[b] <-
            json_data$`Run Details`[[parent_run_num]]$`Rig Name`
          summ_data$run_name[b] <-
            json_data$`Run Details`[[parent_run_num]]$`Run Name`
          summ_data$additional_notes[b] <-
            as.character(json_data$`Run Details`[[parent_run_num]]$`Run Completion Details`$`Additional Notes`)
        } else {
          summ_data$rig_Name[b] <- "DNF"
          summ_data$run_name[b] <- "DNF"
          summ_data$additional_notes[b] <- "DNF"
        }
      }
    }

    if (length(json_data$`Non-Detect Cycle Data`) > 0) {

      # for (i in 1: length(json_data$`Non-Detect Cycle Data`)) { # nolint
      for (i in seq_len(length(json_data$`Non-Detect Cycle Data`))) {
        b <- nrow(summ_data) + 1
        summ_data[b, ] <- NA
        summ_data$data_type[b] <- "Non-Detect Cycle Data"
        summ_data$index[b] <- json_data$`Non-Detect Cycle Data`[[i]]$index
        summ_data$run_num[b] <- json_data$`Non-Detect Cycle Data`[[i]]$run_num
        summ_data$label[b] <- json_data$`Non-Detect Cycle Data`[[i]]$label
        summ_data$experiment_name[b] <-
          json_data$`Non-Detect Cycle Data`[[i]]$experiment_name
        summ_data$script_name[b] <-
          json_data$`Non-Detect Cycle Data`[[i]]$script_name
        summ_data$avg_final_wash_thru_dp[b] <-
          json_data$`Non-Detect Cycle Data`[[i]]$cycle_metrics[[1]]

        if ("cycle_end_date" %in% names(json_data$`Non-Detect Cycle Data`[[i]])) {
          summ_data$cycle_end_date <
            as.integer(json_data$`Non-Detect Cycle Data`[[i]]$cycle_end_date)
        }

        if (length(json_data$`Run Details`) >=
              json_data$`Non-Detect Cycle Data`[[i]]$run_num) {
          parent_run_num <- json_data$`Non-Detect Cycle Data`[[i]]$run_num
          summ_data$rig_Name[b] <-
            json_data$`Run Details`[[parent_run_num]]$`Rig Name`
          summ_data$run_name[b] <-
            json_data$`Run Details`[[parent_run_num]]$`Run Name`
          summ_data$additional_notes[b] <-
            as.character(json_data$`Run Details`[[parent_run_num]]$`Run Completion Details`$`Additional Notes`)
        } else {
          summ_data$rig_Name[b] <- "DNF"
          summ_data$run_name[b] <- "DNF"
          summ_data$additional_notes[b] <- "DNF"
        }
      }
    }
    # for (j in 1: length(json_data$`Lane Content`)) { # nolint
    for (j in seq_len(length(json_data$`Lane Content`))) {
      summ_data[, 14 + 2 * j] <- json_data$`Lane Content`[[j]]$sample
      summ_data[, 15 + 2 * j] <- json_data$`Lane Content`[[j]]$concentration
    }

    summ_data$no_of_lanes <- length(json_data$`Lane Content`)
    summ_data$carrierID <- carr_id
    summ_data$ctg_pkg_info <- json_data$`Cartridge ID`
    summ_data$ctg_prep_notes <- json_data$`Carrier Rev`
    summary_data <- bind_rows(summary_data, summ_data)
  }
}
fname <- paste0("summary_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
write.csv(summary_data, fname, row.names = FALSE)
rm(list = ls(), envir = globalenv())


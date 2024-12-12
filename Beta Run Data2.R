library(dplyr)
library(RJSONIO)
setwd("//ProteowiseNAS2/Run Data/Beta Run Data")
fi <- list.files(pattern = "ReagentLog.json", recursive = TRUE, full.names = TRUE)

summaryData <- data.frame(carrierID = character(), data_type = character(),
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

for (i in 2: length(fi))
{
  JSONdata <- fromJSON(fi[i], nullValue = NA)
  
  if (length(JSONdata) == 18) {
    
    summData <- summaryData[0,]
    
    if ("Carrier ID" %in% names(JSONdata)) {
      carrID <- JSONdata$`Carrier ID`
      } else if ("Carrier Number" %in% names(JSONdata)){
        carrID <- JSONdata$`Carrier Number`
        } else {
          carrID <- NA
        }
  
    for (j in 1: length(JSONdata$`Run Details`)) {
      a = nrow(summData) + 1
      summData[a,] <- NA
      summData$data_type[a] = "Run Details"
      summData$rig_Name[a] = JSONdata$`Run Details`[[j]]$`Rig Name`
      summData$run_num[a] = JSONdata$`Run Details`[[j]]$`Run Num`
      summData$run_name[a] = JSONdata$`Run Details`[[j]]$`Run Name`
      summData$additional_notes[a] = JSONdata$`Run Details`[[j]]$`Run Completion Details`$`Additional Notes`
    }

    if (length(JSONdata$`Cycle Data`) > 0) {
      
      for (k in 1: length(JSONdata$`Cycle Data`)) {
        
        b = nrow(summData) + 1
        summData[b,] <- NA
        summData$data_type[b] = "Cycle Data"
        summData$index[b] = JSONdata$`Cycle Data`[[k]]$index
        summData$run_num[b] = JSONdata$`Cycle Data`[[k]]$run_num
        summData$label[b] = JSONdata$`Cycle Data`[[k]]$label
        summData$experiment_name[b] = JSONdata$`Cycle Data`[[k]]$experiment_name
        summData$script_name[b] = JSONdata$`Cycle Data`[[k]]$script_name
        summData$avg_final_wash_thru_dp[b] = JSONdata$`Cycle Data`[[k]]$cycle_metrics[[1]]
        
        if (length(JSONdata$`Run Details`) >= JSONdata$`Cycle Data`[[k]]$run_num) {
          summData$rig_Name[b] <- JSONdata$`Run Details`[[JSONdata$`Cycle Data`[[k]]$run_num]]$`Rig Name`
          summData$run_name[b] <- JSONdata$`Run Details`[[JSONdata$`Cycle Data`[[k]]$run_num]]$`Run Name`
          summData$additional_notes[b] <- JSONdata$`Run Details`[[JSONdata$`Cycle Data`[[k]]$run_num]]$`Run Completion Details`$`Additional Notes`
          } else {
            summData$rig_Name[b] <- 'DNF'
            summData$run_name[b] <- 'DNF'
            summData$additional_notes[b] <- 'DNF'
          }
        
        # if (length(JSONdata$`Run Details`) >= JSONdata$`Cycle Data`[[k]]$run_num) {
        #   summData$run_name[b] <- JSONdata$`Run Details`[[JSONdata$`Cycle Data`[[k]]$run_num]]$`Run Name`
        # } else {
        #   summData$run_name[b] <- 'DNF'
        # }
        # 
        # if (length(JSONdata$`Run Details`) >= JSONdata$`Cycle Data`[[k]]$run_num) {
        #   summData$additional_notes[b] <- JSONdata$`Run Details`[[JSONdata$`Cycle Data`[[k]]$run_num]]$`Run Completion Details`$`Additional Notes`
        # } else {
        #   summData$additional_notes[b] <- 'DNF'
        # }
      }
    }
    
    if (length(JSONdata$`Non-Detect Cycle Data`) > 0) {
      
      for (i in 1: length(JSONdata$`Non-Detect Cycle Data`)) {
        b = nrow(summData) + 1
        summData[b,] <- NA
        summData$data_type[b] = "Non-Detect Cycle Data"
        summData$index[b] = JSONdata$`Non-Detect Cycle Data`[[i]]$index
        summData$run_num[b] = JSONdata$`Non-Detect Cycle Data`[[i]]$run_num
        summData$label[b] = JSONdata$`Non-Detect Cycle Data`[[i]]$label
        summData$experiment_name[b] = JSONdata$`Non-Detect Cycle Data`[[i]]$experiment_name
        summData$script_name[b] = JSONdata$`Non-Detect Cycle Data`[[i]]$script_name
        summData$avg_final_wash_thru_dp[b] = JSONdata$`Non-Detect Cycle Data`[[i]]$cycle_metrics[[1]]
        
        if (length(JSONdata$`Run Details`) >= JSONdata$`Non-Detect Cycle Data`[[i]]$run_num) {
          summData$rig_Name[b] <- JSONdata$`Run Details`[[JSONdata$`Non-Detect Cycle Data`[[i]]$run_num]]$`Rig Name`
          summData$run_name[b] <- JSONdata$`Run Details`[[JSONdata$`Non-Detect Cycle Data`[[i]]$run_num]]$`Run Name`
          summData$additional_notes[b] <- JSONdata$`Run Details`[[JSONdata$`Non-Detect Cycle Data`[[i]]$run_num]]$`Run Completion Details`$`Additional Notes`
        } else {
          summData$rig_Name[b] <- 'DNF'
          summData$run_name[b] <- 'DNF'
          summData$additional_notes[b] <- 'DNF'
        }
        
        # if (length(JSONdata$`Run Details`) >= JSONdata$`Non-Detect Cycle Data`[[i]]$run_num) {
        #   summData$run_name[b] <- JSONdata$`Run Details`[[JSONdata$`Non-Detect Cycle Data`[[i]]$run_num]]$`Run Name`
        # } else {
        #   summData$run_name[b] <- 'DNF'
        # }
        # 
        # if (length(JSONdata$`Run Details`) >= JSONdata$`Non-Detect Cycle Data`[[i]]$run_num) {
        #   summData$additional_notes[b] <- JSONdata$`Run Details`[[JSONdata$`Non-Detect Cycle Data`[[i]]$run_num]]$`Run Completion Details`$`Additional Notes`
        # } else {
        #   summData$additional_notes[b] <- 'DNF'
        # }
      }
    }
    for (j in 1: length(JSONdata$`Lane Content`)) {
      summData[, 11+2*j] = JSONdata$`Lane Content`[[j]]$sample
      summData[, 12+2*j] = JSONdata$`Lane Content`[[j]]$concentration
    }
    
    summData$no_of_lanes = length(JSONdata$`Lane Content`)
    summData$carrierID = carrID
    summaryData <- bind_rows(summaryData, summData)
  }
}
fname <- paste0("SummaryData_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
write.csv(summaryData,fname, row.names = FALSE)
rm(list = ls(), envir = globalenv())
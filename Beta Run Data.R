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
    if ("Carrier ID" %in% names(JSONdata)) {
      carrID <- JSONdata$`Carrier ID`
      } else if ("Carrier Number" %in% names(JSONdata)){
        carrID <- JSONdata$`Carrier Number`
        } else {
          carrID <- NA
        }
    
    runDetailsData <- summaryData[0,]

    for (j in 1: length(JSONdata$`Run Details`)) {
      a = nrow(runDetailsData) + 1
      runDetailsData[a,] <- NA
      runDetailsData$carrierID[a] = carrID
      runDetailsData$data_type[a] = "Run Details"
      runDetailsData$rig_Name[a] = JSONdata$`Run Details`[[j]]$`Rig Name`
      runDetailsData$run_num[a] = JSONdata$`Run Details`[[j]]$`Run Num`
      runDetailsData$run_name[a] = JSONdata$`Run Details`[[j]]$`Run Name`
      runDetailsData$additional_notes[a] = JSONdata$`Run Details`[[j]]$`Run Completion Details`$`Additional Notes`
    }
    
    for (j in 1: length(JSONdata$`Lane Content`)) {
      runDetailsData[, 11+2*j] = JSONdata$`Lane Content`[[j]]$sample
      runDetailsData[, 12+2*j] = JSONdata$`Lane Content`[[j]]$concentration
    }
    
    runDetailsData$no_of_lanes = length(JSONdata$`Lane Content`)
    
    summaryData <- bind_rows(summaryData, runDetailsData)
    
    cycleData <- summaryData[0,]

    if (length(JSONdata$`Cycle Data`) > 0) {
      
      for (k in 1: length(JSONdata$`Cycle Data`)) {
        
        b = nrow(cycleData) + 1
        cycleData[b,] <- NA
        
        cycleData$carrierID[b] = carrID
        cycleData$data_type[b] = "Cycle Data"
        cycleData$index[b] = JSONdata$`Cycle Data`[[k]]$index
        cycleData$run_num[b] = JSONdata$`Cycle Data`[[k]]$run_num
        cycleData$label[b] = JSONdata$`Cycle Data`[[k]]$label
        cycleData$experiment_name[b] = JSONdata$`Cycle Data`[[k]]$experiment_name
        cycleData$script_name[b] = JSONdata$`Cycle Data`[[k]]$script_name
        cycleData$avg_final_wash_thru_dp[b] = JSONdata$`Cycle Data`[[k]]$cycle_metrics[[1]]
        
        if (length(JSONdata$`Run Details`) >= JSONdata$`Cycle Data`[[k]]$run_num) {
          cycleData$rig_Name[b] <- JSONdata$`Run Details`[[JSONdata$`Cycle Data`[[k]]$run_num]]$`Rig Name`
          } else {
            cycleData$rig_Name[b] <- 'DNF'
          }
        
        if (length(JSONdata$`Run Details`) >= JSONdata$`Cycle Data`[[k]]$run_num) {
          cycleData$run_name[b] <- JSONdata$`Run Details`[[JSONdata$`Cycle Data`[[k]]$run_num]]$`Run Name`
        } else {
          cycleData$run_name[b] <- 'DNF'
        }
        
        if (length(JSONdata$`Run Details`) >= JSONdata$`Cycle Data`[[k]]$run_num) {
          cycleData$additional_notes[b] <- JSONdata$`Run Details`[[JSONdata$`Cycle Data`[[k]]$run_num]]$`Run Completion Details`$`Additional Notes`
        } else {
          cycleData$additional_notes[b] <- 'DNF'
        }
      }
      
      for (j in 1: length(JSONdata$`Lane Content`)) {
        cycleData[, 11+2*j] = JSONdata$`Lane Content`[[j]]$sample
        cycleData[, 12+2*j] = JSONdata$`Lane Content`[[j]]$concentration
      }
      
      cycleData$no_of_lanes = length(JSONdata$`Lane Content`)
      
      summaryData <- bind_rows(summaryData, cycleData)
    }
    
    nonDetectCycleData <- summaryData[0,]
    
    if (length(JSONdata$`Non-Detect Cycle Data`) > 0) {
      
      for (i in 1: length(JSONdata$`Non-Detect Cycle Data`)) {
        b = nrow(nonDetectCycleData) + 1
        nonDetectCycleData[b,] <- NA
        nonDetectCycleData$carrierID[b] = carrID
        nonDetectCycleData$data_type[b] = "Non-Detect Cycle Data"
        nonDetectCycleData$index[b] = JSONdata$`Non-Detect Cycle Data`[[i]]$index
        nonDetectCycleData$run_num[b] = JSONdata$`Non-Detect Cycle Data`[[i]]$run_num
        nonDetectCycleData$label[b] = JSONdata$`Non-Detect Cycle Data`[[i]]$label
        nonDetectCycleData$experiment_name[b] = JSONdata$`Non-Detect Cycle Data`[[i]]$experiment_name
        nonDetectCycleData$script_name[b] = JSONdata$`Non-Detect Cycle Data`[[i]]$script_name
        nonDetectCycleData$avg_final_wash_thru_dp[b] = JSONdata$`Non-Detect Cycle Data`[[i]]$cycle_metrics[[1]]
        
        if (length(JSONdata$`Run Details`) >= JSONdata$`Non-Detect Cycle Data`[[i]]$run_num) {
          nonDetectCycleData$rig_Name[b] <- JSONdata$`Run Details`[[JSONdata$`Non-Detect Cycle Data`[[i]]$run_num]]$`Rig Name`
        } else {
          nonDetectCycleData$rig_Name[b] <- 'DNF'
        }
        
        if (length(JSONdata$`Run Details`) >= JSONdata$`Non-Detect Cycle Data`[[i]]$run_num) {
          nonDetectCycleData$run_name[b] <- JSONdata$`Run Details`[[JSONdata$`Non-Detect Cycle Data`[[i]]$run_num]]$`Run Name`
        } else {
          nonDetectCycleData$run_name[b] <- 'DNF'
        }
        
        if (length(JSONdata$`Run Details`) >= JSONdata$`Non-Detect Cycle Data`[[i]]$run_num) {
          nonDetectCycleData$additional_notes[b] <- JSONdata$`Run Details`[[JSONdata$`Non-Detect Cycle Data`[[i]]$run_num]]$`Run Completion Details`$`Additional Notes`
        } else {
          nonDetectCycleData$additional_notes[b] <- 'DNF'
        }
      }
      for (j in 1: length(JSONdata$`Lane Content`)) {
        nonDetectCycleData[, 11+2*j] = JSONdata$`Lane Content`[[j]]$sample
        nonDetectCycleData[, 12+2*j] = JSONdata$`Lane Content`[[j]]$concentration
      }
      nonDetectCycleData$no_of_lanes = length(JSONdata$`Lane Content`)
      
      summaryData <- bind_rows(summaryData, nonDetectCycleData)
    }
  }
}
fname <- paste0("SummaryData_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
write.csv(summaryData,fname, row.names = FALSE)

# df <- data.frame(sample_1 = character(), concentration_1 = double(), sample_2 = character(), concentration_2 = double(),
#                  sample_3 = character(), concentration_3 = double(), sample_4 = character(), concentration_4 = double(),
#                  sample_5 = character(), concentration_5 = double(), sample_6 = character(), concentration_6 = double(),
#                  sample_7 = character(), concentration_7 = double(), sample_8 = character(), concentration_8 = double(),
#                  sample_9 = character(), concentration_9 = double(), sample_10 = character(), concentration_10 = double(),
#                  sample_11 = character(), concentration_11 = double(), sample_12 = character(), concentration_12 = double(),
#                  sample_13 = character(), concentration_13 = double(), sample_14 = character(), concentration_14 = double(),
#                  sample_15 = character(), concentration_15 = double(), sample_16 = character(), concentration_16 = double(),
#                  sample_17 = character(), concentration_17 = double(), sample_18 = character(), concentration_18 = double(),
#                  sample_19 = character(), concentration_19 = double(), sample_20 = character(), concentration_20 = double())
# 
# for (i in 2: length(fi)) {
#   JSONdata1 <- fromJSON(fi[i], nullValue = NA)
#   a = nrow(df)
#   for (i in 1: length(JSONdata1$`Lane Content`)) {
#     df[a+1, 2*i-1] = JSONdata1$`Lane Content`[[i]]$sample
#     df[a+1, 2*i] = JSONdata1$`Lane Content`[[i]]$concentration
#   }
# }
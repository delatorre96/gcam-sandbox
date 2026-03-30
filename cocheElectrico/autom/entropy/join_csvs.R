csv_paths <- "C:/GCAM/Nacho/gcam_8.2/exe/DB/"

costs_list <- list()
outputs_list <- list()
shareWheights_list <- list()

for (file in list.files(csv_paths, full.names = TRUE)) {
  
  if (grepl("COST", file)) {
    costs_list[[length(costs_list) + 1]] <- read.csv(file, skip = 1)
  }
  
  if (grepl("OUTPUT", file)) {
    outputs_list[[length(outputs_list) + 1]] <- read.csv(file, skip = 1)
  }
  
  if (grepl("SW", file)) {
    shareWheights_list[[length(shareWheights_list) + 1]] <- read.csv(file, skip = 1)
  }
}

costs_df <- do.call(rbind, costs_list)
outputs_df <- do.call(rbind, outputs_list)
shareWheights_df <- do.call(rbind, shareWheights_list)

costs_df <- costs_df[
  costs_df$technology %in% c('MORDOR', 'BEV') &
    costs_df$sector == 'trn_pass_road_LDV_4W' &
    costs_df$subsector == 'Car', 
]
rownames(costs_df) <- NULL
outputs_df <- outputs_df[
  outputs_df$technology %in% c('MORDOR', 'BEV') &
    outputs_df$sector == 'trn_pass_road_LDV_4W' &
    outputs_df$subsector == 'Car', 
]
rownames(outputs_df) <- NULL
shareWheights_df <- shareWheights_df[
  shareWheights_df$technology %in% c('MORDOR', 'BEV') &
    shareWheights_df$sector == 'trn_pass_road_LDV_4W' &
    shareWheights_df$subsector == 'Car', 
]
rownames(shareWheights_df) <- NULL

write.csv(costs_df, paste0(thisScript_path,"/csvs/costs0.csv"), row.names = FALSE)
write.csv(outputs_df, paste0(thisScript_path,"/csvs/outputs0.csv"), row.names = FALSE)
write.csv(shareWheights_df, paste0(thisScript_path,"/csvs/shareWheights0.csv"), row.names = FALSE)

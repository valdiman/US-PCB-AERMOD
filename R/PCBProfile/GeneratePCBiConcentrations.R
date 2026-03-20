# Code to estimate individual PCB congener concentrations
# from total PCB for Anacotia, Kalamazoo & Housatonic rivers

# Read Aroclor profile ----------------------------------------------------
ar <- read.csv("Data/AroclorPCBProfileGrouped.csv", stringsAsFactors = FALSE)

# Normalize the PCB profiles to 1
# Replace NA with 0
ar$A1016[is.na(ar$A1016)] <- 0
ar$A1248[is.na(ar$A1248)] <- 0
ar$A1260[is.na(ar$A1260)] <- 0
# Convert percent to fraction
ar$A1016 <- ar$A1016 / 100
ar$A1248 <- ar$A1248 / 100
ar$A1260 <- ar$A1260 / 100
# Normalize to sum 1
ar$A1016 <- ar$A1016 / sum(ar$A1016)
ar$A1248 <- ar$A1248 / sum(ar$A1248)
ar$A1260 <- ar$A1260 / sum(ar$A1260)

# Read Anacostia River ----------------------------------------------------
anr <- read.csv("Data/AnacostiaRiver/AnacostiaRiverMeteoWaterTempConcV0.csv",
                stringsAsFactors = FALSE)

# Aroclor 1248
anr_matrix <- outer(anr$tPCB, ar$A1248)

# Convert to dataframe and name columns
anr_df <- as.data.frame(anr_matrix)
colnames(anr_df) <- ar$Congener

# Bind ALL metadata from anr
final_anr <- cbind(anr, anr_df)

# save
write.csv(final_anr, "Data/AnacostiaRiver/AnacostiaRiverMeteoWaterTempConcVF.csv",
          row.names = FALSE)

# Read Kalamazoo River ----------------------------------------------------
kar <- read.csv("Data/Kalamazoo/KalamazooRiverMeteoWaterTempFlowConV0.csv",
                stringsAsFactors = FALSE)

# Aroclor 1016 
kar_matrix <- outer(kar$tPCB, ar$A1016)

# Convert to dataframe and name columns
kar_df <- as.data.frame(kar_matrix)
colnames(kar_df) <- ar$Congener

# Bind ALL metadata from kar
final_kar <- cbind(kar, kar_df)

# save
write.csv(final_kar, "Data/Kalamazoo/KalamazooRiverMeteoWaterTempFlowConVF.csv",
          row.names = FALSE)

# Read Housatonic River ---------------------------------------------------
hor <- read.csv("Data/HousatonicRiver/housatonicRiverMeteoWaterTempFlowConcV0.csv",
                stringsAsFactors = FALSE)

# Aroclor 1260
hor_matrix <- outer(hor$tPCB, ar$A1260)

# Convert to dataframe and name columns
hor_df <- as.data.frame(hor_matrix)
colnames(hor_df) <- ar$Congener

# Bind ALL metadata from hor
final_hor <- cbind(hor, hor_df)

# save
write.csv(final_hor, "Data/housatonicRiver/housatonicRiverMeteoWaterTempFlowConcVF.csv",
          row.names = FALSE)


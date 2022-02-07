# Code that can be used to test whether the meta-analysis functionality works
library(SkeletonComparativeEffectStudy)
exportFolder <- "D:/SkeletonComparativeEffectStudy/export"

# Create a dummy additional database by copying all CSV files and changing the database ID --------------------------------
databaseId <- "Dummy"
newExportFolder <- file.path(exportFolder, databaseId)
dir.create(newExportFolder)

files <- list.files(exportFolder, pattern = "*.csv")
for (file in files) {
  message(sprintf("Copying %s", file))
  data <- readr::read_csv(file.path(exportFolder, file), col_types = readr::cols())
  if ("database_id" %in% colnames(data)) {
    data <- data %>%
      mutate(database_id = databaseId)
  }
  readr::write_csv(data, file.path(newExportFolder, file)) 
}

message("Adding results to zip file")
zipName <- file.path(newExportFolder, sprintf("Results_%s.zip", databaseId))
files <- list.files(newExportFolder, pattern = ".*\\.csv$")
oldWd <- setwd(newExportFolder)
DatabaseConnector::createZipFile(zipFile = zipName, files = files)
message("Results are ready for sharing at:", zipName)
setwd(oldWd)

file.copy(from = zipName, to = exportFolder)

# Run meta-analysis -----------------------------------------
synthesizeResults(exportFolder)

# View results in Shiny app ----------------------------------
resultsZipFiles <- list.files(exportFolder, "*.zip")
dataFolder <- file.path(exportFolder, "shinyData")
for (resultsZipFile in resultsZipFiles) {
  message(sprintf("Preparing %s", resultsZipFile))
  prepareForEvidenceExplorer(resultsZipFile = file.path(exportFolder, resultsZipFile), dataFolder = dataFolder)
}
launchEvidenceExplorer(dataFolder = dataFolder, blind = F, launch.browser = FALSE)

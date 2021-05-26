# This file contains code to be used by the study coordinator to download result files from the SFTP server and perform a meta-analysis.
library(SkeletonComparativeEffectStudy)
library(OhdsiSharing)

# Folder on the local file system where the result zip files will be downloaded to from the SFTP server:
allDbsFolder <- "s:/SkeletonComparativeEffectStudy/AllResults"
if (!file.exists(allDbsFolder))
  dir.create(allDbsFolder, showWarnings = FALSE, recursive = TRUE)

# Maximum number of cores to be used:
maxCores <- parallel::detectCores()

# Create a connection to the SFTP server:
connection <- sftpConnect(privateKeyFileName = "c:/path/to/keyfile",
                          userName = "theSftpUserName")

# Change directories (i.e., cd) to folder on SFTP server where results are saved:
sftpCd(connection, "theRemoteSftpFolder")

# List the files in the current directory on the SFTP server:
files <- sftpLs(connection)

# Download all files from the current directory on the SFTP server:
sftpGetFiles(connection, files$fileName, localFolder = allDbsFolder)

# !!! DANGER !!! (Optional) Remove files from the SFTP server
# sftpRm(connection, files$fileName)

# Disconnect from SFTP server:
sftpDisconnect(connection)

# Synthesize results across databases, creating a new meta-analysis result set in allDbsFolder:
synthesizeResults(allDbsFolder = allDbsFolder, maxCores = maxCores)

# Export results for evidence exploration:
dataFolder <- file.path(allDbsFolder, "shinyData")
for(resultsZipFile in list.files(allDbsFolder, pattern = "Results_.*\\.zip", full.names = TRUE)) {
  prepareForEvidenceExplorer(resultsZipFile = resultsZipFile, dataFolder = dataFolder)
}

# Launch evidence explorer:
launchEvidenceExplorer(dataFolder = dataFolder, blind = TRUE, launch.browser = FALSE)


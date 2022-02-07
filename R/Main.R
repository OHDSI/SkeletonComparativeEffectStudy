# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of SkeletonComparativeEffectStudy
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Execute the Study
#'
#' @details
#' This function executes the SkeletonComparativeEffectStudy Study.
#' 
#' The \code{createCohorts}, \code{synthesizePositiveControls}, \code{runAnalyses}, and \code{runDiagnostics} arguments
#' are intended to be used to run parts of the full study at a time, but none of the parts are considered to be optional.
#'
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortTable                  Name of the cohort table.
#' @param cohortInclusionTable         Name of the inclusion table, one of the tables for storing
#'                                     inclusion rule statistics.
#' @param cohortInclusionResultTable   Name of the inclusion result table, one of the tables for
#'                                     storing inclusion rule statistics.
#' @param cohortInclusionStatsTable    Name of the inclusion stats table, one of the tables for storing
#'                                     inclusion rule statistics.
#' @param cohortSummaryStatsTable      Name of the summary stats table, one of the tables for storing
#'                                     inclusion rule statistics.
#' @param cohortCensorStatsTable       Name of the censor stats table, one of the tables for storing
#'                                     inclusion rule statistics.
#' @param oracleTempSchema    DEPRECATED: use `tempEmulationSchema` instead.
#' @param tempEmulationSchema Some database platforms like Oracle and Impala do not truly support temp tables. To
#'                            emulate temp tables, provide a schema with write privileges where temp tables
#'                            can be created.
#' @param verifyDependencies   Check whether correct package versions are installed?
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param databaseId           A short string for identifying the database (e.g.
#'                             'Synpuf').
#' @param databaseName         The full name of the database (e.g. 'Medicare Claims
#'                             Synthetic Public Use Files (SynPUFs)').
#' @param databaseDescription  A short description (several sentences) of the database.
#' @param createCohorts        Create the cohortTable table with the exposure and outcome cohorts?
#' @param synthesizePositiveControls  Should positive controls be synthesized?
#' @param runAnalyses          Perform the cohort method analyses?
#' @param packageResults       Should results be packaged for later sharing?     
#' @param maxCores             How many parallel cores should be used? If more cores are made available
#'                             this can speed up the analyses.
#' @param minCellCount         The minimum number of subjects contributing to a count before it can be included 
#'                             in packaged results.
#'
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(dbms = "postgresql",
#'                                              user = "joe",
#'                                              password = "secret",
#'                                              server = "myserver")
#'
#' execute(connectionDetails,
#'         cdmDatabaseSchema = "cdm_data",
#'         cohortDatabaseSchema = "study_results",
#'         cohortTable = "cohort",
#'         oracleTempSchema = NULL,
#'         outputFolder = "c:/temp/study_results",
#'         maxCores = 4)
#' }
#'
#' @export
execute <- function(connectionDetails,
                    cdmDatabaseSchema,
                    cohortDatabaseSchema = cdmDatabaseSchema,
                    cohortTable = "cohort",
                    cohortInclusionTable = paste0(cohortTable, "_inclusion"),
                    cohortInclusionResultTable = paste0(cohortTable, "_inclusion_result"),
                    cohortInclusionStatsTable = paste0(cohortTable, "_inclusion_stats"),
                    cohortSummaryStatsTable = paste0(cohortTable, "_summary_stats"),
                    cohortCensorStatsTable = paste0(cohortTable, "_censor_stats"),
                    oracleTempSchema = NULL,
                    tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                    verifyDependencies = TRUE,
                    outputFolder,
                    databaseId = "Unknown",
                    databaseName = "Unknown",
                    databaseDescription = "Unknown",
                    createCohorts = TRUE,
                    synthesizePositiveControls = TRUE,
                    runAnalyses = TRUE,
                    packageResults = TRUE,
                    maxCores = 4,
                    minCellCount = 5) {
  outputFolder <- normalizePath(outputFolder, mustWork = FALSE)
  if (!file.exists(outputFolder)) {
    dir.create(outputFolder, recursive = TRUE)
  }

  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  ParallelLogger::addDefaultErrorReportLogger(file.path(outputFolder, "errorReportR.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE), add = TRUE)
  
  if (!is.null(oracleTempSchema) && oracleTempSchema != "") {
    warning("The 'oracleTempSchema' argument is deprecated. Use 'tempEmulationSchema' instead.")
    tempEmulationSchema <- oracleTempSchema
  }
  if (connectionDetails$dbms %in% c("oracle", "bigquery", "impala", "spark") && is.null(tempEmulationSchema)) {
    stop(sprintf("DBMS '%s' requires 'tempEmulationSchema' to be set.", connectionDetails$dbms))
  }
  if (!is.null(getOption("andromedaTempFolder")) && !file.exists(getOption("andromedaTempFolder"))) {
    warning("andromedaTempFolder '", getOption("andromedaTempFolder"), "' not found. Attempting to create folder")
    dir.create(getOption("andromedaTempFolder"), recursive = TRUE)
  }
  
  if (verifyDependencies) {
    message("Checking whether correct package versions are installed")
    verifyDependencies()
  }
  
  if (createCohorts) {
    message("Creating exposure and outcome cohorts")
    createCohorts(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTableNames = list(cohortTable = cohortTable,
                                          cohortInclusionTable = cohortInclusionTable,
                                          cohortInclusionResultTable = cohortInclusionResultTable,
                                          cohortInclusionStatsTable = cohortInclusionStatsTable,
                                          cohortSummaryStatsTable = cohortSummaryStatsTable,
                                          cohortCensorStatsTable = cohortCensorStatsTable),
                  tempEmulationSchema = tempEmulationSchema,
                  outputFolder = outputFolder)
  }
  
  # Set doPositiveControlSynthesis to FALSE if you don't want to use synthetic positive controls:
  # Start doPositiveControlSynthesis
  doPositiveControlSynthesis <- TRUE
  # End doPositiveControlSynthesis
  if (doPositiveControlSynthesis) {
    if (synthesizePositiveControls) {
      message("Synthesizing positive controls")
      synthesizePositiveControls(connectionDetails = connectionDetails,
                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                 cohortTable = cohortTable,
                                 tempEmulationSchema = tempEmulationSchema,
                                 outputFolder = outputFolder,
                                 maxCores = maxCores)
    }
  }
  
  if (runAnalyses) {
    message("Running CohortMethod analyses")
    runCohortMethod(connectionDetails = connectionDetails,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    cohortDatabaseSchema = cohortDatabaseSchema,
                    cohortTable = cohortTable,
                    tempEmulationSchema = tempEmulationSchema,
                    outputFolder = outputFolder,
                    maxCores = maxCores)
  }
  
  if (packageResults) {
    message("Packaging results")
    exportResults(outputFolder = outputFolder,
                  databaseId = databaseId,
                  databaseName = databaseName,
                  databaseDescription = databaseDescription,
                  connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  minCellCount = minCellCount,
                  maxCores = maxCores)
  }
  
  invisible(NULL)
}

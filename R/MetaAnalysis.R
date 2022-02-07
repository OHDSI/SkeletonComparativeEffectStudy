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


#' Conducts a meta-analysis across PLE result sets
#' 
#' @details 
#' Conducts a meta-analysis across result sets generated from a population level
#' effect (PLE) study package.  Meta-analysis methodology is based on 
#' DerSimonian and Laird (1986) or Schuemie et al. (2021).
#' 
#'
#' @param allDbsFolder    Folder on the local file system containing the individual zip files across databases (i.e., sites)
#' @param maExportFolder  A local folder where the meta-analysis results will be written. If not specified, results will be
#'                        written to same directory with all other results.
#' @param maxCores        Maximum number of CPU cores to be used when computing the meta-analyses.
#' @param method          The meta-analysis method to use.  Possible values are "BayesianNonNormal" (Schumie et al.) or "DL" (DerSimonian-Laird).
#' @param resultsZipPattern  The pattern of the names of the zip files containing the exported results of each database.
#' @param addTraditional  Boolean indicating if traditional meta-analysis (i.e., "DL") results should be added to result (if \code{method} is "BayesianNonNormal").
#' 
#' 
#' @references
#' DerSimonian R, Laird N. Meta-analysis in clinical trials.
#' Control Clin Trials. 1986 Sep;7(3):177-88. doi: 10.1016/0197-2456(86)90046-2
#' 
#' Schuemie M, Chen Y, Madigan D, Suchard M, Combining Cox Regressions Across a
#' Heterogeneous Distributed Research Network Facing Small and Zero Counts. arXiv: 2101.01551, 2021
#' 
#'
#' @return
#' Does not return a value, but creates a new zip file in the \code{maExportFolder} for the meta-analyses.
#'
#' @export
synthesizeResults <- function(allDbsFolder,
                              maExportFolder = allDbsFolder,
                              maxCores = 1,
                              method = "BayesianNonNormal",
                              resultsZipPattern = "^Results_.*\\.zip",
                              addTraditional = TRUE) {
  allDbsFolder <- normalizePath(allDbsFolder)
  maExportFolder <- normalizePath(maExportFolder, mustWork = FALSE)
  if (!file.exists(maExportFolder)) {
    dir.create(maExportFolder, recursive = TRUE)
  }
  
  ParallelLogger::addDefaultFileLogger(file.path(maExportFolder, "metaAnalysisLog.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
  
  if (!file.exists(allDbsFolder)) {
    stop(sprintf("The allDbsFolder path does not exist:\n\t", allDbsFolder))
  }
  
  if (!(method %in% c("BayesianNonNormal", "DL"))) {
    stop("Accepted method arguments are \"BayesianNonNormal\" or \"DL\"")
  }
  
  resultSets <- list.files(path = allDbsFolder, pattern = resultsZipPattern)
  message(sprintf("Found %d zip files matching pattern %s for synthesizing", length(resultSets), resultsZipPattern))
  if (length(resultSets) == 0) {
    stop(sprintf("No results found matching pattern %s in directory %s", resultsZipPattern, allDbsFolder))
  } else if (length(resultSets) == 1) {
    stop(sprintf("Only single result set found matching pattern %s in directory %s", resultsZipPattern, allDbsFolder))
  }
  
  mainResults <- lapply(resultSets, loadDatabaseResults, allDbsFolder = allDbsFolder)
  mainResults <- do.call(rbind, mainResults)
  mainResults <- split(mainResults, paste(mainResults$targetId, mainResults$comparatorId, mainResults$analysisId))
  
  if (method == "BayesianNonNormal") {
    profiles <- lapply(resultSets, loadLikelihoodProfiles, allDbsFolder = allDbsFolder)
    profiles <- do.call(rbind, profiles)
    profiles <- split(profiles, paste(profiles$targetId, profiles$comparatorId, profiles$analysisId))
    
    groups <- lapply(names(mainResults), function(name) list(mainResults = mainResults[[name]], profiles = profiles[[name]]))
    
    rm(mainResults)
    rm(profiles)
    
  } else {
    groups <- lapply(names(mainResults), function(name) list(mainResults = mainResults[[name]]))
  }
  
  message("Performing cross-database evidence synthesis")
  cluster <- ParallelLogger::makeCluster(min(maxCores, 10))
  results <- ParallelLogger::clusterApply(cluster, groups, computeGroupMetaAnalysis, method, addTraditional)
  ParallelLogger::stopCluster(cluster)
  
  results <- do.call(rbind, results)
  results$trueEffectSize <- NULL
  
  tempFolder <- tempfile()
  dir.create(tempFolder, recursive = TRUE)
  on.exit(unlink(tempFolder, recursive = TRUE, force = TRUE))
  
  colnames(results) <- SqlRender::camelCaseToSnakeCase(colnames(results))
  fileName <-  file.path(tempFolder, "cohort_method_result.csv")
  write.csv(results, fileName, row.names = FALSE)
  
  message("Creating database table")
  database <- data.frame(databaseId = "Meta-analysis",
                         databaseName = "Random effects meta-analysis",
                         description = getDescriptionFromMethod(method),
                         isMetaAnalysis = 1)
  
  colnames(database) <- SqlRender::camelCaseToSnakeCase(colnames(database))
  
  fileName <- file.path(tempFolder, "database.csv")
  write.csv(database, fileName, row.names = FALSE)

  
  message("Adding results to zip file")
  zipName <- file.path(maExportFolder, sprintf("Results_%s.zip", "MetaAnalysis"))
  files <- list.files(tempFolder, pattern = ".*\\.csv$")
  pwd <- setwd(tempFolder)
  on.exit(setwd(pwd))
  DatabaseConnector::createZipFile(zipFile = zipName, files = files)
  message("Results are ready for sharing at:", zipName)
}

loadDatabaseResults <- function(zipFile, allDbsFolder) {
  message("Loading results from ", zipFile, " for evidence synthesis")
  
  tempFolder <- tempfile()
  dir.create(tempFolder)
  on.exit(unlink(tempFolder, recursive = TRUE, force = TRUE))
  
  suppressWarnings(utils::unzip(zipfile = file.path(allDbsFolder, zipFile),
               files = c("cohort_method_result.csv",
                         "negative_control_outcome.csv",
                         "positive_control_outcome.csv"),
               exdir = tempFolder,
               overwrite = TRUE))
  
  cohortMethodResultsFile <- file.path(tempFolder, "cohort_method_result.csv")
  
  if (!file.exists(cohortMethodResultsFile)) {
    stop(sprintf("No cohort method result found for result set %s", zipFile))
  }
  
  results <- readr::read_csv(cohortMethodResultsFile, col_types = readr::cols(), guess_max = 1e5)
  colnames(results) <- SqlRender::snakeCaseToCamelCase(colnames(results))
  
  
  negativeControlsFile <- file.path(tempFolder, "negative_control_outcome.csv")
  if (!file.exists(negativeControlsFile)) {
    warning(sprintf("No negative controls found for result set ", zipFile))
  } else {
    ncs <- readr::read_csv(negativeControlsFile, col_types = readr::cols(), guess_max = 1e5)
    colnames(ncs) <- SqlRender::snakeCaseToCamelCase(colnames(ncs))
    
    results$trueEffectSize <- NA
    idx <- results$outcomeId %in% ncs$outcomeId
    results$trueEffectSize[idx] <- 1
  }
  
  positiveControlsFile <- file.path(tempFolder, "positive_control_outcome.csv")
  if (file.exists(positiveControlsFile)) {
    pcs <- readr::read_csv(positiveControlsFile, col_types = readr::cols(), guess_max = 1e5)
    colnames(pcs) <- SqlRender::snakeCaseToCamelCase(colnames(pcs))
    idx <- results$outcomeId %in% pcs$outcomeId
    results$trueEffectSize[idx] <- pcs$effectSize[match(results$outcomeId[idx],
                                                        pcs$outcomeId)]
  }
  
  return(results)
}

loadLikelihoodProfiles <- function(zipFile, allDbsFolder) {
  message("Loading likelihood profiles from ", zipFile, " for evidence synthesis")
  tempFolder <- tempfile()
  dir.create(tempFolder)
  on.exit(unlink(tempFolder, recursive = TRUE, force = TRUE))
  
  suppressWarnings(utils::unzip(zipfile = file.path(allDbsFolder, zipFile),
               files = c("likelihood_profile.csv"),
               exdir = tempFolder))
  likelihoodFile <- file.path(tempFolder, "likelihood_profile.csv")
  if (!file.exists(likelihoodFile)) {
    stop(sprintf("Results zip %s does not contain likelihood_profile.csv required for meta-analysis method", zipFile))
  }
  profiles <- readr::read_csv(likelihoodFile, col_types = readr::cols(), guess_max = 1e3)
  colnames(profiles) <- SqlRender::snakeCaseToCamelCase(colnames(profiles))
  return(profiles)
}

loadDatabase <- function(zipFile, allDbsFolder) {
  message("Loading database information from ", zipFile)
  tempFolder <- tempfile()
  dir.create(tempFolder)
  on.exit(unlink(tempFolder, recursive = TRUE, force = TRUE))
  
  suppressWarnings(utils::unzip(zipfile = file.path(allDbsFolder, zipFile),
               files = c("database.csv"),
               exdir = tempFolder))
  
  dbFile <- file.path(tempFolder, "database.csv")
  
  if (!file.exists(dbFile)) {
    warning(sprintf("Results zip %s does not contain database.csv", zipFile))
    return(NULL)
  }
  database <- readr::read_csv(dbFile, col_types = readr::cols(), guess_max = 1e3)
  colnames(database) <- SqlRender::snakeCaseToCamelCase(colnames(database))
  return(database)
}

computeGroupMetaAnalysis <- function(group, method, addTraditional) {
  mainResults <- group$mainResults
  
  if (nrow(mainResults) == 0) {
    return(NULL)
  }
  
  analysisId <- mainResults$analysisId[1]
  targetId <- mainResults$targetId[1]
  comparatorId <- mainResults$comparatorId[1]
  
  message("Performing meta-analysis for target ", targetId, ", comparator ", comparatorId, ", analysis ", analysisId)
  
  outcomeIds <- unique(mainResults$outcomeId)
  outcomeGroupResults <- lapply(outcomeIds, computeSingleMetaAnalysis, group, method, addTraditional)
  groupResults <- do.call(rbind, outcomeGroupResults)
  
  
  ncs <- groupResults[groupResults$trueEffectSize == 1, ]
  validNcs <- ncs[!is.na(ncs$seLogRr), ]
  
  pcs <- groupResults[!is.na(groupResults$trueEffectSize) &
                        groupResults$trueEffectSize != 1, ]
  validPcs <- pcs[!is.na(pcs$seLogRr), ]
  
  
  if (nrow(validPcs) >= 5) {
    model <- EmpiricalCalibration::fitSystematicErrorModel(logRr = c(validNcs$logRr, validPcs$logRr),
                                                           seLogRr = c(validNcs$seLogRr,
                                                                       validPcs$seLogRr),
                                                           trueLogRr = c(rep(0, nrow(validNcs)),
                                                                         log(validPcs$trueEffectSize)),
                                                           estimateCovarianceMatrix = FALSE)
    calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(logRr = groupResults$logRr,
                                                                      seLogRr = groupResults$seLogRr,
                                                                      model = model)
    groupResults$calibratedRr <- exp(calibratedCi$logRr)
    groupResults$calibratedCi95Lb <- exp(calibratedCi$logLb95Rr)
    groupResults$calibratedCi95Ub <- exp(calibratedCi$logUb95Rr)
    groupResults$calibratedLogRr <- calibratedCi$logRr
    groupResults$calibratedSeLogRr <- calibratedCi$seLogRr
  } else {
    groupResults$calibratedRr <- rep(NA, nrow(groupResults))
    groupResults$calibratedCi95Lb <- rep(NA, nrow(groupResults))
    groupResults$calibratedCi95Ub <- rep(NA, nrow(groupResults))
    groupResults$calibratedLogRr <- rep(NA, nrow(groupResults))
    groupResults$calibratedSeLogRr <- rep(NA, nrow(groupResults))
  }
  
  
  if (nrow(validNcs) >= 5) {
    null <- EmpiricalCalibration::fitMcmcNull(validNcs$logRr, validNcs$seLogRr)
    calibratedP <- EmpiricalCalibration::calibrateP(null = null,
                                                    logRr = groupResults$logRr,
                                                    seLogRr = groupResults$seLogRr)
    groupResults$calibratedP <- calibratedP$p
    
    if (nrow(validPcs) < 5) {
      model <- EmpiricalCalibration::convertNullToErrorModel(null)
      calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(logRr = groupResults$logRr,
                                                                        seLogRr = groupResults$seLogRr,
                                                                        model = model)
      groupResults$calibratedRr <- exp(calibratedCi$logRr)
      groupResults$calibratedCi95Lb <- exp(calibratedCi$logLb95Rr)
      groupResults$calibratedCi95Ub <- exp(calibratedCi$logUb95Rr)
      groupResults$calibratedLogRr <- calibratedCi$logRr
      groupResults$calibratedSeLogRr <- calibratedCi$seLogRr
    }
  } else {
    groupResults$calibratedP <- NA
  }

  return(groupResults)
}

sumMinCellCount <- function(counts) {
  total <- sum(abs(counts))
  if (any(counts < 0)) {
    total <- -total
  }
  return(total)
}

computeSingleMetaAnalysis <- function(outcomeId, group, method, addTraditional) {
  
  rows <- group$mainResults[group$mainResults$outcomeId == outcomeId, ]
  
  maRow <- rows[1, ]
  maRow$databaseId <- "Meta-analysis"
  
  if (method == "DL") {
    rows <- rows[!is.na(rows$seLogRr), ]
    if (nrow(rows) == 0) {
      maRow$targetSubjects <- 0
      maRow$comparatorSubjects <- 0
      maRow$targetDays <- 0
      maRow$comparatorDays <- 0
      maRow$targetOutcomes <- 0
      maRow$comparatorOutcomes <- 0
      maRow$rr <- NA
      maRow$ci95Lb <- NA
      maRow$ci95Ub <- NA
      maRow$p <- NA
      maRow$logRr <- NA
      maRow$seLogRr <- NA
      maRow$i2 <- NA
      return(maRow)
    } else if (nrow(rows) == 1) {
      maRow <- rows[1, ]
      maRow$i2 <- 0
      return(maRow)
    } else {
      maRow$targetSubjects <- sumMinCellCount(rows$targetSubjects)
      maRow$comparatorSubjects <- sumMinCellCount(rows$comparatorSubjects)
      maRow$targetDays <- sum(rows$targetDays)
      maRow$comparatorDays <- sum(rows$comparatorDays)
      maRow$targetOutcomes <- sumMinCellCount(rows$targetOutcomes)
      maRow$comparatorOutcomes <- sumMinCellCount(rows$comparatorOutcomes)
      meta <- meta::metagen(TE = rows$logRr,
                            seTE = rows$seLogRr,
                            sm = "RR",
                            hakn = FALSE)
      s <- summary(meta)
      maRow$i2 <- s$I2$TE
      rnd <- s$random
      maRow$rr <- exp(rnd$TE)
      maRow$ci95Lb <- exp(rnd$lower)
      maRow$ci95Ub <- exp(rnd$upper)
      maRow$p <- rnd$p
      maRow$logRr <- rnd$TE
      maRow$seLogRr <- rnd$seTE
    }
  } else if (method == "BayesianNonNormal") {
    
    profileDbs <- if (is.null(group$profiles)) c() else group$profiles$databaseId[group$profiles$outcomeId == outcomeId]
    
    maRow$targetSubjects <- sumMinCellCount(rows$targetSubjects)
    maRow$comparatorSubjects <- sumMinCellCount(rows$comparatorSubjects)
    maRow$targetDays <- sum(rows$targetDays)
    maRow$comparatorDays <- sum(rows$comparatorDays)
    maRow$targetOutcomes <- sumMinCellCount(rows$targetOutcomes)
    maRow$comparatorOutcomes <- sumMinCellCount(rows$comparatorOutcomes)
    maRow$i2 <- NA
    
    if (length(profileDbs) <= 1) {
      if (length(profileDbs) == 1) {
        idx <- (rows$databaseId == profileDbs)
      } else {
        idx <- 1
      }
      maRow$rr <- rows$rr[idx]
      maRow$ci95Lb <- rows$ci95Lb[idx]
      maRow$ci95Ub <- rows$ci95Ub[idx]
      maRow$p <- rows$p[idx]
      maRow$logRr <- rows$logRr[idx]
      maRow$seLogRr <- rows$seLogRr[idx]
      maRow$tau <- 0
      maRow$traditionalLogRr <- rows$logRr[idx]
      maRow$traditionalSeLogRr <- rows$seLogRr[idx]
    } else {
      profiles <- group$profiles$profile[group$profiles$outcomeId == outcomeId]
      profiles <- strsplit(profiles, ";")
      profiles <- as.data.frame(t(sapply(profiles, as.numeric)))
      colnames(profiles) <- seq(log(0.1), log(10), length.out = 1000)
      
      estimate <- EvidenceSynthesis::computeBayesianMetaAnalysis(profiles)

      #  columns missing \Leftrightarrow all NA?
      if (!("logRr" %in% colnames(estimate)) || !("seLogRr" %in% colnames(estimate))) {
        
        maRow$rr <- NA
        maRow$ci95Lb <- NA
        maRow$ci95Ub <- NA
        
        maRow$p <- NA
        maRow$logRr <- NA
        maRow$seLogRr <- NA
        maRow$tau <- NA
        
      } else {
        
        maRow$rr <- exp(estimate$mu)
        maRow$ci95Lb <- exp(estimate$mu95Lb)
        maRow$ci95Ub <- exp(estimate$mu95Ub)
        
        maRow$p <- EmpiricalCalibration::computeTraditionalP(estimate$logRr, estimate$seLogRr)
        maRow$logRr <- estimate$logRr
        maRow$seLogRr <- estimate$seLogRr
        maRow$tau <- estimate$tau
        
      }
      
      if (addTraditional) {
        meta <- meta::metagen(TE = rows$logRr,
                              seTE = rows$seLogRr,
                              sm = "RR",
                              hakn = FALSE)
        s <- summary(meta)
        rnd <- s$random
        maRow$traditionalLogRr <- rnd$TE
        maRow$traditionalSeLogRr <- rnd$seTE
      }
    }
  }
  
  return(maRow)
}

getDescriptionFromMethod <- function(method) {
  return(
    switch(method,
    "BayesianNonNormal" = "Random effects meta-analysis using non-normal likelihood approximation to avoid bias due to small and zero counts.",
    "DL" = "Random effects meta-analysis using the DerSimonian-Laird estimator",
    "Unknown meta-analysis method"
    )
  )
}

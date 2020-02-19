# @file DataPulls.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
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


.getPreferenceScoreDist <- function(connectionDetails = NULL,
                                    preferenceScoreDist = NULL,
                                    databaseSchema = NULL,
                                    targetId,
                                    comparatorId,
                                    databaseId,
                                    analysisId) {
  if (!is.null(connectionDetails)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    sql <-   "SELECT *
              FROM  @databaseSchema.preference_score_dist
              WHERE target_id = @targetId
            	AND comparator_id = @comparatorId
            	AND database_id = '@databaseId'
            	AND analysis_id = @analysisId;"
    
    sql <-
      SqlRender::render(
        sql = sql,
        databaseSchema = databaseSchema,
        targetId = targetId,
        comparatorId = comparatorId,
        databaseId = databaseId,
        analysisId = analysisId
      )
    sql <-
      SqlRender::translate(sql = sql, targetDialect = connection@dbms)
    result <-
      DatabaseConnector::querySql(
        connection = connection,
        sql = sql,
        snakeCaseToCamelCase = TRUE
      )
  } else {
    result <- preferenceScoreDist %>%
      dplyr::filter(
        targetId %in% !!targetId,
        comparatorId %in% !!comparatorId,
        databaseId %in% !!databaseId,
        analysisId %in% !!analysisId
      )
  }
  return(result)
}


.getExposureOfInterest <- function(connectionDetails = NULL,
                                   exposureOfInterest = NULL,
                                   databaseSchema = NULL,
                                   targetId ,
                                   comparatorId) {
  if (!is.null(connectionDetails)) {
    connection <- DatabaseConnector::connect(connectionDetails)
    sql <-   "SELECT *
              FROM @databaseSchema.exposure_of_interest
              WHERE exposure_id IN (@targetId, @comparatorId);"
    
    sql <-
      SqlRender::render(
        sql = sql,
        databaseSchema = databaseSchema,
        targetId = targetId,
        comparatorId = comparatorId
      )
    sql <-
      SqlRender::translate(sql = sql,
                           targetDialect = connection@dbms)
    result <-
      DatabaseConnector::querySql(
        connection = connection,
        sql = sql,
        snakeCaseToCamelCase = TRUE
      )
  } else {
    result <- exposureOfInterest %>%
      dplyr::filter(exposureId %in% c(targetId, comparatorId))
  }
  return(result)
}

#' Get the preference score distribution for plotting
#'
#' @details
#' Get preference score data from data stored in OHDSI comparative effectiveness data model,
#' The output of this function  maybe used to create plots or tables
#' that compares the preference score distribution between target and comparators.
#'
#' @template optionalConnectionDetails
#' @template optionalDatabaseSchema
#' @template targetId
#' @template comparatorId
#' @template databaseId
#' @template analysisId
#' @param preferenceScoreDist  (optional) A R-dataFrame object with fields named in camelCase containing
#'                             data from preference_score_dist table in comparative effectiveness data model.
#' @param exposureOfInterest   (optional) A R-dataFrame object with fields named in camelCase containing
#'                             data from exposure_of_interest table in comparative effectiveness data model.
#' @return                     Tibble with fields preference score, density, cohort-name,
#'                             group for identifying if the cohort is target vs comparator.
#'
#' @examples
#' # example1: If the comparative effectiveness results are stored in a 
#' # relational database server: then please provide connection details 
#' # using DatabaseConnector::createConnectionDetails. 
#' # If connectionDetails parameter is not null, the function will attempt to
#' # connect to relational database server, and ignore using at R dataframe objects
#' # example2: If the comparative effectiveness results are stored as a R dataframe object.
#' # Note: R dataframes are expected to follow camelCase for both object names 
#' # and variable names.
#' # Note: schema parameter in createConnectionDetails should not be used. Use
#' # databaseSchema in function call instead.                  
#' \dontrun{
#'
#'
#' connectionDetails <- createConnectionDetails(dbms = "postgresql",
#'                                              user = "joe",
#'                                              password = "secret",
#'                                              server = "myserver")
#' example1 <- getPreferenceScore(connectionDetails = connectionDetails,
#'                                databaseSchema = 'hcup.version2020',
#'                                targetId = 100,
#'                                comparatorId = 200,
#'                                databaseId = 'HCUP data',
#'                                analysisId = 2)
#' example2 <- getPreferenceScore(preferenceScoreDist = preferenceScoreDist,
#'                                exposureOfInterest = exposureOfInterest,
#'                                targetId = 100,
#'                                comparatorId = 200,
#'                                databaseId = 'HCUP data',
#'                                analysisId = 2)
#' }
#' @export
#'
getPreferenceScoreDistribution <- function(connectionDetails = NULL,
                                           databaseSchema = NULL,
                                           preferenceScoreDist = NULL,
                                           exposureOfInterest = NULL,
                                           targetId,
                                           comparatorId,
                                           databaseId,
                                           analysisId) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(targetId, add = errorMessage)
  checkmate::assertInt(comparatorId, add = errorMessage)
  checkmate::assertInt(analysisId, add = errorMessage)
  checkmate::assertScalar(databaseId, add = errorMessage)
  checkmate::assertCharacter(databaseId, add = errorMessage)
  
  if (!is.null(connectionDetails)) {
    checkmate::assertCharacter(databaseSchema)
    checkmate::reportAssertions(errorMessage)
    
  } else if (is.null(connectionDetails)) {
    checkmate::assertDataFrame(preferenceScoreDist, add = errorMessage)
    checkmate::assertDataFrame(exposureOfInterest, add = errorMessage)
    checkmate::reportAssertions(errorMessage)
  } else {
    stop(
      "No connection details for an RDMS provided.
         No R-data frame object preferenceScoreDist or exposureOfInterest provided"
    )
  }
  
  preferenceScoreDist <-
    .getPreferenceScoreDist(
      connectionDetails = connectionDetails,
      databaseSchema = databaseSchema,
      preferenceScoreDist = preferenceScoreDist,
      targetId = targetId,
      comparatorId = comparatorId,
      databaseId = databaseId,
      analysisId = analysisId
    ) %>%
    dplyr::select(preferenceScore, targetDensity, comparatorDensity) %>%
    tidyr::pivot_longer(
      cols = dplyr::ends_with("Density"),
      values_to = "density" ,
      names_to = "group"
    ) %>%
    dplyr::mutate(group = stringr::str_replace(
      string = group,
      pattern = "Density",
      replacement = ""
    ))
  
  exposureOfInterest <- .getExposureOfInterest(
    connectionDetails = connectionDetails,
    databaseSchema = databaseSchema,
    exposureOfInterest = exposureOfInterest,
    targetId = targetId,
    comparatorId = comparatorId
  ) %>%
    dplyr::mutate(
      group = dplyr::case_when(
        exposureId == targetId ~ 'target',
        exposureId == comparatorId ~ 'comparator'
      )
    ) %>%
    dplyr::select(group, exposureName) %>%
    unique()
  
  result <- preferenceScoreDist %>%
    dplyr::left_join(y = exposureOfInterest, by = c("group" = "group")) %>%
    dplyr::mutate(group = SqlRender::camelCaseToTitleCase(group))
  
  return(result)
}

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


.getPreferenceScoreDist <- function(connection = NULL,
                                    preferenceScoreDist = NULL,
                                    databaseSchema = NULL,
                                    targetId,
                                    comparatorId,
                                    databaseId,
                                    analysisId) {
  if (!is.null(connection)) {
    sql <-   "SELECT *
              FROM  @databaseSchema.preference_score_dist
              WHERE target_id = @targetId
            	AND comparator_id = @comparatorId
            	AND database_id = '@databaseId'
            	AND analysis_id = @analysisId;"
    
    sql <- SqlRender::render(
      sql = sql,
      databaseSchema = databaseSchema,
      targetId = targetId,
      comparatorId = comparatorId,
      databaseId = databaseId,
      analysisId = analysisId
    )
    sql <-  SqlRender::translate(sql = sql,
                                 targetDialect = connection@dbms)
    result <- DatabaseConnector::querySql(
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


.getExposureOfInterest <- function(connection = NULL,
                                   exposureOfInterest = NULL,
                                   databaseSchema = NULL,
                                   targetId ,
                                   comparatorId) {
  if (!is.null(connection)) {
    sql <-   "SELECT *
              FROM @databaseSchema.exposure_of_interest
              WHERE exposure_id IN (@targetId, @comparatorId);"
    
    sql <- SqlRender::render(
      sql = sql,
      databaseSchema = databaseSchema,
      targetId = targetId,
      comparatorId = comparatorId
    )
    sql <- SqlRender::translate(sql = sql,
                                targetDialect = connection@dbms)
    result <- DatabaseConnector::querySql(
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


.getCmFollowUpDist <- function(connection = NULL,
                               cmFollowUpDist = NULL,
                               databaseSchema = NULL,
                               targetId,
                               comparatorId,
                               outcomeId,
                               databaseId,
                               analysisId) {
  if (!is.null(connection)) {
    sql <-  "SELECT *
            FROM @databaseSchema.cm_follow_up_dist
            WHERE target_id = @targetId
            	and comparator_id = @comparatorId
            	and outcome_id = @outcomeId
            	and analysis_id = @analysisId
            	and database_id = '@databaseId';"
    
    sql <- SqlRender::render(
      sql = sql,
      databaseSchema = databaseSchema,
      targetId = targetId,
      comparatorId = comparatorId,
      outcomeId = outcomeId,
      analysisId = analysisId,
      databaseId = databaseId
    )
    sql <- SqlRender::translate(sql = sql,
                                targetDialect = connection@dbms)
    result <- DatabaseConnector::querySql(
      connection = connection,
      sql = sql,
      snakeCaseToCamelCase = TRUE
    )
  } else if (!is.null(cmFollowUpDist)) {
    result <- cmFollowUpDist %>%
      dplyr::filter(
        targetId %in% !!targetId,
        comparatorId %in% !!comparatorId,
        outcomeId %in% !!outcomeId,
        databaseId %in% !!databaseId,
        analysisId %in% !!analysisId
      )
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
#' If the comparative effectiveness results are stored in a
#' relational database server, then please provide connection
#' using DatabaseConnector::connect. If connection
#' parameter is not null, the function will attempt to connect to
#' relational database server, and ignore using R dataframe object.
#'
#'
#' If the comparative effectiveness results are stored as a R dataframe object,
#' please make sure connection is NULL (Default value). The function
#' will expect data in R dataframe. This R dataframe object is expected to follow
#' camelCase for both object names and variable names.
#'
#'
#' @template connection
#' @template targetId
#' @template comparatorId
#' @template databaseId
#' @template analysisId
#' @param preferenceScoreDist  (optional) A R-dataFrame object with fields named in camelCase containing
#'                             data from preference_score_dist table in comparative effectiveness data model.
#' @param exposureOfInterest   (optional) A R-dataFrame object with fields named in camelCase containing
#'                             data from exposure_of_interest table in comparative effectiveness data model.
#' @return                     Tibble with fields preference score, density, cohort-name, group.
#'                             group is for identifying if the cohort is target vs comparator.
#' @examples
#' \dontrun{
#'
#' #If your data is in database, provide connection.
#' connection <- connect(createConnectionDetails(dbms = "postgresql",
#'                                              user = "joe",
#'                                              password = "secret",
#'                                              server = "myserver")
#'                                              )
#' #If connection is provided, function will attempt to read data from database
#' # data in R-dataframe format will be ignored
#' example1 <- getPreferenceScore(connection = connection,
#'                                databaseSchema = 'hcup.version2020',
#'                                targetId = 100,
#'                                comparatorId = 200,
#'                                databaseId = 'HCUP data',
#'                                analysisId = 2)
#' #If no connection is provided, then function will check for data in dataFrame
#' example2 <- getPreferenceScore(preferenceScoreDist = preferenceScoreDist,
#'                                exposureOfInterest = exposureOfInterest,
#'                                targetId = 100,
#'                                comparatorId = 200,
#'                                databaseId = 'HCUP data',
#'                                analysisId = 2)
#' }
#' @export
#'
getPreferenceScoreDistribution <- function(connection = NULL,
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
  
  if (!is.null(connection)) {
    checkmate::assertCharacter(databaseSchema)
    checkmate::reportAssertions(errorMessage)
    
  } else if (!is.null(preferenceScoreDist) &
             !is.null(exposureOfInterest)) {
    checkmate::assertDataFrame(preferenceScoreDist, add = errorMessage)
    checkmate::assertDataFrame(exposureOfInterest, add = errorMessage)
    checkmate::reportAssertions(errorMessage)
  } else {
    stop(
      "No connection for an RDMS provided.
         No R-data frame object preferenceScoreDist or exposureOfInterest provided"
    )
  }
  
  preferenceScoreDist <- .getPreferenceScoreDist(
    connection = connection,
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
    connection = connection,
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



#' Get followup distribution data.
#'
#' @details
#' Get follow-up distribution data from data stored in OHDSI comparative effectiveness data model,
#' The output of this function  maybe used to create plots or tables
#' that uses the follow-up distribution data.
#'
#' If the comparative effectiveness results are stored in a
#' relational database server, then please provide connection details
#' using DatabaseConnector::connection. If connection
#' parameter is not null, the function will attempt to connect to
#' relational database server, and ignore using R dataframe object.
#'
#' If the comparative effectiveness results are stored as a R dataframe object,
#' please make sure connection is NULL (Default value). The function
#' will expect data in R dataframe. This R dataframe object is expected to follow
#' camelCase for both object names and variable names.
#'
#'
#' @template connection
#' @template targetId
#' @template comparatorId
#' @template databaseId
#' @template analysisId
#' @template outcomeId
#' @param cmFollowUpDist       (optional) A R-dataFrame object with fields named in camelCase containing
#'                             data from cm_follow_up_dist table in comparative effectiveness data model.
#' @return                     Tibble with fields Id, Min, P10, P25, Median, P75, P90,Max, group.
#'                             group is for identifying if the cohort is target vs comparator.
#'
#' @examples
#' \dontrun{
#' #If your data is in database, provide connection
#' connection <- connect(createConnectionDetails(dbms = "postgresql",
#'                                              user = "joe",
#'                                              password = "secret",
#'                                              server = "myserver")
#'                                              )
#' #If connection is provided, function will attempt to read data from database
#' example1 <- getFollowUpDaysDistribution(connection = connection,
#'                                         databaseSchema = 'database.schema',
#'                                         targetId = 100,
#'                                         comparatorId = 200,
#'                                         outcomeId = 111,
#'                                         databaseId = 'HCUP data',
#'                                         analysisId = 2)
#' #If no connection is provided, then function will check for data in dataFrame
#' example2 <- getFollowUpDaysDistribution(cmFollowUpDist = cmFollowUpDist,
#'                                         targetId = 100,
#'                                         comparatorId = 200,
#'                                         outcomeId = 111,
#'                                         databaseId = 'HCUP data',
#'                                         analysisId = 2)
#' }
#' @export
#'
getFollowUpDistribution <- function(connection = NULL,
                                    databaseSchema = NULL,
                                    cmFollowUpDist = NULL,
                                    targetId,
                                    comparatorId,
                                    outcomeId,
                                    databaseId,
                                    analysisId) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(targetId, add = errorMessage)
  checkmate::assertInt(comparatorId, add = errorMessage)
  checkmate::assertInt(outcomeId, add = errorMessage)
  checkmate::assertInt(analysisId, add = errorMessage)
  checkmate::assertScalar(databaseId, add = errorMessage)
  checkmate::assertCharacter(databaseId, add = errorMessage)
  
  if (!is.null(connection)) {
    checkmate::assertCharacter(databaseSchema)
    checkmate::reportAssertions(errorMessage)
    
  } else if (is.null(connection)) {
    checkmate::assertDataFrame(cmFollowUpDist, add = errorMessage)
    checkmate::reportAssertions(errorMessage)
  } else {
    stop(
      "No connection details for an RDMS provided.
         No R-data frame object cmFollowUpDist provided"
    )
  }
  
  .prepareData <- function(data, type) {
    data %>%
      dplyr::select(dplyr::starts_with(type)) %>%
      purrr::map_at(names(.)[stringr::str_detect(string = names(.), pattern = ".*(?:(?!Id).).$")], #not ends with Id regex
                    scales::comma, big.mark = ",") %>%
      tidyr::as_tibble() %>% #map_at returns a list, not a dataframe.
      dplyr::rename_all(~ stringr::str_replace(., type, "")) %>%
      dplyr::rename_all(~ stringr::str_replace(., "Days", "")) %>%
      dplyr::mutate(group = type)
  }
  
  cmFollowUpDist <- .getCmFollowUpDist(
    connection = connection,
    databaseSchema = databaseSchema,
    cmFollowUpDist = cmFollowUpDist,
    targetId = targetId,
    comparatorId = comparatorId,
    outcomeId = outcomeId,
    databaseId = databaseId,
    analysisId = analysisId
  )
  
  result <- dplyr::bind_rows(
    .prepareData(data = cmFollowUpDist, type = "target"),
    .prepareData(data = cmFollowUpDist, type = "comparator")
  )
  
  return(result)
}
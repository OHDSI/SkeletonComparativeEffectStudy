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


.getcohortMethodResult <- function(connection = NULL,
                                   cohortMethodResult = NULL,
                                   databaseSchema = NULL,
                                   targetId,
                                   comparatorId,
                                   outcomeId,
                                   databaseId,
                                   analysisId) {
  if (!is.null(connection)) {
    sql <-   "SELECT *
              FROM @databaseSchema.cohort_method_result
              WHERE target_id = @targetId
              and comparator_id = @comparatorId
              and outcome_id = @outcomeId
              and database_id = '@databaseId'
              and analysis_id = @analysisId;"
    sql <- SqlRender::render(
        sql = sql,
        databaseSchema = databaseSchema,
        targetId = targetId,
        comparatorId = comparatorId,
        outcomeId = outcomeId,
        databaseId = databaseId,
        analysisId = analysisId
      )
    sql <- SqlRender::translate(sql = sql,
                                targetDialect = connection@dbms)
    result <-
      DatabaseConnector::querySql(
        connection = connection,
        sql = sql,
        snakeCaseToCamelCase = TRUE
      )
  } else if (!is.null(cohortMethodResult)) {
    result <- cohortMethodResult %>%
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


.getcohortMethodAnalysis <- function(connection = NULL,
                                     cohortMethodAnalysis = NULL,
                                     databaseSchema = NULL,
                                     analysisId) {
  if (!is.null(connection)) {
    sql <-   "SELECT *
              FROM @databaseSchema.cohort_method_analysis
              WHERE analysis_id = @analysisId;"
    sql <- SqlRender::render(sql = sql,
                             databaseSchema = databaseSchema,
                             analysisId = analysisId)
    sql <- SqlRender::translate(sql = sql,
                                targetDialect = connection@dbms)
    result <- DatabaseConnector::querySql(
      connection = connection,
      sql = sql,
      snakeCaseToCamelCase = TRUE
    )
  } else if (!is.null(cohortMethodAnalysis)) {
    result <- cohortMethodAnalysis %>%
      dplyr::filter(analysisId %in% !!analysisId)
  }
  return(result)
}

.getCovariateBalance <- function(connection = NULL,
                                 covariateBalance = NULL,
                                 databaseSchema = NULL,
                                 targetId,
                                 comparatorId,
                                 outcomeId,
                                 databaseId,
                                 analysisId,
                                 interactionCovariateId = NULL) {
  
  if (!is.null(connection)) {
    interactionCovariateIdNull <- is.null(interactionCovariateId)
    sql <-   "SELECT *
              FROM @databaseSchema.covariate_balance
              WHERE target_id = @targetId
              and comparator_id = @comparatorId
              and outcome_id = @outcomeId
              and database_id = '@databaseId'
              {@interactionCovariateIdNull} ? {and interaction_covariate_id IS NULL}:{and interaction_covariate_id = @interactionCovariateId}
              and analysis_id = @analysisId;"
    sql <- SqlRender::render(
        sql = sql,
        databaseSchema = databaseSchema,
        targetId = targetId,
        comparatorId = comparatorId,
        outcomeId = outcomeId,
        databaseId = databaseId,
        analysisId = analysisId,
        interactionCovariateId = interactionCovariateId,
        interactionCovariateIdNull = interactionCovariateIdNull
      )
    sql <- SqlRender::translate(sql = sql,
                           targetDialect = connection@dbms)
    result <- DatabaseConnector::querySql(
        connection = connection,
        sql = sql,
        snakeCaseToCamelCase = TRUE
      )
  } else if (!is.null(covariateBalance)) {
    result <- covariateBalance %>%
      dplyr::filter(
        targetId %in% !!targetId,
        comparatorId %in% !!comparatorId,
        outcomeId %in% !!outcomeId,
        databaseId %in% !!databaseId,
        analysisId %in% !!analysisId
      )
    if (is.null(interactionCovariateId)) {
      result <- result %>% 
                dplyr::filter(is.na(interactionCovariateId)) #data.frames cant have NULL
    } else {
      result <- result %>% 
                dplyr::filter(interactionCovariateId %in% !!interactionCovariateId)
    }
  }
  return(result)
}


.getCovariate <- function(connection = NULL,
                          covariate = NULL,
                          databaseSchema,
                          databaseId,
                          analysisId) {
  if (!is.null(connection)) {
    sql <- "SELECT *
            FROM @databaseSchema.covariate
            WHERE database_id = '@databaseId'
            and analysis_id = @analysisId;"
    sql <- SqlRender::render(
        sql = sql,
        databaseId = databaseId,
        analysisId = analysisId,
        databaseSchema = databaseSchema
      )
    sql <- SqlRender::translate(sql = sql,
                                targetDialect = connection@dbms)
    result <- DatabaseConnector::querySql(
        connection = connection,
        sql = sql,
        snakeCaseToCamelCase = TRUE
      )
  } else if (!is.null(covariate)) {
    result <- covariate %>%
      dplyr::filter(
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
#' @template databaseSchema
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
#' @template databaseSchema
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


#' Get the main study results data.
#'
#' @details
#' Get the main results data for target, comparator and outcome cohorts in data frame format from
#' OHDSI comparative effectiveness data model. The output maybe used to create plots or tables
#' that use the main study results data.
#'
#' @template connection
#' @template databaseSchema
#' @template targetId
#' @template comparatorId
#' @template databaseId
#' @template analysisId
#' @template outcomeId
#' @param cohortMethodResult   (optional) A R-dataFrame object with fields named in camelCase containing
#'                             data from cohort_method_result table in comparative effectiveness data model.
#' @param cohortMethodAnalysis (optional) A R-dataFrame object with fields named in camelCase containing
#'                             data from cohort_method_analysis table in comparative effectiveness data model.
#' @return                     Tibble. Fields with column names 'HR (95% CI)', 'Cal. HR (95% CI)' are provided
#'                             in print friendly format.
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
#' # data in R-dataframe format will be ignored
#' example1 <- getCohortMethodResult(connection = connection,
#'                                   databaseSchema = 'database.schema',
#'                                   targetId = 100,
#'                                   comparatorId = 200,
#'                                   outcomeId = 111,
#'                                   databaseId = 'HCUP data',
#'                                   analysisId = 2)
#' #If no connection is provided, then function will check for data in dataFrame
#' example2 <- getCohortMethodResult(cohortMethodResult = cohortMethodResult,
#'                                   cohortMethodAnalysis = cohortMethodAnalysis,
#'                                   targetId = 100,
#'                                   comparatorId = 200,
#'                                   outcomeId = 111,
#'                                   databaseId = 'HCUP data',
#'                                   analysisId = 2)
#' }
#' @export
#'
getCohortMethodResult <- function(connection = NULL,
                                  databaseSchema = NULL,
                                  cohortMethodResult = NULL,
                                  cohortMethodAnalysis = NULL,
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
    checkmate::assertDataFrame(cohortMethodResult, add = errorMessage)
    checkmate::assertDataFrame(cohortMethodAnalysis, add = errorMessage)
    checkmate::reportAssertions(errorMessage)
  } else {
    stop(
      "No connection for an RDMS provided.
         No R-data frame object cohortMethodResult or cohortMethodAnalysis provided"
    )
  }
  
  cohortMethodResult <- .getcohortMethodResult(
      connection = connection,
      databaseSchema = databaseSchema,
      cohortMethodResult = cohortMethodResult,
      targetId = targetId,
      comparatorId = comparatorId,
      outcomeId = outcomeId,
      databaseId = databaseId,
      analysisId = analysisId
    )
  
  cohortMethodAnalysis <- .getcohortMethodAnalysis(
      connection = connection,
      databaseSchema = databaseSchema,
      cohortMethodAnalysis = cohortMethodAnalysis,
      analysisId = analysisId
    )
  
  result <- cohortMethodResult %>%
    purrr::map_at(
      c(
        'rr',
        'ci95Lb',
        'ci95Ub',
        'calibratedRr',
        'p',
        'calibratedCi95Lb',
        'calibratedCi95Ub',
        'calibratedP'
      ),
      sprintf,
      fmt = "%.2f"
    ) %>%
    #sprintF is able to handle NA better than scales
    # if we use NA will result in error e.g. scales::comma(NA) -- Error in UseMethod("round_any")
    tidyr::as_tibble() %>%
    dplyr::mutate(
      "HR (95% CI)" = glue::glue('{rr} ({ci95Lb} - {ci95Ub})'),
      "Cal. HR (95% CI)" = glue::glue('{calibratedRr} ({calibratedCi95Lb} - {calibratedCi95Ub})')
    ) %>%
    dplyr::left_join(y = cohortMethodAnalysis, by = c('analysisId' = 'analysisId')) %>%
    dplyr::rename(Analysis = description,
                  P = p,
                  "Cal. p" = calibratedP) %>%
    dplyr::select(-definition)
  
  return(result)
}


#' Get Power data
#'
#' @details
#' Get the power results for target, comparator and outcome cohorts in data frame format from
#' OHDSI comparative effectiveness data model. The output maybe used to create plots or tables.
#'
#' @template connection
#' @template databaseSchema
#' @template targetId
#' @template comparatorId
#' @template databaseId
#' @template analysisId
#' @template outcomeId
#' @param    alpha             (optional) Default value = 0.05. Threshold for Type 1 error in hypothesis testing,
#'                             i.e. rejection of a true null hypothesis (also known as a "false positive" 
#'                             finding or conclusion)
#' @param    power             (optional) Default value = 0.8. Threshold for Type 2 error in hypothesis testing,
#'                             i.e. non-rejection of a false null hypothesis (also known as a "false negative" 
#'                             finding or conclusion)
#' @param cohortMethodResult   (optional) A R-dataFrame object with fields named in camelCase containing
#'                             data from cohort_method_result table in comparative effectiveness data model.
#' @param cohortMethodAnalysis (optional) A R-dataFrame object with fields named in camelCase containing
#'                             data from cohort_method_analysis table in comparative effectiveness data model.
#' @return                     Tibble. 
#'
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(dbms = "postgresql",
#'                                            user = "joe",
#'                                            password = "secret",
#'                                            server = "myserver")
#' #If connection is provided, function will attempt to read data from database
#' # data in R-dataframe format will be ignored
#' example1 <- getPower(connection = connectionDetails,
#'                      databaseSchema = 'database.schema',
#'                      targetId = 100,
#'                      comparatorId = 200,
#'                      outcomeId = 111,
#'                      databaseId = 'HCUP data',
#'                      analysisId = 2)
#' #If no connection is provided, then function will check for data in dataFrame
#' example2 <- getPower(cohortMethodResult = cohortMethodResult,
#'                      cohortMethodAnalysis = cohortMethodAnalysis,
#'                      targetId = 100,
#'                      comparatorId = 200,
#'                      outcomeId = 111,
#'                      databaseId = 'HCUP data',
#'                      analysisId = 2)
#' }
#' @export
#'
getPower <- function(connection = NULL,
                     databaseSchema = NULL,
                     cohortMethodResult = NULL,
                     cohortMethodAnalysis = NULL,
                     targetId,
                     comparatorId,
                     outcomeId,
                     databaseId,
                     analysisId,
                     alpha = 0.05,
                     power = 0.8) {
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
    checkmate::assertDataFrame(cohortMethodResult, add = errorMessage)
    checkmate::assertDataFrame(cohortMethodAnalysis, add = errorMessage)
    checkmate::reportAssertions(errorMessage)
  } else {
    stop(
      "No connection for an RDMS provided.
         No R-data frame object cohortMethodResult and cohortMethodAnalysis provided"
    )
  }
  
  cohortMethodResult <- .getcohortMethodResult(
      connection = connection,
      databaseSchema = databaseSchema,
      cohortMethodResult = cohortMethodResult,
      targetId = targetId,
      comparatorId = comparatorId,
      outcomeId = outcomeId,
      databaseId = databaseId,
      analysisId = analysisId
    )
  
  cohortMethodAnalysis <- .getcohortMethodAnalysis(
      connection = connection,
      databaseSchema = databaseSchema,
      cohortMethodAnalysis = cohortMethodAnalysis,
      analysisId = analysisId
    ) %>%
    dplyr::select(-definition)
  
  z1MinAlpha <- qnorm(1 - alpha / 2)
  zBeta <- -qnorm(1 - power)
  
  result <- cohortMethodResult %>%
    dplyr::left_join(y = cohortMethodAnalysis, by = c("analysisId" = "analysisId")) %>%
    dplyr::mutate(
      pA = targetSubjects / (targetSubjects + comparatorSubjects),
      pB = 1 - (targetSubjects / (targetSubjects + comparatorSubjects)),
      totalEvents = abs(targetOutcomes) + comparatorOutcomes,
      targetYears = targetDays / 365.25,
      comparatorYears = comparatorDays / 365.25
    ) %>%
    dplyr::mutate(
      mdrr = exp(sqrt((zBeta + z1MinAlpha) ^ 2 / (totalEvents * pA * pB))),
      targetIr = 1000 * (targetOutcomes / targetYears),
      comparatorIr = 1000 * (comparatorOutcomes / comparatorYears)
    ) %>%
    purrr::map_at(c('targetIr','comparatorIr','mdrr'),
                  sprintf,
                  fmt = "%.2f") %>%
    purrr::map_at(
      c(
        'targetSubjects',
        'comparatorSubjects',
        'targetYears',
        'comparatorYears',
        'targetOutcomes',
        'comparatorOutcomes'
      ),
      formatC,
      big.mark = ",",
      format = "d"
    ) %>%
    purrr::map_at(
      c(
        "targetSubjects",
        "comparatorSubjects",
        "targetOutcomes",
        "comparatorOutcomes",
        "targetIr",
        "comparatorIr"
      ),
      stringr::str_replace,
      pattern = "^-",
      replacement = "<"
    ) %>%
    tidyr::as_tibble() %>%
    dplyr::select(
      description,
      targetSubjects,
      comparatorSubjects,
      targetYears,
      comparatorYears,
      targetOutcomes,
      comparatorOutcomes,
      targetIr,
      comparatorIr,
      mdrr
    )
  
  idx = (result$targetOutcomes < 0 | result$comparatorOutcomes < 0)
  result$mdrr[idx] <- paste0(">", result$mdrr[idx])
  return(result)
}


#' Get covariate balance data.
#'
#' @details
#' Get covariate balance data from data stored in OHDSI comparative effectiveness data model,
#' The output of this function  maybe used to create plots or tables
#' that uses the covariate balance data.
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
#' @template databaseSchema
#' @param interactionCovariateId (optoinal) An integer identifying the covariate in interactionCovariateId
#'                                field of covariate_balance table of OHDSI comparative effectiveness data model
#' @param covariateBalance     (optional) A R-dataFrame object with fields named in camelCase containing
#'                             data from covariate_balance table in comparative effectiveness data model.
#' @param covariate            (optional) A R-dataFrame object with fields named in camelCase containing
#'                             data from covariate table in comparative effectiveness data model.
#' @return                     Tibble.
#'
#' @examples
#' \dontrun{
# #If your data is in database, provide connection
# connection <- connect(createConnectionDetails(dbms = "postgresql",
#                                               user = "joe",
#                                               password = "secret",
#                                               server = "myserver")
# )
# #If connection is provided, function will attempt to read data from database
# example1 <- getCovariateBalance(connection = connection,
#                                 databaseSchema = 'database.schema',
#                                 targetId = 100,
#                                 comparatorId = 200,
#                                 outcomeId = 111,
#                                 databaseId = 'HCUP data',
#                                 analysisId = 2)
# #If no connection is provided, then function will check for data in dataFrame
# example2 <- getCovariateBalance(covariateBalance = covariateBalance,
#                                 targetId = 100,
#                                 comparatorId = 200,
#                                 outcomeId = 111,
#                                 databaseId = 'HCUP data',
#                                 analysisId = 2)
#' }
#' @export
#'
getCovariateBalance <- function(connection = NULL,
                                databaseSchema = NULL,
                                covariateBalance = NULL,
                                covariate = NULL,
                                targetId,
                                comparatorId,
                                outcomeId,
                                databaseId,
                                analysisId,
                                interactionCovariateId = NULL) {
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
    checkmate::assertDataFrame(covariateBalance, add = errorMessage)
    checkmate::assertDataFrame(covariate, add = errorMessage)
    checkmate::reportAssertions(errorMessage)
  } else {
    stop(
      "No connection details for an RDMS provided.
         No R-data frame object covariateBalance and covariate provided"
    )
  }
  
  covariateBalance <- .getCovariateBalance(
    connection = connection,
    databaseSchema = databaseSchema,
    covariateBalance = covariateBalance,
    targetId = targetId,
    comparatorId = comparatorId,
    outcomeId = outcomeId,
    databaseId = databaseId,
    analysisId = analysisId,
    interactionCovariateId = interactionCovariateId
  ) %>% dplyr::tibble()
  
  covariate <- .getCovariate(
    connection = connection,
    databaseSchema = databaseSchema,
    databaseId = databaseId,
    covariate = covariate,
    analysisId = analysisId
  ) %>% dplyr::tibble()
  
  balance <- covariateBalance %>% 
    dplyr::left_join(y = covariate, by = c("databaseId" = "databaseId", 
                                           "analysisId" = "analysisId", 
                                           "covariateId" = "covariateId")
    ) %>% 
    dplyr::rename(beforeMatchingMeanTreated = targetMeanBefore,
                  beforeMatchingMeanComparator = comparatorMeanBefore,
                  beforeMatchingStdDiff = stdDiffBefore,
                  afterMatchingMeanTreated = targetMeanAfter,
                  afterMatchingMeanComparator = comparatorMeanAfter,
                  afterMatchingStdDiff = stdDiffAfter) %>% 
    dplyr::mutate(absBeforeMatchingStdDiff = abs(beforeMatchingStdDiff),
                  absAfterMatchingStdDiff = abs(afterMatchingStdDiff))
  return(balance)
}

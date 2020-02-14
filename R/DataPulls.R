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


#' gets a table with data from preference score distribution.
#'
#' @details
#' gets preference score data for target and comparator in data frame format from
#' OHDSI comparative effectiveness data model. The output maybe used to create plots or tables
#' that compares the preference score distribution between target and comparators.
#'
#' @param connectionDetails    (optional) An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param preferenceScoreDist  (optional) A R-dataFrame object with fields named in camelCase containing
#'                             data from preference_score_dist table in comparative effectiveness data model.
#' @param exposureOfInterest   (optional) A R-dataFrame object with fields named in camelCase containing
#'                             data from exposure_of_interest table in comparative effectiveness data model.
#' @param databaseId           A short string for identifying the database for query from comparative effectiveness
#'                             data model (e.g. 'Synpuf')
#' @param targetId             An integer identifying a cohort in exposure_id field of exposure_of_interest
#'                             of OHDSI comparative effectiveness data model
#' @param comparatorId         An integer identifying a cohort in exposure_id field of exposure_of_interest
#'                             of OHDSI comparative effectiveness data model
#' @param analysisId           An integer identifying the analysis_id field in cohort_method_analysis table
#'                             of OHDSI comparative effectiveness data model

#' @return                     tibble
#'
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(dbms = "postgresql",
#'                                              user = "joe",
#'                                              password = "secret",
#'                                              server = "myserver")
#' example1 <- getPreferenceScore(connectionDetails = connectionDetails,
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
getPreferenceScore <- function(connectionDetails = NULL,
                               preferenceScoreDist = NULL,
                               exposureOfInterest = NULL,
                               targetId,
                               comparatorId,
                               databaseId,
                               analysisId) {
  #Argument checks
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(targetId, add = errorMessage)
  checkmate::assertInt(comparatorId, add = errorMessage)
  checkmate::assertInt(analysisId, add = errorMessage)
  checkmate::assertScalar(databaseId, add = errorMessage)
  checkmate::assertCharacter(databaseId, add = errorMessage)
  
  
  if (!is.null(connectionDetails)) {
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
      preferenceScoreDist = preferenceScoreDist,
      targetId = targetId,
      comparatorId = comparatorId,
      databaseId = databaseId,
      analysisId = analysisId
    ) %>%
    dplyr::select(preferenceScore, targetDensity, comparatorDensity) %>%
    dplyr::rename(x = preferenceScore) %>%
    tidyr::pivot_longer(
      cols = dplyr::ends_with("Density"),
      values_to = "y" ,
      names_to = "group"
    ) %>%
    dplyr::mutate(group = stringr::str_replace(
      string = group,
      pattern = "Density",
      replacement = ""
    ))
  
  
  exposureOfInterest <- .getExposureOfInterest(
    connectionDetails = connectionDetails,
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
  
  preferenceScore <- preferenceScoreDist %>%
    dplyr::left_join(y = exposureOfInterest, by = c("group" = "group")) %>%
    dplyr::mutate(group = SqlRender::camelCaseToTitleCase(group))
  
  return(preferenceScore)
}
# @file EstimationDataFromComparativeEffectivenessModel.R
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
#' @param formatName           (optional) Should the cohort name be formatted to remove prefixes and underscores? Default = FALSE
#'
#' @return                     tibble data frame
#' 
#' @examples                    SkeletonComparativeEffectStudy::getDataForPreferenceScorePlot(preferenceScoreDist = preferenceScoreDist, 
#'                                                                                           exposureOfInterest = exposureOfInterest,
#'                                                                                           targetId = 100,
#'                                                                                           comparatorId = 200,
#'                                                                                           databaseId = 'HCUP data',
#'                                                                                           analysisId = 2)
#' @export
#'
getDataForPreferenceScorePlot <- function(connection = NULL,
                                          preferenceScoreDist = NULL,
                                          exposureOfInterest = NULL,
                                          targetId,
                                          comparatorId,
                                          outcomeId,
                                          databaseId,
                                          analysisId
) {

  
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertInt(targetId, add = errorMessage)
  checkmate::assertInt(comparatorId, add = errorMessage)
  checkmate::assertInt(analysisId, add = errorMessage)
  checkmate::assertScalar(databaseId, add = errorMessage)
  checkmate::assertCharacter(databaseId, add = errorMessage)

  
  if (!is.null(connection)) {
    checkmate::reportAssertions(errorMessage)
  } else if (is.null(connection)) {
    checkmate::assertDataFrame(preferenceScoreDist, add = errorMessage)
    checkmate::assertDataFrame(exposureOfInterest, add = errorMessage)
    checkmate::reportAssertions(errorMessage)
  } else {
    stop("No connection or data frame.")
  }

  preferenceScoreDist <- .getPreferenceScoreDist(connection = connection, 
                                                 preferenceScoreDist = preferenceScoreDist,
                                                 targetId = targetId,
                                                 comparatorId = comparatorId,
                                                 outcomeId = outcomeId,
                                                 databaseId = databaseId,
                                                 analysisId = analysisId) %>%
                        dplyr::select(preferenceScore, targetDensity, comparatorDensity) %>%
                        dplyr::rename(x = preferenceScore) %>%
                        tidyr::pivot_longer(cols = dplyr::ends_with("Density"),
                                            values_to = "y" ,
                                            names_to = "group") %>% 
                        dplyr::mutate(group = stringr::str_replace(string = group, 
                                                                   pattern = "Density", 
                                                                   replacement = "")
                                      )
  
  
  exposureOfInterest <- .getExposureOfInterest(connection, 
                                               exposureOfInterest = exposureOfInterest,
                                               targetId = targetId,
                                               comparatorId = comparatorId) %>%
                        dplyr::mutate(group = dplyr::case_when(exposureId == targetId ~ 'target',
                                                               exposureId == comparatorId ~ 'comparator'
                                                              )) %>%
                        dplyr::select(group, exposureName) %>% 
                        unique()

  preferenceScoreDist %>%
      dplyr::left_join(y = exposureOfInterest, by = c("group" = "group")) %>%
      dplyr::mutate(group = snakecase::to_sentence_case(group))
}

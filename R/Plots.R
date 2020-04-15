# @file Plots
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

.getDefaultGgplotTheme <- function() {
  ggplot2::theme_set(ggplot2::theme_minimal())
  ggplot2::theme_update(
    panel.grid = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = 'top'
  )
  return(ggplot2::theme_get())
}

#' Get the preference score distribution plot for data in OHDSI comparative effectiveness data model.
#'
#' @details
#' Get preference score distribution plot for data stored in OHDSI 
#' comparative effectiveness data model. The output of this function  visualizes 
#' the preference score distribution between target and comparators.
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
#' The plots are customizable by using ggplot2 options. Use ggplot2.theme to set theme.
#' Example how to create ggplot2Theme
#'    ggplot2::theme_set(ggplot2::theme_minimal())
#'    ggplot2::theme_update(panel.grid = ggplot2::element_blank(),
#'                          legend.title = ggplot2::element_blank(),
#'                          legend.position = 'top')
#'    ggplot2Theme <- ggplot2::theme_get()
#'    
#' Similarly, plot colors and borders may be changed using ggplot2.scale_fill and 
#' ggplot2::scale_color
#'
#' @template connection
#' @template databaseSchema
#' @template targetId
#' @template comparatorId
#' @template databaseId
#' @template analysisId
#' @param preferenceScoreDist  (optional) A R-dataFrame object with fields named in camelCase containing
#'                             data from preference_score_dist table in comparative effectiveness data model.
#' @param exposureOfInterest   (optional) A R-dataFrame object with fields named in camelCase containing
#'                             data from exposure_of_interest table in comparative effectiveness data model.
#' @template ggplot2.theme 
#' @template ggplot2.scale_fill 
#' @template ggplot2.scale_color       
#' @return                     Tibble with fields preference score, density, cohort-name, group.
#'                             group is for identifying if the cohort is target vs comparator.
#' @examples
#' \dontrun{
#'
#' #If your data is in database, provide connection.
#' connection <- connect(createConnectionDetails(dbms = "postgresql",
#'                                               user = "joe",
#'                                               password = "secret",
#'                                               server = "myserver")
#' )
#' If connection is provided, function will attempt to read data from database
#' data in R-dataframe format will be ignored
#' example1 <- plotPreferenceScore(connection = connection,
#'                    databaseSchema = 'hcup.version2020',
#'                    targetId = 100,
#'                    comparatorId = 200,
#'                    databaseId = 'HCUP data',
#'                    analysisId = 2)
#' If no connection is provided, then function will check for data in dataFrame
#' example2 <- plotPreferenceScore(preferenceScoreDist = preferenceScoreDist,
#'                    exposureOfInterest = exposureOfInterest,
#'                    targetId = 100,
#'                    comparatorId = 200,
#'                    databaseId = 'HCUP data',
#'                    analysisId = 2)
#' }
#' 
#' @export
plotPreferenceScore <- function(connection = NULL,
                                databaseSchema = NULL,
                                preferenceScoreDist = NULL,
                                exposureOfInterest = NULL,
                                targetId,
                                comparatorId,
                                databaseId,
                                analysisId,
                                ggplot2.scale_fill = ggplot2::scale_fill_viridis_d(alpha = 0.5, option = "E"),
                                ggplot2.scale_color = ggplot2::scale_color_viridis_d(alpha = 0.5, option = "E"),
                                ggplot2.theme = NULL,
                                xAxisLabel = "Preference score",
                                yAxisLabel = "Density"
) {
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
  
  preferenceScore <- getPreferenceScoreDistribution(connection = connection,
                                                    databaseSchema = databaseSchema,
                                                    preferenceScoreDist = preferenceScoreDist,
                                                    exposureOfInterest = exposureOfInterest,
                                                    targetId = targetId,
                                                    comparatorId = comparatorId,
                                                    databaseId = databaseId,
                                                    analysisId = analysisId) %>% 
    dplyr::rename(x = preferenceScore, y = density) %>% 
    dplyr::select(-exposureName)
  # set ggplot2 theme for plotting
  if (!is.null(ggplot2.theme)) {
    ggplot2::theme_set(ggplot2.theme)
  } else {
    ggplot2::theme_set(.getDefaultGgplotTheme())
  }
  # create the plot
  plot <- ggplot2::ggplot(preferenceScore,
                          ggplot2::aes(x = x, y = y, color = group, group = group, fill = group)) +
    ggplot2::geom_density(stat = "identity") + 
    ggplot2::scale_x_continuous(xAxisLabel, limits = c(0, 1)) +
    ggplot2::scale_y_continuous(yAxisLabel)
  # apply optional color to plot
  if (!is.null(ggplot2.scale_fill)) {
    plot <- plot + ggplot2.scale_fill
  }
  if (!is.null(ggplot2.scale_color)) {
    plot <- plot + ggplot2.scale_color
  }
  return(plot)
}

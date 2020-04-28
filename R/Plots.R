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

#' Get the preference score distribution plot for data preferenceScore object.
#'
#' @details
#' This function creates the preference score distribution plot for data in 
#' preferenceScore object. The preferenceScore object is created from OHDSI
#' comparative effectiveness data model by function \code{getPreferenceScoreDistribution}. 
#' The output of this function visualizes the preference score distribution 
#' between target and comparators.
#' 
#' (Advanced) The output is a ggplot object. It maybe modified to change color, title, label etc.
#'
#' @param preferenceScore   A R-dataFrame preferenceScore object that was derived by 
#'                          function \code{getPreferenceScoreDistribution}
#' @param fileName          (optional) Name of the file where the plot should be saved, for example 'plot.png'.
#'                          See the function \code{ggsave} in the ggplot2 package for supported file
#'                          formats.
#' @return                  ggplot object. if fileName is provided, a ggsave will be used to save the object.
#' @examples
#' \dontrun{
#' example1 <- plotPreferenceScore(preferenceScore = preferenceScore)
#' } 
#' @export
plotPreferenceScore <- function(preferenceScore = NULL,
                                fileName = NULL
) {
  
  if (is.null(preferenceScore)) {
    stop("Must provide preferenceScore object.")
  }
  plotData <- preferenceScore %>% 
    dplyr::rename(x = preferenceScore, y = density) %>% 
    dplyr::select(-exposureName)
  
  # create the plot
  plot <- ggplot2::ggplot(plotData,
                          ggplot2::aes(x = x, y = y, color = group, group = group, fill = group)) +
    ggplot2::geom_density(stat = "identity") + 
    ggplot2::scale_x_continuous("Preference score", limits = c(0, 1)) +
    ggplot2::scale_y_continuous("Density")
  
  # apply theme and colors.
  ggplot2::theme_set(.getDefaultGgplotTheme())
  plot <- plot + ggplot2::scale_fill_viridis_d(alpha = 0.5, option = "D")
  plot <- plot + ggplot2::scale_color_viridis_d(alpha = 0.5, option = "D")
  
  if (!is.null(fileName)) {
    ggplot2::ggsave(fileName, plot, width = 5, height = 3.5, dpi = 400)
  }
  return(plot)
}

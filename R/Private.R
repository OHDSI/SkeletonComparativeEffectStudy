.getPreferenceScoreDist <- function(connectionDetails = NULL,
                                    preferenceScoreDist = NULL,
                                    targetId,
                                    comparatorId,
                                    databaseId,
                                    analysisId) {
  if (!is.null(connectionDetails)) {
    if (exists("connectionDetails@schema")) {
      schema <- connectionDetails@schema
    }
    connection <- DatabaseConnector::connect(connectionDetails)
    sql <-   "SELECT *
              FROM  {@schema !=} ? {@schema.preference_score_dist}:{preference_score_dist}
              WHERE target_id in (@targetId)
            	and comparator_id in (@comparatorId)
            	and database_id in (@databaseId)
            	and analysis_id in (@analysisId)"
    
    sql <- SqlRender::render(sql = sql)
    sql <-
      SqlRender::translate(sql, targetDialect = connection@dbms)
    preferenceScoreDist <-
      DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  } else if (!is.null(preferenceScoreDist)) {
    preferenceScoreDist <- preferenceScoreDist %>%
      dplyr::filter(
        targetId %in% !!targetId,
        comparatorId %in% !!comparatorId,
        databaseId %in% !!databaseId,
        analysisId %in% !!analysisId
      ) %>%
      dplyr::select(preferenceScore, targetDensity, comparatorDensity)
  } else {
    preferenceScoreDist <- NULL
  }
  return(preferenceScoreDist)
}


.getExposureOfInterest <- function(connectionDetails = NULL,
                                   exposureOfInterest = NULL,
                                   targetId,
                                   comparatorId) {
  if (!is.null(connectionDetails)) {
    if (exists("connectionDetails@schema")) {
      schema <- connectionDetails@schema
    }
    connection <- DatabaseConnector::connect(connectionDetails)
    sql <-   "SELECT exposure_id, exposure_name
              FROM {@schema !=} ? {exposure_of_interest}:{exposure_of_interest}
              WHERE exposure_id in (@targetId, @comparatorId)"
    
    sql <- SqlRender::render(sql = sql)
    sql <-
      SqlRender::translate(sql, targetDialect = connection@dbms)
    exposureOfInterest <-
      DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  } else if (!is.null(exposureOfInterest)) {
    exposureOfInterest <- exposureOfInterest %>%
      dplyr::filter(exposureId %in% c(targetId, comparatorId)) %>%
      dplyr::select(exposureId, exposureName)
  } else {
    exposureOfInterest <- NULL
  }
  return(exposureOfInterest)
}
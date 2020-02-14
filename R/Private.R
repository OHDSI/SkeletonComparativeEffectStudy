.getPreferenceScoreDist <- function(connectionDetails = NULL,
                                    preferenceScoreDist = NULL,
                                    targetId,
                                    comparatorId,
                                    databaseId,
                                    analysisId) {
  if (!is.null(connectionDetails)) {
    if (exists("connectionDetails@schema")) {
      schema <- connectionDetails@schema
    } else {
      schema <- NULL
    }
    connection <- DatabaseConnector::connect(connectionDetails)
    sql <-   "SELECT *
              FROM  {@schema !=} ? {@schema.preference_score_dist}:{preference_score_dist}
              WHERE target_id = @targetId
            	AND comparator_id = @comparatorId
            	AND database_id = '@databaseId'
            	AND analysis_id = @analysisId;"
    
    sql <-
      SqlRender::render(
        sql = sql,
        targetId = targetId,
        comparatorId = comparatorId,
        schema = schema,
        databaseId = databaseId,
        analysisId = analysisId
      )
    sql <-
      SqlRender::translate(sql = sql, targetDialect = connection@dbms)
    preferenceScoreDist <-
      DatabaseConnector::querySql(
        connection = connection,
        sql = sql,
        snakeCaseToCamelCase = TRUE
      )
  } else {
    preferenceScoreDist <- preferenceScoreDist %>%
      dplyr::filter(
        targetId %in% !!targetId,
        comparatorId %in% !!comparatorId,
        databaseId %in% !!databaseId,
        analysisId %in% !!analysisId
      ) %>%
      dplyr::select(preferenceScore, targetDensity, comparatorDensity)
  }
  return(preferenceScoreDist)
}


.getExposureOfInterest <- function(connectionDetails = NULL,
                                   exposureOfInterest = NULL,
                                   targetId ,
                                   comparatorId) {
  if (!is.null(connectionDetails)) {
    if (exists("connectionDetails@schema")) {
      schema <- connectionDetails@schema
    } else {
      schema <- NULL
    }
    connection <- DatabaseConnector::connect(connectionDetails)
    sql <-   "SELECT exposure_id, exposure_name
              FROM {@schema !=} ? {exposure_of_interest}:{exposure_of_interest}
              WHERE exposure_id IN (@targetId, @comparatorId);"
    
    sql <-
      SqlRender::render(
        sql = sql,
        targetId = targetId,
        comparatorId = comparatorId,
        schema = schema
      )
    sql <-
      SqlRender::translate(sql = sql, targetDialect = connection@dbms)
    exposureOfInterest <-
      DatabaseConnector::querySql(
        connection = connection,
        sql = sql,
        snakeCaseToCamelCase = TRUE
      )
  } else {
    exposureOfInterest <- exposureOfInterest %>%
      dplyr::filter(exposureId %in% c(targetId, comparatorId)) %>%
      dplyr::select(exposureId, exposureName)
  }
  return(exposureOfInterest)
}
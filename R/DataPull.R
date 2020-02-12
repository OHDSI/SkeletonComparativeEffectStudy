.getPreferenceScoreDist <- function(connection = NULL, 
                                    preferenceScoreDist = NULL,
                                    targetId,
                                    comparatorId,
                                    outcomeId,
                                    databaseId,
                                    analysisId) {
  if (!is.null(connection)) {
  sql <-   "SELECT *
            FROM @schema.@preferenceScoreDist
            WHERE target_id in (@targetId)
            	and comparator_id in (@comparatorId)
            	and outcome_id in (@outcomeId)
            	and database_id in (@databaseId)
            	and analysis_id in (@analysisId)"
  
  schema <- connection@schema
  sql <- SqlRender::renderSql(sql = sql)
  sql <- SqlRender::translate(sql, targetDialect = connection@dbms)$sql
  preferenceScoreDist <- DatabaseConnector::querySql(connection, sql)
  colnames(preferenceScoreDist) <- SqlRender::snakeCaseToCamelCase(colnames(preferenceScoreDist))
  } else {
  preferenceScoreDist <- preferenceScoreDist %>%
    dplyr::filter(targetId %in% !!targetId,
                  comparatorId %in% !!comparatorId,
                  outcomeId %in% !!outcomeId,
                  databaseId %in% !!databaseId,
                  analysisId %in% !!analysisId
                  ) %>% 
    dplyr::select(preferenceScore, targetDensity, comparatorDensity)
  }
  return(preferenceScoreDist)
}


.getExposureOfInterest <- function(connection, 
                                   exposureOfInterest = exposure_of_interest,
                                   targetId,
                                   comparatorId) {
  if (!is.null(connection)) {
    sql <-   "SELECT exposure_id, exposure_name
              FROM @schema.@exposureOfInterest
              WHERE exposure_id in (@targetId, @comparatorId)"
    
    schema <- connection@schema
    sql <- SqlRender::renderSql(sql = sql)
    sql <- SqlRender::translate(sql, targetDialect = connection@dbms)$sql
    exposureOfInterest <- DatabaseConnector::querySql(connection, sql)
    colnames(exposureOfInterest) <- SqlRender::snakeCaseToCamelCase(colnames(exposureOfInterest))
  } else {
    exposureOfInterest <- exposureOfInterest %>% 
                          dplyr::filter(exposureId %in% c(targetId,comparatorId)) %>% 
                          dplyr::select(exposureId, exposureName)
  }
  return(exposureOfInterest)
}


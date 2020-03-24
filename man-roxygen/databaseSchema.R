#' @param databaseSchema       (optional) The databaseSchema argument is interpreted differently according
#'                             to the different supported RDMS platforms: SQLServer and PDW: The databaseSchema
#'                             schema should specify both the database and the schema,e.g. ’my_database.dbo’.
#'                             PostgreSQL and Redshift: The databaseSchema should specify the schema.
#'                             Oracle:  The databaseSchema should specify the Oracle ’user’.

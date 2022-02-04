SkeletonComparativeEffectStudy
==============================


Requirements
============

- A database in [Common Data Model version 5](https://ohdsi.github.io/CommonDataModel/) in one of these platforms: SQL Server, Oracle, PostgreSQL, IBM Netezza, Apache Impala, Amazon RedShift, Google BigQuery, Spark, or Microsoft APS.
- R version 4.0.0 or newer
- On Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/)
- [Java](http://java.com)
- 25 GB of free disk space

How to run
==========
1. Follow [these instructions](https://ohdsi.github.io/Hades/rSetup.html) for setting up your R environment, including RTools and Java. 

2. Create an empty folder or new RStudio project, and in `R`, use the following code to install the study package and its dependencies:

    ```r
    install.packages("renv")
    download.file("https://raw.githubusercontent.com/ohdsi-studies/SkeletonComparativeEffectStudy/main/renv.lock", "renv.lock")
    renv::init()
    ```  
    
    If renv mentions that the project already has a lockfile select "*1: Restore the project from the lockfile.*".

3. Once installed, you can execute the study by modifying and using the code below. For your convenience, this code is also provided under `extras/CodeToRun.R`:

    ```r
    library(SkeletonComparativeEffectStudy)

    # Optional: specify where the temporary files (used by the Andromeda package) will be created:
    options(andromedaTempFolder = "s:/andromedaTemp")
	
    # Maximum number of cores to be used:
    maxCores <- parallel::detectCores()
	
    # Minimum cell count when exporting data:
    minCellCount <- 5
	
    # The folder where the study intermediate and result files will be written:
    outputFolder <- "c:/SkeletonComparativeEffectStudy"
	
    # Details for connecting to the server:
    # See ?DatabaseConnector::createConnectionDetails for help
    connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "redshift",
                                                                connectionString = keyring::key_get("redShiftConnectionStringOhdaMdcr"),
                                                                user = keyring::key_get("redShiftUserName"),
                                                                password = keyring::key_get("redShiftPassword"))

    # The name of the database schema where the CDM data can be found:
    cdmDatabaseSchema <- "cdm_truven_mdcr_v1911"

    # The name of the database schema and table where the study-specific cohorts will be instantiated:
    cohortDatabaseSchema <- "scratch_mschuemi"
    cohortTable <- "estimation_skeleton"

    # Some meta-information that will be used by the export function:
    databaseId <- "IBM_MDCR"
    databaseName <- "IBM MarketScan® Medicare Supplemental and Coordination of Benefits Database"
    databaseDescription <- "IBM MarketScan® Medicare Supplemental and Coordination of Benefits Database (MDCR) represents health services of retirees in the United States with primary or Medicare supplemental coverage through privately insured fee-for-service, point-of-service, or capitated health plans.  These data include adjudicated health insurance claims (e.g. inpatient, outpatient, and outpatient pharmacy). Additionally, it captures laboratory tests for a subset of the covered lives."

    # For some database platforms (e.g. Oracle): define a schema that can be used to emulate temp tables:
    options(sqlRenderTempEmulationSchema = NULL)

    execute(connectionDetails = connectionDetails,
            cdmDatabaseSchema = cdmDatabaseSchema,
            cohortDatabaseSchema = cohortDatabaseSchema,
            cohortTable = cohortTable,
            outputFolder = outputFolder,
            databaseId = databaseId,
            databaseName = databaseName,
            databaseDescription = databaseDescription,
            verifyDependencies = TRUE,
            createCohorts = TRUE,
            synthesizePositiveControls = TRUE,
            runAnalyses = TRUE,
            packageResults = TRUE,
            maxCores = maxCores)
    ```

4. Upload the file ```export/Results_<DatabaseId>.zip``` in the output folder to the study coordinator:

	```r
	uploadResults(outputFolder, privateKeyFileName = "<file>", userName = "<name>")
	```
	
	Where ```<file>``` and ```<name<``` are the credentials provided to you personally by the study coordinator.
		
5. To view the results, use the Shiny app:

	```r
	prepareForEvidenceExplorer("Result_<databaseId>.zip", "/shinyData")
	launchEvidenceExplorer("/shinyData", blind = TRUE)
	```
  
  Note that you can save plots from within the Shiny app. It is possible to view results from more than one database by applying `prepareForEvidenceExplorer` to the Results file from each database, and using the same data folder. Set `blind = FALSE` if you wish to be unblinded to the final results.

License
=======
The SkeletonComparativeEffectStudy package is licensed under Apache License 2.0

Development
===========
SkeletonComparativeEffectStudy was developed in ATLAS and R Studio.

### Development status

Unknown

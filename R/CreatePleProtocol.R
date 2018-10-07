#' @export
CreatePleProtocol <- function(outputLocation = getwd()){
  
  predictionAnalysisListFile <- system.file("settings",
                                            "StudySpecification.json",
                                            package = "SkeletonComparativeEffectStudy")
  
  #============== STYLES =======================================================
  style_title <- officer::shortcuts$fp_bold(font.size = 28)
  style_title_italic <- officer::shortcuts$fp_bold(font.size = 30, italic = TRUE)
  style_toc <- officer::shortcuts$fp_bold(font.size = 16)
  style_helper_text <- officer::shortcuts$fp_italic(color = "#FF8C00")
  style_citation <- officer::shortcuts$fp_italic(shading.color = "grey")
  style_table_title <- officer::shortcuts$fp_bold(font.size = 14, italic = TRUE)
  style_hidden_text <- officer::shortcuts$fp_italic(color = "#FFFFFF")
  #-----------------------------------------------------------------------------
  
  #============== VARIABLES ====================================================
  json <- tryCatch({OhdsiRTools::loadSettingsFromJson(file=predictionAnalysisListFile)},
                   error=function(cond) {
                     stop('Issue with json file...')
                   })
  
  #COHORTS ----
  cohortDescriptions <- cbind(
    as.data.frame(sapply(json$cohortDefinitions, function(x) cbind(x$id))),
    as.data.frame(sapply(json$cohortDefinitions, function(x) cbind(x$name)))
  )
  names(cohortDescriptions) <- c("ID","Name")
  cohortDescriptions <- cohortDescriptions[order(cohortDescriptions$ID),]
  
  targetIds <- as.data.frame(unique(sapply(json$estimationAnalysisSettings$analysisSpecification$targetComparatorOutcomes, function(x) x$targetId)))
  names(targetIds) <- c("ID")
  targetCohortDescriptions <- cohortDescriptions[cohortDescriptions$ID %in% targetIds$ID,]
  nTargetIds <- nrow(targetIds)
  targetIdsList <- paste(targetCohortDescriptions$ID,'-',targetCohortDescriptions$Name, collapse = ', ')
  
  comparatorIds <- as.data.frame(unique(sapply(json$estimationAnalysisSettings$analysisSpecification$targetComparatorOutcomes, function(x) x$comparatorId)))
  names(comparatorIds) <- c("ID")
  comparatorCohortDescriptions <- cohortDescriptions[cohortDescriptions$ID %in% comparatorIds$ID,] 
  nComparatorIds <- nrow(comparatorIds)
  comparatorIdsList <- paste(comparatorCohortDescriptions$ID,'-',comparatorCohortDescriptions$Name, collapse = ', ')
  
  tcos <- json$estimationAnalysisSettings$analysisSpecification$targetComparatorOutcomes
  outcomeIds <- lapply(tcos, function(tco) {return(as.numeric(split(tco$outcomeIds, ",")[[1]]))})
  outcomeIds <- do.call("c", outcomeIds)  
  outcomeIds <- unique(outcomeIds)
  nOutcomeIds <- length(outcomeIds)
  outcomeCohortDescriptions <- cohortDescriptions[cohortDescriptions$ID %in% outcomeIds,]
  outcomeIdsList <- paste(outcomeCohortDescriptions$ID,'-',outcomeCohortDescriptions$Name, collapse = ', ')
  
  analysis <- lapply(json$estimationAnalysisSettings$analysisSpecification$cohortMethodAnalysisList, function(x) paste0(x$analysisId,'-',x$description,'-',x$fitOutcomeModelArgs$modelType))
  analysisList <- paste(analysis, collapse=', ')
  nAnalysis <- length(json$estimationAnalysisSettings$analysisSpecification$cohortMethodAnalysisList)
  
  nCompare <- length(json$estimationAnalysisSettings$analysisSpecification$targetComparatorOutcomes) * nAnalysis
  
  comparisons <- function(x){
    t <- cohortDescriptions[cohortDescriptions$ID %in% x$targetId,]
    tList <- as.data.frame(paste(t$ID,'-',t$Name,collapse = ', '))
    names(tList) <- c("Target Cohorts")
    c <- cohortDescriptions[cohortDescriptions$ID %in% x$comparatorId,]
    cList <- as.data.frame(paste(c$ID,'-',c$Name,collapse = ', '))
    names(cList) <- c("Comparator Cohorts")
    o <- cohortDescriptions[cohortDescriptions$ID %in% x$outcomeIds,]
    oList <- as.data.frame(paste(o$ID,'-',o$Name,collapse = ', '))
    names(oList) <- c("Outcome Cohorts")
    comparison <- cbind(tList,cList,oList)
  }

  comparisonsDf <- do.call("rbind",lapply(tcos, FUN = function(x2) comparisons(x2)))
  
  negativeControls <- lapply(json$negativeControls, as.data.frame)
  negativeControls <- do.call("rbind", negativeControls)
  negativeControls <- negativeControls[order(negativeControls$comparatorId,negativeControls$outcomeName,negativeControls$targetId),]
  
  #CONCEPTS ----
  concepts <- formatConcepts(json)
  #-----------------------------------------------------------------------------
  
  #============== CITATIONS =====================================================
  pleCitation <- paste0("Citation:  ", citation("CohortMethod")$textVersion)
  empiricalPerformanceCitation <- paste0("Citation:  Ryan, P.B., et al., Empirical performance of a new user cohort method: lessons for developing a risk identification and analysis system. Drug Saf, 2013. 36 Suppl 1: p. S59-72.")
  empiricalCICitation <- paste0("Citation:  Schuemie, M.J., et al., Empirical confidence interval calibration for population-level effect estimation studies in observational healthcare data. Proceedings of the National Acadamy of Science, 2017.")
  rCitation <- paste0("Citation:  R Core Team (2013). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL http://www.R-project.org/.")
  cdmCitation <- paste0("Citation:  OMOP Common Data Model:  'OMOP Common Data Model (CDM).' from https://github.com/OHDSI/CommonDataModel.")
  #-----------------------------------------------------------------------------
 
  #============== CREATE DOCUMENT ==============================================
  # create new word document
  doc = officer::read_docx()
  #-----------------------------------------------------------------------------
  
  #============ TITLE PAGE =====================================================
  title <- officer::fpar(
    officer::ftext("Population Level Estimation:  ", prop = style_title), 
    officer::ftext(json$name, prop = style_title_italic)
  )
  
  doc <- doc %>%
    officer::body_add_par("") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(title) %>%
    officer::body_add_par("") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("Prepared on:  ", Sys.Date()), style = "Normal") %>%
    officer::body_add_par(paste0("Created by:  ", json$createdBy$name), style = "Normal") %>%
    officer::body_add_break() 
  #-----------------------------------------------------------------------------  
   
  #============ TOC ============================================================
  toc <- officer::fpar(
    officer::ftext("Table of Contents", prop = style_toc)
  )
  
  doc <- doc %>%
    officer::body_add_fpar(toc) %>%
    officer::body_add_toc(level = 2) %>%
    officer::body_add_break() 
  #----------------------------------------------------------------------------- 
  
  #============ LIST OF ABBREVIATIONS ==========================================
  abb <- data.frame(rbind(
    c("CDM","Common Data Model"),
    c("C","Comparator Cohort"),
    c("O","Outcome Cohort"),
    c("OHDSI","Observational Health Data Sciences & Informatics"),
    c("OMOP","Observational Medical Outcomes Partnership"),
    c("T", "Target Cohort"),
    c("NC","Negative Control Cohort"),
    c("PC", "Positive Control Cohort")
  ))
  names(abb) <- c("Abbreviation","Phrase")
  abb <- abb[order(abb$Abbreviation),]
  
  doc <- doc %>%
    officer::body_add_par("List of Abbreviations", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_table(abb, header = TRUE) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< Rest to be completed outside of ATLAS >>", prop = style_helper_text)
      ))
  #-----------------------------------------------------------------------------
  
  #============ RESPONSIBLE PARTIES ============================================
  doc <- doc %>%
    officer::body_add_par("Responsible Parties", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS ", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Includes author, investigator, and reviewer names and sponsor information. >>", prop = style_helper_text)
      ))
  #----------------------------------------------------------------------------- 
  
  #============ Executive Summary ==============================================
  executiveSummary1 <- paste0("The objective of this study is to compare ",nTargetIds, 
                             "target cohorts (",targetIdsList,
                             ") to ",nComparatorIds,
                             " comparator cohorts (",comparatorIdsList,
                             ") to evaluate their risk of developing ", nOutcomeIds, 
                             " outcomes (",outcomeIdsList,
                             ").")
  
  executeiveSummary2 <- paste0("The comparisons will use ",nAnalysis,
                               " analysis variants (",analysisList,
                               ").")
  
  doc <- doc %>%
    officer::body_add_par("Executive Summary", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(executiveSummary1, style = "Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(executeiveSummary2, style = "Normal") 
  #----------------------------------------------------------------------------- 

  #============ RATIONAL & BACKGROUND ==========================================
  doc <- doc %>%
    officer::body_add_par("Rational & Background", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Provide a short description of the reason that led to the initiation of or need for the study and add a short critical review of available published and unpublished data to explain gaps in knowledge that the study is intended to fill. >>", prop = style_helper_text)
      )) 
  #-----------------------------------------------------------------------------
  
  #============ Executive Summary ==============================================
  doc <- doc %>%
    officer::body_add_par("Objective", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("The objective of this study is to make the following comparisons:", style="Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_table(comparisonsDf, header = TRUE, style = "Table Professional") %>%
    officer::body_add_par("")
  #----------------------------------------------------------------------------- 
  
  #============ METHODS ========================================================
  targetCohortTable <- cbind(targetCohortDescriptions,rep("TBD",nrow(targetCohortDescriptions)))
  names(targetCohortTable) <- c("ID","Name","Description")
  
  comparatorCohortTable <- cbind(comparatorCohortDescriptions,rep("TBD",nrow(comparatorCohortDescriptions)))
  names(comparatorCohortTable) <- c("ID","Name","Description")
  
  outcomeCohortTable <- cbind(outcomeCohortDescriptions,rep("TBD",nrow(outcomeCohortDescriptions)))
  names(outcomeCohortTable) <- c("ID","Name","Description")
  
  negativeControlOutcomeCohortDef <- lapply(json$negativeControlOutcomeCohortDefinition, as.data.frame)
  negativeControlOutcomeCohortDef <- do.call("rbind",negativeControlOutcomeCohortDef)
  rn <- as.data.frame(rownames(negativeControlOutcomeCohortDef))
  names(negativeControlOutcomeCohortDef) <- c("Settings")
  names(rn) <- c("Covariates")
  negativeControlOutcomeCohortDef <- cbind(rn,negativeControlOutcomeCohortDef)
  
  df <- lapply(json$positiveControlSynthesisArgs$control, as.data.frame)
  df <- do.call("rbind",df)
  rn <- as.data.frame(rownames(df))
  names(df) <- c("Settings")
  names(rn) <- c("Covariates")
  positiveControlControlSettings <- cbind(rn,df)
  
  df <- lapply(json$positiveControlSynthesisArgs$prior, as.data.frame)
  df <- do.call("rbind",df)
  rn <- as.data.frame(rownames(df))
  names(df) <- c("Settings")
  names(rn) <- c("Covariates")
  positiveControlPriorSettings <- cbind(rn,df)
  
  doc <- doc %>%
    officer::body_add_par("Methods", style = "heading 1") %>%
    officer::body_add_par("") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Study Design", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("This study will follow a retrospective, observational, comparative cohort design. We define 'retrospective' to mean the study will be conducted using data already collected prior to the start of the study. We define 'observational' to mean there is no intervention or treatment assignment imposed by the study. We define 'cohort' to mean a set of patients satisfying one or more inclusion criteria for a duration of time. We define 'comparative cohort design' to mean the formal comparison between two cohorts, a target cohort and comparator cohort, for the risk of an outcome during a defined time-period after cohort entry.", style="Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(empiricalPerformanceCitation, prop = style_citation)
      )) %>%
    officer::body_add_par("") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Data Source(s)", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("For each database, provide database full name, version information (if applicable), the start and end dates of data capture, and a brief description of the data source.  Also include information on data storage (e.g. software and IT environment, database maintenance and anti-fraud protection, archiving) and data protection. >>", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(cdmCitation, prop = style_citation)
      )) %>%
    officer::body_add_par("") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Study Populations", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Target Cohort(s) [T]", style = "heading 3") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< Currently cohort definitions need to be grabbed from ATLAS, in a Cohort Definition, Export Tab, from Text View. >>", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_table(targetCohortTable, header = TRUE, style = "Table Professional") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Comparator Cohort(s) [C]", style = "heading 3") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< Currently cohort definitions need to be grabbed from ATLAS, in a Cohort Definition, Export Tab, from Text View. >>", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_table(comparatorCohortTable, header = TRUE, style = "Table Professional") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Outcome Cohort(s) [O]", style = "heading 3") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< Currently cohort definitions need to be grabbed from ATLAS, in a Cohort Definition, Export Tab, from Text View. >>", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_table(outcomeCohortTable, header = TRUE, style = "Table Professional") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Negative Control Cohort(s) [NC]", style = "heading 3") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Negative controls are defined where there is no evidence that either the target or comparator causes the outcome.  See a full list in the 'Negative Control List' section of the appendix.", style="Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_table(negativeControlOutcomeCohortDef, header = TRUE, style = "Table Professional") %>%
    officer::body_add_par("")
  
  if(json$doPositiveControlSynthesis){
    doc <- doc %>%
      officer::body_add_par("Positive Control Cohort(s) [PC]", style = "heading 3") %>%
      officer::body_add_par("") %>%
      officer::body_add_par("In addition to negative control outcomes, we will also include synthetic positive control outcomes. These are outcomes based on the real negative controls, but where the true effect size is artificially increased to a desired effect size by injection of additional, simulated outcomes. To preserve confounding, these additional outcomes are sampled from predicted probabilities generated using a fitted predictive model. For each negative control outcome, three positive control outcomes will be generated with true relative risk is 1.5, 2, and 4. Using both negative and positive controls, we will fit a systematic error model and perform confidence interval calibration.", style = "Normal") %>%
      officer::body_add_par("") %>%
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext("Control Settings", prop = style_table_title)
        )) %>%
      officer::body_add_table(positiveControlControlSettings, header = TRUE, style = "Table Professional") %>%
      officer::body_add_par("") %>%
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext("Prior Settings", prop = style_table_title)
        )) %>%
      officer::body_add_table(positiveControlPriorSettings, header = TRUE, style = "Table Professional") %>%
      officer::body_add_par("") %>%
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext(empiricalCICitation, prop = style_citation)
        ))  %>%
      officer::body_add_par("") %>%
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext("<< Please grab additional information about positive controls from ATLAS.  >>", prop = style_helper_text)
        ))
      
  } else {
    doc <- doc %>%
      officer::body_add_par("Positive Control Cohort(s) [PC]", style = "heading 3") %>%
      officer::body_add_par("") %>%
      officer::body_add_par("This study will not use positive controls.", style = "Normal")
  }
    #```````````````````````````````````````````````````````````````````````````
  doc <- doc %>%
    officer::body_add_par("Other Variables of Interest", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Describe potential demographic characteristics, confounding variables, effect modifiers, and other variables including measured risk factors, comorbidities, concomitant medications, with operational definitions and measurement, diagnostic and pharmacy codes.  >>", prop = style_helper_text)
      )) %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Tools", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("This study will be designed using OHDSI tools and run with R.", style = "Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(rCitation, prop = style_citation)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
    officer::fpar(
      officer::ftext(pleCitation, prop = style_citation)
    )) %>%
    officer::body_add_par("") %>%
    officer::body_add_par("More information about the tools can be found in the Appendix 'Study Generation Version Information'.", style = "Normal") 
  #----------------------------------------------------------------------------- 
    
  #============ Data Analysis Plan =============================================
  dataAnalysisPlan <- paste0("There are ",nTargetIds, 
                             " target cohorts compared to ",nComparatorIds,
                             " comparator cohorts where each pair is evaluated for risk of ",nOutcomeIds,
                             " outcomes for ",nAnalysis,
                             " analysis.  In total there are ",nCompare,
                             " comparisons performed.  For a full list refer to the Appendix 'Complete Analysis List'. ")
  
  doc <- doc %>%
    officer::body_add_par("Data Analysis Plan", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(dataAnalysisPlan, style = "Normal") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< Please grab analysis information from ATLAS. >>", prop = style_helper_text)
      ))
    #```````````````````````````````````````````````````````````````````````````
  # doc <- doc %>%  
  #   officer::body_add_par("Analysis 1", style = "heading 2") %>%
  #   officer::body_add_par("") %>%
  #   officer::body_add_par("Study Population Settings", style = "heading 3") %>%
  #   officer::body_add_par("") %>%
  #   officer::body_add_par("Covariate Settings", style = "heading 3") %>%
  #   officer::body_add_par("") %>%
  #   officer::body_add_par("Time-at-Risk Settings", style = "heading 3") %>%
  #   officer::body_add_par("") %>%
  #   officer::body_add_par("Propensity Score Adjustment", style = "heading 3") %>%
  #   officer::body_add_par("") %>%
  #   officer::body_add_par("Outcome Model", style = "heading 3") %>%
  #   officer::body_add_par("")
  #----------------------------------------------------------------------------- 
  
  #============ DIAGNOSTICS ====================================================
  doc <- doc %>%
    officer::body_add_par("Diagnostics", style = "heading 1") %>%
    officer::body_add_par("") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Sample Size & Study Power", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("A table that displays the study power according to study size and varying study effects is an efficient way to display this information.  The R Package will export a 'Minimum Detectable Relative Risk' (MDRR) table. >>", prop = style_helper_text)
      )) %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Cohort Comparability", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("The R Package will export a 'Preference Score' table and 'Covariate Balance' tables. >>", prop = style_helper_text)
      )) %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Systematic Error Assessment", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Negative Controls", style = "heading 3") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< Produce negative control plots used in empirical calibration and insert here. >>", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_par("Positive Controls", style = "heading 3") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< Produce positive control plots used in empirical calibration and insert here. >>", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") 
  #----------------------------------------------------------------------------- 

  #============ STRENGTHS & LIMITATIONS ========================================
  doc <- doc %>%
    officer::body_add_par("Strengths & Limitations", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) 
  #----------------------------------------------------------------------------- 
  
  #============ PROTECTION OF HUMAN SUBJECTS ===================================
  doc <- doc %>%
    officer::body_add_par("Protection of Human Subjects", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Describe any additional safeguards that are appropriate for the data being used.", 
                       prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("Here is an example statement:", prop = style_helper_text),
        officer::ftext("Confidentiality of patient records will be maintained always. All study reports will contain aggregate data only and will not identify individual patients or physicians. At no time during the study will the sponsor receive patient identifying information except when it is required by regulations in case of reporting adverse events.", prop = style_helper_text),
        officer::ftext(">>", prop = style_helper_text)
      ))
  #----------------------------------------------------------------------------- 
  
  #============ DISSEMINATING & COMMUNICATING ==================================
  doc <- doc %>%
    officer::body_add_par("Plans for Disseminating & Communicating Study Results", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS.", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("List any plans for submission of progress reports, final reports, and publications.", 
                       prop = style_helper_text),
        officer::ftext(">>", 
                       prop = style_helper_text)
      )) %>%
    officer::body_add_break()
  #----------------------------------------------------------------------------- 
  
  #============ APPENDICES =====================================================
  doc <- doc %>%
    officer::body_add_par("Appendices", style = "heading 1") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Study Generation Version Information", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_par(paste0("Skeleton Version:  ",json$skeletonType," - ", json$skeletonVersion),style="Normal") %>%
    officer::body_add_par(paste0("Identifier / Organization: ",json$organizationName),style="Normal") %>%
    officer::body_add_break() %>%   
    officer::body_end_section_continuous() %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Code List", style = "heading 2") %>%
    officer::body_add_par("")
  
  for(i in 1:length(concepts$uniqueConceptSets)){
    conceptSetId <- paste0("Concept Set #",concepts$uniqueConceptSets[[i]]$conceptId,
                           " - ",concepts$uniqueConceptSets[[i]]$conceptName)
    conceptSetTable <- as.data.frame(concepts$uniqueConceptSets[[i]]$conceptExpressionTable)
    
    id <- as.data.frame(concepts$conceptTableSummary[which(concepts$conceptTableSummary$newConceptId == i),]$cohortDefinitionId)
    names(id) <- c("ID")
    cohortsForConceptSet <- cohortDescriptions[cohortDescriptions$ID %in% id$ID,]
    #targetCohortsForConceptSet <- targetCohorts[targetCohorts$`Cohort ID` %in% id$ID,]
    
    #cohortsForConceptSet <- rbind(outcomeCohortsForConceptSet,targetCohortsForConceptSet)
    #cohortsForConceptSet <- cohortsForConceptSet[,1:2]
    
    doc <- doc %>% 
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext(conceptSetId, prop = style_table_title)
        )) %>%
      officer::body_add_table(conceptSetTable[,c(1,2,4,6,7,8,9,10,11,12)], header = TRUE, style = "Table Professional") %>% 
      #officer::body_add_par("") %>%
      #officer::body_add_par("Cohorts that use this Concept Set:", style = "Normal") %>%
      #officer::body_add_par("") %>%
      #officer::body_add_table(cohortsForConceptSet, header = TRUE, style = "Table Professional") %>%
      officer::body_add_par("")
    
  }
    #```````````````````````````````````````````````````````````````````````````
  doc <- doc %>%   
    officer::body_add_break() %>%
    officer::body_end_section_landscape() %>%
    officer::body_add_par("Negative Control List", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_table(negativeControls, header = TRUE, style = "Table Professional") %>%
    officer::body_add_par("") %>%
    #```````````````````````````````````````````````````````````````````````````
    officer::body_add_par("Complete Analysis List", style = "heading 2") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS. >>", prop = style_helper_text)
      )) %>%
    officer::body_add_par("") %>%
    officer::body_add_break()
  #-----------------------------------------------------------------------------

  #============ REFERNCES ======================================================
  doc <- doc %>%
    officer::body_add_par("References", style = "heading 1") %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext("<< To be completed outside of ATLAS. >>", prop = style_helper_text)
      ))
  #----------------------------------------------------------------------------- 
        
  #============ Document Build =================================================
  if(!dir.exists(outputLocation)){
    dir.create(outputLocation, recursive = T)
  }
  print(doc, target = file.path(outputLocation,'protocol.docx'))
  #-----------------------------------------------------------------------------
}

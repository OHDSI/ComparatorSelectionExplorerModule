execute <- function(jobContext) {

  checkmate::assert_list(x = jobContext)
  if (is.null(jobContext$settings)) {
    stop("Analysis settings not found in job context")
  }
  if (is.null(jobContext$sharedResources)) {
    stop("Shared resources not found in job context")
  }
  if (is.null(jobContext$moduleExecutionSettings)) {
    stop("Execution settings not found in job context")
  }

  message("Creating cohort definition set from job context")
  cohortDefinitionSet <- createCohortDefinitionSetFromJobContext(sharedResources = jobContext$sharedResources)

  exportFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
  exportZipFile <- sprintf("Results_%s.zip", jobContext$moduleExecutionSettings$databaseId)

  executionSettings <- ComparatorSelectionExplorer::createExecutionSettings(connectionDetails = jobContext$moduleExecutionSettings$connectionDetails,
                                                                            cohortDefinitionSet = cohortDefinitionSet,
                                                                            databaseId = jobContext$moduleExecutionSettings$databaseId,
                                                                            cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
                                                                            vocabularyDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
                                                                            resultsDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
                                                                            cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
                                                                            cohortTable = jobContext$moduleExecutionSettings$cohortTable,
                                                                            minExposureSize = jobContext$settings$minExposureSize,
                                                                            generateCohortDefinitionSet = FALSE,
                                                                            exportDir = exportFolder,
                                                                            removeExportDir = FALSE,
                                                                            exportZipFile = exportZipFile)
  rlang::inform("Executing")
  ComparatorSelectionExplorer::execute(executionSettings)

  moduleInfo <- ParallelLogger::loadSettingsFromJson("MetaData.json")
  resultsDataModel <- readr::read_csv(file = system.file("settings", "resultsDataModelSpecification.csv", package = "ComparatorSelectionExplorer"),
                                      show_col_types = FALSE)


}

# Private methods -------------------------
createCohortDefinitionSetFromJobContext <- function(sharedResources) {
  cohortDefinitions <- list()
  if (length(sharedResources) <= 0) {
    stop("No shared resources found")
  }
  for (i in 1:length(sharedResources)) {
    if (which(class(sharedResources[[i]]) %in% "CohortDefinitionSharedResources") > 0) {
      cohortDefinitions <- sharedResources[[i]]$cohortDefinitions
      break;
    }
  }
  if (length(cohortDefinitions) <= 0) {
    stop("No cohort definitions found")
  }
  cohortDefinitionSet <- CohortGenerator::createEmptyCohortDefinitionSet()
  for (i in 1:length(cohortDefinitions)) {
    cohortJson <- cohortDefinitions[[i]]$cohortDefinition
    cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
    cohortSql <- CirceR::buildCohortQuery(cohortExpression, options = CirceR::createGenerateOptions(generateStats = FALSE))
    cohortDefinitionSet <- rbind(cohortDefinitionSet, data.frame(cohortId = as.integer(cohortDefinitions[[i]]$cohortId),
                                                                 cohortName = cohortDefinitions[[i]]$cohortName,
                                                                 sql = cohortSql,
                                                                 json = cohortJson,
                                                                 stringsAsFactors = FALSE))
  }
  return(cohortDefinitionSet)
}

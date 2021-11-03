#  14.0 ALL-IN-ONE OPTIMISATION WORKFLOW ----
#' Start an all in one optimisation process
#'
#' @description
#' Start the all in one optimisation workflow
#'
#' @param url API url
#' @param APIKey API key provided
#' @param train training data set
#' @param test test data set
#' @param Independent_Variables regression variables that are used to predict dependent variable
#' @param Dependent_Variable dependent variable for which you want to predict or optimise
#' @param Upstream_Forecasting_Variables a subset of independent variables which you can't easily control
#' @param Process_Control_Variables a subset of independent variables that you want to optimise
#' @param number_clusters number of clusters to search for in upstream forecasting variables
#' @param maximise_dependent_variable logical to indicate whether to maximise or minimise the dependent variable
#' @param monitor_status logical to indicate whether session is to wait for the process to complete or return id for later retrieval
#'
#' @details
#' # Optimisation Workflow
#' - Returns the optimisation job identifier
#' - Setting monitor_status = TRUE means your current session process will
#'   be in a held state until the job is completed.
#' - Setting monitor_status = FALSE means you can continue to use your current
#'   session without waiting for the optimisation to complete.  You will be able
#'   to use the returned jobId to fetch the optimisation results at a later time.
#'
#' @examples
#'
#' \dontrun{
#'
#' all_in_one_jobId <-
#' ClarofyFinalOptimiseStart(
#'                  train = train,
#'                  test = holdout,
#'                  Dependent_Variable = Dependent_Variable,
#'                  Independent_Variables = Independent_Variables,
#'                  Process_Control_Variables = Process_Control_Variables,
#'                  Upstream_Forecasting_Variables = Upstream_Forecasting_Variables,
#'                  number_clusters = 2,
#'                  maximise_dependent_variable = FALSE,
#'                  monitor_status = TRUE,
#'                  url = url,
#'                  APIKey = APIKey)
#'
#' }
#'
#' @export
ClarofyFinalOptimiseStart <- function(url,
                                      APIKey,
                                      train,
                                      test,
                                      Dependent_Variable,
                                      Independent_Variables,
                                      Process_Control_Variables,
                                      Upstream_Forecasting_Variables,
                                      number_clusters,
                                      maximise_dependent_variable = TRUE,
                                      monitor_status = TRUE
                                      ) {

  body <- list(
    Training_Data = train
    ,Testing_Data = test
    ,Dependent_Variable = Dependent_Variable
    ,Independent_Variables = Independent_Variables
    ,Process_Control_Variables = Process_Control_Variables
    ,Upstream_Forecasting_Variables = Upstream_Forecasting_Variables
    ,Prediction_Variable_Name = "Prediction"
    ,Optimised_Prediction_Variable_Name = "Optimised_Prediction"
    ,Model_Selections = c("support vector machine")
    ,Model_Ensemble_Weights = c(1)
    ,TimeStamp_Variable_Name = "TimeStamp"
    ,Number_Of_Clusters = number_clusters
    ,Buffer = 3
    ,Weights = NULL
    ,Feature_Scaling_Method = "z-score normalization"
    ,Maximise_Model_To_Optimise = maximise_dependent_variable
    ,Export_Model_Name = stringr::str_glue("svm_", Sys.Date())
  )

  # Create JSON for Session Jobs
  jsonlite::write_json(x = body, path = "./allinone.json")
#
  # CREATE SESSION
  path <- '/Session/CreateSession'
  bodyPost <- "{}"
  raw.result <- httr::POST(url = url, path = path, body = bodyPost, encode = 'json', httr::add_headers(APIKey = APIKey))
  Data <- rawToChar(raw.result$content)
  print(paste0("[",Sys.time(),"] Session ID: ",Data))
  id <- Data


  # SPAWN SESSION
  path <- '/Session/Interlate_All_In_One_Optimisation_Start'
  bodyPost <- jsonlite::read_json(path="./allinone.json")
  raw.result <- httr::POST(url = url, path = path, body = bodyPost, encode = 'json', httr::add_headers(SessionKey = id))
  Data <- rawToChar(raw.result$content)
  jobId <- Data
  print(paste0("[",Sys.time(),"] Load JSON Result: ",jobId))

  # Save copy of session and job to restore if required
  restore_last_session <- list(
    id = id,
    jobId = jobId
  )

  saveRDS(restore_last_session, file = "restore_last_session")

  if (monitor_status) {

    repeat {
      path <- paste0('/Session/Interlate_Conditional_Optimisation_Progress/', jobId)
      raw.result <- httr::GET(url = url, path = path)
      progress <- rawToChar(raw.result$content)
      print(paste0("[",Sys.time(),"] ",progress))
      if (grepl("Completed", progress)){
        break;
      }
      Sys.sleep(60)

    }

    return(jobId)

  } else {

    return(jobId)

  }

}


#' Check status of all-in-one optimisation workflow
#'
#' @description
#' Returns status of all-in-one optimisation workflow
#'
#' @param url API url
#' @param id Session id created by [CreateSession]
#' @param jobId job id for temporary processes run during a session
#'
#' @details
#' # Optimisation Workflow
#' - Returns optimised predictions for the dependent variable using the optimal
#'   process control variables
#'
#' @examples
#'
#' \dontrun{
#'
#' all_in_one_jobStatus <-
#' ClarofyFinalOptimiseStatus(url = url, id = sessionID, jobId = all_in_one_jobId)
#'
#' }
#'
#' @export
ClarofyFinalOptimiseStatus <- function (url, id, jobId) {

  print(paste0("[",Sys.time(),"] Fetching Job Final Status"))
  path <- paste0('/Session/Interlate_All_In_One_Optimisation_Results/', jobId)
  raw.result <- httr::GET(url = url, path = path, httr::add_headers(SessionKey = id))
  results <- rawToChar(raw.result$content)
  print(paste0("[",Sys.time(),"] Job Status: ",results))

  return(results)

}

#' Get the productivity groups
#'
#' @description
#' Returns the productivity groups from the all-in-one optimisation workflow
#'
#' @param url API url
#' @param id Session id created by [CreateSession]
#'
#' @details
#' # Optimisation Workflow
#' - Returns the producitivity groups from the all-in-one optimisation workflow
#'
#' @examples
#'
#' \dontrun{
#'
#' productivityGroups <- ClarofyGetProductivityGroups(url = url, id = sessionID)
#'
#' }
#'
#' @export
ClarofyGetProductivityGroups <- function(url, id) {

  # Load Productivity Groups
  print(paste0("[",Sys.time(),"] Getting Productivity Groups..."))
  path <- '/Session/SessionData/ProductivityGroups'
  raw.result <- httr::GET(url = url, path = path, httr::add_headers(SessionKey = id))
  productivityGroups <- jsonlite::fromJSON(rawToChar(raw.result$content))

  # Print out the productivity groups to Viewer
  print(productivityGroups[[2]] %>% DT::datatable(options = list(
    initComplete = DT::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#251b29', 'color': '#fff'});",
      "}")
  )))

  return(productivityGroups)
}

#' Get the optimised results
#'
#' @description
#' Returns the optimised results from the all-in-one optimisation workflow
#'
#' @param url API url
#' @param id Session id created by [CreateSession]
#'
#' @details
#' # Optimisation Workflow
#' - Returns the optimised results from the all-in-one optimisation workflow
#' - Returns a time series plot of the predictions
#'
#' @examples
#'
#' \dontrun{
#'
#' FinalOptimisedPredictions <-
#' ClarofyGetFinalOptimisedResults(url = url, id = sessionID)
#'
#' }
#'
#' @export
ClarofyGetFinalOptimisedResults <- function(url, id) {

    print(paste0("[",Sys.time(),"] Getting Optimised Predictions..."))
    path <- '/Session/SessionData/OptimisedPredictions'
    raw.result <- httr::GET(url = url, path = path, httr::add_headers(SessionKey = id))
    optimisedPredictions <- jsonlite::fromJSON(rawToChar(raw.result$content))

    # ** Visualise Optimised Prediction Vs Actual KPI ----
    print(optimisedPredictions %>%
            dplyr::select(TimeStamp, Dependent_Variable, Prediction, Optimised_Prediction, type) %>%
            dplyr::mutate(TimeStamp = lubridate::ymd_hms(TimeStamp)) %>%
            tidyr::pivot_longer(-c(TimeStamp, type)) %>%
            timetk::plot_time_series(.date_var = TimeStamp,
                             .value = value,
                             .color_var = name,
                             .line_alpha = 0.4,
                             .facet_vars = type,
                             .smooth = FALSE,
                             .title = "Modeled vs Optimised Predictions for Training and Holdout Dataset"))

    return(optimisedPredictions)

}

#' Get the sensitivity analysis from the all-in-one optimisation workflow
#'
#' @description
#' Returns the sensitivity estimates from the all-in-one optimisation workflow
#'
#' @param url API url
#' @param id Session id created by [CreateSession]
#'
#' @details
#' # Optimisation Workflow
#' - Returns the sensitivity estimates from the all-in-one optimisation workflow.
#' - The values of each process control variable in the recommendation results is
#'   used to replace the values of the identical process control variable in the
#'   original dataset.
#' - The modified dataset is passed into the trained Prediction model and the
#'   predicted value for the dependent variable is stored.
#' - The percentage contribution of each individual optimised process control
#'   variable to the overall optimised prediction is returned as sensitivity
#'   estimates.
#'
#' @examples
#'
#' \dontrun{
#'
#' sensitivityAnalysis <-
#' ClarofyGetSensitivityAnalysis(url = url, id = sessionID)
#'
#' }
#'
#' @export
ClarofyGetSensitivityAnalysis <- function(url, id) {

  # Sensitivity Analysis ----
  print(paste0("[",Sys.time(),"] Getting Sensitivity Analysis..."))
  path <- '/Session/SessionData/SensitivityAnalysis'
  raw.result <- httr::GET(url = url, path = path, httr::add_headers(SessionKey = id))
  sensitivityAnalysis <- jsonlite::fromJSON(rawToChar(raw.result$content))

  return(sensitivityAnalysis)

}



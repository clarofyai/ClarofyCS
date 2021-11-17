# API Client Side Functions ----
# 1.0 CREATE SESSION FUNCTION ----
#' Call Clarofy API endpoints
#'
#' @description
#' Creates a new session to use the Clarofy API
#'
#' @param APIKey API key provided
#' @param url API url
#'
#' @details
#' # All In One Optimisation Workflow
#' - Creates New API Session
#'
#' @examples
#'
#' \dontrun{
#'
#' url <- "SUBSTITUTE-API-URL"
#' APIKEY <- "YOUR-API-KEY"
#' sessionID <- CreateSession(APIKey = APIKey, url = url)
#'
#' }
#' @export
CreateSession <- function(APIKey, url = "http://127.0.0.1:8000") {

  if (url == "http://127.0.0.1:8000") {

    url <- "http://127.0.0.1:8000"
    if (Sys.info()['sysname'] == "Linux")
    {
      url <- "http://127.0.0.1:18000"
    }
  }

  path <- '/Session/CreateSession'
  print(path)
  bodyPost <- "{}"
  raw.result <- httr::POST(url = url, path = path, body = bodyPost, encode = 'json', httr::add_headers(APIKey = APIKey))
  sessionIDData <- rawToChar(raw.result$content)
  print(paste0("[",Sys.time(),"] Session ID: ",sessionIDData))
  id <- sessionIDData

  return (id)

}


# # 2.0 IMPORT DATA FUNCTION ----
# removed this private testing function for  public release

# # 3.0 LOAD DATAFRAME INTO SESSION FUNCTION ----
#' Load data into session
#'
#' @description
#' Load data into the Clarofy API session
#'
#' @param df Dataframe to be loaded into session
#' @param url API url
#' @param id Session id created by [CreateSession]
#'
#' @details
#' # Optimisation Workflow
#' - Load data into session
#'
#' @examples
#'
#' \dontrun{
#'
#' loadStatus <- LoadDataFrameIntoSession(
#' df      = Data,
#' url     = url,
#' id      = sessionID)
#'
#' }
#'
#' @export
LoadDataFrameIntoSession <- function(df, url, id) {

  dataForLoading <- list(Data = df)

  # LOAD DATA FROM JSON TO SESSION (via JOB)
  # Returns load status

  path <- '/Session/Interlate_Data_Load_From_JSON'
  bodyPost <- jsonlite::toJSON(dataForLoading)
  print(paste0("[",Sys.time(),"] Pre: ",id))
  raw.result <- httr::POST(url = url, path = path,
                           body = bodyPost, encode = 'json',
                           httr::add_headers(SessionKey = id))

  print(paste0("[",Sys.time(),"] Post: ",id))
  jobIdData <- rawToChar(raw.result$content)

  print(raw.result$status_code)

  jobId <- jobIdData
  print(paste0("[",Sys.time(),"] Load JSON Result: ",jobId))
  return(jobId)
}

# GET LOADED DATA FUNCTION ----
#' Get loaded data back from session
#'
#' @description
#' Get data back from a Clarofy API session
#'
#' @param url API url
#' @param id Session id created by [CreateSession]
#'
#' @details
#' # Optimisation Workflow
#' - Get raw data back from session
#'
#' @examples
#'
#' \dontrun{
#'
#' getData <- GetData(id = sessionID, url = url)
#'
#' }
#'
#' @export
GetData <- function(id, url) {

  path <- '/Session/SessionData/Data'
  print(path)
  tempdf <- data.frame(id)
  bodyPost <- jsonlite::toJSON(tempdf)
  raw.result <- httr::POST(url = url, path = path, body = bodyPost, encode = 'json', httr::add_headers(SessionKey = id))

  getData <- jsonlite::fromJSON(rawToChar(raw.result$content))

  print(raw.result$status_code)

  getData <- jsonlite::fromJSON(rawToChar(raw.result$content))

  getData <- getData %>% dplyr::mutate(TimeStamp = lubridate::ymd_hms(TimeStamp))

  return(getData)

}

# 4.0 GET SUMMARY OF LOADED DATA ----
#' Get loaded data back from session
#'
#' @description
#' Get data back from a Clarofy API session
#'
#' @param url API url
#' @param id Session id created by [CreateSession]
#'
#' @details
#' # Optimisation Workflow
#' - Get raw data back from session
#'
#' @examples
#'
#' \dontrun{
#'
#' Summary <- GetSummaryOfSessionData(url  = url, id   = sessionID)
#'
#' }
#'
#' @export
GetSummaryOfSessionData <- function(id, url) {

  path <- paste0('/Session/Interlate_Data_Summary')
  print(paste0("[",Sys.time(),"] Next Command: ",path))

  raw.result <- httr::GET(url = url, path = path, httr::add_headers(SessionKey = id))
  summaryInfo <- rawToChar(raw.result$content)
  print(paste0("[",Sys.time(),"] /Interlate_Data_Summary"))
  Summary <- jsonlite::fromJSON(summaryInfo)

  print(DT::datatable(Summary[[2]], options = list(
    initComplete = DT::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#251b29', 'color': '#fff'});",
      "}")
  )))

}

# 5.0 LOAD TRAINING DATA INTO SESSION ----
#' Load training data into session
#'
#' @description
#' Load training data into Clarofy API session
#'
#' @param train Model training data
#' @param url API url
#' @param id Session id created by [CreateSession]
#'
#' @details
#' # Optimisation Workflow
#' - Get raw data back from session
#'
#' @examples
#'
#' \dontrun{
#'
#' train_load_status <- LoadTrainingDataIntoSession(train = train, url = url, id = sessionID)
#'
#' }
#'
#' @export
LoadTrainingDataIntoSession <- function(train, url, id) {

  dataForLoading <- list(Data = train)

  # LOAD DATA FROM JSON TO SESSION (via JOB)

  path <- '/Session/Interlate_TrainingData_Load_From_JSON'
  print(paste0("[",Sys.time(),"] Next Command: ",path))

  bodyPost <- jsonlite::toJSON(dataForLoading)
  raw.result <- httr::POST(url = url, path = path, body = bodyPost, encode = 'json', httr::add_headers(SessionKey = id))
  jobIdData <- rawToChar(raw.result$content)
  jobId <- jobIdData
  print(paste0("[",Sys.time(),"] Load JSON Result: ",jobId))

  return(jobId)

}

# 6.0 LOAD TESTING DATA INTO SESSION ----
#' Load testing data into session
#'
#' @description
#' Load testing data into Clarofy API session
#' Returns status of load
#'
#' @param test Model testing data
#' @param url API url
#' @param id Session id created by [CreateSession]
#'
#' @details
#' # Optimisation Workflow
#' - Get raw data back from session
#'
#' @examples
#'
#' \dontrun{
#'
#' test_load_status <- LoadTestingDataIntoSession(test = holdout, url = url, id = sessionID)
#'
#' }
#'
#' @export
LoadTestingDataIntoSession <- function(test, url, id) {

  dataForLoading <- list( Data = test)

  path <- '/Session/Interlate_TestingData_Load_From_JSON'
  print(path)
  bodyPost <- jsonlite::toJSON(dataForLoading)
  raw.result <- httr::POST(url = url, path = path, body = bodyPost, encode = 'json', httr::add_headers(SessionKey = id))
  jobIdData <- rawToChar(raw.result$content)
  jobId <- jobIdData
  print(paste0("[",Sys.time(),"] Load JSON Result: ",jobId))

  return(jobId)
}

# # 7.0  CLEANSE DATA ----
#' Cleanse session training data using density cleaning operation
#'
#' @description
#' Cleanse training data using density cleansing operation
#' Returns cleaned data with outliers replaced with NAs.
#' Does not alter training data.
#' Use [LoadTrainingDataIntoSession] if wanting to train the model on this cleansed data.
#' NB: Training data must not contain NA values.
#'
#' @param Independent_Variables regression variables that are used to predict dependent variable
#' @param Dependent_Variable dependent variable for which you want to predict or optimise
#' @param url API url
#' @param id Session id created by [CreateSession]
#'
#' @details
#' # Optimisation Workflow
#' - Custom Clarofy cleaning algorithm for simultaneous cleaning of all variables.
#' - This function cleansing the uploaded training data but does not change it.
#' - The returned cleansed dataset can be inspected, compared and loaded as new training data if desired.
#' - The best practice is to train models on cleaned data and test on uncleaned data.
#'
#' @examples
#'
#' \dontrun{
#'
#' Data_Cleaned <- CleanseAllTestingData(
#'    Dependent_Variable = Dependent_Variable,
#'    Independent_Variables = Independent_Variables,
#'    url = url, id = sessionID)
#'
#' }
#' @import httr
#' @export
CleanseAllTestingData <- function(Dependent_Variable,
                                  Independent_Variables,
                                  url, id) {

  path <- '/Session/Interlate_Data_Density_Cleansing'
  print(paste0("[",Sys.time(),"] Next Command: ",path))

  body <- list(
    TimeStamp_Variable_Name = "TimeStamp"
    ,Variables_To_Clean = c(Dependent_Variable, Independent_Variables)
    ,Extreme_Value_Filter = TRUE
    ,Density_Filter = TRUE
    ,Left_Peak_Filter = FALSE
    ,Large_Gap_Filter = FALSE
    ,Extreme_Value_Filter_Multiplier = 2
    ,Density_Filter_Threshold = 0.25
    ,Left_Peak_Filter_Threshold = 0.15
    ,Large_Gap_Filter_Threshold = 0.2
    ,Zero_Identification_Threshold = 0.005
  )


  raw.result <- httr::POST(url = url, path = path, body = body, encode = 'json', httr::add_headers(SessionKey = id))
  Data_Cleaned <- jsonlite::unserializeJSON(rawToChar(raw.result$content))

  Data_Cleaned <- Data_Cleaned %>%
    dplyr::mutate(TimeStamp = lubridate::ymd_hms(TimeStamp))

  return(Data_Cleaned)

}

# # 9.0  TRAIN A MODEL ----
#' Train a model on the session training data
#'
#' @description
#' Train a model on the session training data
#' Returns returns details for the trained model
#'
#' @param Independent_Variables regression variables that are used to predict dependent variable
#' @param Dependent_Variable dependent variable for which you want to predict or optimise
#' @param url API url
#' @param id Session id created by [CreateSession]
#'
#' @details
#' # Optimisation Workflow
#' - Train a model for optimisation of your process control variables
#'
#' @examples
#'
#' \dontrun{
#'
#' Model_Trainer_Output <- ClarofyFit(Dependent_Variable = Dependent_Variable,
#'                                   Independent_Variables = Independent_Variables,
#'                                   url = url,
#'                                   id = sessionID)
#'
#' }
#'
#' @import httr
#' @export
ClarofyFit <- function(Dependent_Variable,
                       Independent_Variables,
                       url, id) {

  path <- '/Session/Interlate_Model_Trainer'
  print(paste0("[",Sys.time(),"] Next Command: ",path))


  body <- list(
    Dependent_Variable = Dependent_Variable
    ,Independent_Variables = Independent_Variables
    ,Model_Selections = c("support vector machine")
  )

  raw.result1 <- httr::POST(url = url, path = path, body = body, encode = 'json', httr::add_headers(SessionKey = id))
  Model_Trainer_Status <- jsonlite::fromJSON(rawToChar(raw.result1$content))

  print(Model_Trainer_Status)


  # Fetch model data from sessiondata

  path <- '/Session/SessionData/ModelData'
  print(paste0("[",Sys.time(),"] Next Command: ",path))

  tempdf <- data.frame(id)
  bodyPost <- jsonlite::toJSON(tempdf)
  raw.result <- httr::POST(url = url, path = path, body = bodyPost, encode = 'json', httr::add_headers(SessionKey = id))
  md <- rawToChar(raw.result$content)
  md <- jsonlite::fromJSON(md)

  # Restore the model list after grabbing the model data from the api,
  # we must convert it from base64 back into an object.

  # Convert Base64 back to binary
  md$model_list <- jsonlite::base64_dec(md$model_list)
  # Generate temp filename
  tempFileName <- paste("./", stringr::str_trim(id), "_model_list_resurrection.rds")
  # Open write/binary connection to tempfile
  tempFile = file(tempFileName, "wb")
  # Write the binary data out to that temp file
  writeBin(md$model_list, tempFile)
  # Close temp file
  close(tempFile)
  # Overwrite the model_list by using readRDS to load the data from the temp file
  md$model_list <- readRDS(tempFileName)
  # Remove temp file
  file.remove(tempFileName)
  Model_Trainer_Output <- md
  Model_Trainer_Output

  # Model_Trainer_Output$model_list

  print(raw.result$status_code)

  return(Model_Trainer_Output)


}

# # 10.0 MAKE PREDICTIONS WITH MODEL ----
#' Predict the dependent variable using the built model
#'
#' @description
#' Predict the dependent variable
#'
#' @param Dependent_Variable dependent variable for which you want to predict or optimise
#' @param url API url
#' @param id Session id created by [CreateSession]
#' @param df dataframe used to make new predictions
#'
#' @details
#' # Optimisation Workflow
#' - Make predictions with trained model
#'
#' @examples
#'
#' \dontrun{
#'
#' Data_Predictions_train <- ClarofyPredict(Dependent_Variable = Dependent_Variable,
#' url = url, id = sessionID, data = train)
#'
#' }
#'
#' @export
ClarofyPredict <- function(Dependent_Variable, url, id, df) {

  LoadTestingDataIntoSession(test = df, url = url, id = id)

  # * 7.1 Making predictions on data  ----

  path <- '/Session/Interlate_Prediction'
  print(paste0("[",Sys.time(),"] Next Command: ",path))


  body <- list(
    Prediction_Variable_Name = "Prediction"
    ,Dependent_Variable = Dependent_Variable
    ,TimeStamp_Variable_Name = "TimeStamp"
  )

  raw.result <- httr::POST(url = url, path = path, body = body, encode = 'json', httr::add_headers(SessionKey = id))
  Data_Predictions_train <- jsonlite::fromJSON(rawToChar(raw.result$content))


  # Data_Predictions_train %>% glimpse()

  print(raw.result$status_code)

  # Fetch Prediction Data

  print("# Fetch Prediction Data")

  path <- '/Session/SessionData/FetchPredictions'
  print(paste0("[",Sys.time(),"] Next Command: ",path))

  tempdf <- data.frame(id)
  bodyPost <- jsonlite::toJSON(tempdf)
  raw.result <- httr::POST(url = url, path = path, body = bodyPost, encode = 'json', httr::add_headers(SessionKey = id))

  Data_Predictions <- jsonlite::fromJSON(rawToChar(raw.result$content))

  # Data_Predictions %>% glimpse()

  Data_Predictions <- Data_Predictions %>% dplyr::mutate(TimeStamp = lubridate::ymd_hms(TimeStamp))

  return(Data_Predictions)

}

# # 11.0 GENERATE PRODUCTIVITY GROUPS ----
#' Generate productivity groups using the training data set
#'
#' @description
#' Productivity groups are used to generate optimised process control variable
#' recommendations for common operating scenarios.  A productivity group can be
#' thought of as a pattern of upstream forecasting variables that may be reflective
#' of different feed types or blends.
#'
#' @param Dependent_Variable dependent variable for which you want to predict or optimise
#' @param Upstream_Forecasting_Variables a subset of independent variables which you can't easily control
#' @param url API url
#' @param id Session id created by [CreateSession]
#' @param number_clusters number of clusters to search for in upstream forecasting variables
#'
#' @details
#' # Optimisation Workflow
#' - Generated productivity groups for use in optimisation
#'
#' @examples
#'
#' \dontrun{
#'
#' ProductivityGroups <- ClarofyGroup(Dependent_Variable = Dependent_Variable,
#' Upstream_Forecasting_Variables = Upstream_Forecasting_Variables,
#' url = url, id = sessionID, number_clusters = 10)
#'
#' }
#'
#' @export
ClarofyGroup <- function(Dependent_Variable,
                         Upstream_Forecasting_Variables,
                         url,
                         id,
                         number_clusters = 5) {

  print("# Productivity Group Generation ----")
  path1 <- '/Session/Interlate_Productivity_Group_Generation'
  print(paste0("[",Sys.time(),"] Next Command: ",path1))

  body <- list(
    Dependent_Variable = Dependent_Variable
    ,Independent_Variables =  Upstream_Forecasting_Variables
    ,Number_Of_Clusters = number_clusters
    ,Weights = NULL
    ,Feature_Scaling_Method = "z-score normalization"
    ,TimeStamp_Variable_Name= "TimeStamp"
  )

  raw.result <- httr::POST(url = url, path = path1, body = body, encode = 'json', httr::add_headers(SessionKey = id))
  something <- rawToChar(raw.result$content)

  print(raw.result$status_code)

  print("Fetching /Session/SessionData/ProductivityGroupDefinitions 1")
  path2 <- '/Session/SessionData/ProductivityGroupDefinitions'
  print(paste0("[",Sys.time(),"] Next Command: ",path2))

  tempdf <- data.frame(id)
  bodyPost <- jsonlite::toJSON(tempdf)
  raw.result <- httr::POST(url = url, path = path2, body = bodyPost, encode = 'json', httr::add_headers(SessionKey = id))
  Clusters <- rawToChar(raw.result$content)
  Clusters <- jsonlite::fromJSON(Clusters)

  print(Clusters[[2]] %>% DT::datatable(options = list(
    initComplete = DT::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#251b29', 'color': '#fff'});",
      "}")
  )))

  return(Clusters)

}

# 13.0 OPTIMISE GROUP ----
#' Start optimisation job using a single productivity group or time weighted aggregate
#'
#' @description
#' Commence an optimisation job.
#'
#' @param Dependent_Variable dependent variable for which you want to predict or optimise
#' @param Process_Control_Variables a subset of independent variables that you want to optimise
#' @param Upstream_Forecasting_Variables a subset of independent variables which you can't easily control
#' @param upstream_values the upstream variables from a selected productivity group or aggregate
#' @param maximise_dependent_variable logical to indicate whether to maximise or minimise the dependent variable
#' @param monitor_status logical to indicate whether session is to wait for the process to complete or return id for later retrieval
#' @param url API url
#' @param id Session id created by [CreateSession]
#'
#' @details
#' # Optimisation Workflow
#' - Returns the jobId for later retrieval of the optimal values
#'
#' @examples
#'
#' \dontrun{
#'
#' jobId <- ClarofyOptimiseStartJob(Dependent_Variable = Dependent_Variable,
#'                                  Upstream_Forecasting_Variables = Upstream_Forecasting_Variables,
#'                                  Process_Control_Variables = Process_Control_Variables,
#'                                  upstream_values = upstream_values,
#'                                  maximise_dependent_variable = FALSE,
#'                                  monitor_status = TRUE,
#'                                  url = url,
#'                                  id = sessionID)
#'
#' }
#'
#' @export
ClarofyOptimiseStartJob <- function(Dependent_Variable,
                                    Process_Control_Variables,
                                    Upstream_Forecasting_Variables,
                                    upstream_values,
                                    maximise_dependent_variable = TRUE,
                                    monitor_status = TRUE,
                                    url,
                                    id) {

  print("# Conditional Optimisation Started ----")

  path <- '/Session/Interlate_Conditional_Optimisation_Start'
  print(paste0("[",Sys.time(),"] Next Command: ",path))

  body <- list(
    Dependent_Variable = Dependent_Variable
    ,Process_Control_Variables = Process_Control_Variables
    ,Upstream_Forecasting_Variables = Upstream_Forecasting_Variables
    ,Upstream_Forecasting_Variables_Centroids = upstream_values
    ,Maximise_Model_To_Optimise = maximise_dependent_variable
  )

  raw.result <- httr::POST(url = url, path = path, body = body, encode = 'json', httr::add_headers(SessionKey = id))

  # The jobId can be used to fetch optimisation results from session at a later time
  jobId <- rawToChar(raw.result$content)

  print(paste0("[",Sys.time(),"] Interlate_Conditional_Optimisation Job ID: ",jobId))

  # Save copy of session and job to restore if required
  restore_last_session <- list(
    id = id,
    jobId = jobId
  )

  saveRDS(restore_last_session, file = "restore_last_session.rds")

  if (monitor_status) {

    # Setting monitor_status = TRUE means your current session process will
    # be in a held state until the job is completed.
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

#' Check on status of conditional optimisation job
#'
#' @description
#' Returns the status of the optimisation job
#'
#' @param url API url
#' @param id Session id created by [CreateSession]
#' @param jobId job id for temporary processes run during a session
#'
#' @details
#' # Optimisation Workflow
#' - Returns status of optimisation job
#'
#' @examples
#'
#' \dontrun{
#'
#' ClarofyOptimiseJobStatus(url = url, id = sessionID, jobId = jobId)
#'
#' }
#'
#' @export
ClarofyOptimiseJobStatus <- function(url, id, jobId) {

  print(paste0("[",Sys.time(),"] Fetching Job Final Status"))
  path <- paste0('/Session/Interlate_Conditional_Optimisation_Results/', jobId)
  raw.result <- httr::GET(url = url, path = path, httr::add_headers(SessionKey = id))
  results <- rawToChar(raw.result$content)
  print(paste0("[",Sys.time(),"] Job Status: ",results))

  # print(results %>% dplyr::glimpse())

  print(raw.result$status_code)

  # Once the job is completed the jobId is no longer active in the session

  return(results)

}

#' Collect recommendations resulting from the completed optimisation job
#'
#' @description
#' Collect optimal results from job
#'
#' @param url API url
#' @param id Session id created by [CreateSession]
#'
#' @details
#' # Optimisation Workflow
#' - Returns optimal results from completed job
#'
#' @examples
#'
#' \dontrun{
#'
#' ProcessControlRecommendations <- ClarofyRecommend(url = url, id = sessionID)
#'
#' }
#'
#' @export
ClarofyRecommend <- function(url, id) {

  print("Fetching /Session/SessionData/ConditionalOptimisations")
  path <- '/Session/SessionData/ConditionalOptimisations'
  print(paste0("[",Sys.time(),"] Next Command: ",path))

  tempdf <- data.frame(id)
  bodyPost <- jsonlite::toJSON(tempdf)
  raw.result <- httr::POST(url = url, path = path, body = bodyPost, encode = 'json', httr::add_headers(SessionKey = id))
  Optimisations <- rawToChar(raw.result$content)
  Optimisations <- jsonlite::fromJSON(Optimisations)

  # Optimisations %>% glimpse()
  #
  # print("# **************************************************")
  # print(Optimisations$OptimalResult)
  # print("# **************************************************")
  #
  # Summary of Process Control Recommendations
  print(t(Optimisations$OptimalResult) %>% DT::datatable(options = list(
    initComplete = DT::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#251b29', 'color': '#fff'});",
      "}")
  )))

  return(Optimisations$OptimalResult)

}

#' Collect optimised predictions for the dependent variable if using the optimum values
#'
#' @description
#' Collect optimal results from job
#'
#' @param url API url
#' @param id Session id created by [CreateSession]
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
#' OptimisedPredictions <- ClarofyOptimisedPredictions(url = url, id = sessionID)
#'
#' }
#'
#' @export
ClarofyOptimisedPredictions <- function(url, id) {

  # Predict Dependent Variable Using Optimised process control variables
  print("Fetching /Session/SessionData/OptimisedPredictions")
  path <- '/Session/SessionData/OptimisedPredictions'
  print(paste0("[",Sys.time(),"] Next Command: ",path))

  tempdf <- data.frame(id)
  bodyPost <- jsonlite::toJSON(tempdf)
  raw.result <- httr::POST(url = url, path = path, body = bodyPost, encode = 'json', httr::add_headers(SessionKey = id))
  OptimisedPredictions <- rawToChar(raw.result$content)
  OptimisedPredictions <- jsonlite::fromJSON(OptimisedPredictions) %>%
    dplyr::mutate(TimeStamp = lubridate::ymd_hms(TimeStamp))

  g <- OptimisedPredictions %>%
    dplyr::select(TimeStamp, Optimised_Prediction, Optim_Value) %>%
    dplyr::left_join(Data_Predictions_train, by = c("TimeStamp" = "TimeStamp")) %>%
    dplyr::filter(Prediction > 0.02) %>%
    dplyr::select(TimeStamp, !! sym(Dependent_Variable), Optimised_Prediction, Prediction) %>%
    tidyr::pivot_longer(-TimeStamp) %>%
    ggplot2::ggplot(ggplot2::aes(TimeStamp, value, color = name)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Modeled vs Optimised Predictions for Training Dataset",
         subtitle = "Using Optimum for Selected Productivity Group",
         color = "",
         y = "Dependent Variable")

  print(plotly::ggplotly(g))

  return(OptimisedPredictions)

}


#' Plot optimisation results
#'
#' @description
#' This function plots the range of optimisation results
#'
#' @param url API url
#' @param id Session id created by [CreateSession]
#' @param Dependent_Variable dependent variable for which you want to predict or optimise
#' @param ncol Number of columns on the plot
#'
#' @details
#' # Optimisation Workflow
#' - Plots the optimisation results
#'
#' @examples
#'
#' \dontrun{
#'
#' ClarofyPlotOptimisationRun(
#'     Dependent_Variable = Dependent_Variable,
#'     url = url,
#'     id = sessionID)
#'
#' }
#'
#' @export
ClarofyPlotOptimisationRun <- function(Dependent_Variable, ncol, url, id) {

  getData <- GetData(id = id, url = url)

  print("Fetching /Session/SessionData/ConditionalOptimisations")
  path <- '/Session/SessionData/ConditionalOptimisations'
  print(paste0("[",Sys.time(),"] Next Command: ",path))

  df <- data.frame(id)
  bodyPost <- jsonlite::toJSON(df)
  raw.result <- httr::POST(url = url, path = path, body = bodyPost, encode = 'json', httr::add_headers(SessionKey = id))
  Optimisations <- rawToChar(raw.result$content)
  Optimisations <- jsonlite::fromJSON(Optimisations)

  Complete_Optimisation_Results_Tbl <- Optimisations$Results %>%
    dplyr::mutate(run_type = "Cycles") %>%
    dplyr::bind_rows(Optimisations$OptimalResult %>%
                dplyr::mutate(run_type = "Final"))

  print(plot_optimisation(
    data = Complete_Optimisation_Results_Tbl,
    kpi = Dependent_Variable,
    ncol = ncol,
    max_value = getData %>% dplyr::summarise(max(!! sym(Dependent_Variable), na.rm = TRUE)) %>% dplyr::pull(),
    min_value = getData %>% dplyr::summarise(min(!! sym(Dependent_Variable), na.rm = TRUE)) %>% dplyr::pull()
  ))

  return(Complete_Optimisation_Results_Tbl)

}


#' Finish the current session
#'
#' @description
#' This function closed the current session
#'
#' @param url API url
#' @param id Session id
#'
#' @details
#' - Closes the current session
#'
#' @examples
#'
#' \dontrun{
#'
#' ClarofyEndSession(url = url, id = sessionID)
#'
#' }
#'
#' @export
ClarofyEndSession <- function(url, id) {

  path <- '/Session/FinishSession'
  df <- data.frame(id)
  bodyPost <- jsonlite::toJSON(df)
  raw.result <- httr::POST(url = url, path = path, body = bodyPost, encode = 'json')
  Data <- rawToChar(raw.result$content)
  Data <- jsonlite::fromJSON(Data)
  print(paste0("[",Sys.time(),"] Finish Session Result: ", Data))


}

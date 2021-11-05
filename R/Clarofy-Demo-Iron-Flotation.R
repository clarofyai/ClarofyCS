# Test ClarofyCS API Client Side Functions ----
# This script can be run in conjunction with an active Clarofy API Key and
# valid API url provided to each user.

{
library(ClarofyCS)
library(DT)
library(rsample)
library(timetk)
library(dplyr)
library(tidyr)
library(ggplot2)
library(yardstick)
library(janitor)
library(lubridate)
library(stringr)


url <- "enter-api-url"
APIKey <- "enter-api-key"
}



# 1.0 CREATE SESSION FUNCTION ----

sessionID <- CreateSession(APIKey = APIKey, url = url)
sessionID

# 2.0 Set Up Global Input Definitions ----
{
  Dependent_Variable = c('XIronConcentrate')
  
  Upstream_Forecasting_Variables <- c('XIronFeed'
                                      ,'XSilicaFeed'
                                      ,'OrePulpFlow'
                                      ,'OrePulppH'
                                      ,'OrePulpDensity'
                                      ,'FlotationColumn01AirFlow'
                                      ,'FlotationColumn02AirFlow'
                                      ,'FlotationColumn03AirFlow'
                                      ,'FlotationColumn04AirFlow'
                                      ,'FlotationColumn05AirFlow'
                                      ,'FlotationColumn06AirFlow'
                                      ,'FlotationColumn07AirFlow'
                                      ,'FlotationColumn01Level'
                                      ,'FlotationColumn02Level'
                                      ,'FlotationColumn03Level'
                                      ,'FlotationColumn04Level'
                                      ,'FlotationColumn05Level'
                                      ,'FlotationColumn06Level'
                                      ,'FlotationColumn07Level')
  
  Process_Control_Variables <- c('StarchFlow'
                                 ,'AminaFlow')
  
  
  
  Independent_Variables <- c(Upstream_Forecasting_Variables, Process_Control_Variables)
}

# 3.0 IMPORT/LOAD DATA ----

databaseData <- iron_flotation_data

databaseData %>% glimpse()


# Data Prep ----
{
  # Data Prep ----
  Data_prep <- databaseData %>%
    mutate(TimeStamp = ymd_hms(TimeStamp)) %>%
    arrange(TimeStamp)
  
  splits <- initial_time_split(Data_prep, prop = 0.8)
  
  splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(TimeStamp, !! sym(Dependent_Variable))
  
  train <- training(splits)
  holdout <- testing(splits)
}

# 3.0 LOAD DATAFRAME INTO SESSION FUNCTION ----

# Test Load Dataframe and Get Load Status ----
loadStatus <- LoadDataFrameIntoSession(df      = databaseData, 
                                       url     = url, 
                                       id      = sessionID)  

loadStatus


# GET LOADED DATA FUNCTION ----

getData <- GetData(id = sessionID, url = url)

getData %>% glimpse()


# 4.0 GET SUMMARY OF LOADED DATA ----

Summary <- GetSummaryOfSessionData(url  = url, 
                                   id   = sessionID)


# 5.0 LOAD TRAINING DATA INTO SESSION ----

train_load_status <- LoadTrainingDataIntoSession(train = train, 
                                                 url = url, 
                                                 id = sessionID)

train_load_status


# 6.0 LOAD TESTING DATA INTO SESSION ----

test_load_status <- LoadTestingDataIntoSession(test = holdout, 
                                               url = url, 
                                               id = sessionID)

test_load_status


# 7.0 CLEAN ALL VARIABLES USING DATA DENSITY CLEANSING OPERATION ----


Data_Cleaned <- CleanseAllTestingData(Dependent_Variable = Dependent_Variable,
                                      Independent_Variables = Independent_Variables,
                                      url = url, id = sessionID)

Data_Cleaned %>% glimpse()

# 8.0 COMPARE CLEANED DATA WITH RAW TRAINING DATA ----
{
raw_pivot <- train %>%
  pivot_longer(cols = contains("Column"),
               names_to = "metric",
               values_to = "value") %>%
  mutate(type = "raw")

clean_pivot <- Data_Cleaned %>%
  pivot_longer(cols = contains("Column"),
               names_to = "metric",
               values_to = "value") %>%
  mutate(type = "filtered")

raw_pivot %>%
  bind_rows(clean_pivot) %>%
  ggplot(aes(value, fill = type)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ metric, scales = "free") +
  theme_minimal()


data_compare <- Data_Cleaned %>%
  left_join(databaseData, by = c("TimeStamp" = "TimeStamp")) %>%
  select(TimeStamp, starts_with(Dependent_Variable)) %>%
  rename(Cleaned_KPI = str_glue("{Dependent_Variable}.x"),
         Original_KPI = str_glue("{Dependent_Variable}.y"))

data_compare %>%
  pivot_longer(-TimeStamp) %>%
  plot_time_series(.date_var = TimeStamp,
                   .value = value,
                   .color_var = name,
                   .line_alpha = 0.4,
                   # .facet_vars = name,
                   .smooth = FALSE,
                   .title = "Comparison of Cleaned and Original Datasets")

}

# 9.0  TRAIN A MODEL ----

Model_Trainer_Output <- ClarofyFit(Dependent_Variable = Dependent_Variable,
                                   Independent_Variables = Independent_Variables,
                                   url = url,
                                   id = sessionID)

Model_Trainer_Output

# 10.0 MAKE PREDICTIONS WITH MODEL ----

# Training Data predictions - in sample
Data_Predictions_train <- ClarofyPredict(Dependent_Variable = Dependent_Variable,
                                         url = url, id = sessionID, df = train)


# 11.0 PLOT PREDICTIONS VS ACTUAL OBSERVATIONS ----
{
  custom_metrics_reg <- yardstick::metric_set(yardstick::mae, yardstick::rmse, yardstick::rsq)
  
  print('Custom metrics function')
  galaxy_model_metrics_train <- Data_Predictions_train %>%
    custom_metrics_reg(!! sym(Dependent_Variable), "Prediction")
  
  print('Custom metrics function')
  galaxy_model_metrics_train %>%
    clean_names(case = 'title') %>%
    select(-Estimator) %>%
    mutate(Metric = toupper(Metric),
           Estimate = round(Estimate, digits = 3)) %>%
    datatable(options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#251b29', 'color': '#fff'});",
        "}")
    ))
  
  Data_Predictions_train %>%
    filter(!!sym(Dependent_Variable) > 0.025) %>%
    ggplot(aes(!! sym(Dependent_Variable),Prediction)) +
    geom_point(alpha = 0.5) +
    geom_abline(color = 'blue', linetype = 2) +
    tune::coord_obs_pred() +
    labs(title = "R squared plot for evaluating model performance - In Sample",
         subtitle = "Actual vs Model",
         x = 'Actual KPI', y = 'Model Predicted KPI')
}
{
Data_Predictions_train %>%
  select(TimeStamp, contains(Dependent_Variable), Prediction) %>%
  filter(Prediction > 0.02) %>%
  pivot_longer(-TimeStamp) %>%
  plot_time_series(.date_var = TimeStamp,
                   .value = value,
                   .color_var = name,
                   .smooth = FALSE,
                   .title = "Model Performance on Training Data (In Sample)",
                   .line_alpha = 0.6)
}

# Testing Data predictions - in sample
Data_Predictions_test <- ClarofyPredict(Dependent_Variable = Dependent_Variable,
                                         url = url, id = sessionID, df = holdout)


{
  custom_metrics_reg <- yardstick::metric_set(yardstick::mae, yardstick::rmse, yardstick::rsq)
  
  print('Custom metrics function')
  galaxy_model_metrics_test <- Data_Predictions_test %>%
    custom_metrics_reg(!! sym(Dependent_Variable), "Prediction")
  
  print('Custom metrics function')
  galaxy_model_metrics_test %>%
    clean_names(case = 'title') %>%
    select(-Estimator) %>%
    mutate(Metric = toupper(Metric),
           Estimate = round(Estimate, digits = 3)) %>%
    datatable(options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#251b29', 'color': '#fff'});",
        "}")
    ))
  
  Data_Predictions_test %>%
    filter(!!sym(Dependent_Variable) > 0.025) %>%
    ggplot(aes(!! sym(Dependent_Variable),Prediction)) +
    geom_point(alpha = 0.5) +
    geom_abline(color = 'blue', linetype = 2) +
    tune::coord_obs_pred() +
    labs(title = "R squared plot for evaluating model performance - Out of Sample",
         subtitle = "Actual vs Model",
         x = 'Actual KPI', y = 'Model Predicted KPI')
}
{
Data_Predictions_test %>%
  select(TimeStamp, contains(Dependent_Variable), Prediction) %>%
  filter(Prediction > 0.02) %>%
  pivot_longer(-TimeStamp) %>%
  plot_time_series(.date_var = TimeStamp,
                   .value = value,
                   .color_var = name,
                   .smooth = FALSE,
                   .title = "Model Performance on Training Data (Out of Sample)",
                   .line_alpha = 0.6)
}
# 12.0 GENERATE TARGET FOR OPTIMISATION ----
ProductivityGroups <- ClarofyGroup(Dependent_Variable = Dependent_Variable,
                                   Upstream_Forecasting_Variables = Upstream_Forecasting_Variables,
                                   n = 10,
                                   url = url,
                                   id = sessionID)
{
# For demonstration of an iteration step we select one productivity grouping of
# upstream forecasting variables.  This could also be a time weighted average for
# # real time recommendations using actual conditions.
# 
# ProductivityGroups$Summary_Of_Clusters %>% 
#   select(Upstream_Forecasting_Variables) %>% as_tibble()

# Run Optimisation for Each Productivity Group

# Filter each productivity group and run optimisations:
prod_group <- 6
centroid <- ProductivityGroups$Summary_Of_Clusters[prod_group,] %>%
  select(all_of(Upstream_Forecasting_Variables))

centroid

}

# 13.0 SINGLE OPTIMISATION ----
# The optimisation function must be set to either minimise or maximise the KPI depending on need
# The returned object includes the optimised process control variables as defined in step 2.0
# Use the recommended process control setpoints and actual upstream forecasting variables to
# observe the expected difference between the actual and optimal KPI.

jobId <- ClarofyOptimiseStartJob(Dependent_Variable = Dependent_Variable,
                         Upstream_Forecasting_Variables = Upstream_Forecasting_Variables,
                         Process_Control_Variables = Process_Control_Variables,
                         upstream_values = centroid, 
                         maximise_dependent_variable = TRUE, 
                         monitor_status = TRUE, 
                         url = url, id = sessionID)

jobStatus <- ClarofyOptimiseJobStatus(url = url, id = sessionID, jobId = jobId)

jobStatus

ProcessControlRecommendations <- ClarofyRecommend(url = url, id = sessionID)

OptimisedPredictions <- ClarofyOptimisedPredictions(url = url, id = sessionID)

OptimisedPredictions %>% glimpse()

ClarofyPlotOptimisationRun(Dependent_Variable = Dependent_Variable, ncol = 3,
                           url = url, id = sessionID)

# 14.0 All IN ONE OPTIMISATION ----
all_in_one_jobId <- ClarofyCS::ClarofyFinalOptimiseStart(url = url, 
                                           APIKey = APIKey, 
                                           train = train, 
                                           test = holdout, 
                                           Dependent_Variable = Dependent_Variable, 
                                           Independent_Variables = Independent_Variables, 
                                           Process_Control_Variables = Process_Control_Variables,
                                           Upstream_Forecasting_Variables = Upstream_Forecasting_Variables, 
                                           number_clusters = 2, 
                                           maximise_dependent_variable = FALSE, 
                                           monitor_status = TRUE)



# * Restore point ----
# Restore points are created by the client services package during the long running optimisation processes.
# Uncomment and run code below if RStudio session is aborted or closed while session and job are running
# readRDS(file = "restore_last_session.rds")


all_in_one_jobStatus <- ClarofyFinalOptimiseStatus(url = url, id = sessionID, jobId = all_in_one_jobId)

productivityGroups <- ClarofyGetProductivityGroups(url = url, id = sessionID)

FinalOptimisedPredictions <- ClarofyGetFinalOptimisedResults(url = url, id = sessionID)

FinalOptimisedPredictions %>% glimpse()

sensitivityAnalysis <- ClarofyGetSensitivityAnalysis(url = url, id = sessionID)

sensitivityAnalysis %>% glimpse()

# 15.0 FINISH SESSION ----

ClarofyEndSession(url = url, id = sessionID)

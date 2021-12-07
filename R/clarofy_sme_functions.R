# SME Slurry Conversion ----
#' Function calculates slurry concentrations, densities and solids ratios
#'
#' @description
#' Placeholder description
#'
#' @param url API url
#' @param API_Key API key provided
#' @param p_liquid Density of the liquid
#' @param p_solid Density of the solid
#' @param p_slurry Density of the solid
#' @param ratio_solid Solid percent ratio
#'
#' @details
#' # More details
#' - Bullet 1
#' - Bullet 2
#' - etc
#'
#' @examples
#'
#' \dontrun{
#'
#' Calculate the density of the liquid
#' CalculateSlurryConversion(API_Key = API_Key,
#'                            url = url,
#'                            # p_solid = 3900,
#'                            p_slurry = 1341,
#'                            ratio_solid = 0.25,
#'                            p_liquid = NULL)
#'
#' }
#'
#' @export
CalculateSlurryConversion <- function(API_Key,
                                      url,
                                      p_liquid = NULL,
                                      p_solid = NULL,
                                      p_slurry = NULL,
                                      ratio_solid = NULL) {

  path <- 'SME/SME_slurry_conv'

  body <- list(
    API_Key = API_Key
    ,p_liquid = p_liquid
    ,p_solid = p_solid
    ,p_slurry = p_slurry
    ,ratio_solid = ratio_solid
  )

  raw.result <- httr::POST(url = url, path = path, body = body, encode = 'json')
  slurry_result <- jsonlite::fromJSON(rawToChar(raw.result$content))

  return(slurry_result)
}


# # Two Product Recovery Calculation ----
#' Function calculates Feed, concentrate, tail and recovery
#'
#' @description
#' Placeholder
#'
#' @param url API url
#' @param API_Key API key provided
#' @param feed Value of the feed
#' @param concentrate Value of the concentrate
#' @param tail Value of the tail
#' @param recovery Value of the recovery
#'
#'
#'
#' @details
#' # More details
#' -Bullet 1
#' -Bullet2
#'
#' @examples
#'
#' \dontrun{
#'
#' CalculateTwoProductRecovery(API_Key = API_Key,
#'                                   url = url,
#'                                  feed = 0.7,
#'                           concentrate = 25,
#'                                  # tail= 0.02,
#'                              recovery = 0.9722)
#'
#'
#' }
#'
#' @export


CalculateTwoProductRecovery <- function(API_Key,
                                        url,
                                        feed = NULL,
                                        concentrate = NULL,
                                        tail= NULL,
                                        recovery= NULL) {

  path <- 'SME/SME_rec2prod'


  body <- list(
    API_Key = API_Key
    ,feed = feed
    ,concentrate = concentrate
    ,tail = tail
    ,recovery = recovery
  )

  raw.result <- httr::POST(url = url, path = path, body = body, encode = 'json')
  recovery <- jsonlite :: fromJSON(rawToChar(raw.result$content))

  return(recovery)
}


# Test code
# p_liquid_test <- CalculateSlurryConversion(API_Key = API_Key,
#                           url = url,
#                           p_solid = 3900,
#                           p_slurry = 1341,
#                           ratio_solid = 0.25
#                             )
# # #
# # # CalculateSlurryConversion(API_Key = API_Key,
# # #                           url = url,
# # #                           p_solid = 3900,
# # #                           p_slurry = 1341,
# # #                           ratio_solid = 0.25,
# # #                           # p_liquid = p_liquid_test
# # # )
# #
# p_liquid_test
#
#
#
# # 1.0 Slurry Conversion ----
#
# # test case answers
# # p_solid<-3900
# # ratio_solid<-0.25
# # p_liquid<-1100
# # p_slurry<-1341
# # p_liquid

# # * Local test slurry calculations ----
# {
#   API_Key  <- body$API_Key
#   p_liquid <- body$p_liquid
#   p_solid  <- body$p_solid
#   p_slurry <- body$p_slurry
#   ratio_solid <- body$ratio_solid
# }
#
# Galaxy::SME_slurry_conv(API_Key = API_Key,
#                         p_liquid = p_liquid,
#                         p_solid = p_solid,
#                         p_slurry = p_slurry,
#                         ratio_solid = ratio_solid
# )
#
#
# # 2.0 Two Product Recovery Calculation ----
# #  to find recovery, recovery<-SME_Rec2Prod(API_key,feed,concentrate,tail)
# # to find feed, feed<-SME_Rec2Prod(API_key,,concentrate,tail,recovery)
# # to find concentrate, concentrate<-SME_Rec2Prod(API_key,feed,,tail,recovery)
# # to find tail, tail<-SME_Rec2Prod(API_key,feed,concentrate,,recovery)
#
# # test case answers
# # feed<-0.7
# # concentrate<-25
# # tail<-0.02
# # recovery<-0.9722
#
# path <- 'SME/SME_rec2prod'
#
#
# {
#   body <- list(
#     API_Key = API_Key
#     ,feed = .7
#     ,concentrate = 25
#     ,tail = .02
#     ,recovery = NULL
#   )
# }
#
# {
#
#   #Query API endpoint
#   tic()
#   raw.result <- POST(url = url, path = path, body = body, encode = 'json')
#   recovery <- fromJSON(rawToChar(raw.result$content))
#   toc()
# }
#
# recovery
#
# # * Local test - recovery calculation ----
#
# {
#   API_Key  <- body$API_Key
#   feed <- body$feed
#   concentrate  <- body$concentrate
#   tail <- body$tail
#   recovery <- body$recovery
# }
#
# Galaxy::SME_rec2prod(API_Key = API_Key
#                      ,feed = feed
#                      ,concentrate = concentrate
#                      ,tail = tail
#                      ,recovery = NULL
# )
#
#
# # 3.0 Unit conversion ----
# #Example 1
# #unit_conv(100,"g","L","kg","KL")
#
# #Example 2
# #  raw_flowrates<-c(500,300,400,550,250,300,590) #my flow rates in mL/sec
#
# #  conv_flowrates<-unit_conv(raw_flowrates,"mL","sec","L","min")#  to change my flow from mL/sec to L/min
#
# ##Declaration of Variables conversions and there compatability.
# ##to add unit species to the list choose the base S.I. Unit as the letter and two zeroes to the magnitude
#
# path <- 'SME/SME_unit_conv'
#
#
# {
#   body <- list(
#     API_Key = API_Key
#     ,measure = 100
#     ,from_numerator = "g"
#     ,from_denominator = "L"
#     ,to_numerator = "kg"
#     ,to_denominator = "KL"
#   )
# }
#
# # Example 2
#
# raw_flowrates<-c(500,300,400,550,250,300,590) #my flow rates in mL/sec
#
#
# body2 <- list(API_Key = API_Key,
#               measure = raw_flowrates,
#               from_numerator = "mL",
#               from_denominator = "sec",
#               to_numerator = "L",
#               to_denominator = "min")
#
#
# #Query API endpoint
# {
#   # Example 1
#   tic()
#   raw.result <- POST(url = url, path = path, body = body, encode = 'json')
#   example_1 <- fromJSON(rawToChar(raw.result$content))
#   toc()
#
#   example_1
#
#   # Example 2
#   tic()
#   raw.result <- POST(url = url, path = path, body = body2, encode = 'json')
#   example_2 <- fromJSON(rawToChar(raw.result$content))
#   toc()
#
#   example_2
# }
#
# # * Local unit conversion ----
# {
#   API_Key <- body$API_Key
#   measure <- body$measure
#   from_numerator <- body$from_numerator
#   from_denominator <- body$from_denominator
#   to_numerator <- body$to_numerator
#   to_denominator <- body$to_denominator
# }
#
#
# SME_unit_conv(API_Key = API_Key
#               ,measure = measure
#               ,from_numerator = from_numerator
#               ,from_denominator = from_denominator
#               ,to_numerator = to_numerator
#               ,to_denominator = to_denominator)
#
#
# conv_flowrates<-SME_unit_conv(API_Key = API_Key,
#                               measure = raw_flowrates,
#                               from_numerator = "mL",
#                               from_denominator = "sec",
#                               to_numerator = "L",
#                               to_denominator = "min")#  to change my flow from mL/sec to L/min
#
#
# conv_flowrates
#
# # 4.0 Aggregate variables by time ----
#
# path <- 'Interlate_Aggregate_By_Time'
#
# # API Argument Options
# # type = c("floor", "ceiling", "round")
# # by = 	c("minute", "hour", "day", "month", "year") including multiples.
# # fn = c("sum", "mean", "median", "sd", "var", "min", "max", "count")
#
# # Data_prep %>% glimpse()
# # names(Data_prep)
# body <- list(
#   API_Key = API_Key
#   ,Data = Data_prep
#   ,by = "1 week"
#   ,type = "round"
#   ,fn = "count"
#   ,TimeStamp_Variable_Name = "TimeStamp"
#   ,Variables = Independent_Variables
#
# )
#
# rm(Aggregated_Data)
# {
#   tic()
#   raw.result <- POST(url = url, path = path, body = body, encode = 'json')
#   Aggregated_Data <- fromJSON(rawToChar(raw.result$content))
#   toc()
# }
#
# glimpse(Aggregated_Data)
#
# Aggregated_Data %>%
#   mutate(TimeStamp = ymd(!!sym(body$TimeStamp_Variable_Name))) %>%
#   pivot_longer(-TimeStamp) %>%
#   timetk::plot_time_series(
#     .date_var = TimeStamp,
#     .value = value,
#     .facet_vars = name,
#     .facet_ncol = 3,
#     .interactive = T)
#
#
# # 5.0 Auto Cross Correlation ----
# path <- 'Interlate_Auto_Cross_Correlation'
#
# body <- list(
#   API_Key = API_Key
#   ,Data = Data_prep
#   ,TimeStamp_Variable_Name = "TimeStamp"
#   ,Variables = Independent_Variables
#   ,Dependent_Variable = Dependent_Variable
#   # lags can be specified as time-based phrase indicating duration,
#   # a maximum number of lags or a sequence of lags as a vector
#   ,lags = 6
#
# )
#
# {
#   tic()
#   raw.result <- POST(url = url, path = path, body = body, encode = 'json')
#   acf_data <- fromJSON(rawToChar(raw.result$content))
#   toc()
# }
#
# glimpse(acf_data)
#
#
# # Local testing for ACF function ----
#
# Data = body$Data
# TimeStamp_Variable_Name = body$TimeStamp_Variable_Name
# Variables = body$Variables
# Dependent_Variable = body$Dependent_Variable
# lags = body$lags
#
# acf_data <- Data %>%
#   mutate(TimeStamp = ymd_hms(!! sym(TimeStamp_Variable_Name))) %>%
#   select(TimeStamp, Dependent_Variable, all_of(Variables)) %>%
#   as_tibble() %>%
#   timetk::tk_acf_diagnostics(
#     .date_var = TimeStamp,
#     .value = diff_vec(!! sym(Dependent_Variable)),
#     .ccf_vars = Variables,
#     # .show_ccf_vars_only = TRUE
#     .lags = lags
#   )
#
# acf_data %>% glimpse()
#
# g <- acf_data %>%
#   select(-contains("noise")) %>%
#   pivot_longer(-c(lag, ACF, PACF)) %>%
#   group_by(name) %>%
#   ggplot(aes(lag, value)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~name) +
#   labs(
#     title = "Cross Correlation Plot"
#   )
#
# ggplotly(g)
#
# acf_data %>%
#   select(-contains("noise"), -contains(Variables)) %>%
#   pivot_longer(c(ACF, PACF)) %>%
#   group_by(name) %>%
#   ggplot(aes(lag, value)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~name) +
#   labs(
#     title = "Auto/Partial Auto Correlation Plot"
#   )
#
# # 5.0 Lagged Feature Engineering ----
# path <- 'Interlate_Lag_Transform'
#
#
# body <- list(
#   API_Key = API_Key
#   ,Data = Data_prep
#   ,Variables = Independent_Variables  # Variable vector to transform with lags/leads
#   ,lags = 1:12            # Positive integers for lags, Negative integers for leads
#   ,names = "auto"        # String or vector of names, default = 'auto'
# )
#
# {
#   tic()
#   raw.result <- POST(url = url, path = path, body = body, encode = 'json')
#   lagged_data <- fromJSON(rawToChar(raw.result$content))
#   toc()
# }
#
# glimpse(lagged_data)
#
# # Local testing for lagged feature engineering ----
# Data = body$Data
# Variables = body$Variables  # Variable vector to transform with lags/leads
# lags = body$lags            # Positive integers for lags, Negative integers for leads
# names = body$names
#
# lagged_data <- Data %>%
#   select(all_of(Variables)) %>%
#   as_tibble() %>%
#   timetk::tk_augment_lags(
#     .value = Variables,
#     .lags = lags,
#     .names = names
#   )
# # ?tk_augment_lags()
# lagged_data %>% glimpse()
#
# {
#   set.seed(2021)
#   lagged_data %>%
#     slice_sample(n = 2000, replace = FALSE) %>%
#     select(contains("Con1T1P80PercCPV")) %>%
#     drop_na() %>%
#     pivot_longer(-Con1T1P80PercCPV) %>%
#     group_by(name) %>%
#     ggplot(aes(value, Con1T1P80PercCPV)) +
#     geom_point(alpha = 0.2) +
#     facet_wrap(~name) +
#     labs(
#       title = "Cross Correlation Plot"
#     )
#
# }
#
# # 6.0 Interlate Integration ----
# path <- 'Interlate_Integration'
#
# {
#   body <- list(
#     API_Key = API_Key
#     ,Data = Data_prep
#     ,Dependent_Variable = Dependent_Variable
#     ,Independent_Variables = Independent_Variables
#     ,Model_Selection = "support vector machine"
#     # ,Save_Model = FALSE
#     # ,Output_Directory = NULL
#     # ,Model_File_Names = NULL
#     ,TimeStamp_Variable_Name = "TimeStamp"
#   )
# }
#
# {
#   tic()
#   raw.result <- POST(url = url, path = path, body = body, encode = 'json')
#   integrated_data <- fromJSON(rawToChar(raw.result$content))
#   toc()
# }
#
# # local test
#
# local_integrated_test <- Interlate_Integration(
#   API_Key = API_Key
#   ,Data = Data_prep
#   ,Dependent_Variable = Dependent_Variable
#   ,Independent_Variables = Independent_Variables
#   ,Model_Selection = "support vector machine"
#   # ,Save_Model = FALSE
#   # ,Output_Directory = NULL
#   # ,Model_File_Names = NULL
#   ,TimeStamp_Variable_Name = "TimeStamp"
# )
#
# # test for equivalence of integrated data from API method and non-API
# # identical test - will fail due to the rawToChar conversion for API call
# identical(local_integrated_test$Con1T1RghrScavTailsCuPercPV_Integrated, integrated_data$Con1T1RghrScavTailsCuPercPV_Integrated)
# # test with all.equal to allow for a tolerance of 15 decimal places; this demonstrates functionally identical results
# all.equal(local_integrated_test$Con1T1RghrScavTailsCuPercPV_Integrated, integrated_data$Con1T1RghrScavTailsCuPercPV_Integrated, tolerance = 1e-15)
# # plot outputs
# {
#   comparison_data <- cbind(local_integrated_test$Con1T1RghrScavTailsCuPercPV_Integrated, integrated_data$Con1T1RghrScavTailsCuPercPV_Integrated)
#   colnames(comparison_data) <- c("Local", "API")
#   comparison_data <- data.frame(comparison_data)
#   comparison_data <- melt(comparison_data[,1:2])
#   ggplot() + geom_histogram(data = comparison_data, aes(value, fill=variable), position="dodge")
# }
#
# subset((local_integrated_test$Con1T1RghrScavTailsCuPercPV_Integrated-integrated_data$Con1T1RghrScavTailsCuPercPV_Integrated), local_integrated_test$Con1T1RghrScavTailsCuPercPV_Integrated!=integrated_data$Con1T1RghrScavTailsCuPercPV_Integrated)
#
# # 7.0 Statistical Data Cleansing ----
#
# # path <- 'Interlate_Statistical_Data_Cleansing'
# #
# # 7.1
# # Test filtering variable by statement
# #
# # {
# #   body <- list(
# #     API_Key = API_Key
# #     ,Data = Data_prep
# #     ,TimeStamp_Variable_Name = "TimeStamp"
# #     ,Filtering_Variables = Data_prep$Con1BallMl002MtrTotPwrCtKwPV
# #     ,Filtering_Queries = "> 1500"
# #     ,Complete_Records = TRUE
# #   )
# # }
# #
# # {
# #   tic()
# #   raw.result <- POST(url = url, path = path, body = body, encode = 'json')
# #   statment_api_data <- fromJSON(rawToChar(raw.result$content))
# #   toc()
# # }
#
# # 7.2
# # Test statistical filtering by quantile (remove upper/lower percentile)
#
# # API Test
# {
#   path <- 'Interlate_Statistical_Data_Cleansing'
#   body <- list(
#     API_Key = API_Key
#     ,Data = Data_prep
#     ,Statistical_Filtering_Technique = "Quantile Filtering"
#     ,Statistical_Filtering_Technique_Parameters = "Quantile Filtering - Percentile to Filter"
#     ,Statistical_Filtering_Technique_Parameters_Values = 0.01
#     ,Statistical_Filtering_Variables_To_Include = c("Con1CleanerScavTailsCuPercPV")
#     #,Statistical_Filtering_Variables_To_Exclude = NULL
#     ,Complete_Records = FALSE
#   )
# }
#
#
# {
#   tic()
#   raw.result <- POST(url = url, path = path, body = body, encode = 'json')
#   quantile_api_data <- fromJSON(rawToChar(raw.result$content))
#   toc()
# }
#
# # plot pre-cleanse and API-cleanse to demonstrate change
# {
#   comparison_data <- cbind(Data_prep$Con1CleanerScavTailsCuPercPV, quantile_api_data$Con1CleanerScavTailsCuPercPV)
#   colnames(comparison_data) <- c("Pre-Cleanse", "API Cleanse")
#   comparison_data <- data.frame(comparison_data)
#   comparison_data <- melt(comparison_data[,1:2])
#   ggplot() + geom_density(data = comparison_data, aes(value, fill=variable), alpha=0.5)
# }
#
# # local test
#
# quantile_local_data <- Interlate_Data_Cleansing(API_Key = API_Key
#                                                 ,Data = Data_prep
#                                                 ,Statistical_Filtering_Technique = "Quantile Filtering"
#                                                 ,Statistical_Filtering_Technique_Parameters = "Quantile Filtering - Percentile to Filter"
#                                                 ,Statistical_Filtering_Technique_Parameters_Values = 0.01
#                                                 ,Statistical_Filtering_Variables_To_Include = c("Con1CleanerScavTailsCuPercPV")
#                                                 #,Statistical_Filtering_Variables_To_Exclude = NULL
#                                                 ,Complete_Records = FALSE)
# # API results
# {
#   summary(Data_prep$Con1CleanerScavTailsCuPercPV)
#   summary(quantile_api_data$Con1CleanerScavTailsCuPercPV)
# }
# # Local results
# {
#   summary(Data_prep$Con1CleanerScavTailsCuPercPV)
#   summary(quantile_local_data$Con1CleanerScavTailsCuPercPV)
# }
#
# # check outcomes are identical
# identical(quantile_api_data$Con1CleanerScavTailsCuPercPV, quantile_local_data$Con1CleanerScavTailsCuPercPV)
#
# # plot outputs of API vs Local to demonstrate identical data
# {
#   comparison_data <- cbind(quantile_local_data$Con1T1RghrScavTailsCuPercPV, quantile_api_data$Con1T1RghrScavTailsCuPercPV)
#   colnames(comparison_data) <- c("Local", "API")
#   comparison_data <- data.frame(comparison_data)
#   comparison_data <- melt(comparison_data[,1:2])
#   ggplot() + geom_histogram(data = comparison_data, aes(value, fill=variable), position="dodge")
# }
#
#
# # 7.3
# # Test statistical filtering by standard deviation (remove upper/lower tails at designated number of standard deviations from mean)
#
# # API test
# {
#   path <- 'Interlate_Statistical_Data_Cleansing'
#   body <- list(
#     API_Key = API_Key
#     ,Data = Data_prep
#     ,Statistical_Filtering_Technique = "Standard Deviation Filtering"
#     ,Statistical_Filtering_Technique_Parameters = "Standard Deviation Filtering - Number of Deviations"
#     ,Statistical_Filtering_Technique_Parameters_Values = 1
#     ,Statistical_Filtering_Variables_To_Include = c("Con1CleanerScavTailsCuPercPV", "Con1T1RghrCell11BubbleVelocityMmpsPV")
#     #,Statistical_Filtering_Variables_To_Exclude = NULL
#     ,Complete_Records = FALSE
#   )
# }
#
# {
#   tic()
#   raw.result <- POST(url = url, path = path, body = body, encode = 'json')
#   sd_api_data <- fromJSON(rawToChar(raw.result$content))
#   toc()
# }
#
# # API results
# {
#   # Pre-run
#   summary(Data_prep$Con1CleanerScavTailsCuPercPV)
#   summary(Data_prep$Con1T1RghrCell11BubbleVelocityMmpsPV)
#   # API results
#   summary(sd_api_data$Con1CleanerScavTailsCuPercPV)
#   summary(sd_api_data$Con1T1RghrCell11BubbleVelocityMmpsPV)
# }
#
# # plot pre-cleanse and API-cleanse to demonstrate change
# # Note non-normal distribution of data pre-cleansing which impacts distribution of cleansed data
# # as this would be unlikely to result in a cleansed distribution that retained general properties of the original data
# {
#   comparison_data <- cbind(Data_prep$Con1CleanerScavTailsCuPercPV, sd_api_data$Con1CleanerScavTailsCuPercPV)
#   colnames(comparison_data) <- c("Pre-Cleanse", "API Cleanse")
#   comparison_data <- data.frame(comparison_data)
#   comparison_data <- melt(comparison_data[,1:2])
#   ggplot() + geom_density(data = comparison_data, aes(value, fill=variable), alpha=0.5)
# }
# {
#   comparison_data <- cbind(Data_prep$Con1T1RghrCell11BubbleVelocityMmpsPV, sd_api_data$Con1T1RghrCell11BubbleVelocityMmpsPV)
#   colnames(comparison_data) <- c("Pre-Cleanse", "API Cleanse")
#   comparison_data <- data.frame(comparison_data)
#   comparison_data <- melt(comparison_data[,1:2])
#   ggplot() + geom_density(data = comparison_data, aes(value, fill=variable), alpha=0.5)
# }
#
# # local test
#
# sd_local_data <- Interlate_Data_Cleansing(API_Key = API_Key
#                                           ,Data = Data_prep
#                                           ,Statistical_Filtering_Technique = "Standard Deviation Filtering"
#                                           ,Statistical_Filtering_Technique_Parameters = "Standard Deviation Filtering - Number of Deviations"
#                                           ,Statistical_Filtering_Technique_Parameters_Values = 1
#                                           ,Statistical_Filtering_Variables_To_Include = c("Con1CleanerScavTailsCuPercPV", "Con1T1RghrCell11BubbleVelocityMmpsPV")
#                                           #,Statistical_Filtering_Variables_To_Exclude = NULL
#                                           ,Complete_Records = FALSE)
#
# # Pre-run
# {
#   summary(Data_prep$Con1CleanerScavTailsCuPercPV)
#   summary(Data_prep$Con1T1RghrCell11BubbleVelocityMmpsPV)
# }
# # Local results
# {
#   summary(sd_local_data$Con1CleanerScavTailsCuPercPV)
#   summary(sd_local_data$Con1T1RghrCell11BubbleVelocityMmpsPV)
# }
#
# # test for equivalence
# identical(sd_local_data$Con1T1RghrCell11BubbleVelocityMmpsPV, sd_api_data$Con1T1RghrCell11BubbleVelocityMmpsPV)
# identical(sd_local_data$Con1CleanerScavTailsCuPercPV, sd_api_data$Con1CleanerScavTailsCuPercPV)
#
# # plot outputs of API vs Local to demonstrate identical data
# {
#   comparison_data <- cbind(sd_local_data$Con1T1RghrCell11BubbleVelocityMmpsPV, sd_api_data$Con1T1RghrCell11BubbleVelocityMmpsPV)
#   colnames(comparison_data) <- c("Local", "API")
#   comparison_data <- data.frame(comparison_data)
#   comparison_data <- melt(comparison_data[,1:2])
#   ggplot() + geom_histogram(data = comparison_data, aes(value, fill=variable), position="dodge")
# }
# {
#   comparison_data2 <- cbind(sd_local_data$Con1CleanerScavTailsCuPercPV, sd_api_data$Con1CleanerScavTailsCuPercPV)
#   colnames(comparison_data2) <- c("Local", "API")
#   comparison_data2 <- data.frame(comparison_data2)
#   comparison_data2 <- melt(comparison_data2[,1:2])
#   ggplot() + geom_histogram(data = comparison_data2, aes(value, fill=variable), position="dodge")
# }

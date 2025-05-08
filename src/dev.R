# Load packages
library(tidyverse) # data handling tools
library(lubridate) # date handling
library(tidymodels) # modeling package
library(xgboost) # predictive algo
library(factoextra) # cluster analysis visualization
library(ranger) # model fitting engine
library(modelsummary) # df summary statistics
library(probably) # probability thresholds
library(parallel) # parallel computing for optimization
library(psych) # intra-class correlations



# Load the provided synthetic trip data  ----------------------------------

# Load data
d <- read_csv("input/boost_df.csv", show_col_types = FALSE)




# EDA ---------------------------------------------------------------------

# How many trip observations are there?
length(d$trip_id)

# How many unique trips are there?
length(unique(d$trip_id))

# What percent of observations in the trip data are duplicate trip records?
prop.table(table(duplicated(d$trip_id)))

# What is the distribution of trips by boost sequence?
table(d$seq_boost_count)

# How do boost sequences distribute over trips?
d %>%
  group_by(seq_boost_count) %>%
  summarize(count = n()) %>%
  mutate(as.factor(seq_boost_count)) %>%
  ggplot(., aes(x = seq_boost_count, y = count)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal()

# How do trips breakdown by geographic area (for inferring drivers)?
d %>%
  group_by(origin_metro_area_name) %>%
  summarize(metro_count = n()) %>%
  arrange(desc(metro_count))

# How are boost offer amounts distributed?
d %>%
  group_by(seq_boost_count) %>%
  summarize(average_cumulative_boost = mean(cumulative_boost_amount_cents, na.rm = TRUE) / 100) %>%
  mutate(as.factor(seq_boost_count)) %>%
  ggplot(., aes(x = seq_boost_count, y = average_cumulative_boost)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous("Boost Sequence") + 
  scale_y_continuous("Avg Cumulative Boost Amount", labels = scales::dollar_format()) + 
  theme_minimal()





# Simulate/synthesize driver data -----------------------------------------

# Create theoretically informed features from raw data
d <- d %>%
  mutate(trip_starts_weekday = wday(scheduled_starts_at, label = FALSE),
         trip_ends_weekday = wday(scheduled_ends_at, label = FALSE),
         trip_starts_time = as.numeric(hms::as_hms(ymd_hms(scheduled_starts_at))),
         trip_ends_time = as.numeric(hms::as_hms(ymd_hms(scheduled_ends_at))),
         metro_num = as.numeric(as.factor(origin_metro_area_name)),
         trip_starts_during_peak_hours = as.numeric(trip_starts_during_peak_hours)) %>%
  mutate(trip_starts_time = ifelse(is.na(trip_starts_time), trip_ends_time, trip_starts_time)) # Handle few parsing errors, so I don't have to drop these cases

# Filter records to only the final claimed ride
d_cluster <- d %>%
  group_by(trip_id) %>%
  filter(seq_boost_count == max(seq_boost_count)) %>%
  ungroup(.)

# Select numeric features for clustering trips to synthesize drivers
d_cluster <- d_cluster %>%
  select(trip_id,
         total_predicted_duration_mins,
         total_predicted_distance_miles,
         total_predicted_distance_miles_for_fare,
         dollars_paid_to_driver,
         trip_starts_weekday,
         trip_starts_time,
         trip_ends_time,
         metro_num
  )

# Check for missing data
check_for_na <- function(df){
  sapply(df, function(x) sum(is.na(x)))
}
check_for_na(d_cluster)

# Check for non-numeric classes
check_for_class <- function(df){
  sapply(df, class)
}
check_for_class(d_cluster)


# Run test to ensure no missing data are present in analytic df
testthat::expect_true(all(check_for_na(d_cluster) == 0))

# Run test to ensure no missing data are present in analytic df
testthat::expect_true(all(check_for_class(d_cluster) == "numeric"))





# Define initial loop parameters
max_driver_id <- 0 # add this to driver_id
d_driver <- d_cluster[0, ] # empty data.frame


# Cluster within metro area
# Iterate over every metro number in the dataset
for (i in sort(unique(d_cluster$metro_num))){ 
  
  message(paste("Sythensizing drivers for metro_num", i))
  
  # Store IDs for this metro area, to be removed during cluster analysis
  d_temp_ids <- d_cluster %>%
    dplyr::filter(metro_num == i) %>%
    select(metro_num, trip_id)
  
  # Remove IDs from cluster analysis metro area data
  d_temp_cluster <- d_cluster %>%
    dplyr::filter(metro_num == i) %>%
    select(-metro_num, -trip_id)
  
  # Scale clustering data.frame, because k-means is distance based
  d_temp_cluster <- scale(d_temp_cluster) %>% 
    as.data.frame(.)
  
  # Determine number of clusters for k-means
  clusters_n <- ifelse(nrow(d_temp_cluster) > 5, nrow(d_temp_cluster) / 5, 2)
  
  # Fit k-means cluster model on this metro area
  driver_model_temp <- kmeans(as.data.frame(d_temp_cluster),
                              iter.max = 1000,
                              centers = clusters_n)
  
  if (i == 3){ # Geneate visual example of clustering to produce drivers
    # Render fviz plot of k-means model
    pfviz_plot <- fviz_cluster(driver_model_temp, 
                               data = d_temp_cluster,
                               ggtheme = theme_minimal())
  }
  
  # Assign cluster id as driver_id number to each record
  d_temp_cluster$driver_id <- driver_model_temp$cluster
  
  # Add current max_driver_id value to this metro's iteration of driver id numbers
  d_temp_cluster$driver_id <- d_temp_cluster$driver_id + max_driver_id
  
  # Store max driver_id number to start incrementing from that in next iteration. 
  # This will avoid duplication of driver id numbers between metro areas
  max_driver_id <- max(d_temp_cluster$driver_id)
  
  # Reattach ID fields to the metro area data
  d_temp_cluster <- bind_cols(d_temp_cluster, d_temp_ids)
  
  # Append new driver clusters onto earlier metro areas
  d_driver <- bind_rows(d_driver, d_temp_cluster)
  
  # Remove objects to be recreated on next iteration
  rm(d_temp_cluster, driver_model_temp)
}

# Separate IDs for joining onto trip data.frame
d_ids <- d_driver %>%
  select(trip_id, driver_id, metro_num)



# For presentation, write out IDs in summary table
write_csv(d_ids, "presentation/assets/d_ids.csv")

# Review the distribution of unique drivers by metro area
d_ids %>%
  group_by(metro_num) %>%
  summarize(total_trips = length(unique(trip_id)),
            total_drivers = length(unique(driver_id))) %>%
  arrange(desc(total_trips))

# Review total number of assigned drivers
length(unique(d_ids$driver_id))

# Remove unnecessary metro_num id from ids for later joins
d_ids <- d_ids %>%
  select(-metro_num)



# Review the example cluster plot. This calculates the first two principal
# components from the analytic data, and plots those on X and Y. Then
# the k-means clusters are grouped on those two orthogonal dimensions.
print(pfviz_plot)

# Store the plot for rendering in presentation
ggsave("presentation/assets/img/cluster_driver_example.png",
       pfviz_plot, width = 10, height = 6)



# Generate intraclass correlations for driver data
# Does clustering by drivers really assign similar trips to the same driver?

# Attach ids to driver training data
d_cluster_ids <- d_cluster %>%
  left_join(d_ids, by = "trip_id")

# Define varnames for features to correlate
cor_vars <- c("total_predicted_duration_mins", 
              "total_predicted_distance_miles", 
              "total_predicted_distance_miles_for_fare",
              "dollars_paid_to_driver",
              "trip_starts_weekday",
              "trip_starts_time",
              "trip_ends_time") 

# Generate within and between correlations
cor_results <- statsBy(d_cluster_ids[, c("driver_id", cor_vars)],
                       group = "driver_id",
                       cor = TRUE)

# Generate plot of ICC by feature
icc_plot <- tibble(ICC = cor_results$ICC1,
       Feature = names(cor_results$ICC1)) %>%
  mutate(Feature = fct_reorder(Feature, ICC)) %>%
  ggplot(., aes(x = ICC, y = Feature)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#01AAC7") +
  theme_minimal()

# Save plot for presentation
write_rds(icc_plot, "presentation/assets/img/icc_plot.rds")







# Predict Unclaimed Rides -------------------------------------------------

# Calculate average elapsed time from trip creation and first boost offer.
# Most of this elapsed time could be eliminated if boost offers were made as 
# soon as a model predicts it will go unclaimed.
d_first_boost_offered <- d %>%
  arrange(trip_id, seq_boost_count) %>%
  filter(seq_boost_count == 1) %>%
  select(created_at, boost_timestamp) %>%
  mutate(days_to_first_boost = as.numeric(difftime(boost_timestamp, 
                                                   created_at, units = "days")))

# Create summary statistics for elapsed time
days_to_first_boost_summary <- d_first_boost_offered %>%
  summarize(min_days = min(days_to_first_boost),
            mean_days = mean(days_to_first_boost),
            max_days = max(days_to_first_boost))

# Review histogram distribution of days to first boost offer, since created at date
ggplot(d_first_boost_offered, aes(x = days_to_first_boost)) +
  geom_histogram(fill = "#F57E44") +
  theme_minimal()



# To build analytic data.frame, select proper records.
# Reduce the trip data.frame to only the initial trip record, 
# before boost awards are made.
d_unclaimed_w_trip_ids <- d %>%
  arrange(trip_id, seq_boost_count) %>%
  group_by(trip_id) %>%
  slice_min(seq_boost_count) %>%
  ungroup(.)


# Identify the outcome, that being whether a trip will need a boost 
# award offer to motivate a driver to claim the trip. Use the 
# "ever_unclaimed" field as it is already coded. Identify predictor 
# fields in the trip data that are not leakage, meaning fields that are 
# known before a trip goes unclaimed.
d_unclaimed_w_trip_ids <- d_unclaimed_w_trip_ids %>%
  select(trip_id, 
         metro_num,
         ever_unclaimed, # outcome
         trip_starts_weekday,
         trip_starts_time,
         trip_ends_time,
         total_predicted_duration_mins,
         total_predicted_distance_miles,
         trip_starts_during_peak_hours,
         is_same_day_ride
  ) %>%
  mutate(ever_unclaimed = as.factor(ever_unclaimed),
         is_same_day_ride = as.numeric(is_same_day_ride)
  )

# Store a copy of these data with ids for joining later. For now, remove ids.
d_unclaimed <- d_unclaimed_w_trip_ids %>%
  select(-trip_id)

# For presentation, write out summary statistics of analytic data.frame
write_rds(datasummary_skim(d_unclaimed, histogram = FALSE), 
          "presentation/assets/d_unclaimed.rds")




# Start fitting the unclaimed model. Set the seed and define train and test
# datasets
set.seed(123)
# Using a small test set to try and get the most leverage out of the available
# training data.
d_unclaimed_split <- initial_split(d_unclaimed, prop = 4/5, strata = metro_num)
d_train <- training(d_unclaimed_split)
d_test <- testing(d_unclaimed_split)


# Tried XGBoost for this and it performed worse. Switched to Random Forest.
# Frankly, both models are awaful fits to these data.

# Define the algorithm to be used
rf_model <- rand_forest(
  mode = "classification",
  engine = "ranger")

# Fit the model on the training data
rf_fit <-
  rf_model %>%
  fit_xy(x = select(d_train, -ever_unclaimed),
         y = select(d_train, ever_unclaimed))

# Apply the model out of sample on the test set
rf_results <- 
  d_test %>%
  bind_cols(
    predict(rf_fit, new_data = select(d_test, -ever_unclaimed)),
    predict(rf_fit, new_data = select(d_test, -ever_unclaimed), type = "prob")
  )

# Review the ROC curve. It is negative and terrible.
rf_results %>% 
  mutate(.pred_class = as.numeric(.pred_class)) %>%
  roc_curve(., ever_unclaimed, .pred_class) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()

# Examine additional model performance statistics
unclaimed_auc <- rf_results %>% 
  roc_auc(truth = ever_unclaimed, 
          .pred_0)

unclaimed_accuracy <- rf_results %>% 
  accuracy(truth = ever_unclaimed, 
           .pred_class)

unclaimed_conf_mat <- rf_results %>% 
  conf_mat(truth = ever_unclaimed, 
           .pred_class)

# Review model performance statistics and write them out to presentation location
print(unclaimed_auc)
print(unclaimed_accuracy)
print(unclaimed_conf_mat)

write_csv(unclaimed_auc, "presentation/assets/unclaimed_auc.csv")
write_csv(unclaimed_accuracy, "presentation/assets/unclaimed_accuracy.csv")
write_rds(unclaimed_conf_mat, "presentation/assets/unclaimed_conf_mat.rds")

levels(rf_results$ever_unclaimed)


# Refit model with full data for purposes of better predicting trips that will be used to determine driver-specific price sensitivity.
rf_refit <-
  rf_model %>%
  fit_xy(x = select(d_unclaimed_w_trip_ids, -ever_unclaimed),
         y = select(d_unclaimed_w_trip_ids, ever_unclaimed))

# Predict the full dataset, assuming we don't know which rides will be claimed
d_unclaimed_predicted <- d_unclaimed_w_trip_ids %>%
  bind_cols(
    predict(rf_refit, new_data = d_unclaimed_w_trip_ids),
    predict(rf_refit, new_data = d_unclaimed_w_trip_ids, type = "prob")
  )

# Filter trip data to only trips that we predict will go unclaimed
d_unclaimed_predicted <- d_unclaimed_predicted %>%
  filter(.pred_class == 1) %>%
  rename(ever_unclaimed_predicted = .pred_class)

# Review unclaimed prediction data.frame with class and probability predictions
datasummary_skim(d_unclaimed_predicted)





# Find Price Sensitivity for Each Driver ----------------------------------

# Preprocess trip data into a shape that will support price sensitivity analysis

# We know the cumulative boost amount that motivated a driver to claim a ride.
# We know the cumulative boost totals that did not motivate a driver to claim a ride.
# This will be our outcome to predict.

# Filter data to only trips with boost awards, as these are the boosted trips 
# that will help us determine price sensitivity by driver for predicted-to-be 
# unclaimed trips.
d_boost <- d %>%
  filter(boost_ind == 1)

# Code the outcome, which is that the boost award was claimed.
# Here we assume that the maximum boost sequence number represents the claimed 
# boost for each trip.
d_boost <- d_boost %>%
  arrange(trip_id, seq_boost_count) %>%
  group_by(trip_id) %>%
  mutate(boost_claimed = ifelse(seq_boost_count == max(seq_boost_count), 1, 0)) %>%
  ungroup(.)

# Join on synthetic driver data
d_boost <- d_boost %>%
  left_join(d_ids)


# Store order of trip_ids for later use
d_boost_trip_ids <- d_boost$trip_id

# Assemble predictors, including driver_id and the focal variable, which is cumulative_boost_amount_cents

# I'm not including elapsed boost time and other timing of boost measures, 
# because I want to know the price sensitivity, ignoring the influence of time.
d_boost <- d_boost %>%
  select(driver_id,
         metro_num,
         boost_claimed, # outcome
         cumulative_boost_amount_cents, # focal predictor
         trip_starts_weekday,
         trip_starts_time,
         trip_ends_time,
         total_predicted_duration_mins,
         total_predicted_distance_miles,
         trip_starts_during_peak_hours,
         is_same_day_ride
  ) %>%
  mutate(boost_claimed = as.factor(boost_claimed),
         is_same_day_ride = as.numeric(is_same_day_ride)
  )


# For presentation, write out analytic dataframe summary statistics
write_rds(datasummary_skim(d_boost, histogram = FALSE), 
          "presentation/assets/d_boost.rds")



# Initiate fitting boost claimed model. Still using a small test set to
# try and get the most leverage possible out of the small training data.
set.seed(123)
d_boost_split <- initial_split(d_boost, prop = 4/5, strata = driver_id)
d_train <- training(d_boost_split)
d_test <- testing(d_boost_split)


# Define the boost claimed trip model parameters and specification
# I'm using XGBoost due to its general high degree of utility and peformance,
# but also because I want to identify fine-grained interactional differences 
# in boost amounts in their effect on the likelihood a driver will claim
# a trip. Using a lot of trees to really dig into the granular effects of
# price sensitivity is what I'm going for here. I also want varying effects
# across drivers, hence the nonparametric approach.
xgb_model <- boost_tree(
  trees = 1000,
  mode = "classification",
  engine = "xgboost"
)

# Fit the XGBoost model to the training data
xgb_fit <-
  xgb_model %>%
  fit_xy(x = select(d_train, -boost_claimed),
         y = select(d_train, boost_claimed))

# Predict the test set using the trainined model, both class and probability predictions
xgb_results <- 
  d_test %>%
  bind_cols(
    predict(xgb_fit, new_data = select(d_test, -boost_claimed)),
    predict(xgb_fit, new_data = select(d_test, -boost_claimed), type = "prob")
  )



# Review the ROC curve. It is also terrible and negative.
xgb_results %>% 
  mutate(.pred_class = as.numeric(.pred_class)) %>%
  roc_curve(., boost_claimed, .pred_class) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()


# Examine and store model performance statistics
boosted_auc <- xgb_results %>% 
  roc_auc(truth = boost_claimed, 
          .pred_0)

boosted_accuracy <- xgb_results %>% 
  accuracy(truth = boost_claimed, 
           .pred_class)

boosted_recall <- xgb_results %>% 
  recall(truth = boost_claimed, 
         .pred_class)

boosted_conf_mat <- xgb_results %>% 
  conf_mat(truth = boost_claimed, 
           .pred_class)


# Review model performance stats and write out for presentation
print(boosted_auc)
print(boosted_accuracy)
print(boosted_conf_mat)

write_csv(boosted_auc, "presentation/assets/boosted_auc.csv")
write_csv(boosted_accuracy, "presentation/assets/boosted_accuracy.csv")
write_rds(boosted_conf_mat, "presentation/assets/boosted_conf_mat.rds")




# Define optimization threshold -------------------------------------------

# Threshold analysis to determine the best possible probability cutoff
# Prioritize recall to minimize false negatives, ensuring that more rides
# are offered boosts when there is any question.
thresholds <- seq(0.5, 0.9, by = 0.1)

# Calculate recall thresholds
recall_estimates <- xgb_results %>%
  threshold_perf(boost_claimed, .pred_1, thresholds) %>%
  filter(.metric == "sensitivity") # Recall thresholds
print(recall_estimates)

# Select the threshold with the highest recall (which happens to be 0.5)
selected_prob_threshold <- recall_estimates %>%
  filter(.estimate == max(.estimate)) %>%
  pull(.threshold)






# Optimize boost offer amounts --------------------------------------------

# Calculate deciles for cumulative boost amounts, so we have 10 incremental boost amounts for testing price sensitivity
boost_quantiles <- quantile(d_boost$cumulative_boost_amount_cents, 
                            probs = seq(0.1, 1, by = 0.1))



# This optimization function runs on one metro area at a time. 
# It is designed to be executed inside a loop over metro areas.
# This function creates all possible combinations of drivers, trips,
# and boost offer amounts and predicts the likelihood that each trip
# will be claimed. It then sorts and filters the results to the optimal
# driver/boost/trip combination.
optimize_trip_boosts <- function(metro.num){
  
  require(dplyr)
  message("Optimizing trip boosts for drivers in metro_num = ", metro.num)
  
  # Locate all driver ids in the geographic area
  driver_ids <- d_boost %>%
    filter(metro_num == metro.num) %>%
    pull(driver_id) %>%
    unique(.)
  
  # Locate all trip ids in the geographic area
  trip_ids <- d_unclaimed_predicted %>%
    filter(metro_num == metro.num) %>%
    pull(trip_id) %>%
    unique(.)
  
  # Create data.frame with all combinations of boost amounts, 
  # drivers, and (predicted) unclaimed trips
  d_expand <- expand.grid(cumulative_boost_amount_cents = boost_quantiles,
                          driver_id = driver_ids,
                          trip_id = trip_ids)
  
  # Build dataset for determining each driver's price sensitivity 
  # on each available trip in their area
  d_trip_geo <- d_unclaimed_predicted %>%
    filter(metro_num == metro.num) %>%
    select(trip_id,
           metro_num,
           trip_starts_weekday,
           trip_starts_time,
           trip_ends_time,
           total_predicted_duration_mins,
           total_predicted_distance_miles,
           trip_starts_during_peak_hours,
           is_same_day_ride)
  
  # Add trip data to expanded dimensional driver and boost-level data
  d_expand <- d_expand %>%
    left_join(d_trip_geo, by = c("trip_id"))
  
  # Make class and probability predictions for each boost/driver/trip combo
  d_expand <- 
    d_expand %>%
    bind_cols(
      # predict(xgb_fit, new_data = d_expand),
      predict(xgb_fit, new_data = d_expand, type = "prob")
    ) %>%
    mutate(.pred_class = ifelse(.pred_1 >= selected_prob_threshold, "1", "0"))
  
  
  # I want the case with the lowest boost amount that crosses the 
  # threshold, and that has the highest probability of claiming the trip 
  # to be offered the first boost award
  minimized_boosts <- d_expand %>%
    # pick only trips for boosts where threshold is exceeded
    filter(.pred_class == "1") %>% 
    group_by(trip_id) %>% 
    # sort within trip, lowest boost, and the highest probability
    arrange(trip_id, cumulative_boost_amount_cents, desc(.pred_1)) %>% 
    # select the driver within the lowest boost amount, and then has the highest probability
    slice(1) %>%
    mutate(metro_num = metro.num)
  
  rm(d_expand)
  
  return(minimized_boosts)

}


# Serial method for optimizing boosts within each metro area
# BE PREPARED TO WAIT 5-10 MINUTES WHEN YOU RUN THIS
# boost_iteration_1 <- lapply(sort(unique(d_unclaimed_predicted$metro_num)), 
#                                function(i) {
#                                  optimize_trip_boosts(i)
#                                }) %>%
#   bind_rows()


# Parallelized method for optimizes boosts within each metro area
# RUNS A LITLE FASTER THAN SERIAL METHOD ABOVE ON MACBOOK AIR WITH 8 CORES
cl <- makeCluster(detectCores())
clusterExport(cl, c('d_boost',
                    'd_unclaimed_predicted',
                    'boost_quantiles',
                    'xgb_fit',
                    'selected_prob_threshold',
                    'optimize_trip_boosts'))

boost_iteration_1 <- parLapply(cl,
                               sort(unique(d_unclaimed_predicted$metro_num)),
                               function(i) {
                                 optimize_trip_boosts(i)
                               })
stopCluster(cl)
boost_iteration_1 <- Reduce("bind_rows", boost_iteration_1)






# Do we have any duplicate trips after optimization?
testthat::expect_true(all(table(duplicated(boost_iteration_1$trip_id))))

# Retrieve actual cumulative boost amounts for comparison to optimized amounts
d_boost_actual <- d %>%
  filter(boost_ind == 1) %>%
  arrange(trip_id, seq_boost_count) %>%
  group_by(trip_id) %>%
  mutate(boost_claimed = ifelse(seq_boost_count == max(seq_boost_count), 1, 0)) %>%
  ungroup(.) %>%
  filter(boost_claimed == 1) %>%
  rename(boost_actual = cumulative_boost_amount_cents) %>%
  select(trip_id, boost_actual)


# Compare optimized boost offers against actual offers: Are they lower on average?
minimized_boosts <- boost_iteration_1 %>%
  rename(boost_predicted = cumulative_boost_amount_cents) %>%
  left_join(d_boost_actual, by = "trip_id") %>%
  mutate(boost_actual_minus_predicted = boost_actual - boost_predicted)




# Calculate the difference between predicted and actual. Plot results
# as histogram. Use actual minus predicted approach, so more positive results
# are better, as that indicates dollars saved with the optimization approach.
p_actual_predicted_boost_difference <- minimized_boosts %>%
  mutate(boost_actual_minus_predicted = boost_actual_minus_predicted / 100, # convert to dollars
         color = ifelse(boost_actual_minus_predicted >= 0, "#01AAC7", "#F57E44")) %>%
  ggplot(., aes(x = boost_actual_minus_predicted, fill = color)) +
  geom_histogram(color = "grey", bins = 30) +
  scale_fill_identity() +
  scale_y_continuous("Count") +
  scale_x_continuous("Actual Boost Minus Predicted Boost",
                breaks = scales::pretty_breaks(n = 6),
                     labels = scales::dollar_format()) +
  theme_minimal()

# Review the actual minus predicted analysis results and write out for presentation
print(p_actual_predicted_boost_difference)
ggsave("presentation/assets/img/p_actual_predicted_boost_difference.png",
       p_actual_predicted_boost_difference, width = 10, height = 6)



# Display summary statistics of actual minus predicted boosts
# Export for presentation
summary_optim_boosts <- summary(minimized_boosts$boost_actual_minus_predicted) / 100
write_rds(summary_optim_boosts, "presentation/assets/summary_optim_boosts.rds")

# Review table of distribution of boosted awards
table(minimized_boosts$boost_predicted)




# Compare actual vs optimized distributions of boost amounts
# Calculate density curve comparison of optimized vs actual boost awards
p_actual_predicted_boost_dist <- minimized_boosts %>%
  select(boost_predicted,
         boost_actual) %>%
  gather(key, value, -trip_id) %>%
  mutate(value = value / 100) %>% # convert cents to dollars
  ggplot(., aes(x = value, group = key, color = key)) +
  geom_density(size = 2) +
  scale_color_manual(values = c("#F57E44", "#01AAC7")) +
  scale_y_continuous("Density") + 
  scale_x_continuous("Boost Dollars", 
                     labels = scales::dollar_format(),
                     breaks = scales::pretty_breaks(n = 6)) + 
  theme_minimal() +
  theme(legend.position = "bottom")

# Review the density curves and write the results out for presentation
print(p_actual_predicted_boost_dist)
ggsave("presentation/assets/img/p_actual_predicted_boost_dist.png",
       p_actual_predicted_boost_dist, width = 10, height = 6)




# What is the total likelihood of trips claimed for optimized versus actual boosts?
# I only have one iteration of boost optimization.
# Need to compare that prediction to the total likelihood of trips with the 
# actual boost amount. That will tell us whether we need to run additional 
# iterations (theoretically)
d_boost_actual <- 
  d_boost %>%
  bind_cols(
    predict(xgb_fit, new_data = select(d_boost, -boost_claimed)),
    predict(xgb_fit, new_data = select(d_boost, -boost_claimed), type = "prob"),
    trip_id = d_boost_trip_ids
  ) %>%
  filter(boost_claimed == 1)

d_boost_actual <- d_boost_actual %>%
  filter(trip_id %in% boost_iteration_1$trip_id) 

d_boost_predicted <- boost_iteration_1 %>%
  filter(trip_id %in% d_boost_actual$trip_id)


# Review the actual and predicted results. The optimized (predicted) totals
# are actually larger than the actual totals. This is what we want, but is
# possible wildly overestimating the effectiveness of this approach.
sum(d_boost_actual$.pred_1)
sum(d_boost_predicted$.pred_1)

sum(d_boost_actual$cumulative_boost_amount_cents)
sum(d_boost_predicted$cumulative_boost_amount_cents)






#XGBoost Prediction Model on a sample data

model_vars <- c(
  "PackagePricing",
  "AGE", "GENDER", "BODY_WEIGHT", "BODY_HEIGHT",
  "HR_PULSE", "BP_HIGH", "BP_LOW", "RR", "HB",
  "TOTAL_LENGTH_OF_STAY", "LENGTH_OF_STAY_ICU",
  "LENGTH_OF_STAY_WARD", "IMPLANT_USED", "COST_OF_IMPLANT",
  "Diabetes1", "Diabetes2",
  "hypertension1", "hypertension2", "hypertension3"
)

df_model <- df_pricing %>% select(all_of(model_vars))

df_model$GENDER <- as.factor(df_model$GENDER)
df_model$Diabetes1 <- as.factor(df_model$Diabetes1)
df_model$Diabetes2 <- as.factor(df_model$Diabetes2)
df_model$hypertension1 <- as.factor(df_model$hypertension1)
df_model$hypertension2 <- as.factor(df_model$hypertension2)
df_model$hypertension3 <- as.factor(df_model$hypertension3)
df_model$IMPLANT_USED <- as.factor(df_model$IMPLANT_USED)
df_model$PackagePricing <- as.factor(df_model$PackagePricing)

df_dummy <- model.matrix(PackagePricing ~ ., data = df_model)[ , -1]
df_dummy <- as.data.frame(df_dummy)

df_dummy$PackagePricing <- df_model$PackagePricing

set.seed(123)

train_idx <- createDataPartition(df_dummy$PackagePricing, p = 0.8, list = FALSE)
train_data <- df_dummy[train_idx, ]
test_data  <- df_dummy[-train_idx, ]

library(xgboost)

train_matrix <- xgb.DMatrix(
  data = as.matrix(train_data %>% select(-PackagePricing)),
  label = as.numeric(train_data$PackagePricing) - 1
)

test_matrix <- xgb.DMatrix(
  data = as.matrix(test_data %>% select(-PackagePricing)),
  label = as.numeric(test_data$PackagePricing) - 1
)

final_xgb <- xgboost(
  data = train_matrix,
  objective = "binary:logistic",
  max.depth = 6,
  eta = 0.1,
  nrounds = 200,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 0
)

xgb_preds <- predict(final_xgb, newdata = test_matrix)
xgb_class <- ifelse(xgb_preds > 0.5, 1, 0)

confusionMatrix(as.factor(xgb_class), test_data$PackagePricing)




### Sample patient data ----
predict_package_price <- function(new_patient, df_model, train_data, final_xgb) {
  
  # Apply factor levelson actual patient testing data
  gender_levels <- levels(df_model$GENDER)
  implant_levels <- levels(df_model$IMPLANT_USED)
  Diabetes1_levels <- levels(df_model$Diabetes1)
  Diabetes2_levels <- levels(df_model$Diabetes2)
  hypertension1_levels <- levels(df_model$hypertension1)
  hypertension2_levels <- levels(df_model$hypertension2)
  hypertension3_levels <- levels(df_model$hypertension3)
  
  #converting into factors
  new_patient$GENDER <- factor(new_patient$GENDER, levels = gender_levels)
  new_patient$IMPLANT_USED <- factor(new_patient$IMPLANT_USED, levels = implant_levels)
  new_patient$Diabetes1 <- factor(new_patient$Diabetes1, levels = Diabetes1_levels)
  new_patient$Diabetes2 <- factor(new_patient$Diabetes2, levels = Diabetes2_levels)
  new_patient$hypertension1 <- factor(new_patient$hypertension1, levels = hypertension1_levels)
  new_patient$hypertension2 <- factor(new_patient$hypertension2, levels = hypertension2_levels)
  new_patient$hypertension3 <- factor(new_patient$hypertension3, levels = hypertension3_levels)
  
  # Dummy encode new patient
  new_dummy <- model.matrix(~ ., new_patient)[ , -1] %>% as.data.frame()
  
  # Align columns with training data
  train_cols <- colnames(train_data %>% select(-PackagePricing))
  
  for (col in train_cols) {
    if (!(col %in% colnames(new_dummy))) {
      new_dummy[[col]] <- 0
    }
  }
  
  new_dummy <- new_dummy[, train_cols]
  
  # Convert to matrix
  new_matrix <- xgb.DMatrix(as.matrix(new_dummy))
  
  # Predict
  prob <- predict(final_xgb, new_matrix)
  class <- ifelse(prob > 0.5, 1, 0)
  
  return(list(predicted_class = class, probability = prob))
}

new_patient <- data.frame(
  AGE = 59,
  GENDER = "M",
  BODY_WEIGHT = 72,
  BODY_HEIGHT = 168,
  HR_PULSE = 82,
  BP_HIGH = 140,
  BP_LOW = 90,
  RR = 20,
  HB = 12.5,
  TOTAL_LENGTH_OF_STAY = 5,
  LENGTH_OF_STAY_ICU = 1,
  LENGTH_OF_STAY_WARD = 4,
  IMPLANT_USED = "N",
  COST_OF_IMPLANT = 0,
  Diabetes1 = 0,
  Diabetes2 = 1,
  hypertension1 = 0,
  hypertension2 = 1,
  hypertension3 = 0
)
nrow(new_patient)

result <- predict_package_price(new_patient, df_model, train_data, final_xgb)

nrow(new_dummy)
result

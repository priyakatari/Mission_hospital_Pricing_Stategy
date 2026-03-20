pricing_map <- data.frame(
  KEY_COMPLAINTS_CODE = c("RHD", "other- heart", "CAD-TVD", "CAD-DVD", "NONE"),
  PackagePricing = c(1, 1, 1, 0, 0)
)

df_pricing <- df %>%
  left_join(pricing_map, by = "KEY_COMPLAINTS_CODE")

df_pricing <- df_pricing %>% drop_na(PackagePricing)
df_pricing$PackagePricing <- as.factor(df_pricing$PackagePricing)

model_vars <- c(
  "PackagePricing",
  "AGE", "GENDER", "BODY_WEIGHT", "BODY_HEIGHT",
  "HR_PULSE", "BP_HIGH", "BP_LOW", "RR", "HB",
  "TOTAL_LENGTH_OF_STAY", "LENGTH_OF_STAY_ICU",
  "LENGTH_OF_STAY_WARD", "IMPLANT_USED", "COST_OF_IMPLANT","Diabetes1",
  "Diabetes2","hypertension1","hypertension2","hypertension3"
)

df_model <- df_pricing %>% select(all_of(model_vars))
### CONVERT GENDER AND IMPLANT INTO FACTOR
df_model$GENDER <- as.factor(df_model$GENDER)
df_model$Diabetes1 <- as.factor(df_model$Diabetes1)
df_model$Diabetes2 <- as.factor(df_model$Diabetes2)
df_model$hypertension1 <- as.factor(df_model$hypertension1)
df_model$hypertension2 <- as.factor(df_model$hypertension2)
df_model$hypertension3 <- as.factor(df_model$hypertension3)
df_model$IMPLANT_USED <- as.factor(df_model$IMPLANT_USED)


#DUMMY ENCODING
df_dummy <- model.matrix(PackagePricing ~ ., data = df_model)[ , -1]
df_dummy <- as.data.frame(df_dummy)

df_dummy$PackagePricing <- df_model$PackagePricing

#SPLIT DATASET
set.seed(123)
train_idx <- createDataPartition(df_dummy$PackagePricing, p = 0.8, list = FALSE)
train_data <- df_dummy[train_idx, ]
test_data  <- df_dummy[-train_idx, ]


#MODEL 1 - LOGISTIC REGRESSION
log_model <- glm(PackagePricing ~ ., data = train_data, family = binomial)
log_preds_prob <- predict(log_model, newdata = test_data, type = "response")
log_preds_class <- ifelse(log_preds_prob > 0.5, 1, 0)
log_results <- confusionMatrix(as.factor(log_preds_class), test_data$PackagePricing)
log_results

#MODEL 2 - RANDOM FOREST
library(randomForest)

rf_model <- randomForest(PackagePricing ~ ., data = train_data, ntree = 300)
rf_preds <- predict(rf_model, newdata = test_data)

rf_results <- confusionMatrix(rf_preds, test_data$PackagePricing)
rf_results

#MODEL 3 - XGBOOST
library(xgboost)

train_matrix <- xgb.DMatrix(
  data = as.matrix(train_data %>% select(-PackagePricing)),
  label = as.numeric(train_data$PackagePricing) - 1
)

test_matrix <- xgb.DMatrix(
  data = as.matrix(test_data %>% select(-PackagePricing)),
  label = as.numeric(test_data$PackagePricing) - 1
)

xgb_model <- xgboost(
  data = train_matrix,
  max.depth = 6,
  eta = 0.1,
  nrounds = 200,
  objective = "binary:logistic",
  verbose = 0
)

xgb_preds <- predict(xgb_model, test_matrix)
xgb_class <- ifelse(xgb_preds > 0.5, 1, 0)

xgb_results <- confusionMatrix(as.factor(xgb_class), test_data$PackagePricing)
xgb_results

#results
comparison <- data.frame(
  Model = c("Logistic Regression", "Random Forest", "XGBoost"),
  Accuracy = c(
    log_results$overall["Accuracy"],
    rf_results$overall["Accuracy"],
    xgb_results$overall["Accuracy"]
  ),
  Sensitivity = c(
    log_results$byClass["Sensitivity"],
    rf_results$byClass["Sensitivity"],
    xgb_results$byClass["Sensitivity"]
  ),
  Specificity = c(
    log_results$byClass["Specificity"],
    rf_results$byClass["Specificity"],
    xgb_results$byClass["Specificity"]
  )
)

print(comparison)
#Randomforest and XGBoost has the highest and same accuracy value out of all 3 models
###XGBoost has the better sensitivity than Random Forest
#We will cxhoose XGBoost over Randomforest
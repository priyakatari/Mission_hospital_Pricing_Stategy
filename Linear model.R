library(caret)
library(tidyverse)

results_list <- list()

complaints <- unique(df$KEY_COMPLAINTS_CODE)
model_vars <- c(
  "TOTAL_COST_TO_HOSPITAL",
  "AGE", "GENDER", "BODY_WEIGHT", "BODY_HEIGHT",
  "HR_PULSE", "BP_HIGH", "BP_LOW", "RR", "HB",
  "TOTAL_LENGTH_OF_STAY", "LENGTH_OF_STAY_ICU",
  "LENGTH_OF_STAY_WARD", "IMPLANT_USED", "COST_OF_IMPLANT"
)

df_comp <- df_selected %>%
  filter(KEY_COMPLAINTS_CODE == comp) %>%
  select(all_of(model_vars))

for (comp in complaints) {
  
  cat("\n=============================\n")
  cat("Processing Complaint:", comp, "\n")
  cat("=============================\n")
  
  df_comp <- df_selected %>% filter(KEY_COMPLAINTS_CODE == comp)
  
  # Must have at least 20 rows to model reliably
  if (nrow(df_comp) < 20) {
    cat("Skipping:", comp, "(too few records:", nrow(df_comp), ")\n")
    next
  }
  
  # Build model.matrix WITHOUT losing the target
  formula <- as.formula(paste("TOTAL_COST_TO_HOSPITAL ~ ."))
  
  df_dummy <- model.matrix(formula, data = df_comp) %>% as.data.frame()
  
  # model.matrix includes intercept as "(Intercept)"
  # The target is in df_comp, so add it back explicitly
  df_dummy$TOTAL_COST_TO_HOSPITAL <- df_comp$TOTAL_COST_TO_HOSPITAL
  
  # Final check: must have at least 2 unique y values
  if (length(unique(df_dummy$TOTAL_COST_TO_HOSPITAL)) < 2) {
    cat("Skipping:", comp, "(target has <2 unique values)\n")
    next
  }
  
  # Train/test split
  set.seed(123)
  
  train_idx <- tryCatch({
    createDataPartition(df_dummy$TOTAL_COST_TO_HOSPITAL, p = 0.8, list = FALSE)
  }, error = function(e) {
    cat("Skipping:", comp, "(Partition error:", e$message, ")\n")
    return(NULL)
  })
  
  if (is.null(train_idx)) next
  
  train_data <- df_dummy[train_idx, ]
  test_data  <- df_dummy[-train_idx, ]
  
  # Fit linear model
  lm_model <- lm(TOTAL_COST_TO_HOSPITAL ~ ., data = train_data)
  
  preds <- predict(lm_model, newdata = test_data)
  
  rmse <- RMSE(preds, test_data$TOTAL_COST_TO_HOSPITAL)
  mae  <- MAE(preds, test_data$TOTAL_COST_TO_HOSPITAL)
  r2   <- R2(preds, test_data$TOTAL_COST_TO_HOSPITAL)
  
  results_list[[comp]] <- data.frame(
    Complaint = comp,
    Count = nrow(df_comp),
    RMSE = rmse,
    MAE = mae,
    R2 = r2
  )
}

final_results <- bind_rows(results_list) %>% arrange(desc(R2))
print(final_results)

###########################################
# Mission Hospital – Cost Prediction Model (Updated)
# File: MH Modified Data.xlsx
# Steps: Load → Clean → Select → Visualize → Model → Compare → Pricing Strategy
###########################################

### Step 1: Load Libraries and Data ----
# Load libraries
library(readxl)
library(tidyverse)
library(skimr)
library(corrplot)
library(caret)
library(randomForest)
library(ggplot2)
library(xgboost)
library(glmnet)

# Load dataset
df <- read_excel("MH Modified Data.xlsx")
glimpse(df)

# Optional: Rename columns if Excel headers have spaces or special chars
names(df) <- make.names(names(df))
df <-df[,-1]

### Step 2: Data Cleaning ----
# Check for missing values
colSums(is.na(df))  #missing values found in BP_HIGH and BP_LOW

#df$BP_LOW <- gsub("[^0-9.]", "", df$BP_LOW)   # remove non-numeric characters
df$BP_LOW <- as.numeric(df$BP_LOW)            # convert to numeric

# Numeric and categorical columns
num_cols <- sapply(df, is.numeric)
num_cols  #retuned BP_LOW as not numeric , so need to change it 

num_cols #retunred all numerical columns as numerical

cat_cols <- names(df)[!num_cols]
str(df$BP_LOW)
# Impute missing numeric values with mean (in BP_HIGH and BP_LOW)
df[num_cols] <- lapply(df[num_cols], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)) 

# Replace missing categorical values with "Unknown"
df[cat_cols] <- lapply(df[cat_cols], function(x) ifelse(is.na(x), "Unknown", x))

# Remove duplicates
#df <- df %>% distinct()

### Step 3: Feature Selection ----
# Select most relevant columns based on your updated dataset
# Adjust column names below based on your Excel file headers
df_selected <- df

# Convert categorical columns to factors
df_selected <- df_selected %>%
  mutate_if(is.character, as.factor)

str(df_selected)

### Step 4: Visualization & Data Insights ----
### visual 1 ------
library(scales)
ggplot(df_selected, aes(x = TOTAL_COST_TO_HOSPITAL)) +
  geom_histogram(bins = 30, fill = "#2B83BA", color = "white", alpha = 0.9) +
  
  # Add mean and median lines
  geom_vline(aes(xintercept = mean(TOTAL_COST_TO_HOSPITAL)), 
             color = "#ff7f00", linetype = "dashed", linewidth = 1.2) +
  geom_vline(aes(xintercept = median(TOTAL_COST_TO_HOSPITAL)), 
             color = "#CC3366", linetype = "dotted", linewidth = 1.2) +
  
  # Custom labels
  labs(
    title = "Hospital Cost Distribution Across Patients",
    Msubtitle = "Shows how treatment costs are spread across all medical cases",
    x = "Total Treatment Cost (INR)",
    y = "Number of Patients"
  ) +
  
  # Format axis and style
  scale_x_continuous(labels = label_comma(prefix = "₹")) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#003366"),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.title = element_text(face = "bold", color = "gray25"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 1)
  ) +
  annotate("text", x = mean(df_selected$TOTAL_COST_TO_HOSPITAL), 
           y = 45, label = "Mean", color = "#ff7f00", fontface = "bold", angle = 90, vjust = -0.5) +
  annotate("text", x = median(df_selected$TOTAL_COST_TO_HOSPITAL), 
           y = 45, label = "Median", color = "#CC3366", fontface = "bold", angle = 90, vjust = -0.5)



### Visual 2 Correlation ------
num_vars <- df_selected %>%
  select(
    AGE, BODY_WEIGHT, BODY_HEIGHT, HR_PULSE, BP_HIGH, RR, HB,
    TOTAL_LENGTH_OF_STAY, LENGTH_OF_STAY_ICU, LENGTH_OF_STAY_WARD,
    COST_OF_IMPLANT, TOTAL_COST_TO_HOSPITAL
  )

library(corrplot)
library(RColorBrewer)


#checking if all columns are numeric
names(num_vars)[!sapply(num_vars, is.numeric)] #BP_LOW returned as not numeric


# Compute correlation
corr_matrix <- cor(num_vars, use = "complete.obs")

# Enhanced color palette
col_scheme <- colorRampPalette(c("#6D9EC1", "white", "#E75480"))(200)

# Generate professional corrplot
par(oma = c(0, 0, 0, 0))         # outer margins
par(mar = c(4, 6, 2, 4))         # inner margins (bottom, left, top, right)
corrplot(
  corr_matrix,
  method = "color",
  col = col_scheme,
  tl.cex = 0.5,
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.7,
  addCoef.col = "black",      # show correlation values
  mar = c(0,0,2,0),
  title = "Correlation Matrix: Key Drivers of Hospital Cost",
  cl.pos = "r",
  cl.cex = 0.8
)



### Visual 3 ----
library(scales)
# Average cost per key complaint
# Compute summary: include both cost and additional context
avg_cost_summary <- df_selected %>%
  group_by(KeyComplaint = KEY_COMPLAINTS_CODE) %>%
  summarise(
    Average_Cost = mean(TOTAL_COST_TO_HOSPITAL, na.rm = TRUE),
    Avg_ICU_Stay = mean(LENGTH_OF_STAY_ICU, na.rm = TRUE),
    Implant_Rate = mean(as.numeric(IMPLANT_USED == "Y"), na.rm = TRUE)
  ) %>%
  arrange(desc(Average_Cost))

# Enhanced visual
ggplot(avg_cost_summary, aes(x = reorder(KeyComplaint, Average_Cost), y = Average_Cost, fill = Average_Cost)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  coord_flip() +
  
  # Add value labels on bars
  geom_text(aes(label = paste0("₹", format(round(Average_Cost, 0), big.mark = ","))), 
            hjust = -0.1, color = "black", size = 3.5, fontface = "bold") +
  
  # Color scale: low = soft blue, high = rose pink
  scale_fill_gradient(low = "#1f77b4", high = "#E75480") +
  
  # Titles and labels
  labs(
    title = "Average Hospital Cost by Key Complaint Category",
    subtitle = "High-cost complaints often align with longer ICU stays or implant usage",
    x = "Key Complaint (Medical Case Type)",
    y = "Average Treatment Cost (INR)"
  ) +
  
  # Currency formatting on y-axis
  scale_y_continuous(labels = label_comma(prefix = "₹")) +
  
  # Clean, business-style theme
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#002147"),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.title.x = element_text(face = "bold", color = "#444444"),
    axis.title.y = element_text(face = "bold", color = "#444444"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    panel.grid.major.y = element_blank(),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 1)
  ) +
  expand_limits(y = max(avg_cost_summary$Average_Cost) * 1.15)   # add space for labels

#second layer cost with stay in ICU
ggplot(avg_cost_summary, aes(x = reorder(KeyComplaint, Average_Cost))) +
  geom_col(aes(y = Average_Cost, fill = Average_Cost), show.legend = FALSE) +
  geom_point(aes(y = Avg_ICU_Stay * 50000), color = "#003366", size = 3) +
  geom_line(aes(y = Avg_ICU_Stay * 50000, group = 1), color = "#003366", linetype = "dashed") +
  coord_flip() +
  scale_y_continuous(
    name = "Average Cost (₹)",
    sec.axis = sec_axis(~./50000, name = "Average ICU Stay (Days)")
  ) +
  labs(title = "Average Cost vs ICU Stay by Key Complaint",
       subtitle = "Dual-axis view showing cost alongside ICU duration") +
  theme_minimal()
### Visual 4 -----
library(scales)
library(ggplot2)

# Count patients per gender (optional)
gender_summary <- df_selected %>%
  group_by(GENDER) %>%
  summarise(
    Mean_Cost = mean(TOTAL_COST_TO_HOSPITAL, na.rm = TRUE),
    Median_Cost = median(TOTAL_COST_TO_HOSPITAL, na.rm = TRUE),
    Count = n()
  )

# Enhanced Boxplot
ggplot(df_selected, aes(x = GENDER, y = TOTAL_COST_TO_HOSPITAL, fill = GENDER)) +
  geom_boxplot(alpha = 0.85, width = 0.6, outlier.color = "#999999", outlier.shape = 16) +
  
  # Overlay mean points
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "#E75480") +
  
  # Add labels for sample size under each box
  geom_text(data = gender_summary,
            aes(x = GENDER, y = 0, label = paste0("n = ", Count)),
            vjust = 1.5, size = 3.5, color = "gray30") +
  
  # Professional colors
  scale_fill_manual(values = c("Male" = "#1f77b4", "Female" = "#E75480")) +
  
  # Titles and labels
  labs(
    title = "Treatment Cost Distribution by Gender",
    subtitle = "Median, mean, and spread of hospital costs among male and female patients",
    x = "Gender",
    y = "Total Treatment Cost (INR)"
  ) +
  
  # Axis formatting
  scale_y_continuous(labels = label_comma(prefix = "₹")) +
  
  # Business presentation theme
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#002147"),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.title.x = element_text(face = "bold", color = "#444444"),
    axis.title.y = element_text(face = "bold", color = "#444444"),
    axis.text = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    plot.caption = element_text(size = 10, color = "gray40", hjust = 1)
  )

### Step 5: Linear Regression ----
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

### Step 6: Build Multiple Models ----
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


### Step 7: Package Pricing Strategy  ----
# Determine  package pricing per key complaint

library(dplyr)

# STEP 1: Complaints eligible for package pricing
eligible_complaints <- c("RHD", "other- heart", "CAD-TVD")

# Filter dataset
df_eligible <- df_selected %>% 
  filter(KEY_COMPLAINTS_CODE %in% eligible_complaints)

# STEP 2: Function to calculate package price using your formula
calculate_package_price <- function(costs) {
  p75 <- quantile(costs, 0.75, na.rm = TRUE)   # 75th percentile
  safety_buffer <- 0.10                        # 10%
  profit_margin <- 0.15                        # 15%
  
  package_price <- p75 * (1 + safety_buffer + profit_margin)
  return(package_price)
}

# STEP 3: Summary table for each eligible complaint
package_pricing_table <- df_eligible %>%
  group_by(KEY_COMPLAINTS_CODE) %>%
  summarise(
    Count = n(),
    Mean_Cost = mean(TOTAL_COST_TO_HOSPITAL, na.rm = TRUE),
    Median_Cost = median(TOTAL_COST_TO_HOSPITAL, na.rm = TRUE),
    P75_Cost = quantile(TOTAL_COST_TO_HOSPITAL, 0.75, na.rm = TRUE),
    Recommended_Package = calculate_package_price(TOTAL_COST_TO_HOSPITAL)
  ) %>%
  arrange(KEY_COMPLAINTS_CODE)

print(package_pricing_table)

formatted_table <- package_pricing_table %>%
  mutate(
    Mean_Cost = comma(Mean_Cost, prefix = "₹"),
    Median_Cost = comma(Median_Cost, prefix = "₹"),
    P75_Cost = comma(P75_Cost, prefix = "₹"),
    Recommended_Package = comma(Recommended_Package, prefix = "₹")
  )

formatted_table


### Method 2 by cleaning outliers and calculating estimated price -----
library(dplyr)

eligible_complaints <- c("RHD", "other- heart", "CAD-TVD")

df_eligible <- df_selected %>% 
  filter(KEY_COMPLAINTS_CODE %in% eligible_complaints)

# IQR function to remove outliers
remove_outliers_iqr <- function(df) {
  Q1  <- quantile(df$TOTAL_COST_TO_HOSPITAL, 0.25, na.rm = TRUE)
  Q3  <- quantile(df$TOTAL_COST_TO_HOSPITAL, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  
  df %>% 
    filter(TOTAL_COST_TO_HOSPITAL >= lower & TOTAL_COST_TO_HOSPITAL <= upper)
}

# Apply outlier removal per complaint
df_cleaned <- df_eligible %>%
  group_by(KEY_COMPLAINTS_CODE) %>%
  group_modify(~ remove_outliers_iqr(.x)) %>%
  ungroup()

# Package price formula
calculate_package_price <- function(costs) {
  p75 <- quantile(costs, 0.75, na.rm = TRUE)
  safety_buffer <- 0.10
  profit_margin <- 0.15
  
  p75 * (1 + safety_buffer + profit_margin)
}

# Final price table
package_pricing_table <- df_cleaned %>%
  group_by(KEY_COMPLAINTS_CODE) %>%
  summarise(
    Count_after_cleaning = n(),
    Mean_Cost = mean(TOTAL_COST_TO_HOSPITAL),
    Median_Cost = median(TOTAL_COST_TO_HOSPITAL),
    P75_Cost = quantile(TOTAL_COST_TO_HOSPITAL, 0.75),
    Recommended_Package = calculate_package_price(TOTAL_COST_TO_HOSPITAL)
  )

package_pricing_table

package_pricing_table%>% select(KEY_COMPLAINTS_CODE,P75_Cost, Recommended_Package)


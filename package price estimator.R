library(dplyr)

# STEP 1: Complaints eligible for package pricing
eligible_complaints <- c("RHD", "other- heart", "CAD-TVD")

# Filter dataset
df_eligible <- df_selected %>% 
  filter(KEY_COMPLAINTS_CODE %in% eligible_complaints)

# STEP 2: Function to calculate package price using your formula
calculate_package_price <- function(costs) {
  p80 <- quantile(costs, 0.8, na.rm = TRUE)   # 80th percentile
  safety_buffer <- 0.10                        # 10%
  profit_margin <- 0.15                        # 15%
  
  package_price <- p80 * (1 + safety_buffer + profit_margin)
  return(package_price)
}

# STEP 3: Summary table for each eligible complaint
package_pricing_table <- df_eligible %>%
  group_by(KEY_COMPLAINTS_CODE) %>%
  summarise(
    Count = n(),
    Mean_Cost = mean(TOTAL_COST_TO_HOSPITAL, na.rm = TRUE),
    Median_Cost = median(TOTAL_COST_TO_HOSPITAL, na.rm = TRUE),
    P75_Cost = quantile(TOTAL_COST_TO_HOSPITAL, 0.80, na.rm = TRUE),
    Recommended_Package = calculate_package_price(TOTAL_COST_TO_HOSPITAL)
  ) %>%
  arrange(KEY_COMPLAINTS_CODE)

print(package_pricing_table)

formatted_table <- package_pricing_table %>%
  mutate(
    Mean_Cost = comma(Mean_Cost, prefix = "₹"),
    Median_Cost = comma(Median_Cost, prefix = "₹"),
    P75_Cost = comma(P80_Cost, prefix = "₹"),
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


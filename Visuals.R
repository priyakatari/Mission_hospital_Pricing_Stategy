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


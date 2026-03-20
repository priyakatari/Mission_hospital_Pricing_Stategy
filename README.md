# 🏥 Package Pricing Prediction at Mission Hospital

A predictive analytics project to help Mission Hospital (Durgapur, India) build a data-driven, transparent, and competitive **package pricing system** for cardiac treatments — using R and Power BI.

---

## 📌 Project Overview

Mission Hospital is a super-specialty cardiology hospital serving patients from diverse socio-economic backgrounds, including medical tourists from neighboring countries. Package pricing — where a fixed cost is quoted before admission — improves transparency for patients and financial planning for the hospital.

This project develops a full pipeline to:
- Analyze historical treatment cost patterns across cardiac complaints
- Identify which complaints are **suitable for package pricing** (low cost variability)
- Build **ML models** to predict patient-level pricing suitability
- Calculate a **risk-adjusted package price** using a statistical pricing formula

---

## 📂 Repository Structure

```
├── DEA.R                        # Data loading, cleaning, EDA, and cost prediction models
├── multiple_models.R            # Classification models: Logistic Regression, Random Forest, XGBoost
├── package_price_estimator.R    # Package price calculation logic (P75 + buffer + margin)
├── Visuals.R                    # ggplot2 visualizations (cost distribution, correlation, etc.)
├── Mission_Hospital_Dashboard.pbix   # Power BI interactive dashboard
├── proposal.docx                # Full project proposal and methodology
└── Package_Pricing_prediction_at_mission_hospitals.pptx  # Final presentation
```

---

## 📊 Dataset

- **248 patients** treated at Mission Hospital
- **Source:** Internal hospital records (`MH Modified Data.xlsx`)

| Category | Features |
|---|---|
| Demographics | Age, Gender, Marital Status, Body Weight, Height |
| Clinical Vitals | Blood Pressure (High/Low), Pulse, Respiratory Rate, Hemoglobin |
| Resource Utilization | ICU Stay (days), Ward Stay (days), Total Length of Stay |
| Procedure | Implant Used (Y/N), Cost of Implant |
| Comorbidities | Diabetes Type, Hypertension Type |
| Complaint | `KEY_COMPLAINTS_CODE` (RHD, CAD-TVD, CAD-DVD, etc.) |
| **Target** | **Total Cost to Hospital (INR)** |

---

## 🔬 Methodology

### 1. Data Preparation (`DEA.R`)
- Identified and imputed missing values in `BP_HIGH` and `BP_LOW` using column means
- Standardized categorical variables and encoded them as factors
- Removed duplicates and converted non-numeric fields
- Performed exploratory data analysis (EDA) including outlier detection

### 2. Complaint-Wise Cost Predictability (Linear Regression)
Each cardiac complaint was evaluated independently for cost predictability using Linear Regression. Complaints were assessed on:
- **R²** — proportion of cost variation explained (threshold: ≥ 0.6 for package pricing eligibility)
- **RMSE** and **MAE** — prediction accuracy

**Eligible complaints identified for package pricing:**

| Complaint | Pricing Recommendation |
|---|---|
| RHD (Rheumatic Heart Disease) | ✅ Package Pricing |
| Other-Heart | ✅ Package Pricing |
| CAD-TVD | ✅ Package Pricing |
| CAD-DVD | ❌ Flat Pricing |
| NONE | ❌ Flat Pricing |

### 3. Package Price Formula (`package_price_estimator.R`)
For eligible complaints, outliers are removed using the IQR method, then the package price is calculated as:

```
Package Price = P75 × (1 + 10% Safety Buffer + 15% Profit Margin)
```

- **P75** — 75th percentile cost captures typical upper-end clinical variation
- **10% Safety Buffer** — accounts for unforeseen complexity (extra ICU days, complications)
- **15% Profit Margin** — ensures financial sustainability while remaining competitive

### 4. ML Classification Models (`multiple_models.R`)
A binary classification model was built to predict patient-level suitability for package pricing (`1 = suitable`, `0 = flat pricing`).

**Features used:**
- Age, Gender, Weight, Height
- BP, Pulse, RR, Hemoglobin
- ICU/Ward/Total Length of Stay
- Implant usage & cost
- Diabetes and Hypertension indicators

**Models trained (80/20 train-test split):**

| Model | Notes |
|---|---|
| Logistic Regression | Baseline classifier |
| Random Forest | Ensemble method (300 trees) |
| **XGBoost** | **Best model — highest sensitivity** |

XGBoost was selected as the final model because it achieved the highest **sensitivity**, which is the critical metric for minimizing financial risk to the hospital (correctly identifying patients *safe* to offer package pricing to).

### 5. Visualizations (`Visuals.R`)
- Treatment cost distribution (histogram with mean/median lines)
- Correlation heatmap of clinical drivers vs. cost
- Average cost by complaint type
- Dual-axis view: ICU days vs. treatment cost

---

## 📈 Power BI Dashboard

`Mission_Hospital_Dashboard.pbix` provides interactive exploration of:
- Cost trends across complaints and patient demographics
- ICU utilization and its impact on total cost
- Complaint-wise pricing suitability overview

> Requires **Power BI Desktop** to open.

---

## 🛠️ Requirements

### R Packages
```r
install.packages(c(
  "readxl", "tidyverse", "skimr", "corrplot",
  "caret", "randomForest", "xgboost", "glmnet",
  "ggplot2", "scales", "dplyr"
))
```

### R Version
Developed and tested on **R 4.x** with RStudio.

### Data File
Place `MH Modified Data.xlsx` in your working directory before running any script.

---

## 🚀 How to Run

1. Clone the repository
   ```bash
   git clone https://github.com/your-username/mission-hospital-package-pricing.git
   cd mission-hospital-package-pricing
   ```

2. Add the dataset (`MH Modified Data.xlsx`) to the project folder

3. Run scripts in order:
   ```
   DEA.R                    → Data prep + EDA + complaint-wise regression
   Visuals.R                → Generate all ggplot2 charts
   package_price_estimator.R → Calculate recommended package prices
   multiple_models.R         → Train and evaluate ML classifiers
   ```

---

## 📋 Key Outcomes

- Identified **3 cardiac complaints** suitable for standardized package pricing
- Designed a **risk-adjusted pricing formula** grounded in statistical percentiles
- Selected **XGBoost** as the best classifier for patient-level pricing decisions
- Built an interactive **Power BI dashboard** for hospital management

---

## 👤 Authors

Developed as part of a predictive analytics capstone project.

📧 priyakatari.analyst@gmail.com

---

## 📄 License

This project is intended for academic and research purposes. All patient data is de-identified and used with permission from Mission Hospital.

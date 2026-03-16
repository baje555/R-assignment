setwd("/home/baje/Documents/R/NP2F2511IT_CT127-3-2-PFDA/CT127-3-2-PFDA")
# ============================================================
# Retail Transactional Data Analysis and Ratings Classification
# CT127-3-2-PFDA Group Assignment
# ============================================================
# Student 1: [Pankaj Kshetri], [NP070258]
# Student 2: [Pragesh Bashnet], [NP070268]
# Student 3: [ROJIT PRAJAPATI ], [NP070321]
# Student 4: [HARISH KUSLE ], [NP070193]
# ============================================================
# STUDENT 1 SCOPE:
# - Introduction, Data Description, Assumptions
# - Hypothesis 1 & Objective 1
# - Data Preparation (Import, Cleaning, Validation)
# - Analysis for Objective 1
# ============================================================

# ============================================================
# SECTION 0: Install & Load Required Libraries
# ============================================================
required_packages <- c("ggplot2", "dplyr", "tidyr", "corrplot",
                       "scales", "ggcorrplot", "moments")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# ============================================================
# SECTION 1: DATA IMPORT
# ============================================================

# Set file path
file_path <- "/home/baje/Documents/R/NP2F2511IT_CT127-3-2-PFDA/CT127-3-2-PFDA/6. retail_data.csv"

cat("=== IMPORTING DATA ===\n")
retail_data <- read.csv(file_path, stringsAsFactors = FALSE,
                        na.strings = c("", "NA", "N/A", "null", "NULL"))

cat("Dataset successfully loaded.\n")
cat("Dimensions:", nrow(retail_data), "rows x", ncol(retail_data), "columns\n\n")

cat("=== FIRST 6 ROWS ===\n")
print(head(retail_data))

cat("\n=== COLUMN NAMES ===\n")
print(names(retail_data))

cat("\n=== DATA TYPES ===\n")
print(str(retail_data))

# ============================================================
# SECTION 2: DATA CLEANING & PRE-PROCESSING
# ============================================================
cat("\n=== DATA CLEANING & PRE-PROCESSING ===\n")

# --- 2.1 Check for Missing Values ---
cat("\n--- Missing Values Per Column ---\n")
missing_summary <- colSums(is.na(retail_data))
print(missing_summary[missing_summary > 0])
cat("Total missing values:", sum(is.na(retail_data)), "\n")

# Remove rows where Ratings (target variable) is missing
retail_data <- retail_data[!is.na(retail_data$Ratings), ]
cat("Rows after removing missing Ratings:", nrow(retail_data), "\n")

# --- 2.2 Remove Duplicate Records ---
cat("\n--- Checking for Duplicates ---\n")
dup_count <- sum(duplicated(retail_data))
cat("Duplicate rows found:", dup_count, "\n")

if (dup_count > 0) {
  retail_data <- retail_data[!duplicated(retail_data), ]
  cat("Duplicates removed. Remaining rows:", nrow(retail_data), "\n")
} else {
  cat("No duplicates found.\n")
}

# --- 2.3 Standardize Column Names (remove spaces) ---
names(retail_data) <- gsub(" ", "_", names(retail_data))
cat("\nColumn names after standardization:\n")
print(names(retail_data))

# --- 2.4 Data Type Conversion ---
cat("\n--- Converting Data Types ---\n")

# Convert Ratings from text ("High","Low","Medium") to ordered numeric
# Check what unique values exist first
cat("Unique Ratings values found:", paste(unique(retail_data$Ratings), collapse=", "), "\n")

retail_data$Ratings_Numeric <- dplyr::case_when(
  retail_data$Ratings == "Low"    ~ 1,
  retail_data$Ratings == "Medium" ~ 2,
  retail_data$Ratings == "High"   ~ 3,
  TRUE ~ NA_real_
)

cat("Ratings mapped: Low=1, Medium=2, High=3\n")
cat("Ratings_Numeric distribution:\n")
print(table(retail_data$Ratings_Numeric, useNA = "always"))

# Convert categorical columns to factors
retail_data$Gender           <- as.factor(retail_data$Gender)
retail_data$Income           <- as.factor(retail_data$Income)
retail_data$Customer_Segment <- as.factor(retail_data$Customer_Segment)
retail_data$Shipping_Method  <- as.factor(retail_data$Shipping_Method)
retail_data$Payment_Method   <- as.factor(retail_data$Payment_Method)
retail_data$Order_Status     <- as.factor(retail_data$Order_Status)
retail_data$Feedback         <- as.factor(retail_data$Feedback)
retail_data$Product_Category <- as.factor(retail_data$Product_Category)

# Ratings as ordered factor (for plots)
retail_data$Ratings <- factor(retail_data$Ratings,
                              levels = c("Low", "Medium", "High"),
                              ordered = TRUE)

# Numeric columns — using correct column names from dataset
retail_data$Age             <- as.numeric(retail_data$Age)
retail_data$Total_Purchases <- as.numeric(retail_data$Total_Purchases)
retail_data$Amount          <- as.numeric(retail_data$Amount)        # per-item amount
retail_data$Total_Amount    <- as.numeric(retail_data$Total_Amount)  # total spend

cat("Data type conversion complete.\n")

# --- 2.5 Handle Outliers in Key Numeric Variables (IQR Method) ---
cat("\n--- Outlier Check (IQR Method) ---\n")

remove_outliers_report <- function(df, col_name) {
  x <- df[[col_name]]
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  outliers <- sum(x < lower | x > upper, na.rm = TRUE)
  cat(col_name, "- Outliers:", outliers,
      "| Valid range: [", round(lower,2), "to", round(upper,2), "]\n")
  df <- df[is.na(df[[col_name]]) | (df[[col_name]] >= lower & df[[col_name]] <= upper), ]
  return(df)
}

retail_data <- remove_outliers_report(retail_data, "Age")
retail_data <- remove_outliers_report(retail_data, "Total_Purchases")
retail_data <- remove_outliers_report(retail_data, "Total_Amount")
cat("Rows after outlier removal:", nrow(retail_data), "\n")

# --- 2.6 Impute Remaining Missing Values ---
cat("\n--- Imputing Remaining Missing Values ---\n")

# Numeric: fill with median
for (col in c("Age", "Total_Purchases", "Amount", "Total_Amount")) {
  if (any(is.na(retail_data[[col]]))) {
    med_val <- median(retail_data[[col]], na.rm = TRUE)
    retail_data[[col]][is.na(retail_data[[col]])] <- med_val
    cat("Imputed", col, "with median:", med_val, "\n")
  }
}

# Categorical: fill with mode
get_mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}

for (col in c("Gender", "Income", "Customer_Segment", "Payment_Method")) {
  if (any(is.na(retail_data[[col]]))) {
    mode_val <- get_mode(retail_data[[col]])
    retail_data[[col]][is.na(retail_data[[col]])] <- mode_val
    cat("Imputed", col, "with mode:", as.character(mode_val), "\n")
  }
}

cat("\nCleaning complete. Final dimensions:", nrow(retail_data), "x", ncol(retail_data), "\n")

# ============================================================
# SECTION 3: DATA VALIDATION
# ============================================================
cat("\n=== DATA VALIDATION ===\n")

# Validate Age (18–100)
invalid_age <- sum(retail_data$Age < 18 | retail_data$Age > 100, na.rm = TRUE)
cat("Invalid Age entries (outside 18-100):", invalid_age, "\n")
retail_data <- retail_data[retail_data$Age >= 18 & retail_data$Age <= 100, ]

# Validate Ratings_Numeric (must be 1, 2, or 3)
invalid_ratings <- sum(!retail_data$Ratings_Numeric %in% c(1,2,3), na.rm = TRUE)
cat("Invalid Ratings_Numeric entries:", invalid_ratings, "\n")
retail_data <- retail_data[retail_data$Ratings_Numeric %in% c(1,2,3), ]

# Validate Total_Amount (must be positive)
invalid_amount <- sum(retail_data$Total_Amount <= 0, na.rm = TRUE)
cat("Invalid Total_Amount (<=0):", invalid_amount, "\n")
retail_data <- retail_data[retail_data$Total_Amount > 0, ]

# Validate Total_Purchases (must be positive)
invalid_purchases <- sum(retail_data$Total_Purchases <= 0, na.rm = TRUE)
cat("Invalid Total_Purchases (<=0):", invalid_purchases, "\n")
retail_data <- retail_data[retail_data$Total_Purchases > 0, ]

cat("\nValidation complete. Final clean dataset:", nrow(retail_data), "rows\n")


cat("\n=== OBJECTIVE 1: CUSTOMER SEGMENT vs RATINGS ===\n")

# --- 4.1 Descriptive Statistics by Segment ---
cat("\n--- Descriptive Statistics: Ratings by Customer Segment ---\n")

segment_stats <- retail_data %>%
  group_by(Customer_Segment) %>%
  summarise(
    Count             = n(),
    Mean_Rating       = round(mean(Ratings_Numeric, na.rm = TRUE), 3),
    Median_Rating     = median(Ratings_Numeric, na.rm = TRUE),
    SD_Rating         = round(sd(Ratings_Numeric, na.rm = TRUE), 3),
    Mean_Purchases    = round(mean(Total_Purchases, na.rm = TRUE), 2),
    Mean_Total_Amount = round(mean(Total_Amount, na.rm = TRUE), 2),
    .groups = "drop"
  )
print(segment_stats)

cat("\n--- Overall Ratings Summary ---\n")
print(summary(retail_data$Ratings_Numeric))
cat("Skewness:", round(moments::skewness(retail_data$Ratings_Numeric, na.rm=TRUE), 4), "\n")
cat("Kurtosis:", round(moments::kurtosis(retail_data$Ratings_Numeric, na.rm=TRUE), 4), "\n")

cat("\n--- Ratings Label Distribution ---\n")
print(table(retail_data$Ratings))
print(round(prop.table(table(retail_data$Ratings)) * 100, 2))

cat("\n--- Customer Segment Distribution ---\n")
print(table(retail_data$Customer_Segment))
print(round(prop.table(table(retail_data$Customer_Segment)) * 100, 2))

# ============================================================
# SECTION 5: VISUALIZATIONS — OBJECTIVE 1
# ============================================================
cat("\n=== GENERATING VISUALIZATIONS ===\n")

seg_colors <- c("New"     = "#FF9800",
                "Premium" = "#2196F3",
                "Regular" = "#4CAF50")

# --- PLOT 1: Distribution of Ratings (Bar Chart) ---
rating_counts <- as.data.frame(table(retail_data$Ratings))
names(rating_counts) <- c("Rating", "Count")

p1 <- ggplot(rating_counts, aes(x = Rating, y = Count, fill = Rating)) +
  geom_col(alpha = 0.9, width = 0.6) +
  geom_text(aes(label = scales::comma(Count)), vjust = -0.4, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Low" = "#F44336", "Medium" = "#FF9800", "High" = "#4CAF50")) +
  labs(
    title    = "Distribution of Customer Ratings",
    subtitle = "Overall count of Low / Medium / High ratings",
    x        = "Rating Level",
    y        = "Number of Customers"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "none")

print(p1)
ggsave("plot1_ratings_distribution.png", p1, width = 8, height = 5, dpi = 150)
cat("Plot 1 saved.\n")

# --- PLOT 2: Average Numeric Rating by Customer Segment ---
p2 <- ggplot(segment_stats,
             aes(x = reorder(Customer_Segment, -Mean_Rating),
                 y = Mean_Rating, fill = Customer_Segment)) +
  geom_col(alpha = 0.9, width = 0.6) +
  geom_text(aes(label = Mean_Rating), vjust = -0.4, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = seg_colors) +
  coord_cartesian(ylim = c(0, 3.5)) +
  labs(
    title    = "Average Rating Score by Customer Segment",
    subtitle = "Analysis 1-1 | Low=1, Medium=2, High=3",
    x        = "Customer Segment",
    y        = "Average Rating Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "none")

print(p2)
ggsave("plot2_avg_rating_by_segment.png", p2, width = 8, height = 5, dpi = 150)
cat("Plot 2 saved.\n")

# --- PLOT 3: Stacked Bar — Rating proportions per Segment ---
prop_data <- retail_data %>%
  group_by(Customer_Segment, Ratings) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Customer_Segment) %>%
  mutate(Proportion = round(Count / sum(Count) * 100, 1))

p3 <- ggplot(prop_data, aes(x = Customer_Segment, y = Proportion, fill = Ratings)) +
  geom_col(position = "stack", alpha = 0.9) +
  geom_text(aes(label = paste0(Proportion, "%")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("Low" = "#F44336", "Medium" = "#FF9800", "High" = "#4CAF50")) +
  labs(
    title    = "Rating Proportions by Customer Segment",
    subtitle = "Analysis 1-1 | Percentage breakdown within each segment",
    x        = "Customer Segment",
    y        = "Proportion (%)",
    fill     = "Rating"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p3)
ggsave("plot3_stacked_rating_proportions.png", p3, width = 9, height = 5, dpi = 150)
cat("Plot 3 saved.\n")

# --- PLOT 4: Boxplot of Numeric Ratings by Customer Segment ---
p4 <- ggplot(retail_data,
             aes(x = Customer_Segment, y = Ratings_Numeric, fill = Customer_Segment)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.color = "red") +
  scale_fill_manual(values = seg_colors) +
  scale_y_continuous(breaks = c(1,2,3), labels = c("Low","Medium","High")) +
  labs(
    title    = "Rating Distribution by Customer Segment",
    subtitle = "Analysis 1-1 | Spread and median comparison",
    x        = "Customer Segment",
    y        = "Rating Level"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "none")

print(p4)
ggsave("plot4_boxplot_ratings_segment.png", p4, width = 9, height = 5, dpi = 150)
cat("Plot 4 saved.\n")

# --- PLOT 5: Total Purchases vs Numeric Rating (Scatter + trend) ---
p5 <- ggplot(retail_data,
             aes(x = Total_Purchases, y = Ratings_Numeric, color = Customer_Segment)) +
  geom_jitter(alpha = 0.15, size = 1, height = 0.15) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1),
              color = "black", linetype = "dashed", size = 1) +
  scale_color_manual(values = seg_colors) +
  scale_y_continuous(breaks = c(1,2,3), labels = c("Low","Medium","High")) +
  labs(
    title    = "Total Purchases vs Rating",
    subtitle = "Analysis 1-2 | Dashed line = overall linear trend",
    x        = "Total Purchases",
    y        = "Rating Level",
    color    = "Segment"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p5)
ggsave("plot5_purchases_vs_ratings.png", p5, width = 9, height = 5, dpi = 150)
cat("Plot 5 saved.\n")

# --- PLOT 6: Total Amount vs Rating by Segment ---
p6 <- ggplot(retail_data,
             aes(x = Total_Amount, y = Ratings_Numeric, color = Customer_Segment)) +
  geom_jitter(alpha = 0.15, size = 1, height = 0.15) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  scale_color_manual(values = seg_colors) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(breaks = c(1,2,3), labels = c("Low","Medium","High")) +
  labs(
    title    = "Total Amount Spent vs Rating by Segment",
    subtitle = "Analysis 1-3 | Spending power as external factor",
    x        = "Total Amount Spent ($)",
    y        = "Rating Level",
    color    = "Segment"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p6)
ggsave("plot6_amount_vs_ratings.png", p6, width = 9, height = 5, dpi = 150)
cat("Plot 6 saved.\n")

# --- PLOT 7: Heatmap — Segment vs Rating frequency ---
heatmap_data <- retail_data %>%
  group_by(Customer_Segment, Ratings) %>%
  summarise(Count = n(), .groups = "drop")

p7 <- ggplot(heatmap_data, aes(x = Ratings, y = Customer_Segment, fill = Count)) +
  geom_tile(color = "white", size = 0.8) +
  geom_text(aes(label = scales::comma(Count)), color = "white", fontface = "bold", size = 4) +
  scale_fill_gradient(low = "#BBDEFB", high = "#0D47A1",
                      labels = scales::comma) +
  labs(
    title    = "Heatmap: Customer Segment vs Rating Frequency",
    subtitle = "Analysis 1-1 | Frequency of each rating per segment",
    x        = "Rating Level",
    y        = "Customer Segment",
    fill     = "Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p7)
ggsave("plot7_heatmap_segment_ratings.png", p7, width = 9, height = 5, dpi = 150)
cat("Plot 7 saved.\n")

# --- PLOT 8: Mean Total Amount per Segment per Rating ---
p8_data <- retail_data %>%
  group_by(Customer_Segment, Ratings) %>%
  summarise(Mean_Spend = round(mean(Total_Amount, na.rm=TRUE), 2), .groups="drop")

p8 <- ggplot(p8_data, aes(x = Ratings, y = Mean_Spend, fill = Customer_Segment)) +
  geom_col(position = "dodge", alpha = 0.9) +
  scale_fill_manual(values = seg_colors) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Mean Total Spend by Rating and Customer Segment",
    subtitle = "Analysis 1-3 | Does higher spend correlate with higher rating?",
    x        = "Rating Level",
    y        = "Mean Total Amount ($)",
    fill     = "Segment"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p8)
ggsave("plot8_mean_spend_by_rating_segment.png", p8, width = 9, height = 5, dpi = 150)
cat("Plot 8 saved.\n")

# ============================================================
# SECTION 6: STATISTICAL ANALYSIS — OBJECTIVE 1
# ============================================================
cat("\n=== STATISTICAL TESTS ===\n")

# --- Analysis 1-1: Kruskal-Wallis Test ---
# Non-parametric test: are ratings significantly different across segments?
cat("\n--- Analysis 1-1: Kruskal-Wallis Test (Segment vs Ratings) ---\n")
kw_test <- kruskal.test(Ratings_Numeric ~ Customer_Segment, data = retail_data)
print(kw_test)
if (kw_test$p.value < 0.05) {
  cat("RESULT: p < 0.05 — Significant difference in ratings across customer segments.\n")
} else {
  cat("RESULT: p >= 0.05 — No significant difference detected across segments.\n")
}

# --- Analysis 1-2: Spearman Correlation ---
cat("\n--- Analysis 1-2: Spearman Correlation ---\n")

sp1 <- cor.test(retail_data$Total_Purchases, retail_data$Ratings_Numeric,
                method = "spearman", exact = FALSE)
cat("Total_Purchases vs Ratings | rho =", round(sp1$estimate, 4),
    "| p =", round(sp1$p.value, 4), "\n")

sp2 <- cor.test(retail_data$Total_Amount, retail_data$Ratings_Numeric,
                method = "spearman", exact = FALSE)
cat("Total_Amount vs Ratings    | rho =", round(sp2$estimate, 4),
    "| p =", round(sp2$p.value, 4), "\n")

sp3 <- cor.test(retail_data$Age, retail_data$Ratings_Numeric,
                method = "spearman", exact = FALSE)
cat("Age vs Ratings             | rho =", round(sp3$estimate, 4),
    "| p =", round(sp3$p.value, 4), "\n")

# --- Analysis 1-3: Linear Regression ---
cat("\n--- Analysis 1-3: Linear Regression — Predictors of Ratings ---\n")
retail_data$Customer_Segment <- relevel(retail_data$Customer_Segment, ref = "Regular")

lm_model <- lm(Ratings_Numeric ~ Customer_Segment + Total_Purchases + Total_Amount + Age,
               data = retail_data)
print(summary(lm_model))

r2     <- summary(lm_model)$r.squared
adj_r2 <- summary(lm_model)$adj.r.squared
cat("R-squared:", round(r2, 4), "\n")
cat("Adjusted R-squared:", round(adj_r2, 4), "\n")

# --- Correlation Matrix (Numeric Variables) ---
cat("\n--- Spearman Correlation Matrix ---\n")
num_cols <- retail_data[, c("Age", "Total_Purchases", "Total_Amount", "Ratings_Numeric")]
cor_matrix <- cor(num_cols, use = "complete.obs", method = "spearman")
print(round(cor_matrix, 3))

p_corr <- ggcorrplot::ggcorrplot(cor_matrix,
                                 method = "circle",
                                 type   = "lower",
                                 lab    = TRUE,
                                 title  = "Spearman Correlation Matrix (Numeric Variables)",
                                 colors = c("#F44336", "white", "#2196F3"))
print(p_corr)
ggsave("plot9_correlation_matrix.png", p_corr, width = 7, height = 6, dpi = 150)
cat("Plot 9 saved: Correlation Matrix\n")

# ============================================================
# SECTION 7: FINDINGS SUMMARY
# ============================================================
cat("\n========================================================\n")
cat("         FINDINGS SUMMARY — OBJECTIVE 1\n")
cat("========================================================\n")
cat("\nHypothesis: Customers with higher purchasing power tend to\n")
cat("give higher ratings than those with lower purchasing power.\n\n")

cat("1. SEGMENT DESCRIPTIVE STATS:\n")
print(segment_stats)

cat("\n2. KRUSKAL-WALLIS TEST:\n")
cat("   Chi-sq =", round(kw_test$statistic, 4),
    "| df =", kw_test$parameter,
    "| p =", round(kw_test$p.value, 6), "\n")

cat("\n3. SPEARMAN CORRELATIONS:\n")
cat("   Total_Purchases vs Ratings: rho =", round(sp1$estimate, 4), "\n")
cat("   Total_Amount vs Ratings:    rho =", round(sp2$estimate, 4), "\n")
cat("   Age vs Ratings:             rho =", round(sp3$estimate, 4), "\n")

cat("\n4. REGRESSION MODEL:\n")
cat("   R-squared:", round(r2, 4), "\n")
cat("   Adjusted R-squared:", round(adj_r2, 4), "\n")

cat("\n========================================================\n")
cat("Script execution complete. All plots saved.\n")
cat("========================================================\n")

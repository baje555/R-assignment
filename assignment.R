
# ============================================================
# Retail Transactional Data Analysis and Ratings Classification
# CT127-3-2-PFDA Group Assignment
# ============================================================
# Student 1: [Pankaj Kshetri], [NP070258]
# Student 2: [Pragesh Bashnet], [NP070268]
# Student 3: [ROJIT PRAJAPATI ], [NP070321]
# Student 4: [HARISH KUSLE ], [NP070193]
# ============================================================
# =====================================================
# Retail Data Analysis – Objective 1
# =====================================================

# Load libraries
library(ggplot2)
library(dplyr)
library(moments)
library(ggcorrplot)

# =====================================================
# 1. Import Data
# =====================================================

data <- read.csv("/home/baje/Documents/R/NP2F2511IT_CT127-3-2-PFDA/CT127-3-2-PFDA/6. retail_data.csv")

head(data)
str(data)

# =====================================================
# 2. Data Cleaning
# =====================================================

# Remove rows with missing ratings
data <- data[!is.na(data$Ratings), ]

# Convert ratings to numeric
data$Ratings_Numeric <- ifelse(data$Ratings == "Low",1,
                               ifelse(data$Ratings == "Medium",2,3))

# Convert categorical variables
data$Customer_Segment <- as.factor(data$Customer_Segment)
data$Gender <- as.factor(data$Gender)

# Convert numeric variables
data$Age <- as.numeric(data$Age)
data$Total_Purchases <- as.numeric(data$Total_Purchases)
data$Total_Amount <- as.numeric(data$Total_Amount)

# =====================================================
# 3. Basic Statistics
# =====================================================

summary(data$Ratings_Numeric)

mean(data$Ratings_Numeric)
sd(data$Ratings_Numeric)

skewness(data$Ratings_Numeric)
kurtosis(data$Ratings_Numeric)

# Ratings distribution
table(data$Ratings)

# =====================================================
# 4. Exploratory Data Analysis
# =====================================================

# Ratings distribution
p1<-ggplot(data, aes(Ratings)) +
  geom_bar(fill="steelblue") +
  labs(title="Distribution of Ratings")
ggsave("plot1_ratings.png", p1, width=6, height=4)

# Average rating by segment
segment_stats <- data %>%
  group_by(Customer_Segment) %>%
  summarise(Mean_Rating = mean(Ratings_Numeric))

p2<-ggplot(segment_stats,
       aes(Customer_Segment, Mean_Rating,
           fill=Customer_Segment)) +
  geom_bar(stat="identity") +
  labs(title="Average Rating by Customer Segment")
ggsave("plot2_segment.png", p2, width=6, height=4)

# Purchases vs Ratings
p3<-ggplot(data,
       aes(Total_Purchases, Ratings_Numeric)) +
  geom_point(alpha=0.4) +
  geom_smooth(method="lm") +
  labs(title="Total Purchases vs Ratings")
ggsave("plot3_purchases.png", p3, width=6, heig

# Total Amount vs Ratings
p4<- ggplot(data,
       aes(Total_Amount, Ratings_Numeric)) +
  geom_point(alpha=0.4) +
  geom_smooth(method="lm") +
  labs(title="Total Amount vs Ratings")
ggsave("plot4_amount.png", p4, width=6, height=4)
# =====================================================
# 5. Statistical Analysis
# =====================================================

# Kruskal-Wallis Test
kruskal.test(Ratings_Numeric ~ Customer_Segment, data=data)

# Spearman correlation
cor.test(data$Total_Purchases,
         data$Ratings_Numeric,
         method="spearman")

cor.test(data$Total_Amount,
         data$Ratings_Numeric,
         method="spearman")

# Linear Regression
model <- lm(Ratings_Numeric ~ Customer_Segment +
              Total_Purchases +
              Total_Amount +
              Age,
            data=data)

summary(model)

# =====================================================
# 6. Correlation Matrix
# =====================================================

num_data <- data[,c("Age","Total_Purchases",
                    "Total_Amount","Ratings_Numeric")]

cor_matrix <- cor(num_data, method="spearman")

p5<- ggcorrplot(cor_matrix)
p5
ggsave("plot5_corr.png", p5, width=6, height=5)

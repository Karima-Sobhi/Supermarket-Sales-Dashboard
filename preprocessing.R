data <- read.csv("supermarket_sales - Sheet1.csv")
# ==========================[1] DATA PREPROCESSING:=====================================

# Check if there are any duplicated rows
anyDuplicated(data)
# extract column month from the Date column
data$Month <- format(as.Date(data$Date, format = "%m/%d/%y"), "%b")
# Drop specified columns
data <- data %>%
  select(-Invoice.ID, -Time, -cogs, -gross.margin.percentage)

# View unique values in 'Branch' , 'Customer type' and 'Gender'
unique_branch <- unique(data$Branch)
unique_customer_type <- unique(data$Customer.type)
unique_gender <- unique(data$Gender)
# Print unique values
cat("Unique values in 'Branch':", unique_branch, "\n")
cat("Unique values in 'Customer type':", unique_customer_type, "\n")
cat("Unique values in 'Gender':", unique_gender, "\n")

# Summary statistics for numeric columns
summary(data)

# check nulls
any(is.na(data))

# get columns info
str(data)

data$Date <- mdy(data$Date)

str(data$Date)

# Display the first few rows of the data
head(data)

# =========================== Descriptive Statistics=================================================

# 
# 
# # 4. Customer type
# # Count of customers by type
# customer_type_counts <- table(data$Customer.type)
# cat("Customer type distribution:\n", customer_type_counts, "\n")
# 
# # 5. Gender
# # Count of customers by gender
# gender_counts <- table(data$Gender)
# cat("Gender distribution:\n", gender_counts, "\n")
# 
# # 6. Product line
# # Count of products in each category
# product_line_counts <- table(data$Product.line)
# cat("Product line distribution:\n", product_line_counts, "\n")
# 
# 
# # 15. Gross margin percentage
# # Mean and distribution of gross margin percentages
# mean_margin <- mean(data$gross.margin.percentage)
# cat("Mean Gross Margin Percentage:", mean_margin, "\n")
# 
# # 17. Rating
# # Mean and distribution of customer ratings
# mean_rating <- mean(data$Rating)
# cat("Mean Customer Rating:", mean_rating, "\n")



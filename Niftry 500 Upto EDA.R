# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the dataset
data <- read.csv("C:/Users/vishu/Downloads/NIFTY 500 Quarterly Result.csv")

# Display structure and summary
str(data)
summary(data)

# View the first few rows and structure of the dataset
View(head(data))
print(str(data))

#Sector-Wise Company Count
sector_count <- data %>% group_by(sector) %>% summarise(Count = n())
ggplot(sector_count, aes(x = reorder(sector, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Number of Companies by Sector", x = "Sector", y = "Count")

# Industry-Wise Distribution
industry_count <- data %>% group_by(industry) %>% summarise(Count = n())
ggplot(industry_count, aes(x = "", y = Count, fill = industry)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Industry Distribution", fill = "Industry")

#Revenue Distribution
data$revenue <- as.numeric(gsub(",", "", data$revenue))# Convert revenue to numeric
ggplot(data, aes(x = revenue)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
  labs(title = "Revenue Distribution", x = "Revenue", y = "Frequency")

#Top 10 Companies by Revenue
top_10_revenue <- data %>% arrange(desc(revenue)) %>% head(10)
ggplot(top_10_revenue, aes(x = reorder(name, revenue), y = revenue)) +
  geom_bar(stat = "identity", fill = "goldenrod") +
  coord_flip() +
  labs(title = "Top 10 Companies by Revenue", x = "Company", y = "Revenue")

#Operating Profit Margin Comparison
data$operating_profit_margin <- as.numeric(gsub("%", "", data$operating_profit_margin))
ggplot(data, aes(x = sector, y = operating_profit_margin)) +
  geom_boxplot(fill = "lightgreen") +
  coord_flip() +
  labs(title = "Operating Profit Margin by Sector", x = "Sector", y = "Profit Margin (%)")

#Correlation Analysis
ggplot(data, aes(x = net_profit, y = EPS)) +
  geom_point(color = "blue") +
  labs(title = "Correlation between Net Profit and EPS", x = "Net Profit", y = "EPS")

#Profit Margin Outliers
ggplot(data, aes(x = "", y = operating_profit_margin)) +
  geom_boxplot(fill = "pink") +
  labs(title = "Operating Profit Margin Outliers", x = "", y = "Profit Margin (%)")

#Revenue vs. Profit Relationship
ggplot(data, aes(x = revenue, y = net_profit)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Revenue vs. Net Profit", x = "Revenue", y = "Net Profit")

#Tax Expense Analysis
average_tax <- data %>% group_by(sector) %>% summarise(AverageTax = mean(as.numeric(tax), na.rm = TRUE))
\ggplot(average_tax, aes(x = reorder(sector, -AverageTax), y = AverageTax)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Average Tax Expenses by Sector", x = "Sector", y = "Average Tax")

#EPS Distribution by Sector
ggplot(data, aes(x = sector, y = EPS)) +
  geom_violin(fill = "cyan") +
  coord_flip() +
  labs(title = "EPS Distribution by Sector", x = "Sector", y = "EPS")

#Sector-Wise Net Profit Distribution
ggplot(data, aes(x = sector, y = net_profit)) +
  geom_boxplot(fill = "lightblue") +
  coord_flip() +
  labs(title = "Net Profit Distribution by Sector", x = "Sector", y = "Net Profit")

#Revenue and Expenses Comparison
data$operating_expenses <- as.numeric(gsub(",", "", data$operating_expenses))
ggplot(data, aes(x = revenue, y = operating_expenses)) +
  geom_point(color = "brown") +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Revenue vs. Operating Expenses", x = "Revenue", y = "Operating Expenses")


  

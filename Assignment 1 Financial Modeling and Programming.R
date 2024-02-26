#install.packages("tidyverse")
#install.packages("zoo")


library(zoo)
library(tidyverse)
library(ggplot2)
library(scales)
library(dplyr)

# Setting up working directory
setwd("C:/Users/mruga/OneDrive/Documents/Temp assigments storage/Financial Programming and modeling/Assignment 1")

# Load the data into R
home_price_data <- read.csv("CSUSHPINSA.csv")
mortgage_rate_data <- read.csv("MORTGAGE30US.csv")

# Checking the structure of the data: 
str(home_price_data)
str(mortgage_rate_data)

head(home_price_data)
head(mortgage_rate_data)

# Convert 'DATE' column to Date type
home_price_data$DATE <- as.Date(home_price_data$DATE)
mortgage_rate_data$DATE <- as.Date(mortgage_rate_data$DATE)

# Checking the updated structure of the data:
str(home_price_data)
str(mortgage_rate_data)

# Checking for missing data
sum(is.na(home_price_data))
sum(is.na(mortgage_rate_data))

# Merge datasets based on the 'DATE' column
merged_data <- merge(home_price_data, mortgage_rate_data, by = "DATE", all = TRUE)

head(merged_data)

# Checking for missing data
sum(is.na(merged_data))


# Sort the data by 'DATE' to ensure proper ordering
merged_data <- merged_data[order(merged_data$DATE), ]

# Checking for missing data
sum(is.na(merged_data))

# Fill missing values in 'CSUSHPINSA' with the last non-missing value for the same month
merged_data$CSUSHPINSA <- na.locf(merged_data$CSUSHPINSA, na.rm = FALSE)

# Fill missing values in 'MORTGAGE30US' with the last non-missing value for the same month
merged_data$MORTGAGE30US <- na.locf(merged_data$MORTGAGE30US, na.rm = FALSE)

# Checking for missing data after imputation
sum(is.na(merged_data))

#3. Plot the Home Prices and Average Monthly Mortgage Rate (beginning Jan-1-1987). 
#a. Home Prices = HPI Index Value*$1,000 
#b. Calculate average monthly values for the 30-year mortgage rates. The print is weekly.

# Adjust Home Prices
merged_data$adjusted_prices <- merged_data$CSUSHPINSA * 1000

# Calculate average monthly values for 30-year mortgage rates


merged_data <- merged_data %>%
  mutate(month = format(DATE, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(
    avg_mortgage_rate = mean(MORTGAGE30US),
    avg_home_price = mean(adjusted_prices)
  )

# Filter data from Jan-1-1987 onwards
merged_data <- merged_data[merged_data$month >= "1987-01", ]

# Convert 'month' column to Date type with specific format
merged_data$month <- as.yearmon(merged_data$month)

summary(merged_data)

# Scale mortgage rate values for better visibility
mortgage_scale_factor <- 10000  # Adjust this value based on the magnitude of your mortgage rate data

# Plotting with modifications

ggplot(merged_data, aes(x = month)) +
  geom_line(aes(y = avg_home_price, color = "Home Prices"), size = 1.5) +
  geom_line(aes(y = avg_mortgage_rate * mortgage_scale_factor, color = "Mortgage Rate"), size = 1.5) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / mortgage_scale_factor, name = "Mortgage Rate"),
    name = "Home Prices"
  ) +
  labs(title = "Home Prices and Average Monthly Mortgage Rate Over Time",
       y = "Values", color = "Legend") +
  theme_minimal()


#4. Calculate the assumed monthly rent for the corresponding monthly home price. The assumed 
#monthly rent has a gross rental yield of 8.00%. (Gross Rental Yield = Annual Rent/Home Price)  


# Assuming gross rental yield is 8.00%
gross_rental_yield <- 0.08

# Calculate annual home price
annual_home_price <- merged_data$avg_home_price

# Calculate annual rent
annual_rent <- annual_home_price * gross_rental_yield

# Calculate assumed monthly rent
monthly_rent <- annual_rent / 12

# Add the assumed monthly rent to the merged_data dataframe
merged_data$assumed_monthly_rent <- monthly_rent

# Calculate the monthly mortgage payment amounts (P&I) assuming a 30 year mortgage on 90% 
# of the value of the home for every period. 
# Assuming a 30-year mortgage and 90% loan-to-value ratio

loan_term_years <- 30
loan_to_value_ratio <- 0.90

# Calculate the loan amount
loan_amount <- merged_data$avg_home_price * loan_to_value_ratio

# Monthly interest rate
monthly_interest_rate <- merged_data$avg_mortgage_rate / 12 / 100

# Number of monthly payments
num_payments <- loan_term_years * 12

# Monthly mortgage payment (P&I) formula
monthly_payment <- (loan_amount * monthly_interest_rate) / (1 - (1 + monthly_interest_rate)^-num_payments)

# Add the monthly mortgage payment to the merged_data dataframe
merged_data$monthly_mortgage_payment <- monthly_payment


#Home ownership also includes certain fixed expenses such as taxes, insurance and HOA fees 
#(if applicable). The fixed expense equates to about 1.7% of the value of the home. 
#a. Calculate the monthly additional fixed expense burden on home owner. 
#b. Calculate the total cost of owning a home.

#a. Calculate the monthly additional fixed expense burden on home owner assuming the interest+hoa is given at annual percent
merged_data$monthly_fixed_expense <- (0.017 * merged_data$avg_home_price)/12

# Calculate the total cost of owning a home
merged_data$monthly_cost_of_owning <- merged_data$monthly_mortgage_payment + merged_data$monthly_fixed_expense

# Print or view the updated merged_data dataframe
head(merged_data)

#7. Compare in which periods it is cheaper to own a home vs renting a home?
# Calculate the difference between assumed monthly rent and total cost of owning
merged_data$ownership_vs_renting_difference <- merged_data$assumed_monthly_rent - merged_data$monthly_cost_of_owning

# Identify periods when it's cheaper to own (positive difference) or rent (negative difference)
merged_data$ownership_vs_renting_status <- ifelse(merged_data$ownership_vs_renting_difference >= 0, "Own", "Rent")

table(merged_data$ownership_vs_renting_status)




# Convert 'month' to Date type
merged_data$month <- as.Date(merged_data$month)

# Your existing ggplot code
ggplot(merged_data, aes(x = month)) +
  geom_line(aes(y = assumed_monthly_rent, color = "Assumed Monthly Rent"), size = 1.5) +
  geom_line(aes(y = monthly_cost_of_owning, color = "Monthly Cost of Owning"), size = 1.5) +
  geom_line(aes(y = ownership_vs_renting_difference, color = "Ownership vs Renting Difference"), size = 1.5) +
  geom_point(aes(y = ifelse(ownership_vs_renting_status == "Own", 450, NA_real_), color = "Own"), size = 1) +
  geom_point(aes(y = ifelse(ownership_vs_renting_status == "Rent", 400, NA_real_), color = "Rent"), size = 1) +
  labs(title = "Comparison of Owning vs Renting",
       y = "Values", color = "Legend") +
  scale_color_manual(values = c("Assumed Monthly Rent" = "blue", 
                                "Monthly Cost of Owning" = "purple", 
                                "Ownership vs Renting Difference" = "orange", 
                                "Own" = "green", 
                                "Rent" = "red")) +
  scale_x_date(breaks = seq(floor_date(min(merged_data$month), "5 years"), 
                            ceiling_date(max(merged_data$month), "5 years"), by = "5 years"),
               labels = date_format("%Y-%m")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5),  # Combine the settings
        axis.ticks.x = element_blank())  # Remove tick marks on the X-axis



# Export the dataframe to a CSV file
write.csv(merged_data, file = "OwnershipVsRent_MrugalNikhar.csv")
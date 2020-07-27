# Exploration for Superstore Sales
# -----

# Libraries
library(tidyverse)
library(tidyselect)
library(readxl)
library(lubridate)

# Config
setwd("~/Documents/School/VT/CMDA3654/DataProject/")

# **********************************************************************
#
# Data Cleaning
#
# **********************************************************************

orders <- read_csv("SampleSuperstoreData/Orders-Table1.csv")
returns <- read_csv("SampleSuperstoreData/Returns-Table2.csv")
# This is official census data put into comma-delimited format, you can find it here: https://gist.github.com/erichurst/7882666
zipcodes <- read_csv("ExtraData/zipcodes.txt", 
                     col_types = list(
                       "Postal Code" = col_number(), 
                       "Latitude" = col_double(), 
                       "Longitude" = col_double()
                     ),
                     col_names = c("Postal Code", "Latitude", "Longitude"),
                     skip = 1
)

order_data <- left_join(orders, returns)
order_data$Returned[is.na(order_data$Returned)] <- "No"
unique(order_data$Returned) # Successful join and clean of Returned column

# Transform to factors
order_data <- order_data %>% 
  mutate_each(funs(as.factor), c(5,8:11,13,15:16))

# Split date into years/months
date_col_names <- c("OrderMonth", "OrderDay", "OrderYear")
order_data <- order_data %>%
  separate(`Order Date`, 
           into = date_col_names,
           sep = "/",
           remove = F,
           convert = T)
# Same for shipping date
ship_col_names <- c("ShipMonth", "ShipDay", "ShipYear")
order_data <- order_data %>%
  separate(`Ship Date`, 
           into = ship_col_names,
           sep = "/",
           remove = F,
           convert = T)
# Convert OrderDate and ShipDate to date type
order_data <- order_data %>%
  mutate_at(.funs = as.Date, 
            format = "%m/%d/%y", 
            .vars = c("Order Date", "Ship Date"))

# Join zip code data
order_data <- left_join(order_data, zipcodes)

# Remove excess columns
order_data <- order_data %>%
  select(-OrderDay, -OrderYear, -ShipDay, -ShipYear, -ShipMonth)

glimpse(order_data)

# Export clean data
write_csv(order_data, path = "Data/StoreData_CLEAN.csv")

# Re-import clean data
clean_orders <- read_csv("Data/StoreData_CLEAN.csv", col_types = cols(
  "Order Date" = col_date(format = "%Y-%m-%d"),
  "Ship Date" = col_date(format = "%Y-%m-%d"),
  "Ship Mode" = col_factor(),
  "Segment" = col_factor(),
  "Country" = col_factor(),
  "City" = col_factor(),
  "State" = col_factor(),
  "Region" = col_factor(),
  "Category" = col_factor(),
  "Sub-Category" = col_factor(),
  "Returned" = col_factor()
))

# **********************************************************************
#
# Data Exploration
#
# **********************************************************************

clean_orders[which(duplicated(clean_orders$`Customer Name`)),]

# Percentage Table - pick any two categorical variables
round(prop.table(table(clean_orders$Region, clean_orders$Category), 1), 3) * 100

# Returns table
round(prop.table(table(clean_orders$`Sub-Category`, clean_orders$Returned), 1), 3) * 100

length(unique(order_data$`Product ID`))

ggplot(clean_orders, aes(y = Profit, x = Discount )) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)

profit.mod.null <- lm(Quantity ~ 1, data = clean_orders)
profit.mod.all <- lm(Quantity ~ Latitude + Longitude + Sales + Discount + Profit + OrderMonth, data = clean_orders)
profit.mod.best <- stepAIC(profit.mod.all, scope = c(profit.mod.null, profit.mod.all), direction = "both")
summary(profit.mod.best)

summary(profit.mod <- lm(Quantity ~ Latitude + Longitude + Sales + Discount + OrderMonth, data = clean_orders))

# **********************************************************************
#
# Basic Linear Regression
#
# **********************************************************************

sales.profit.model <- lm(Profit ~ Sales, data = sales_data)
log.sales_data <- sales_data %>%
  mutate(
    "Log_Sales" = log(Sales)
  )

log.sales.profit.model <- lm(Profit ~ Log_Sales, data = log.sales_data)
summary(log.sales.profit.model)

ggplot(log.sales_data, aes(y = Profit, x = Log_Sales, color = Category)) +
  geom_point()

# **********************************************************************
#
# Basic Logistic Regression
#
# **********************************************************************

glimpse(clean_orders)

no.na.orders <- clean_orders %>%
  filter(!is.na(Latitude))

returned.binom <- ifelse(no.na.orders$Returned == "Yes", 1, 0)

logistic.model.null <- glm(returned.binom ~ 1, data = no.na.orders)
logistic.model.all <- glm(returned.binom ~ Latitude + Longitude + Profit + Sales + Quantity + Segment + Category + Region, data = no.na.orders)
logistic.model.best <- stepAIC(logistic.model.all, method = "glm")

logistic.model.final <- glm(returned.binom ~ Longitude + Category, data = no.na.orders)
summary(logistic.model.final)

log.pred <- predict(logistic.model.final, type = "link")
no.na.orders %>%
  ggplot(aes(y = returned.binom, x = log.pred)) +
    geom_point(aes(color = as.factor(returned.binom)), position = position_jitter(height = 0.07, width = 0)) +
    stat_smooth(method="glm", method.args=list(family="binomial")) +
    labs(title = "Returned Value vs. Log-Odds", x = "Log-Odds", y = "Returned", color = "Returned") +
    scale_color_discrete(labels = c("Not Returned", "Returned"))

table( as.factor(clean_orders$`Product Name`), clean_orders$Returned ) %>%
  as.data.frame() %>%
  filter(Var1 == "Yes")





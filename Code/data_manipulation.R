## --------------------------------------------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)


## --------------------------------------------------------------------------------------------------------------------
getwd()


## --------------------------------------------------------------------------------------------------------------------
# Using the bigger Washington EV data set
new_wa_data = read.csv("Data Sets/new_wa_data.csv")


## --------------------------------------------------------------------------------------------------------------------
# Create a function to change the HYUNDAI model name 'Ioniq' based on what type of EV it is because different ones get different tax credit amount
modify_model_name <- function(model, energy_type) {
  ifelse(new_wa_data$Clean.Alternative.Fuel.Vehicle.Type == "Battery Electric Vehicle (BEV)" & new_wa_data$Model == "Ioniq", paste("BEV", new_wa_data$Model), new_wa_data$Model)
}
# Apply function to data set
new_wa_data <- new_wa_data %>%
  mutate(Model = modify_model_name(Model, Clean.Alternative.Fuel.Vehicle.Type))

# Doing the same thing for KIA model name "Niro"
modify_model_name <- function(model, energy_type) {
  ifelse(new_wa_data$Clean.Alternative.Fuel.Vehicle.Type == "Battery Electric Vehicle (BEV)" & new_wa_data$Model == "Niro", paste("BEV", new_wa_data$Model), new_wa_data$Model)
}
# Apply function to data set
new_wa_data <- new_wa_data %>%
  mutate(Model = modify_model_name(Model, Clean.Alternative.Fuel.Vehicle.Type))

# Do the same for Volvo S60 that is extended range
modify_model_name <- function(model, range) {
  ifelse(new_wa_data$Electric.Range == 41 & new_wa_data$Model == "S60", paste("ER", new_wa_data$Model), new_wa_data$Model)
}
# Apply function to data set
new_wa_data <- new_wa_data %>%
  mutate(Model = modify_model_name(Model, Clean.Alternative.Fuel.Vehicle.Type))

# Volvo S90
modify_model_name <- function(model, range) {
  ifelse(new_wa_data$Electric.Range == 38 & new_wa_data$Model == "S90", paste("ER", new_wa_data$Model), new_wa_data$Model)
}
# Apply function to data set
new_wa_data <- new_wa_data %>%
  mutate(Model = modify_model_name(Model, Clean.Alternative.Fuel.Vehicle.Type))

# Volvo V60
modify_model_name <- function(model, range) {
  ifelse(new_wa_data$Electric.Range == 41 & new_wa_data$Model == "V60", paste("ER", new_wa_data$Model), new_wa_data$Model)
}
# Apply function to data set
new_wa_data <- new_wa_data %>%
  mutate(Model = modify_model_name(Model, Clean.Alternative.Fuel.Vehicle.Type))

# Volvo XC60
modify_model_name <- function(model, range) {
  ifelse(new_wa_data$Electric.Range == 35 & new_wa_data$Model == "XC60", paste("ER", new_wa_data$Model), new_wa_data$Model)
}
# Apply function to data set
new_wa_data <- new_wa_data %>%
  mutate(Model = modify_model_name(Model, Clean.Alternative.Fuel.Vehicle.Type))

# Volvo XC90
modify_model_name <- function(model, range) {
  ifelse(new_wa_data$Electric.Range == 35 & new_wa_data$Model == "XC90", paste("ER", new_wa_data$Model), new_wa_data$Model)
}
# Apply function to data set
new_wa_data <- new_wa_data %>%
  mutate(Model = modify_model_name(Model, Clean.Alternative.Fuel.Vehicle.Type))


## --------------------------------------------------------------------------------------------------------------------
# Convert DOL.Transaction.Date into a date object
new_wa_data$DOL.Transaction.Date <- as.Date(new_wa_data$DOL.Transaction.Date, format = "%B %d %Y")

# Function to rename 'Volt' Model depending on when it was purchased
Phase_Out <- function(model, transaction_date) {
  ifelse(new_wa_data$Model == "Volt" & new_wa_data$DOL.Transaction.Date <= as.Date("2019-03-31"), 
         paste("1", new_wa_data$Model), 
         ifelse(new_wa_data$Model == "Volt" & new_wa_data$DOL.Transaction.Date >= as.Date("2019-04-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2019-09-30"), 
                paste("2", new_wa_data$Model), 
                ifelse(new_wa_data$Model == "Volt" & new_wa_data$DOL.Transaction.Date >= as.Date("2019-10-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2020-03-31"), 
                       paste("3", new_wa_data$Model),
                       new_wa_data$Model
                )
         )
  )
}
# Running function on data set
new_wa_data <- new_wa_data %>%
  mutate(Model = Phase_Out(Model, DOL.Transaction.Date))


# Doing the same for 'Bolt EV' Model
Phase_Out <- function(model, transaction_date) {
  ifelse(new_wa_data$Model == "Bolt EV" & new_wa_data$DOL.Transaction.Date <= as.Date("2019-03-31"), 
         paste("1", new_wa_data$Model), 
         ifelse(new_wa_data$Model == "Bolt EV" & new_wa_data$DOL.Transaction.Date >= as.Date("2019-04-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2019-09-30"), 
                paste("2", new_wa_data$Model), 
                ifelse(new_wa_data$Model == "Bolt EV" & new_wa_data$DOL.Transaction.Date >= as.Date("2019-10-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2020-03-31"), 
                       paste("3", new_wa_data$Model),
                       new_wa_data$Model
                )
         )
  )
}
# Running function on data set
new_wa_data <- new_wa_data %>%
  mutate(Model = Phase_Out(Model, DOL.Transaction.Date))


## --------------------------------------------------------------------------------------------------------------------
# C-Max
Phase_Out <- function(model, transaction_date) {
  ifelse(new_wa_data$Model == "C-Max" & new_wa_data$DOL.Transaction.Date <= as.Date("2022-12-31"), 
         paste("1", new_wa_data$Model), new_wa_data$Model
         
  )
}
new_wa_data <- new_wa_data %>%
  mutate(Model = Phase_Out(Model, DOL.Transaction.Date))

# Escape
Phase_Out <- function(model, transaction_date) {
  ifelse(new_wa_data$Model == "Escape" & new_wa_data$DOL.Transaction.Date <= as.Date("2022-12-31"), 
         paste("1", new_wa_data$Model), new_wa_data$Model
         
  )
}
new_wa_data <- new_wa_data %>%
  mutate(Model = Phase_Out(Model, DOL.Transaction.Date))

# F-150
Phase_Out <- function(model, transaction_date) {
  ifelse(new_wa_data$Model == "F-150" & new_wa_data$DOL.Transaction.Date <= as.Date("2022-12-31"), 
         paste("1", new_wa_data$Model), new_wa_data$Model
         
  )
}
new_wa_data <- new_wa_data %>%
  mutate(Model = Phase_Out(Model, DOL.Transaction.Date))

# Fusion
Phase_Out <- function(model, transaction_date) {
  ifelse(new_wa_data$Model == "Fusion" & new_wa_data$DOL.Transaction.Date <= as.Date("2022-12-31"), 
         paste("1", new_wa_data$Model), new_wa_data$Model
         
  )
}
new_wa_data <- new_wa_data %>%
  mutate(Model = Phase_Out(Model, DOL.Transaction.Date))

# Mustang Mach-E
Phase_Out <- function(model, transaction_date) {
  ifelse(new_wa_data$Model == "Mustang Mach-E" & new_wa_data$DOL.Transaction.Date <= as.Date("2022-12-31"), 
         paste("1", new_wa_data$Model), new_wa_data$Model
         
  )
}
new_wa_data <- new_wa_data %>%
  mutate(Model = Phase_Out(Model, DOL.Transaction.Date))


## --------------------------------------------------------------------------------------------------------------------
# Model 3
Phase_Out <- function(model, transaction_date) {
  ifelse(new_wa_data$Model == "Model 3" & new_wa_data$DOL.Transaction.Date <= as.Date("2018-12-31"), 
         paste("1", new_wa_data$Model), 
         ifelse(new_wa_data$Model == "Model 3" & new_wa_data$DOL.Transaction.Date >= as.Date("2019-01-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2019-06-30"), 
                paste("2", new_wa_data$Model), 
                ifelse(new_wa_data$Model == "Model 3" & new_wa_data$DOL.Transaction.Date >= as.Date("2019-07-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2019-12-31"), 
                       paste("3", new_wa_data$Model),
                       new_wa_data$Model
                )
         )
  )
}
new_wa_data <- new_wa_data %>%
  mutate(Model = Phase_Out(Model, DOL.Transaction.Date))

# Model S
Phase_Out <- function(model, transaction_date) {
  ifelse(new_wa_data$Model == "Model S" & new_wa_data$DOL.Transaction.Date <= as.Date("2018-12-31"), 
         paste("1", new_wa_data$Model), 
         ifelse(new_wa_data$Model == "Model S" & new_wa_data$DOL.Transaction.Date >= as.Date("2019-01-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2019-06-30"), 
                paste("2", new_wa_data$Model), 
                ifelse(new_wa_data$Model == "Model S" & new_wa_data$DOL.Transaction.Date >= as.Date("2019-07-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2019-12-31"), 
                       paste("3", new_wa_data$Model),
                       new_wa_data$Model
                )
         )
  )
}
new_wa_data <- new_wa_data %>%
  mutate(Model = Phase_Out(Model, DOL.Transaction.Date))

# Model X
Phase_Out <- function(model, transaction_date) {
  ifelse(new_wa_data$Model == "Model X" & new_wa_data$DOL.Transaction.Date <= as.Date("2018-12-31"), 
         paste("1", new_wa_data$Model), 
         ifelse(new_wa_data$Model == "Model X" & new_wa_data$DOL.Transaction.Date >= as.Date("2019-01-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2019-06-30"), 
                paste("2", new_wa_data$Model), 
                ifelse(new_wa_data$Model == "Model X" & new_wa_data$DOL.Transaction.Date >= as.Date("2019-07-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2019-12-31"), 
                       paste("3", new_wa_data$Model),
                       new_wa_data$Model
                )
         )
  )
}
new_wa_data <- new_wa_data %>%
  mutate(Model = Phase_Out(Model, DOL.Transaction.Date))

# Roadster
Phase_Out <- function(model, transaction_date) {
  ifelse(new_wa_data$Model == "Roadster" & new_wa_data$DOL.Transaction.Date <= as.Date("2018-12-31"), 
         paste("1", new_wa_data$Model), 
         ifelse(new_wa_data$Model == "Roadster" & new_wa_data$DOL.Transaction.Date >= as.Date("2019-01-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2019-06-30"), 
                paste("2", new_wa_data$Model), 
                ifelse(new_wa_data$Model == "Roadster" & new_wa_data$DOL.Transaction.Date >= as.Date("2019-07-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2019-12-31"), 
                       paste("3", new_wa_data$Model),
                       new_wa_data$Model
                )
         )
  )
}
new_wa_data <- new_wa_data %>%
  mutate(Model = Phase_Out(Model, DOL.Transaction.Date))


## --------------------------------------------------------------------------------------------------------------------
# bZ4X
Phase_Out <- function(model, transaction_date) {
  ifelse(new_wa_data$Model == "bZ4X" & new_wa_data$DOL.Transaction.Date <= as.Date("2022-09-30"), 
         paste("1", new_wa_data$Model), 
         ifelse(new_wa_data$Model == "bZ4X" & new_wa_data$DOL.Transaction.Date >= as.Date("2022-10-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2022-12-31"), 
                paste("2", new_wa_data$Model), 
                new_wa_data$Model
         )
  )
}
new_wa_data <- new_wa_data %>%
  mutate(Model = Phase_Out(Model, DOL.Transaction.Date))

# Prius Plug-in
Phase_Out <- function(model, transaction_date) {
  ifelse(new_wa_data$Model == "Prius Plug-in" & new_wa_data$DOL.Transaction.Date <= as.Date("2022-09-30"), 
         paste("1", new_wa_data$Model), 
         ifelse(new_wa_data$Model == "Prius Plug-in" & new_wa_data$DOL.Transaction.Date >= as.Date("2022-10-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2022-12-31"), 
                paste("2", new_wa_data$Model), 
                new_wa_data$Model
         )
  )
}
new_wa_data <- new_wa_data %>%
  mutate(Model = Phase_Out(Model, DOL.Transaction.Date))

# Prius Prime
Phase_Out <- function(model, transaction_date) {
  ifelse(new_wa_data$Model == "Prius Prime" & new_wa_data$DOL.Transaction.Date <= as.Date("2022-09-30"), 
         paste("1", new_wa_data$Model), 
         ifelse(new_wa_data$Model == "Prius Prime" & new_wa_data$DOL.Transaction.Date >= as.Date("2022-10-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2022-12-31"), 
                paste("2", new_wa_data$Model), 
                new_wa_data$Model
         )
  )
}
new_wa_data <- new_wa_data %>%
  mutate(Model = Phase_Out(Model, DOL.Transaction.Date))

# RAV4 Prime
Phase_Out <- function(model, transaction_date) {
  ifelse(new_wa_data$Model == "RAV4 Prime" & new_wa_data$DOL.Transaction.Date <= as.Date("2022-09-30"), 
         paste("1", new_wa_data$Model), 
         ifelse(new_wa_data$Model == "RAV4 Prime" & new_wa_data$DOL.Transaction.Date >= as.Date("2022-10-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2022-12-31"), 
                paste("2", new_wa_data$Model), 
                new_wa_data$Model
         )
  )
}
new_wa_data <- new_wa_data %>%
  mutate(Model = Phase_Out(Model, DOL.Transaction.Date))

# RAV4
Phase_Out <- function(model, transaction_date) {
  ifelse(new_wa_data$Model == "RAV4" & new_wa_data$DOL.Transaction.Date <= as.Date("2022-09-30"), 
         paste("1", new_wa_data$Model), 
         ifelse(new_wa_data$Model == "RAV4" & new_wa_data$DOL.Transaction.Date >= as.Date("2022-10-01") & new_wa_data$DOL.Transaction.Date <= as.Date("2022-12-31"), 
                paste("2", new_wa_data$Model), 
                new_wa_data$Model
         )
  )
}
new_wa_data <- new_wa_data %>%
  mutate(Model = Phase_Out(Model, DOL.Transaction.Date))


## --------------------------------------------------------------------------------------------------------------------
# Adding binary variable for if the car is new or not
new_wa_data$New <- ifelse(new_wa_data$New.or.Used.Vehicle == "New", 1, 0)

# Load tax credit data set
Tax_Cred <- read.csv("Data Sets/Tax_Cred.csv")

merged_data <- merge(new_wa_data, Tax_Cred, by = c("Make", "Model", "Model.Year", "New"), all.x = TRUE)

View(merged_data)

write.csv(merged_data, "Data Sets/Cleaned Data Sets/WA_FIPS.csv", row.names = F)


## --------------------------------------------------------------------------------------------------------------------
# Removing state and county FIPS codes (first 5 numbers)
merged_data$X2020.Census.Tract <- substr(merged_data$X2020.Census.Tract, 6, nchar(merged_data$X2020.Census.Tract))

# Add a decimal to the column 'X2020.Census.Tract' to match the census data sets
merged_data$X2020.Census.Tract <- paste0(substr(merged_data$X2020.Census.Tract, 1, nchar(merged_data$X2020.Census.Tract) - 2), ".", substr(merged_data$X2020.Census.Tract, nchar(merged_data$X2020.Census.Tract) - 1, nchar(merged_data$X2020.Census.Tract)))

# Convert the columns to numeric
merged_data$X2020.Census.Tract <- as.numeric(merged_data$X2020.Census.Tract)

# Saving Data Set
write.csv(merged_data, "Data Sets/Cleaned Data Sets/WA_EV_data.csv", row.names = FALSE)


## --------------------------------------------------------------------------------------------------------------------
# Loading commute time data set
Commute_Time <- read.csv("Data Sets/Commute_Time.csv")

# Remove column 'X' that has no data
Commute_Time <- Commute_Time[, !colnames(Commute_Time) %in% c("X")]

# Rename all columns and delete the first row
colnames(Commute_Time) <- as.character(unlist(Commute_Time[1, ]))
Commute_Time <- Commute_Time[-1, ]

# Separate Geographic.Area.Name column into new columns 'X2020.Census.Tract' and 'County'
Commute_Time <- separate(Commute_Time, "Geographic Area Name", into = c("X2020.Census.Tract", "County"), sep = ",")

# Removing the words 'Census Tract' from the X2020.Census.Tract column
Commute_Time$X2020.Census.Tract <- as.numeric(gsub("[^0-9.]", "", Commute_Time$X2020.Census.Tract))

# Remove the word 'county' from the County column
Commute_Time$County <- gsub(" County", "", Commute_Time$County)
Commute_Time$County <- gsub(" ", "", Commute_Time$County)

write.csv(Commute_Time, "Data Sets/Cleaned Data Sets/Commute_TimeF.csv", row.names = F)


## --------------------------------------------------------------------------------------------------------------------
# Load Income
Census_Income <- read.csv("Data Sets/Census_Income.csv")

# Isolating just WA data
Census_Income <- Census_Income[80384:82167, ]

# Separate Geographic.Area.Name column into new columns 'X2020.Census.Tract' and 'County'
Census_Income <- separate(Census_Income, Geographic.Area.Name, into = c("X2020.Census.Tract", "County"), sep = ";")


# Remove the word 'county' from the County column
Census_Income$County <- gsub(" County", "", Census_Income$County)
Census_Income$County <- gsub(" ", "", Census_Income$County)

write.csv(Census_Income, "Data Sets/Cleaned Data Sets/Census_IncomeF.csv", row.names = F)


## --------------------------------------------------------------------------------------------------------------------
Age <- read.csv("Data Sets/Age.csv")

# Subsetting columns 1-40
AgeF <- Age[, 1:40]

# Rename all columns and delete the first row
colnames(AgeF) <- as.character(unlist(AgeF[1, ]))
AgeF <- AgeF[-1, ]

# Separate Geographic.Area.Name column into new columns 'X2020.Census.Tract' and 'County'
AgeF <- separate(AgeF, "Geographic Area Name", into = c("X2020.Census.Tract", "County"), sep = ",")

# Removing the words 'Census Tract' from the X2020.Census.Tract column
AgeF$X2020.Census.Tract <- as.numeric(gsub("[^0-9.]", "", AgeF$X2020.Census.Tract))

# Remove the word 'county' from the County column
AgeF$County <- gsub(" County", "", AgeF$County)
AgeF$County <- gsub(" ", "", AgeF$County)

write.csv(AgeF, "AgeF.csv", row.names = F)


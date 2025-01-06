#setwd("E:/R_file/MGSC661/midterm")
data=read.csv("E:/MMA/MGSC 661 stats/Final Proj/Dataset 3 — Shark tank pitches.csv")
attach(data)

library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyr)
library(car)
library(psych)
library(ggpubr)
library(methods)
library(boot)

#entrepreneurs column has some na values and just a few recurring people, it is not informative
data = data %>% select(-entrepreneurs, -title, -episode_season, -description)

# remove missing values
data<- data %>% drop_na()

# Update the 'website' column to indicate whether a website is present or not
data$website <- ifelse(is.na(data$website) | data$website == "", FALSE, TRUE)

# Loop through all columns in the dataset
for (col_name in colnames(data)) {
  # Open a JPEG file for saving the plot
  jpeg(file = paste0("E:/MMA/MGSC 661 stats/Final Proj/",col_name, "_distribution.jpeg"))
  
  # Check if the column is numeric or categorical
  if (is.numeric(data[[col_name]])) {
    # Plot histogram for numeric columns
    hist(data[[col_name]], main = paste("Distribution of", col_name), 
         xlab = col_name, col = "skyblue", border = "white")
  } else if (is.factor(data[[col_name]]) || is.character(data[[col_name]])) {
    # Plot barplot for categorical columns
    barplot(table(data[[col_name]]), main = paste("Distribution of", col_name),
            xlab = col_name, col = "lightgreen", las = 2)
  } else {
    # Skip unsupported column types
    plot.new()  # Placeholder for unsupported types
    text(0.5, 0.5, paste("Unsupported data type for", col_name), cex = 1.5)
  }
  
  # Close the JPEG device
  dev.off()
}

#draw a barchart of distribution of category before generalization
plot_category <- ggplot(data, aes(x = category)) +
  geom_histogram(stat = "count", fill = "skyblue", color = "black", alpha = 0.7) +  # Count histogram for categories
  labs(
    title = "Distribution of Categories",
    x = "Category",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 40, hjust = 1)  # Rotate category labels if they are long
  )

print(plot_category)


#draw a barchart of distribution of location before cleaning
plot_category <- ggplot(data, aes(x = location)) +
  geom_histogram(stat = "count", fill = "skyblue", color = "black", alpha = 0.7) +  # Count histogram for categories
  labs(
    title = "Distribution of Locations",
    x = "Category",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 40, hjust = 1)  # Rotate category labels if they are long
  )

print(plot_category)

length(unique(data$location))

#Remove two outliers: exchangeForStake was 100,70 and there were no deals
data <- data %>% filter(!(exchangeForStake %in% c(100, 70)))

###################Feature Engineering###############

# Create the merged_category column
data$merged_category <- sapply(data$category, function(category) {
  if (category %in% c("Baby and Child Care", "Baby and Children's Entertainment", 
                      "Baby and Children's Apparel and Accessories", 
                      "Baby and Children's Bedding", "Baby and Children's Food")) {
    return("Baby and Children")
  } else if (category %in% c("Men and Women's Apparel", "Women's Accessories", 
                             "Women's Apparel", "Women's Shoes", "Men's Accessories",
                             "Men and Women's Shoes", "Undergarments and Basics",
                             "Fashion Accessories", "Fitness Apparel and Accessories",
                             "Men and Women's Accessories")) {
    return("Men and Women Clothes")
  } else if (category %in% c("Fitness Equipment", "Fitness Programs", "Health and Well-Being",
                             "Cycling")) {
    return("Fitness and Health")
  } else if (category %in% c("Furniture", "Home Improvement", "Storage and Cleaning Products",
                             "Home Accessories", "Gardening", "Home Security Solutions")) {
    return("Home and Garden")
  } else if (category %in% c("Specialty Food", "Non-Alcoholic Beverages", "Alcoholic Beverages",
                             "Wine Accessories")) {
    return("Food and Beverages")
  } else {
    return(category) # Keep other categories as they are
  }
})

# Drop the original category column
data$category <- NULL

length(unique(data$merged_category))

#draw a barchart of ditribution of mereged_category :after generalization
plot_category <- ggplot(data, aes(x = merged_category)) +
  geom_histogram(stat = "count", fill = "skyblue", color = "black", alpha = 0.7) +  # Count histogram for categories
  labs(
    title = "Distribution of Merged Categories",
    x = "Category",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 40, hjust = 1)  # Rotate category labels if they are long
  )

print(plot_category)


# Count the number of states for each category and deal
df_cat_deal <- data %>%
  group_by(merged_category, deal) %>%
  summarise(state_count = n(), .groups = "drop")

# Sort the categories based on the count of deal == TRUE
df_cat_deal_sorted <- df_cat_deal %>%
  group_by(merged_category) %>%
  mutate(deal_ratio = sum(state_count[deal == TRUE]) / sum(state_count)) %>%
  ungroup() %>%
  arrange(desc(deal_ratio))

# Keep only the last two letters of the location column (state abbreviation)
data$state<- substr(data$location, nchar(data$location) - 2, nchar(data$location))

#Now lets look at the distribution of the location again
state_counts <- table(data$state)

barplot(state_counts, main = "Distribution of Sates",
        xlab = "State", col = "skyblue", border = "white",
        las = 2, # Rotate x-axis labels vertically for better readability
        cex.names = 0.7)

# Calculate the frequency of each location in the dataset
state_freq <- data %>%
  count(state) %>%
  mutate(freq = n / nrow(data) * 100)  # Calculate percentage

# Filter locations that appear in more than 1% of the dataset
states_to_keep <- state_freq %>%
  filter(freq >2) %>%
  pull(state)  # Extract the country names

# Update the location column to label locations not in locations_to_keep as "Other"
data <- data %>%
  mutate(state = ifelse(state %in% states_to_keep, state, "Other"))

#Look at the distribution again
state_counts <- table(data$state)

# Drop the original category column
data$location <- NULL

#draw a barchart of ditribution of mereged_category :after generalization
plot_states <- ggplot(data, aes(x = state)) +
  geom_histogram(stat = "count", fill = "skyblue", color = "black", alpha = 0.7) +  # Count histogram for categories
  labs(
    title = "Distribution of Frequent Cities",
    x = "City",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 40, hjust = 1)  # Rotate category labels if they are long
  )

print(plot_states)

#Look at the number of successful and failed deals in each state

# Count the number of success for each location and deal
df_loc_deal <- data %>%
  group_by(location, deal) %>%
  summarise(deal_count = n(), .groups = "drop")

# Sort the categories based on the count of deal == TRUE
df_loc_deal_sorted <- df_loc_deal %>%
  group_by(location) %>%
  mutate(deal_ratio = sum(deal_count[deal == TRUE]) / sum(deal_count)) %>%
  ungroup() %>%
  arrange(desc(deal_ratio))

# Plot
ggplot(df_loc_deal_sorted, aes(x = location, y = deal_count, fill = as.factor(deal))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"),
                    labels = c("TRUE" = "Deal: TRUE", "FALSE" = "Deal: FALSE"),
                    name = "Deal Status") +
  labs(title = "Bar Plot of Locations by Deal Status",
       x = "Location",
       y = "Number of deals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate category names vertically
        panel.grid.major.x = element_blank(),  # Remove vertical grid lines
        panel.grid.minor.x = element_blank())  # Remove minor vertical grid lines


##### Binary features now indicate if each shark is present in a row

# Specify the shark columns by name
shark_columns <- data[, c("shark1", "shark2", "shark3", "shark4", "shark5")]

# Get unique shark names from the specified columns
unique_sharks <- unique(unlist(shark_columns)) # Extract unique shark names

# Create binary features for each unique shark
for (shark in unique_sharks) {
  shark_col_name <- paste0("shark_", gsub(" ", "_", shark)) # Replace spaces with underscores
  data[[shark_col_name]] <- apply(shark_columns, 1, function(row) shark %in% row)
}

#drop the old columns of sharks
data <- data[, !colnames(data) %in% c("shark1", "shark2", "shark3", "shark4", "shark5")]

###################EDA############################

#####Violin plot to check the relationship between exchange for stake and outcome of a deal
plot_deal_exchange <- ggplot(data, aes(x = exchangeForStake, y = deal, fill = factor(deal))) +
  geom_violin(color = "black", alpha = 0.7) +  # Violin plot for deal vs valuation
  scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "purple")) +  # Custom colors for deal outcome
  labs(
    title = "Distribution of Equity Offered for Stake by Deal Outcome",
    x = "Equity Offered",
    y = "Deal Outcome"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(angle = 0, hjust = 1)  # Ensure y-axis labels are aligned properly
  )

print(plot_deal_exchange)

#########Success rate based on investment metrics
library(dplyr)
library(ggplot2)
library(patchwork)

# Helper function to create custom bin labels
create_custom_bins <- function(values, breaks, labels) {
  cut(values, 
      breaks = breaks, 
      labels = labels,
      include.lowest = TRUE, 
      right = FALSE) # [a,b) binning
}

# Custom bins for askedFor
askedFor_breaks <- c(10000, 50000, 100000, 500000, 1000000, Inf)
askedFor_labels <- c("10K-50K\n(%d)", "50K-100K\n(%d)", "100K-500K\n(%d)", "500K-1M\n(%d)", ">1M\n(%d)")

# Custom bins for exchangeForStake
exchangeForStake_breaks <- c(0, 10, 20, 30, 40, 100)
exchangeForStake_labels <- c("[0,10)\n(%d)", "[10,20)\n(%d)", "[20,30)\n(%d)", "[30,40)\n(%d)", "[40,100)\n(%d)")

# Custom bins for valuation
valuation_breaks <- c(40000, 500000, 1000000, 5000000, 10000000, Inf)
valuation_labels <- c("40K-500K\n(%d)", "500K-1M\n(%d)", "1M-5M\n(%d)", "5M-10M\n(%d)", ">10M\n(%d)")

# Create bins and format labels
data$askedFor_bin <- create_custom_bins(data$askedFor, 
                                        breaks = askedFor_breaks, 
                                        labels = sprintf(askedFor_labels, table(cut(data$askedFor, askedFor_breaks, right = FALSE))))

data$exchangeForStake_bin <- create_custom_bins(data$exchangeForStake, 
                                                breaks = exchangeForStake_breaks, 
                                                labels = sprintf(exchangeForStake_labels, table(cut(data$exchangeForStake, exchangeForStake_breaks, right = FALSE))))

data$valuation_bin <- create_custom_bins(data$valuation, 
                                         breaks = valuation_breaks, 
                                         labels = sprintf(valuation_labels, table(cut(data$valuation, valuation_breaks, right = FALSE))))

# Calculate percentages for each bin
deal_percentage_askedFor <- data %>%
  group_by(askedFor_bin, deal) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(askedFor_bin) %>%
  mutate(percentage = count / sum(count) * 100)

deal_percentage_exchangeForStake <- data %>%
  group_by(exchangeForStake_bin, deal) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(exchangeForStake_bin) %>%
  mutate(percentage = count / sum(count) * 100)

deal_percentage_valuation <- data %>%
  group_by(valuation_bin, deal) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(valuation_bin) %>%
  mutate(percentage = count / sum(count) * 100)

# Custom colors
custom_colors <- c("FALSE" = "#FF7F7F", "TRUE" = "#4B0082") # Light red and dark purple

# Plot for askedFor
plot_askedFor <- ggplot(data) +
  geom_bar(aes(x = askedFor_bin, fill = factor(deal)), position = "fill") +
  labs(title = "Distribution of Deals by Amount of Money Asked",
       x = "Money Asked for ($)", 
       y = "Percentage") +
  scale_fill_manual(values = custom_colors) +
  geom_text(data = deal_percentage_askedFor %>% filter(deal == TRUE),
            aes(x = askedFor_bin, y = percentage / 100, label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5), 
            color = "white", size = 4) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Plot for exchangeForStake
plot_exchangeForStake <- ggplot(data) +
  geom_bar(aes(x = exchangeForStake_bin, fill = factor(deal)), position = "fill") +
  labs(title = "Distribution of Deals by Equity Offered for Exchange",
       x = "Offered Equity Exchanged for Stake (%)", 
       y = "Percentage") +
  scale_fill_manual(values = custom_colors) +
  geom_text(data = deal_percentage_exchangeForStake %>% filter(deal == TRUE),
            aes(x = exchangeForStake_bin, y = percentage / 100, label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5), 
            color = "white", size = 4) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Plot for valuation
plot_valuation <- ggplot(data) +
  geom_bar(aes(x = valuation_bin, fill = factor(deal)), position = "fill") +
  labs(title = "Distribution of Deals by Valuation",
       x = "Valuation ($)", 
       y = "Percentage") +
  scale_fill_manual(values = custom_colors) +
  geom_text(data = deal_percentage_valuation %>% filter(deal == TRUE),
            aes(x = valuation_bin, y = percentage / 100, label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5), 
            color = "white", size = 4) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Combine the three plots into a single layout with one column and three rows
combined_plot <- plot_askedFor / plot_exchangeForStake / plot_valuation

# Display the combined plot
print(combined_plot)


#########success rate by category

# Calculate total rows and percentage of deal == TRUE for each merged_category
result <- data %>%
  group_by(merged_category) %>%
  summarise(
    Total_Rows = n(),
    Deal_True_Percentage = sum(deal == TRUE) / n() * 100
  ) %>%
  arrange(desc(Deal_True_Percentage)) # Sort by percentage of deal == TRUE

# Display the result table
print(result)

#Calculate percentages and sort categories
sorted_data <- data %>%
  group_by(merged_category, deal) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(merged_category) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(deal == TRUE) %>%
  arrange(desc(percentage))

# Reorder merged_category based on sorted percentages
data$merged_category <- factor(data$merged_category, levels = sorted_data$merged_category)

# Plot for merged_category with labels for successful deals only
plot_merged_category <- ggplot(data) +
  geom_bar(aes(x = merged_category, fill = factor(deal)), position = "fill") +
  labs(title = "Distribution of Deals by Merged Category",
       x = "Merged Category", 
       y = "Percentage") +
  scale_fill_manual(values = custom_colors) +
  geom_text(data = sorted_data, # Only for deal == TRUE
            aes(x = merged_category, y = percentage / 100, label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5), 
            color = "white", size = 4, angle = 90) + # Rotated text for labels
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Display the plot
print(plot_merged_category)

#########success rate by location

# Calculate total rows and percentage of deal == TRUE for each state
result <- data %>%
  group_by(state) %>%
  summarise(
    Total_Rows = n(),
    Deal_True_Percentage = sum(deal == TRUE) / n() * 100
  ) %>%
  arrange(desc(Deal_True_Percentage)) # Sort by percentage of deal == TRUE

# Display the result table
print(result)

#Calculate percentages and sort categories
sorted_data <- data %>%
  group_by(state, deal) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(state) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  filter(deal == TRUE) %>%
  arrange(desc(percentage))

# Reorder state based on sorted percentages
data$state <- factor(data$state, levels = sorted_data$state)

# Plot for state with labels for successful deals only
plot_state <- ggplot(data) +
  geom_bar(aes(x = state, fill = factor(deal)), position = "fill") +
  labs(title = "Distribution of Deals by City",
       x = "City", 
       y = "Percentage") +
  scale_fill_manual(values = custom_colors) +
  geom_text(data = sorted_data, # Only for deal == TRUE
            aes(x = state, y = percentage / 100, label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5), 
            color = "white", size = 4, angle = 90) + # Rotated text for labels
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Display the plot
print(plot_state)

######Checking the trend of deal outcome through seasons

# Ensure season is ordered as 1, 2, 3, 4, 5, 6
data$season <- factor(data$season, levels = c("1", "2", "3", "4", "5", "6"))

# Calculate total rows and percentage of deal == TRUE for each season
result <- data %>%
  group_by(season) %>%
  summarise(
    Total_Rows = n(),
    Deal_True_Percentage = sum(deal == TRUE) / n() * 100
  )

# Plot for season with labels for successful deals only
plot_season <- ggplot(data) +
  geom_bar(aes(x = season, fill = factor(deal)), position = "fill") +
  labs(title = "Distribution of Deals by Season",
       x = "Season number", 
       y = "Percentage") +
  scale_fill_manual(values = custom_colors) +
  geom_text(data = result, # Only for deal == TRUE
            aes(x = season, y = Deal_True_Percentage / 100, label = sprintf("%.1f%%", Deal_True_Percentage)),
            position = position_stack(vjust = 0.5), 
            color = "white", size = 4, angle = 90) + # Rotated text for labels
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Display the plot
print(plot_season)

###############Models#############################3


# Rename the column 'state' to 'city'
colnames(data)[colnames(data) == "state"] <- "city"
colnames(data)[colnames(data) == "shark_Kevin_O'Leary"] <- "shark_Kevin_OLeary"

#Since we are focusing on actions entrepreneurs can take to increase their chance of getting a deal,
#we remove columns like episode,season, etc and keeping these relevant columns
# Select the desired columns to create df_for_model

# Select the desired columns to create df_for_model with corrected column names
df_for_model <- data[, c("deal", "website", "askedFor", "exchangeForStake", "valuation", 
                         "log_askedFor", "log_valuation", "Multiple.Entreprenuers", 
                         "merged_category", "city", "shark_Barbara_Corcoran", 
                         "shark_Lori_Greiner", "shark_Robert_Herjavec", 
                         "shark_Kevin_OLeary", "shark_Steve_Tisch", "shark_Daymond_John", 
                         "shark_Jeff_Foxworthy", "shark_Mark_Cuban", "shark_Kevin_Harrington", 
                         "shark_John_Paul_DeJoria", "shark_Nick_Woodman")]

# Check the structure of the resulting data frame
str(df_for_model)
write.csv(df_for_model,"E:/MMA/MGSC 661 stats/Final Proj/clesn_Df_shark_tank.csv", row.names = FALSE)

###########LDA, QDA

install.packages("MASS")
install.packages("klaR")
library(MASS)
library(klaR)

attach(data)

deal <- as.factor(deal)
table(deal)

deal_lda1=lda(deal~ exchangeForStake+askedFor)
deal_lda1

partimat(data$deal~ data$exchangeForStake+data$askedFor, method="lda",image.colors=c("#FF7F7F","#4B0082"))

#Log_transformation of money asked for and valuation

data$log_askedFor <- log(data$askedFor + 1) # Adding 1 to avoid issues with log(0)
data$log_valuation <- log(data$valuation + 1) # Adding 1 to avoid issues with log(0)

#Run lda on log form 
deal_lda2=lda(data$deal~ data$exchangeForStake+data$log_askedFor)
deal_lda2

partimat(data$deal~ data$exchangeForStake+data$log_askedFor, method="lda",image.colors=c("#FF7F7F","#4B0082"))

#Run on valuation and money asked for
deal_lda3=lda(data$deal~ data$exchangeForStake+data$log_valuation)
deal_lda3

partimat(data$deal~ data$exchangeForStake+data$log_valuation, method="lda",image.colors=c("#FF7F7F","#4B0082"))


###############3Growing a tree
install.packages("tree")
install.packages("rpart.plot")
library(tree)
library(rpart)				        
library(rpart.plot)	

attach(df_for_model)

#Factor categorical variables
df_for_model$deal <- as.factor(df_for_model$deal)
df_for_model$website <- as.factor(df_for_model$website)
df_for_model$Multiple.Entreprenuers <- as.factor(df_for_model$Multiple.Entreprenuers)
df_for_model$merged_category <- as.factor(df_for_model$merged_category)

deal_tree1=rpart(df_for_model$deal~ df_for_model$website
                 #+df_for_model$merged_category
                 +df_for_model$Multiple.Entreprenuers
                 +df_for_model$askedFor
                 +df_for_model$exchangeForStake
                 +df_for_model$valuation
                 ,control=rpart.control(cp=0.01))

# Save the plot in high resolution
png("E:/MMA/MGSC 661 stats/Final Proj/decision_tree.png", width = 3000, height = 2000, res = 300)

# Adjust parameters for a cleaner version
rpart.plot(deal_tree1,
           cex =0.54,         # Increase font size
           #tweak = 0.3,       # Adjust box size
           main = "Decision Tree for Deal Prediction",
           digits = -3,            # No scientific notation, commas instead
           varlen = 0,             # Show full variable names
           faclen = 0,
           box.palette = c("#D95252", "#4B0082"), # Red for FALSE, Purple for TRUE
           split.box.col = "white", # White background for split labels
           split.col = "black",  # Black text for split labels
           col = "white"         # White text inside the nodes
)         

# Close the device to save the image
dev.off()

summary(deal_tree1)

###########Run random forest regressor to predict the deal
install.packages("randomForest")
library(randomForest)

myforest <- randomForest(
  deal ~ city
  + website
  + merged_category
  + Multiple.Entreprenuers
  + log_askedFor
  + exchangeForStake
  + log_valuation
  + shark_Barbara_Corcoran
  + shark_Lori_Greiner
  + shark_Robert_Herjavec
  + shark_Kevin_OLeary
  + shark_Steve_Tisch
  + shark_Daymond_John
  + shark_Jeff_Foxworthy
  + shark_Mark_Cuban
  + shark_Kevin_Harrington
  + shark_John_Paul_DeJoria
  + shark_Nick_Woodman,
  ntree = 500,
  data = df_for_model,
  importance = TRUE,
  na.action = na.omit
)

myforest
importance(myforest)

library(ggplot2)

# Extract feature importance
importance_df <- as.data.frame(importance(myforest))
importance_df$Feature <- rownames(importance_df)

# Sort by MeanDecreaseAccuracy for visualization
importance_df <- importance_df[order(importance_df$MeanDecreaseAccuracy, decreasing = TRUE), ]

# Plot with black outlines
ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") + # Add black outline with color
  coord_flip() + # Flip axes for horizontal bars
  labs(
    title = "Feature Importance in Random Forest",
    x = "Features",
    y = "Mean Decrease in Accuracy"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14), # Center the title
    axis.text = element_text(size = 10)
  )


install.packages("caret")
library(caret)
set.seed(123)

# Define cross-validation method (5-fold CV)
cv_control <- trainControl(method = "cv", number = 5)

# Train random forest with cross-validation
rf_model <- train(deal ~ city
                  + website
                  + merged_category
                  + Multiple.Entreprenuers
                  + log_askedFor
                  + exchangeForStake
                  + log_valuation
                  #+ shark_Barbara_Corcoran
                  #+ shark_Lori_Greiner
                  + shark_Robert_Herjavec
                  #+ shark_Kevin_OLeary
                  #+ shark_Steve_Tisch
                  #+ shark_Daymond_John
                  + shark_Jeff_Foxworthy
                  + shark_Mark_Cuban
                  + shark_Kevin_Harrington
                  #+ shark_John_Paul_DeJoria
                  + shark_Nick_Woodman,
                  data = df_for_model,
                  ntree = 1000,
                  na.action = na.omit,
                  method = "rf", 
                  trControl = cv_control,
                  tuneLength = 5) # Try 5 different hyperparameter combinations

# View results
print(rf_model)
print(rf_model$bestTune)  # Check the selected best mtry value

#check the number of feautures after dummification
preprocessed_data <- model.matrix(deal ~ ., data = df_for_model)
ncol(preprocessed_data) - 1  # Subtract 1 for the intercept column

# Extract feature importance
importance_df <- varImp(rf_model)$importance
importance_df$Feature <- rownames(importance_df)

# Sort by importance for visualization
importance_df <- importance_df[order(importance_df$Overall, decreasing = TRUE), ]
importance_df <- head(importance_df,20)

# Plot with black outlines
library(ggplot2)

ggplot(importance_df, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") + # Add black outline with color
  coord_flip() + # Flip axes for horizontal bars
  labs(
    title = "Feature Importance in Random Forest (Cross-Validation)",
    x = "Features",
    y = "Importance (Overall)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14), # Center the title
    axis.text = element_text(size = 10)
  )

############Gradient boosting
# Install necessary packages if not already installed
install.packages("xgboost")
require("xgboost") 

# Load the necessary libraries
library(xgboost)
library(caret)

# Ensure the dataset is clean and factorized
df_for_model <- na.omit(df_for_model)
df_for_model$city <- factor(df_for_model$city)
df_for_model$website <- factor(df_for_model$website)
df_for_model$merged_category <- factor(df_for_model$merged_category)

# Create the data matrix for the entire dataset
data_matrix <- model.matrix(~ city 
                            + website 
                            + merged_category 
                            + Multiple.Entreprenuers 
                            + log_askedFor 
                            + exchangeForStake 
                            + log_valuation - 1, 
                            data = df_for_model)

# Target variable
target <- as.numeric(df_for_model$deal) - 1

# Set up 5-fold cross-validation
set.seed(123)
folds <- createFolds(target, k = 5, list = TRUE)

# Define grid of hyperparameters
param_grid <- expand.grid(
  max_depth = c(3, 5, 7),      # Maximum depth of trees
  eta = c(0.01, 0.1, 0.3),     # Learning rate
  subsample = c(0.7, 0.8, 1),  # Fraction of samples for training
  gamma = c(0, 1, 5)           # Minimum loss reduction for a split
)

# Initialize storage for results
results <- data.frame(
  max_depth = integer(),
  eta = numeric(),
  subsample = numeric(),
  gamma = numeric(),
  mean_accuracy = numeric()
)

# Perform grid search with cross-validation
for (i in seq_len(nrow(param_grid))) {
  params <- param_grid[i, ]
  cat("Testing params: max_depth =", params$max_depth, 
      "eta =", params$eta, 
      "subsample =", params$subsample, 
      "gamma =", params$gamma, "\n")
  
  accuracies <- c()
  
  # Perform cross-validation
  for (fold in seq_along(folds)) {
    test_indices <- folds[[fold]]
    train_indices <- setdiff(seq_len(nrow(data_matrix)), test_indices)
    
    train_data <- data_matrix[train_indices, ]
    train_label <- target[train_indices]
    test_data <- data_matrix[test_indices, ]
    test_label <- target[test_indices]
    
    # Train XGBoost model
    xgb_model <- xgboost(
      data = train_data,
      label = train_label,
      nrounds = 500,
      objective = "binary:logistic",
      eval_metric = "error",
      max_depth = params$max_depth,
      eta = params$eta,
      subsample = params$subsample,
      gamma = params$gamma,
      verbose = 0
    )
    
    # Predict on test set
    test_pred <- predict(xgb_model, test_data)
    test_pred_class <- ifelse(test_pred > 0.5, 1, 0)
    
    # Calculate accuracy
    conf_matrix <- confusionMatrix(
      factor(test_pred_class), 
      factor(test_label), 
      positive = "1"
    )
    accuracies <- c(accuracies, conf_matrix$overall["Accuracy"])
  }
  
  # Store the mean accuracy for the current hyperparameters
  mean_accuracy <- mean(accuracies)
  results <- rbind(results, c(params, mean_accuracy))
}

# Display results
results <- results[order(-results$mean_accuracy), ]
print(results)

####PCA analysis on combination of sharks
install.packages("ggbiplot")
library(ggbiplot)

shark_columns <- c(
  "shark_Barbara_Corcoran", "shark_Lori_Greiner", "shark_Robert_Herjavec",
  "shark_Kevin_OLeary", "shark_Steve_Tisch", "shark_Daymond_John",
  "shark_Jeff_Foxworthy", "shark_Mark_Cuban", "shark_Kevin_Harrington",
  "shark_John_Paul_DeJoria", "shark_Nick_Woodman"
)
shark_data <- df_for_model[shark_columns]

# Remove constant/zero-variance columns
shark_data <- shark_data[, apply(shark_data, 2, var) > 0]

# Rename columns to remove "shark_"
colnames(shark_data) <- gsub("shark_", "", colnames(shark_data))

# Add deal outcome to the dataset
deal_outcome <- as.factor(df_for_model$deal)

# Perform PCA
shark_pca <- prcomp(shark_data, center = TRUE, scale. = TRUE)

# Create a biplot without circles and ellipses
biplot <- ggbiplot(
  shark_pca, obs.scale = 1, var.scale = 1, 
  groups = deal_outcome, ellipse = FALSE, 
  circle = FALSE
) +
  geom_point(aes(color = deal_outcome), size = 3) + # Add points
  scale_color_manual(values = c("FALSE" = "#D95252", "TRUE" = "#4B0082")) +
  labs(
    title = "PCA of Shark Combinations and Deal Outcome",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "right"
  )

# Print the biplot
print(biplot)

############2nd versian of PCA
#have separate charts for true and false with the size of point representing the number of datapoints
#install.packages("dplyr")

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# Select shark-related columns
shark_columns <- c(
  "shark_Barbara_Corcoran", "shark_Lori_Greiner", "shark_Robert_Herjavec",
  "shark_Kevin_OLeary", "shark_Steve_Tisch", "shark_Daymond_John",
  "shark_Jeff_Foxworthy", "shark_Mark_Cuban", "shark_Kevin_Harrington",
  "shark_John_Paul_DeJoria", "shark_Nick_Woodman"
)
# Add the deal outcome to the dataset
shark_data$deal <- df_for_model$deal

# Aggregate data to calculate frequency of each unique combination
shark_data_agg <- shark_data %>%
  group_by(across(-deal), deal) %>%
  summarise(frequency = n(), .groups = "drop")

# PCA for deal == TRUE
shark_data_true <- filter(shark_data, deal == TRUE) %>% select(-deal)
shark_pca_true <- prcomp(shark_data_true, center = TRUE, scale. = TRUE)

# Scale the PCA loadings for deal == TRUE
loadings_true <- as.data.frame(shark_pca_true$rotation[, 1:2])
scale_factor <- 5  # Adjust this value for arrow scaling
loadings_true$PC1 <- loadings_true$PC1 * scale_factor
loadings_true$PC2 <- loadings_true$PC2 * scale_factor
loadings_true$shark <- rownames(loadings_true)

# PCA for deal == FALSE
shark_data_false <- filter(shark_data, deal == FALSE) %>% select(-deal)
shark_pca_false <- prcomp(shark_data_false, center = TRUE, scale. = TRUE)

# Scale the PCA loadings for deal == FALSE
loadings_false <- as.data.frame(shark_pca_false$rotation[, 1:2])
scale_factor <- 5  # Adjust this value for arrow scaling
loadings_false$PC1 <- loadings_false$PC1 * scale_factor
loadings_false$PC2 <- loadings_false$PC2 * scale_factor
loadings_false$shark <- rownames(loadings_false)

# Add principal components for aggregated data
shark_data_agg_true <- shark_data_agg %>%
  filter(deal == TRUE) %>%
  mutate(
    PC1 = predict(shark_pca_true, newdata = select(., -deal, -frequency))[, 1],
    PC2 = predict(shark_pca_true, newdata = select(., -deal, -frequency))[, 2]
  )

shark_data_agg_false <- shark_data_agg %>%
  filter(deal == FALSE) %>%
  mutate(
    PC1 = predict(shark_pca_false, newdata = select(., -deal, -frequency))[, 1],
    PC2 = predict(shark_pca_false, newdata = select(., -deal, -frequency))[, 2]
  )

# Plot for deal == TRUE
plot_true <- ggplot(shark_data_agg_true, aes(x = PC1, y = PC2)) +
  geom_point(aes(size = frequency), color = "#4B0082", alpha = 0.7) +
  geom_segment(data = loadings_true, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.3, "cm")), color = "black", alpha = 0.8, size = 1) +
  geom_text(data = loadings_true, aes(x = PC1, y = PC2, label = shark), hjust = -0.2, vjust = -0.5, size = 4) +
  labs(
    title = "PCA of Shark Combinations (Deal = TRUE)",
    x = "Principal Component 1",
    y = "Principal Component 2",
    size = "Frequency"
  ) +
  theme_minimal()

# Plot for deal == FALSE
plot_false <- ggplot(shark_data_agg_false, aes(x = PC1, y = PC2)) +
  geom_point(aes(size = frequency), color = "#D95252", alpha = 0.7) +
  geom_segment(data = loadings_false, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.3, "cm")), color = "black", alpha = 0.8, size = 1) +
  geom_text(data = loadings_false, aes(x = PC1, y = PC2, label = shark), hjust = -0.2, vjust = -0.5, size = 4) +
  labs(
    title = "PCA of Shark Combinations (Deal = FALSE)",
    x = "Principal Component 1",
    y = "Principal Component 2",
    size = "Frequency"
  ) +
  theme_minimal()

# Combine plots into a 1x2 grid
grid.arrange(plot_true, plot_false, ncol = 2)


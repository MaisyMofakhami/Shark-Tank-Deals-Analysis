# Shark Tank Deal Prediction Analysis

## Project Overview
This project analyzes the factors influencing deal outcomes on the popular TV series Shark Tank, where entrepreneurs pitch their ideas to investors ("sharks") for funding. By leveraging data across six seasons, this study examines the relationships between pitch attributes, funding metrics, and the likelihood of securing a deal. The project focuses on exploratory data analysis (EDA) and predictive modeling to derive actionable insights for entrepreneurs.

## Key Features of the Analysis

### 1. Exploratory Data Analysis
Funding Metrics: Relationship between deal outcomes, requested funding, valuation, and equity offered.

Category Insights: Success rates across industries like Food & Beverages, Clothing, and Baby Products.

Location Analysis: Geographic trends in deal outcomes, highlighting hubs like California and New York.

Sharks' Influence: Patterns of shark collaboration and their roles in deal success.

### 2. Predictive Modeling
Decision Trees: Interpretable insights into how funding and valuation impact deal likelihood.
Random Forest & Gradient Boosting: Models to predict deal outcomes, emphasizing feature importance.
Principal Component Analysis (PCA): Visualization of shark influence on deal success.

## Dataset Description

The dataset includes:
Pitch Details: Funding requested, equity offered, valuation, and business category.
Deal Outcome: Whether the pitch resulted in a deal (True/False).
Sharks' Presence: Involvement of sharks during the pitch.
Location: City-level insights into entrepreneurial activity.

Data Preprocessing:
Categories were generalized (e.g., merging baby-related categories into "Baby & Children").
Locations were grouped by state, with low-frequency locations labeled as "Other."
Monetary columns underwent log transformation to address skewness.

## Insights and Findings
Funding Range: Deals were most successful when funding requests were between $50K–$1M and equity offered was below 17%.
Valuation Sweet Spot: Companies valued between $1M–$5M had the highest success rates.
![image](https://github.com/user-attachments/assets/99a923eb-c819-40b6-ac11-12cd75fb5632)

Online Presence: Having a website significantly boosted deal success, particularly with lower equity offers.
Key Categories: Food & Beverages, Clothing, and Baby Products performed exceptionally well.
Sharks' Roles: Sharks like Mark Cuban and Kevin Harrington were instrumental in deal success, with distinct patterns of collaboration.
![image](https://github.com/user-attachments/assets/070b5938-b694-45cb-9c93-80ef7c415c65)

Sharks Play a Greater Role in Successful Deals: In the "Deal = True" chart, arrows are generally longer, indicating sharks have a more significant role in explaining variance for successful deals compared to unsuccessful ones
![image](https://github.com/user-attachments/assets/557b7f11-11cd-48a9-945e-3e264e10fe61)

## Methodology and Limitations

### Models Used:
Decision Trees and Random Forests provided interpretable rules for deal predictions.
PCA highlighted the impact of sharks' presence in pitches.

### Performance Challenges:
Predictive accuracy (~55%) indicated limitations in explaining deal outcomes, suggesting that external factors like negotiation dynamics and investor preferences play a significant role.
Assumptions like normality in LDA constrained the results.

### Acknowledgments:
This project was part of the MMA program's statistical and Python courses. We acknowledge the dataset's limitations, including the absence of negotiation details and shark-specific feedback, which could provide deeper insights into deal-making dynamics.


# Groundwater Quality Analysis in India

This repository contains the code and analysis for a project that explores the relationship between groundwater quality and various economic, social, and environmental factors in India. The aim of the project is to understand the **determinants** of groundwater quality and provide insights for sustainable water management as well as to explore the **Environmental Kuznets curve**.

## Aim of our study

In our study:
* We aim to explore the EKC hypothesis in the context of groundwater quality in India, a country where economic growth has been rapid in recent
years and where groundwater is a vital resource for agriculture, drinking
water, and industry.

* We investigate the link(s) between groundwater quality [as measured by
the concentration of Hydrogen carbonate (HCO3-)] and various economic,
social, and environmental factors, such as state development, income
inequality, education, and poverty.

* We had 22 variables as groundwater quality indicators, among them we
chose concentration of HCO3- as our indicator because its distribution was
closest to normal distribution which would lead our OLS
estimators to be more accurate. Furthermore, based on its box-plot, it had
least number of outliers which made it an ideal candidate for our study.

## Data
The analysis is based on a dataset of groundwater quality measurements and related variables for different states in India. The dataset includes variables such as State Development Index (SDP), Gini Index, HDI (Human Development Index), literacy rate, poverty levels, and margin of victory in elections. The data is collected
from **trusted sources** such as [kaggle](https://www.kaggle.com/), [Central Ground water board, govt of India](https://cgwb.gov.in/GW-data-access.html), [National Data and Analytics Platform (NDAP)](https://ndap.niti.gov.in/), etc.

## Analysis Steps (Broad Overview)
1. **Data Preparation:** The dataset was preprocessed to handle missing values and ensure data quality.
2. **Exploratory Data Analysis:** Initial data exploration was conducted to understand the distribution and relationships between variables. Visualizations and summary statistics were used to gain insights into the data.
3. **Model Specification:** A multiple regression model was specified with the concentration of hydrogen carbonate (HCO3-) as the dependent variable and a set of independent variables including log-transformed SDP, Gini Index, HDI, literacy rate, and relevant interaction terms.
4. **Model Estimation:** The regression model was estimated using the least squares method to obtain the parameter estimates and assess the statistical significance of the variables.
5. **Model Evaluation:** The model's goodness of fit was assessed using R-squared and adjusted R-squared values. Residual analysis was performed to check for any violations of assumptions.
6. **Multicollinearity:** The presence of multicollinearity among the independent variables was assessed using variance inflation factor (VIF) values. Highly correlated variables were identified and, if necessary, removed or transformed to address multicollinearity.
7. **Outlier Detection:** Outliers in the data were identified using visual methods such as boxplots. Outliers with a significant impact on the model were considered for removal.
8. **Maximum Likelihood Estimation:** An alternative estimation strategy using maximum likelihood estimation was conducted to estimate the model parameters and assess the model fit.
9. **Structural Break Analysis:** A Chow test and t-test were performed to test for a structural break in the mean environmental quality across different state groups.
10. **Variance Analysis:** The variance in environmental quality across state groups was assessed using statistical tests such as Bartlett's test of homogeneity of variances.
11. **Feasible GLS:** After figuring out that our data is **Heteroscedastic**, groupwise feasible generalized least squares (GLS) estimation was conducted to account for the variance structure across state groups.

## Conclusion
Our final enhanced model showed significant
improvement with higher goodness-of-fit statistics of **0.37 (without splines * )** and **0.62 (with splines * )** and a
total of 320 degrees of freedom. We also checked for collinearity between variables and used splines to
capture non-linear relationships. Overall, our findings support the existence of a relationship between
economic growth and groundwater quality in India, aligning with the EKC hypothesis.

 To account for this heteroscedasticity in the data, we have used a groupwise Feasible Generalized Least
Squares (FGLS) estimation strategy. This estimation method takes into account the variance structure of the
data by estimating separate error variances for each group and allows for unbiased parameter estimates
even when the variances differ across groups. This showed the **R-squared of 0.38** which is an improvement
over our previous OLS and MLE parameter estimations :-)

*Splines: Splines are a mathematical technique used in regression analysis to model nonlinear relationships between variables. They are used when a simple linear relationship is inadequate to capture the complexity of the data. Splines allow for flexible and smooth curve fitting by breaking the data range into smaller segments and fitting separate polynomial functions to each segment.*

### To understand more about our project, please read our final report i.e Econometrics_final.pdf. Moreover, feel free to explore the directories DataAssignmentOne/ && DataAssignmentTwo/ whose structure is briefed below:

* **DataAssignmentOne** includes initial steps of our analysis i.e until we have come to a fairly accurate model. Please read its report in *DataAssignmentOne.pdf* file which contains all the info used and the results produced. The R script is in the files DataAssignmentOne.R as well as DataAssignmentOne.txt for convenient viewing as well as testing.

* **DataAssignmentTwo** extends the work done in DataAssignmentOne and works towards further enhancing the model and then finally evaluating our model and do #6-11 [Analysis steps](https://github.com/Ashutosh-Gera/Econometrics/new/main?readme=1#:~:text=Analysis%20Steps%20(Broad%20Overview)) on it.

### Thank you for visiting, feel free to direct any queries and/or issues.
### Ashutosh Gera 💙



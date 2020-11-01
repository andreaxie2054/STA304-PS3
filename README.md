# STA304-PS3
by Yuxuan Ju, Ning Xie

knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)
# Loading in the cleaned survey Data
survey_data <- read_csv("/Users/pro/Desktop/PS3surveydata/survey_data.csv")

# Loading in the cleaned census Data
census_data <- read_csv("/Users/pro/Desktop/PS3surveydata/census_data.csv")

# Predict Vote Outcome of 2020 American Federal Election

## YUXUAN JU, NING XIE
## 2020-11-01

# Model

In this project, predicting the vote outcome of the 2020 American federal election is the purpose. we employ the post-stratification technique with larger population size to a logit linear regression model we built with sample data. In the following sub-sections we will describe the model specifics, the post-stratification calculation and predict the vote outcome.


## Model Specifics
To predict the 2020 American federal election voting outcomes, we will be using logistic regression models since the dependent variables we choose are binary. Logistic regression predicts a categorical variable based on a set of independent variables and shows the impact of each variable on the odds ratio of the observed data. When we are analyzing the association of all variables, it prevents confounding effects. 

One of the model is to explore the proportion of voters who will vote for Donald Trump while the other one is for Joe Biden. Since the survey shows that some people are still not sure who they will vote for,we need to build two models to see the supporting rate of each elector. We choose age, gender, race_ethnicity and hispanic, as independent variables since they are relatively the key factor impacting the voting outcomes. The logistic regression model we are using is:

$$ log(p/(1-p)) = \beta_0+\beta_1  x_{age} + \beta_2  x_{sex} + \beta_3  x_{hispanic} + \beta_4  x_{race} +  \epsilon$$

Where $log(p/(1-p))$ represents the proportion of voters who will vote for Donald Trump/Joe Biden. Similarly, $\beta_0$ is the "fixed component" of that component-wise method to describe log odds of 2020 American federal election voting outcomes. Additionally, $\beta_1$ to $\beta_4$, it represents the extend of increase in the log odds of voting for Donald Trump/Joe Biden for every one unit increase in the corresponding continuous independent variable which is age in our model here, otherwise, for categorical independent variables, it represents the performance of each category is evaluated with respect to a base category. 

## Model for Donald Trump

# Creating the Model
model_trump <- glm(vote_trump ~ sex + hispann + racee +age, 
            data=survey_data, family= "binomial")

summary(model_trump)

## Model for Joe Biden

model_biden <- glm(vote_biden ~  sex + hispann + racee +age, 
            data=survey_data, family= "binomial")

summary(model_biden)

## Post-Stratification 

When doing survey analysis, post-stratification is a common technique to incorporate population distributions of variables into survey estimates. It first divides samples into cells and uses demographics to predict how the entire population will vote based on the estimate for each cell. Use the two models we built previously to estimate the proportion of voters in each cell, then weight these proportion estimate via corresponding population size and divide the sum of these value by entire population size.By survey sampling, post-stratification effectively makes an accurate estimate of the population and correct non-sampling errors.

In order to estimate the proportion of voters who will vote for Donald Trump/Joe Biden, we need to perform a post-stratification analysis. Here we create cells based on different ages, genders, race_ethnicities and hispanics.

# Perform the post-stratification calculation
census_data$logodds_estimate <-
  model_trump %>%
  predict(newdata = census_data)

census_data$estimate <-
  exp(census_data$logodds_estimate)/(1+exp(census_data$logodds_estimate))

census_data %>%
  mutate(alp_predict_prop = estimate*n) %>%
  summarize(alp_predict = sum(alp_predict_prop)/sum(n))
  
# Perform the post-stratification calculation
census_data$logodds_estimate <-
  model_biden  %>%
  predict(newdata = census_data)

census_data$estimate <-
  exp(census_data$logodds_estimate)/(1+exp(census_data$logodds_estimate))

census_data %>%
  mutate(alp_predict_prop = estimate*n) %>%
  summarize(alp_predict = sum(alp_predict_prop)/sum(n))

# Results

The result shows that the proportion of voters voting for Donald Trump $$\hat{y}^{PSt}$$ is 38.22557% and the proportion of voters voting for Joe Biden $$\hat{y}^{PSb}$$is 41.37606% based on our post-stratification analysis concerning the proportion of voters voting for either Donald Trump or Joe Biden. This is modelled by the logistic regression model with independent variables sex,age,race and hispanic.

# Discussion

## Summary

The logistic linear regression model was built from a reduced dataset containing the four independent variables we choose to predict the vote result, where it has 6479 observations and 265 variables in the original raw dataset. For preparing the census dataset which contains a higher volume of observations -- 3214539 observations, we divide them into different cells based on the explanatory variables that will be fitted into the previous model. Since the citizens who chose to vote for other candidates or abstain from voters are accounted as blackball when estimating the approval rate for 
both Donald Trump and Joe Biden. We use a separate model to estimate the approval rates and compare the difference between them, and find out the predicted winner in the 2020 American Federal Election.

## Conclusion
Since post-stratification obtains an accurate estimate of the entire population, we predict that Joe Biden will win the election based on our estimation of the proportion of voters voting for Joe Biden being 41.37606% and proportion of voters voting for Donald Trump being  38.22557%, where approval. The rate for Joe Biden is 3.15049% higher than the approval rate of Donald Trump.

## Weaknesses

In our analysis, the number of independent variables we choose is not enough. With more variables being used to make models and set post-stratification, the result would be more accurate. Besides, due to the diversity of data within each corresponding column between the survey data and census data, we make each variable we choose to be binary to simplify post-stratification. 

## Next Steps

To better improve our future estimation, we shall keep the diversity of data by improving our cleaningtechniques to match corresponding subsets in each explanatory variable between two datasets. However, simply clean and classify the response first in the process of collecting data by the survey is another suitable choice. We can also use various plots to visualize our models. At last, we can compare our estimation with the actual election result to check the difference and do another survey for improvement.

# References

1.https://towardsdatascience.com/implementing-binary-logistic-regression-in-r-7d802a9d98fe, ¡°Binary Logistic Regression¡±, by Akanksha Rawat, accessed on Nov. 1st, 2020.
2.https://www.voterstudygroup.org/publication/nationscape-data-set, Democracy Fund + UCLA Nationscape ¡®Full Data Set, accessed on Nov. 1st, 2020
3.https://usa.ipums.org/usa/index.shtml, The American Community Surveys (ACS), accessed on Nov. 1st, 2020
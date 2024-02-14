# The Effect of US Gas Prices on Alternative Methods of Travel

## About the project

How does the average US gas price over time affect ridership numbers of alternative transportation methods? If so, can we predict demand? For our group project, we analyzed historical gas prices and public transit use. Predicting transit demand is valuable for transit providers, transit competitors, and policymakers: transit providers need to know how demand will change, competitors need to know how to market themselves and how to provide their own services, and policymakers may be interested in learning what factors increase adoption and funding of public transportation.

We use multiple tools and strategies for analyzing historical gas prices and transit use, including Granger causality testing, correlation analysis, and predictive modeling.

## Data Sources

Data can be found in the "Data" folder of this repository. The data are sourced from:

- [U.S. Energy Information Energy Administration](https://www.eia.gov/outlooks/steo/realprices/): Real gasoline prices
- [U.S. Department of Transportation National Transit Database](https://www.transit.dot.gov/ntd/data-product/monthly-module-adjusted-data-release): Complete Monthly Ridership (with adjustments and estimates)

## Conclusions and recommendations

Although our models appear to pick up some signal in the training data, vector autoregressive models aren't capable of picking up on the complex trends in the testing data. Poor model performance on test data is likely in part due to the disruptions from COVID-19, but this is also likely due, in part, to the limitations of VAR models for forecasting. We recommend use and analysis of more complex models as well as analysis of smaller markets. Due to time constraints, we focused on the larger markets in our data, and it may be the case that in such markets, all consumers who are price-sensitive already use public transit all or most of the time.

## About the team

Summer 2023's MGT6203 team 104 consists of Caroline Schmitt, Emy Ng, Matthew Kim, Mike Genovese, and Osman Yardimci. 

This upload is my "personal" upload (mirrored from the GA Tech-specific Github instance.) Thanks to my classmates for working with me! All code authored by my classmates is in folders with their last names given.

## Project structure

Code is found in the "Code" folder, organized by contributer and language. Most code is written in R and uses standard libraries, though to run all code, you made need to install the following additional libraries: `changepoint`, `reshape2`, `fpp3`, `astsa`, `vars`, `sGARCH`, and `Metrics`.

PDF copies of reports and presentations can be found in the named folders.

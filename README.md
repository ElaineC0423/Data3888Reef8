# Data3888Reef8
**FinalApp_beta:** contains Shiny App and used data
app.R: the Reef Health prediction App 
> *To use the app, due to of the size of the dataset, there might be a quite long loading time (May cost 30s before it shows).*
- Home Page: Red point in the interactive map shows reef which has more than 3 years record. Double click the point to check fishing effort information
- Model Prediction Page: Reef health prediction model & interactive map showing the change in bleaching while adjusting fish catch rate.
- Graph Page: Contains graph shows predicted Reef bleaching rate in the future and linear relationship between selected fish & bleaching.

**MergeData.Rmd:** Contains code for data cleaning and merging
> Due to the original datasets excess the size limit of GitHub, we cannot upload them. Raw data Could be found in:
- [IMAS fishery data](https://data.gov.au/dataset/ds-aodn-5c4590d3-a45a-4d37-bf8b-ecd145cb356d/details?q=)
- [SAU fishing effort](https://www.seaaroundus.org/data/#/fishing-entity-effort/83?chart=catch-chart&dimension=sector&measure=effort&limit=10)
- [Reef data](https://github.com/InstituteForGlobalEcology/Coral-bleaching-a-global-analysis-of-the-past-two-decades/tree/master)

**VariableSelection.Rmd:** Contains code for ensure the correlation between selected variable and reef bleaching

**PredictionModels.Rmd:** Contains code for using different models to predict reef health

**ModelTesting.Rmd:** Contains code for comparing the performance of each model
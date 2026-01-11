# Early-Prediction-of-Academic-Dropout
Identification of factors associated with academic dropout using machine learning techniques

## Abstract
This project analyses academic dropout in higher education with the aim of identifying relevant risk factors and developing predictive models that support early detection of students at risk. The study is based on a dataset of university students containing academic, demographic and socioeconomic variables. The methodology combines exploratory data analysis, supervised classification models and unsupervised techniques, and is fully implemented in R.

## Dataset
The original dataset used in this project is publicly available and was obtained from the UCI Machine Learning Repository.

As part of the work, the data were cleaned, filtered and preprocessed.  
The processed dataset used for the analysis is included in this repository to facilitate reproducibility.

Original dataset source:
- UCI Machine Learning Repository (link to be added)

Processed data:
- `data/processed/` contains the cleaned dataset used in the models.

## Repository structure
data/
 └── processed/        # cleaned dataset used in the analysis

code/
 ├── 01_eda.R
 ├── 02_logistic_regression.R
 ├── 03_decision_tree.R
 ├── 04_random_forest.R
 └── 05_gower_distance.R
 └── 06_pam.R
 └── 05_compare_gruops.R

results/
 ├── figures/          # plots and visualizations
 └── tables/           # result tables

README.md
LICENSE

## Requirements
- R (version >= 4.0)
- RStudio 

Main R packages used:
- tidyverse
- caret
- randomForest
- rpart
- cluster
- compareGroups

## Reproducibility
The analysis is fully reproducible.  
Once the dataset is available, the scripts can be executed in the order provided in the `code/` folder to reproduce the results reported in the thesis.

## Author
Bachelor’s Thesis  
Universitat Politècnica de Catalunya (UPC)

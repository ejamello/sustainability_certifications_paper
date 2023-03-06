#####
## Trust and Demand for Clientelism: Study 1
## Authors: Rodrigo Cezar, Eduardo Mello, and Juliana Camargo
## Contact: Eduardo.Mello@fgv.br  
## This version: 2023-03-06
## R version 4.2.2 ("Innocent and Trusting") on Macbook OS 13.3.1 Ventura
####

### WARNING: This will clear your workspace
rm(list=ls())

### load packages
library(readxl)

### read in data
conjoint_data <- read_excel("data/SOTOUK_225981_CJ_Q23_20220112.xlsx")
survey_data <- read_excel("data/SOTOUK_225981_iI_20211229 todo_ponde.xlsx")
educ_data <- read_excel("data/EDUCATION LEVEL.xlsm")

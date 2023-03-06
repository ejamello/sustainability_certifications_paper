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
library(tidyverse)

### read in data
df <- read_excel("data/Importações e ExportaçõesFINAL.xlsx")

# Create new variable "state"
df <- df %>% 
  mutate(state = factor(substring(Cidade, nchar(Cidade)-1, nchar(Cidade))))

#####
## Defending the environment or curbing imports? The trade effects of government-backed voluntary certifications
## Authors: Rodrigo Cezar, Eduardo Mello, and Juliana Camargo
## Contact: Eduardo.Mello@fgv.br  
## This version: 2023-03-06
## R version 4.2.2 ("Innocent and Trusting") on Macbook OS 13.3.1 Ventura
####

### WARNING: This will clear your workspace
rm(list=ls())

### Load packages
library(readxl)
library(tidyverse)
library(stringr)
#remotes::install_github("curso-r/munifacil")
library(munifacil)

### Read in data
df <- read_excel("data/Importações e ExportaçõesFINAL.xlsx")

#### Rename variables to make them R-friendly

# "Cidade" into "mun"
df <- df %>%
  rename(mun = Cidade)

### Creating new variables

# Create new variable "state"
df <- df %>% 
  mutate(state = factor(substring(mun, nchar(mun)-1, nchar(mun))))

# Delete characters from "mun" that represent state name 
df <- df %>% 
  mutate(mun = str_sub(mun, end = -6))

# Create variable with IBGE municipality code
sua_base <- readr::read_csv(arquivo) %>% 
  dplyr::select(
    municipio = 1, 
    uf = 3,
    ibge = 2
  ) %>% 
  dplyr::distinct(municipio, .keep_all = TRUE)

resultado <- sua_base %>% 
  limpar_colunas(municipio, uf) %>% 
  # cria uma coluna "uf_join"
  incluir_codigo_ibge()

resultado %>% 
  dplyr::filter(is.na(id_municipio))

resultado %>% 
  dplyr::count(ibge == id_municipio)




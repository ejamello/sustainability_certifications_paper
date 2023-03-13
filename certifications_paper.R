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

# "Cidade" into "municipio"
df <- df %>%
  rename(municipio = Cidade)

### Creating new variables

# Create new variable "state"
df <- df %>% 
  mutate(state = factor(substring(municipio, nchar(municipio)-1, nchar(municipio))))

# Delete characters from "municipio" that represent state name 
df <- df %>% 
  mutate(municipio = str_sub(municipio, end = -6))

# Create variable with IBGE municipality code
arquivo <- system.file("extdata/exemplo.csv", package = "munifacil")
df$municipio <- toupper(df$municipio)


sua_base <- readr::read_csv(arquivo) %>% 
  dplyr::select(
    municipio = 1, 
    uf = 3,
    ibge = 2
  ) %>% 
  dplyr::distinct(municipio, .keep_all = TRUE)

municipio_df <- unique(df$municipio)
municipio_sua_base <- unique(sua_base$municipio)
common_municipios <- intersect(municipio_df, municipio_sua_base)
print(municipio_df)
      

#df <- merge(df, sua_base, by = "municipio")

resultado <- sua_base %>% 
  limpar_colunas(municipio, uf) %>% 
  # cria uma coluna "uf_join"
  incluir_codigo_ibge()

resultado %>% 
  dplyr::filter(is.na(id_municipio))

resultado %>% 
  dplyr::count(ibge == id_municipio)




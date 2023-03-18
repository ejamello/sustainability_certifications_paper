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
# devtools::install_github("hersonpc/isgr")
library(munifacil)
library(isgr)

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

### Create variable with IBGE municipality code
#Open database with IBGE codes from Munifacil
arquivo <- system.file("extdata/exemplo.csv", package = "munifacil")

sua_base <- readr::read_csv(arquivo) %>% 
  dplyr::select(
    municipio = 1, 
    uf = 3,
    ibge = 2
  ) %>% 
  dplyr::distinct(municipio, .keep_all = TRUE)

# Create variable that removes Portuguese characters from municipality names
df$municipios_sem_acentos <- remove_acentos(df$municipio, pattern = "all")
sua_base$municipios_sem_acentos <- remove_acentos(sua_base$municipio, pattern = "all")

# Capital letters for municipality names withouth Portuguese characters
df$municipios_sem_acentos <- toupper(df$municipios_sem_acentos)


# Separate dataframe into states
ac <- df[df$state == "AC", ]
al <- df[df$state == "AL", ]
ap <- df[df$state == "AP", ]
am <- df[df$state == "AM", ]
ba <- df[df$state == "BA", ]
ce <- df[df$state == "CE", ]
df1 <- df[df$state == "DF", ]
es <- df[df$state == "ES", ]
go <- df[df$state == "GO", ]
ma <- df[df$state == "MA", ]
mt <- df[df$state == "MT", ]
ms <- df[df$state == "MS", ]
mg <- df[df$state == "MG", ]
pa <- df[df$state == "PA", ]
pb <- df[df$state == "PB", ]
pr <- df[df$state == "PR", ]
pe <- df[df$state == "PE", ]
pi <- df[df$state == "PI", ]
rj <- df[df$state == "RJ", ]
rn <- df[df$state == "RN", ]
rs <- df[df$state == "RS", ]
ro <- df[df$state == "RO", ]
rr <- df[df$state == "RR", ]
sc <- df[df$state == "SC", ]
sp <- df[df$state == "SP", ]
se <- df[df$state == "SE", ]
to <- df[df$state == "TO", ]

# Separate IBGE base into states
ac_ibge <- sua_base[sua_base$uf == "AC", ]
al_ibge <- sua_base[sua_base$uf == "AL", ]
ap_ibge <- sua_base[sua_base$uf == "AP", ]
am_ibge <- sua_base[sua_base$uf == "AM", ]
ba_ibge <- sua_base[sua_base$uf == "BA", ]
ce_ibge <- sua_base[sua_base$uf == "CE", ]
df1_ibge <- sua_base[sua_base$uf == "DF", ]
es_ibge <- sua_base[sua_base$uf == "ES", ]
go_ibge <- sua_base[sua_base$uf == "GO", ]
ma_ibge <- sua_base[sua_base$uf == "MA", ]
mt_ibge <- sua_base[sua_base$uf == "MT", ]
ms_ibge <- sua_base[sua_base$uf == "MS", ]
mg_ibge <- sua_base[sua_base$uf == "MG", ]
pa_ibge <- sua_base[sua_base$uf == "PA", ]
pb_ibge <- sua_base[sua_base$uf == "PB", ]
pr_ibge <- sua_base[sua_base$uf == "PR", ]
pe_ibge <- sua_base[sua_base$uf == "PE", ]
pi_ibge <- sua_base[sua_base$uf == "PI", ]
rj_ibge <- sua_base[sua_base$uf == "RJ", ]
rn_ibge <- sua_base[sua_base$uf == "RN", ]
rs_ibge <- sua_base[sua_base$uf == "RS", ]
ro_ibge <- sua_base[sua_base$uf == "RO", ]
rr_ibge <- sua_base[sua_base$uf == "RR", ]
sc_ibge <- sua_base[sua_base$uf == "SC", ]
sp_ibge <- sua_base[sua_base$uf == "SP", ]
se_ibge <- sua_base[sua_base$uf == "SE", ]
to_ibge <- sua_base[sua_base$uf == "TO", ]

# Merge by municipality in each state
ac <- merge(ac, ac_ibge, by = "municipios_sem_acentos")
al <- merge(al, al_ibge, by = "municipios_sem_acentos")
ap <- merge(ap, ap_ibge, by = "municipios_sem_acentos")
am <- merge(am, am_ibge, by = "municipios_sem_acentos")
ba <- merge(ba, ba_ibge, by = "municipios_sem_acentos")
ce <- merge(ce, ce_ibge, by = "municipios_sem_acentos")
df1 <- merge(df1, df1_ibge, by = "municipios_sem_acentos")
es <- merge(es, es_ibge, by = "municipios_sem_acentos")
go <- merge(go, go_ibge, by = "municipios_sem_acentos")
ma <- merge(ma, ma_ibge, by = "municipios_sem_acentos")
mt <- merge(mt, mt_ibge, by = "municipios_sem_acentos")
ms <- merge(ms, ms_ibge, by = "municipios_sem_acentos")
mg <- merge(mg, mg_ibge, by = "municipios_sem_acentos")
pa <- merge(pa, pa_ibge, by = "municipios_sem_acentos")
pb <- merge(pb, pb_ibge, by = "municipios_sem_acentos")
pr <- merge(pr, pr_ibge, by = "municipios_sem_acentos")
pe <- merge(pe, pe_ibge, by = "municipios_sem_acentos")
pi <- merge(pi, pi_ibge, by = "municipios_sem_acentos")
rj <- merge(rj, rj_ibge, by = "municipios_sem_acentos")
rn <- merge(rn, rn_ibge, by = "municipios_sem_acentos")
rs <- merge(rs, rs_ibge, by = "municipios_sem_acentos")
ro <- merge(ro, ro_ibge, by = "municipios_sem_acentos")
rr <- merge(rr, rr_ibge, by = "municipios_sem_acentos")
sc <- merge(sc, sc_ibge, by = "municipios_sem_acentos")
sp <- merge(sp, sp_ibge, by = "municipios_sem_acentos")
se <- merge(se, se_ibge, by = "municipios_sem_acentos")
to <- merge(to, to_ibge, by = "municipios_sem_acentos")


### Add FED interest rates

# Read the FED interest rates file into R
fed_rates <- read.table("path/to/fed_rates_file.txt", header = TRUE)

# Convert the "DATE" column to a Date object
fed_rates$DATE <- as.Date(fed_rates$DATE, format = "%m/%d/%Y")

# Extract the year from the "DATE" column
fed_rates$year <- format(fed_rates$DATE, "%Y")





# End of File
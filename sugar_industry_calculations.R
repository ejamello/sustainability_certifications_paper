#####
## Defending the environment or curbing imports? The trade effects of government-backed voluntary certifications
## Authors: Rodrigo Cezar, Eduardo Mello, and Juliana Camargo
## Contact: Eduardo.Mello@fgv.br  
## This version: 2023-04-24
## R version 4.2.2 ("Innocent and Trusting") on Macbook OS 13.3.1 Ventura
####

### WARNING: This will clear your workspace
rm(list=ls())

### Load packages
library(readxl)
library(tidyverse)
library(stringr)
# devtools::install_github("hersonpc/isgr")
library(isgr)
library(sf)
library(writexl)
library(rnaturalearth)


### Read in data
df <- read_excel("data/pessoal_ocupado.xlsx")
municipios <- st_read("data/BR_Municipios_2021/BR_Municipios_2021.shp")

#### Rename variables to make them R-friendly
# "Cidade" into "municipio"
municipios <- municipios %>%
  rename(municipio = NM_MUN)

# Código do IBGE
municipios <- municipios %>%
  rename(ibge = CD_MUN)

# "SIGLA" into "state"
municipios <- municipios %>%
  rename(state = SIGLA)

### Creating new variables
# Extract state abbreviation using regular expressions
df$state <- gsub(".*\\(([A-Z]{2})\\)$", "\\1", df$municipio)

# Remove parentheses and state abbreviation from original variable
df$municipio <- gsub("\\s*\\([A-Z]{2}\\)$", "", df$municipio)

### Create variable with IBGE municipality code
# Create variable that removes Portuguese characters from municipality names
df$municipios_sem_acentos <- remove_acentos(df$municipio, pattern = "all")
municipios$municipios_sem_acentos <- remove_acentos(municipios$municipio, pattern = "all")

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
ac_ibge <- municipios[municipios$state == "AC", ]
al_ibge <- municipios[municipios$state == "AL", ]
ap_ibge <- municipios[municipios$state == "AP", ]
am_ibge <- municipios[municipios$state == "AM", ]
ba_ibge <- municipios[municipios$state == "BA", ]
ce_ibge <- municipios[municipios$state == "CE", ]
df1_ibge <- municipios[municipios$state == "DF", ]
es_ibge <- municipios[municipios$state == "ES", ]
go_ibge <- municipios[municipios$state == "GO", ]
ma_ibge <- municipios[municipios$state == "MA", ]
mt_ibge <- municipios[municipios$state == "MT", ]
ms_ibge <- municipios[municipios$state == "MS", ]
mg_ibge <- municipios[municipios$state == "MG", ]
pa_ibge <- municipios[municipios$state == "PA", ]
pb_ibge <- municipios[municipios$state == "PB", ]
pr_ibge <- municipios[municipios$state == "PR", ]
pe_ibge <- municipios[municipios$state == "PE", ]
pi_ibge <- municipios[municipios$state == "PI", ]
rj_ibge <- municipios[municipios$state == "RJ", ]
rn_ibge <- municipios[municipios$state == "RN", ]
rs_ibge <- municipios[municipios$state == "RS", ]
ro_ibge <- municipios[municipios$state == "RO", ]
rr_ibge <- municipios[municipios$state == "RR", ]
sc_ibge <- municipios[municipios$state == "SC", ]
sp_ibge <- municipios[municipios$state == "SP", ]
se_ibge <- municipios[municipios$state == "SE", ]
to_ibge <- municipios[municipios$state == "TO", ]

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

### Remove dataframes I won't use anymore
rm(pa_ibge, pi_ibge, rn_ibge, sp_ibge, ac_ibge, ap_ibge, go_ibge, ms_ibge, 
   df1_ibge, pb_ibge, pr_ibge, ro_ibge, sc_ibge, al_ibge, ba_ibge, ma_ibge, 
   mt_ibge, to_ibge, es_ibge, pe_ibge, rj_ibge, rr_ibge, se_ibge, am_ibge, 
   ce_ibge, mg_ibge, rs_ibge)

# Join all these dataframes into one single thing
combined_df <- bind_rows(ac, al, am, ap, ba, ce, df1, es, go, ma, mg, ms, mt, 
                         pa, pb, pe, pi, pr, rj, rn, ro, rr, rs, sc, se, sp, to)

rm(ac, al, am, ap, ba, ce, df1, es, go, ma, mg, ms, mt, 
   pa, pb, pe, pi, pr, rj, rn, ro, rr, rs, sc, se, sp, to)

# Delete useless crap from final dataset
combined_df <- select(combined_df, -municipios_sem_acentos, -municipio.y, -state.y)

# Rename stuff to make it look clean
combined_df <- combined_df %>%
  rename(municipio = municipio.x)

combined_df <- combined_df %>%
  rename(state = state.x)

combined_df <- combined_df %>%
  rename(area_km2 = AREA_KM2)

########################## 
### Final Housekeeping ###
########################## 

### Write the damn dataframe on a damn Excel spreadsheet
write_xlsx(combined_df, path = "data/pessoal_ocupado_v.2.xlsx")

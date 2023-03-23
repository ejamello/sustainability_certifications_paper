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
# devtools::install_github("hersonpc/isgr")
library(isgr)
library(sf)
library(writexl)

### Read in data
df <- read_excel("data/Importações e ExportaçõesFINAL.xlsx")
fed_rates <- read_excel("data/fed_rates.xlsx")
sugar_prices <- read_excel("data/sugar_prices.xlsx")
municipios <- st_read("data/BR_Municipios_2021/BR_Municipios_2021.shp")


#### Rename variables to make them R-friendly
# "Cidade" into "municipio"
municipios <- municipios %>%
  rename(municipio = NM_MUN)

df <- df %>%
  rename(municipio = Cidade)

# Código do IBGE
municipios <- municipios %>%
  rename(ibge = CD_MUN)

# "SIGLA" into "state"
municipios <- municipios %>%
  rename(state = SIGLA)

# "Ano"
sugar_prices <- sugar_prices %>%
  rename(Ano = Year)

fed_rates <- fed_rates %>%
  rename(Ano = TIME)

# "Value" 
fed_rates <- fed_rates %>%
  rename(fed_rates = Value)

sugar_prices <- sugar_prices %>%
  rename(sugar_prices = Value)


### Creating new variables
# Create new variable "state"
df <- df %>% 
  mutate(state = factor(substring(municipio, nchar(municipio)-1, nchar(municipio))))

# Delete characters from "municipio" that represent state name 
df <- df %>% 
  mutate(municipio = str_sub(municipio, end = -6))

# Delete useless crap from imported datasets
fed_rates <- select(fed_rates, -LOCATION, -INDICATOR, -SUBJECT, -MEASURE, -FREQUENCY)

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


############################## 
### Add FED interest rates ###
############################## 

# Rates are Fed's short-term interest rates. 
# Source: https://data.oecd.org/interest/short-term-interest-rates.htm#indicator-chart

ac <- merge(ac, fed_rates, by = "Ano")
al <- merge(al, fed_rates, by = "Ano")
ap <- merge(ap, fed_rates, by = "Ano")
am <- merge(am, fed_rates, by = "Ano")
ba <- merge(ba, fed_rates, by = "Ano")
ce <- merge(ce, fed_rates, by = "Ano")
df1 <- merge(df1, fed_rates, by = "Ano")
es <- merge(es, fed_rates, by = "Ano")
go <- merge(go, fed_rates, by = "Ano")
ma <- merge(ma, fed_rates, by = "Ano")
mt <- merge(mt, fed_rates, by = "Ano")
ms <- merge(ms, fed_rates, by = "Ano")
mg <- merge(mg, fed_rates, by = "Ano")
pa <- merge(pa, fed_rates, by = "Ano")
pb <- merge(pb, fed_rates, by = "Ano")
pr <- merge(pr, fed_rates, by = "Ano")
pe <- merge(pe, fed_rates, by = "Ano")
pi <- merge(pi, fed_rates, by = "Ano")
rj <- merge(rj, fed_rates, by = "Ano")
rn <- merge(rn, fed_rates, by = "Ano")
rs <- merge(rs, fed_rates, by = "Ano")
ro <- merge(ro, fed_rates, by = "Ano")
rr <- merge(rr, fed_rates, by = "Ano")
sc <- merge(sc, fed_rates, by = "Ano")
sp <- merge(sp, fed_rates, by = "Ano")
se <- merge(se, fed_rates, by = "Ano")
to <- merge(to, fed_rates, by = "Ano")

######################## 
### Add sugar prices ###
######################## 

# Prices are sugar cane Producer Price Index for Brazil 
# Source: http://www.fao.org/faostat/en/#data

ac <- merge(ac, sugar_prices, by = "Ano")
al <- merge(al, sugar_prices, by = "Ano")
ap <- merge(ap, sugar_prices, by = "Ano")
am <- merge(am, sugar_prices, by = "Ano")
ba <- merge(ba, sugar_prices, by = "Ano")
ce <- merge(ce, sugar_prices, by = "Ano")
df1 <- merge(df1, sugar_prices, by = "Ano")
es <- merge(es, sugar_prices, by = "Ano")
go <- merge(go, sugar_prices, by = "Ano")
ma <- merge(ma, sugar_prices, by = "Ano")
mt <- merge(mt, sugar_prices, by = "Ano")
ms <- merge(ms, sugar_prices, by = "Ano")
mg <- merge(mg, sugar_prices, by = "Ano")
pa <- merge(pa, sugar_prices, by = "Ano")
pb <- merge(pb, sugar_prices, by = "Ano")
pr <- merge(pr, sugar_prices, by = "Ano")
pe <- merge(pe, sugar_prices, by = "Ano")
pi <- merge(pi, sugar_prices, by = "Ano")
rj <- merge(rj, sugar_prices, by = "Ano")
rn <- merge(rn, sugar_prices, by = "Ano")
rs <- merge(rs, sugar_prices, by = "Ano")
ro <- merge(ro, sugar_prices, by = "Ano")
rr <- merge(rr, sugar_prices, by = "Ano")
sc <- merge(sc, sugar_prices, by = "Ano")
sp <- merge(sp, sugar_prices, by = "Ano")
se <- merge(se, sugar_prices, by = "Ano")
to <- merge(to, sugar_prices, by = "Ano")


################################# 
### Create distance variables ###
################################# 


########################## 
### Final housekeeping ###
##########################

# Join all these dataframes into one single thing
combined_df <- bind_rows(ac, al, am, ap, ba, ce, df1, es, go, ma, mg, ms, mt, 
                         pa, pb, pe, pi, pr, rj, rn, ro, rr, rs, sc, se, sp, to)

# Delete useless crap from final dataset
combined_df <- select(combined_df, -municipios_sem_acentos, -municipio.y, -state.y, -geometry)

# Rename stuff to make it look clean
combined_df <- combined_df %>%
  rename(municipio = municipio.x)

combined_df <- combined_df %>%
  rename(state = state.x)

combined_df <- combined_df %>%
  rename(area_km2 = AREA_KM2)

### Write the damn dataframe on a damn Excel spreadsheet
write_xlsx(combined_df, path = "data/import_export_data.xlsx")



### End of File



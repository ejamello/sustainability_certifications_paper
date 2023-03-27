library(readxl)
library(ggplot2)
library(tidyverse)
library(table1)
install.packages("sf")
install.packages("geobr")
install.packages("gdal")


library(sf)
library(geobr)
library(rgdal)

install.packages("table1")

# Criar variável indicando tratamento

mydata$treatment <- factor(mydata$city_red)

mydata_med <- mydata %>% 
  group_by(ibge, year) %>% 
  summarise(mean_export = mean(export))

#sumarizando por municipio
mydata_med_total <- inner_join(mydata_med, mydata, multiple = "any", by= c("year", "ibge"))



#substituindo zero por 1 em exportação
mydata_med_total$mean_export <- ifelse(mydata_med_total$mean_export == 0, 1, mydata_med_total$mean_export)
summary(mydata_med_total$mean_export, useNA = "always")

#criando log de exportação
mydata_med_total$log_mean_export = log(mydata_med_total$mean_export)
summary(mydata_med_total$log_mean_export, useNA = "always")

# Criar plot de mpg "contra" tempo para os grupos de controle e tratamento 
ggplot(mydata_med_total, aes(x = year, y = log_mean_export, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Parallel Trends Plot",
       x = "Weight (1000 lbs)",
       y = "Miles per Gallon")

#Coletando em diferentes momento do tempo
#1)
mydata_med_total2 <- mydata_med_total %>% filter(year == "2009"|year=="2022")

#variável de tratamento usando a var city_red
mydata_med_total2$treated = ifelse(mydata_med_total2$city_red == "1", 1, 0)
table(mydata_med_total2$city_red, useNA = "always")
table(mydata_med_total2$treated, useNA = "always")

# variável de tempo supondo uma intervencao em 2011
mydata_med_total2$time = ifelse(mydata_med_total2$year >= 2011, 1, 0)
table(mydata_med_total2$time, useNA = "always")

#criando variável de did
mydata_med_total2$did = mydata_med_total2$time * mydata_med_total2$treated
table(mydata_med_total2$did, useNA = "always")


#modelo 1, sem efeito fixo de municipio
didreg1 = lm(log_mean_export ~ treated + time + did + log_gdp+ lagged_dummy_exports + dummy_import+ sugar_prices + selic  + fed_rate, data = mydata_med_total2)
summary(didreg1)             
             
#modelo 2, com efeito fixo de municipio
didreg2 = lm(log_mean_export ~ treated + time + did + log_gdp+ lagged_dummy_exports + dummy_import+ sugar_prices + selic  + fed_rate + factor(ibge), data = mydata_med_total2)
summary(didreg2)             

#modelo 3 - dummy exportação sem EF munic
didreg3 = lm(dummy_export ~ treated + time + did + log_gdp+ lagged_dummy_exports + dummy_import+ sugar_prices + selic  + fed_rate, data = mydata_med_total2)
summary(didreg3)             

#modelo 4 - dummy exportação com EF munic
didreg4 = lm(dummy_export ~ treated + time + did + log_gdp+ lagged_dummy_exports + dummy_import+ sugar_prices + selic  + fed_rate + factor(ibge), data = mydata_med_total2)
summary(didreg4)             


#2)
mydata_med_total3 <- mydata_med_total %>% filter(year == "2009"|year=="2015")

#variável de tratamento usando a var city_red
mydata_med_total3$treated = ifelse(mydata_med_total3$city_red == "1", 1, 0)
table(mydata_med_total3$city_red, useNA = "always")
table(mydata_med_total3$treated, useNA = "always")

table(mydata_med_total2$city_red, useNA = "always")
table(mydata_med_total2$treated, useNA = "always")

# variável de tempo supondo uma intervencao em 2011
mydata_med_total3$time = ifelse(mydata_med_total3$year >= 2011, 1, 0)
table(mydata_med_total3$time, useNA = "always")

#criando variável de did
mydata_med_total3$did = mydata_med_total3$time * mydata_med_total3$treated
table(mydata_med_total3$did, useNA = "always")


#modelo 1, sem efeito fixo de municipio
didreg2_1 = lm(log_mean_export ~ treated + time + did + log_gdp+ lagged_dummy_exports + dummy_import+ sugar_prices + selic  + fed_rate, data = mydata_med_total3)
summary(didreg2_1)             

#modelo 2, com efeito fixo de municipio
didreg2_2 = lm(log_mean_export ~ treated + time + did + log_gdp+ lagged_dummy_exports + dummy_import+ sugar_prices + selic  + fed_rate + factor(ibge), data = mydata_med_total3)
summary(didreg2_2)             

#modelo 3 - dummy exportação sem EF munic
didreg2_3 = lm(dummy_export ~ treated + time + did + log_gdp+ lagged_dummy_exports + dummy_import+ sugar_prices + selic  + fed_rate, data = mydata_med_total3)
summary(didreg2_3)             

#modelo 4 - dummy exportação com EF munic
didreg2_4 = lm(dummy_export ~ treated + time + did + log_gdp+ lagged_dummy_exports + dummy_import+ sugar_prices + selic  + fed_rate + factor(ibge), data = mydata_med_total3)
summary(didreg2_4)             

table1::table1(~city_red|year, data=mydata_med)


#modelo OLS cheio de ódio

reg1 = lm(log_mean_export ~  time_red + log_gdp+ lagged_dummy_exports + dummy_import+ sugar_prices + selic  + fed_rate + factor(ibge) + factor(year), data = mydata_med_total)
summary(reg1) 

#modelo OLS cheio de ódio

reg2 = lm(dummy_export ~  time_red + log_gdp+ lagged_dummy_exports + dummy_import+ sugar_prices + selic  + fed_rate + factor(ibge) + factor(year), data = mydata_med_total)
summary(reg2) 








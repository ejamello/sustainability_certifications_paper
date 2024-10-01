
# This code reproduces the results and figures in:
# "The trade effects of voluntary standards: Assessing
# Brazil’s exports to the EU along the sugar supply chain."

# Clear the workspace
rm(list = ls())

# Set a random seed for reproducibility
set.seed(123)

# Load necessary packages
library(tidyverse)
library(did)
library(hrbrthemes)
library(ggplot2)
library(panelView)
library(fect)
library(datazoom.amazonia)

# Load the data
data_final <- read.csv("data_final_bonsucro_all.csv")

# Load deforestation data
deforestation <- datazoom.amazonia::load_prodes(
  dataset = "deforestation",
  raw_data = FALSE,
  language = "pt"
) %>% 
  group_by(cod_municipio, ano) %>% 
  summarise(incremento = sum(incremento))

deforestation <- deforestation %>% 
  rename(CO_MUN = "cod_municipio",
         year = "ano")

deforestation <- deforestation %>% 
  mutate(year = as.integer(year))

# Prepare the data for analysis
data_final_eeu <- data_final %>%
  group_by(CO_MUN) %>%
  mutate(sum_export = sum(export_eu),
         sum_export_upto_2010 = sum(export_eu[year <= 2010]),
         sum_un = sum(prod_un)) %>%
  filter(!(sum_export <= 10 & bonsucro_treated == 0))

# Assign decile labels based on exports up to 2010
data_final_eeu_slice <- data_final_eeu %>%
  group_by(CO_MUN) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(sum_export_upto_2010) %>%
  mutate(decile_ntile = ntile(sum_export_upto_2010, 10)) %>%
  select(CO_MUN, decile_ntile)

# Join the decile information back to the main data
data_final_eeu <- left_join(data_final_eeu, data_final_eeu_slice)

# Define the treatment variable and filter for the relevant years
data_final_eeu <- data_final_eeu %>%
  mutate(treated = ifelse(year >= first_treatment_year, 1, 0)) %>%
  filter(year > 2009)

# Merging final dataset with deforestation data
data_final_eeu <- left_join(data_final_eeu, deforestation, by = c("CO_MUN", "year"))

data_final_eeu <- data_final_eeu %>% 
  mutate(incremento = ifelse(is.na(incremento),0,incremento))

# Visualize the treatment status over time
panelview(export_eu ~ treated, data = data_final_eeu, index = c("CO_MUN", "year"),
          axis.lab = "time", xlab = "Year", ylab = "Municipality", gridOff = FALSE,
          background = "lightgrey", main = "Treatment Status", display.all = TRUE)

# Split the data into upper and lower deciles for heterogeneous effects
upper <- data_final_eeu %>%
  filter(decile_ntile > 5) %>%
  mutate(export_eu = ifelse(export_eu == 0, 1, export_eu)) %>%
  mutate(ln_exp = log(export_eu))

lower <- data_final_eeu %>%
  filter(decile_ntile < 6) %>%
  mutate(export_eu = ifelse(export_eu == 0, 1, export_eu)) %>%
  mutate(ln_exp = log(export_eu))

# Estimate the average treatment effects using the difference-in-differences method
dr.overall <- att_gt(yname = "export_eu",
                     gname = "first_treatment_year",
                     idname = "CO_MUN",
                     tname = "year",
                     xformla = ~ 1,
                     biters = 1000,
                     control_group = "notyettreated",
                     data = data_final_eeu)

dr.lower <- att_gt(yname = "export_eu",
                   gname = "first_treatment_year",
                   idname = "CO_MUN",
                   tname = "year",
                   xformla = ~ 1,
                   biters = 1000,
                   control_group = "notyettreated",
                   data = lower)

dr.upper <- att_gt(yname = "export_eu",
                   gname = "first_treatment_year",
                   idname = "CO_MUN",
                   tname = "year",
                   xformla = ~ 1,
                   biters = 1000,
                   control_group = "notyettreated",
                   data = upper)

# Aggregate the treatment effects over time
dr.overall.eff <- aggte(dr.overall, type = "dynamic", na.rm = TRUE)
dr.lower.eff <- aggte(dr.lower, type = "dynamic", na.rm = TRUE)
dr.upper.eff <- aggte(dr.upper, type = "dynamic", na.rm = TRUE)

# Function to create plots for the models
create_att_plot <- function(model_eff, model_name, output_filename) {
  anos <- model_eff$egt
  att_egt <- model_eff$att.egt
  se <- model_eff$se.egt
  cv <- model_eff$crit.val.egt
  model_eff_df <- as_tibble(cbind(anos, att_egt, se, cv))
  
  # Create the plot
  p <- ggplot(data = model_eff_df, aes(x = anos, y = att_egt)) +
    geom_point(aes(color = ifelse(anos < 0, "Pre-Treatment", "Post-Treatment"))) +
    geom_errorbar(aes(ymin = att_egt - se * cv, ymax = att_egt + se * cv), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Years", y = "Average Treatment Effect on the Treated") +
    ggtitle(paste("", model_name)) +
    theme_ipsum(base_size = 12, base_family = "Arial", grid = "Y") +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)) +
    labs(caption = "Dependent variable: Sugarcane and ethanol exports to the European Union.\nControl group: not yet treated municipalities with non-certified sugar producers that export to the EU.") +
    scale_x_continuous(breaks = unique(model_eff_df$anos)) +
    scale_color_manual(values = c("Pre-Treatment" = "red", "Post-Treatment" = "black")) +
    labs(color = "Period") +
    guides(color = guide_legend(title = "Period"))
  
  # Save the plot
  ggsave(output_filename, plot = p, width = 8000, height = 6000, units = "px", dpi = 1000)
}

# Create and save plots for each model
create_att_plot(dr.overall.eff, "", "att_overall.jpeg")
create_att_plot(dr.lower.eff, "", "att_lower.jpeg")
create_att_plot(dr.upper.eff, "", "att_upper.jpeg")

# Plot to compare the effect across models
data_comparison_plot <- data.frame(
  Category = c('Overall', 'Upper deciles', 'Lower deciles'),
  ATT = c(1.2665, 3.4308, -1.3907),
  Lower_CI = c(0.2788, 1.1694, -2.1926),
  Upper_CI = c(2.2541, 5.6922, -0.5888)
)

jpeg("comparison.jpeg", units="px", width=8000, height=6000, res=1000)

ggplot(data_comparison_plot, aes(x=Category, y=ATT)) +
  geom_point() +
  geom_errorbar(aes(ymin=Lower_CI, ymax=Upper_CI), width=0.2) +
  labs(
    title='',
    x='Group',
    y='Average Treatment Effect on the Treated (ATT)'
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_ipsum(axis_title_size = 12)

dev.off()

# Alternative (FEct package) estimations
out.ife.exp.overall <- fect(export_eu ~ treated,
                            data = data_final_eeu, index = c("CO_MUN", "year"),
                            method = "fe", force = "two-way",
                            se = TRUE, nboots = 300, na.rm = FALSE)

out.ife.exp.lower <- fect(export_eu ~ treated,
                          data = lower, index = c("CO_MUN", "year"),
                          method = "fe", force = "two-way",
                          se = TRUE, nboots = 300, na.rm = FALSE)

out.ife.exp.upper <- fect(export_eu ~ treated,
                          data = upper, index = c("CO_MUN", "year"),
                          method = "fe", force = "two-way",
                          se = TRUE, nboots = 300, na.rm = FALSE)

# Plot and save the fixed effects results for the overall group
jpeg("out_ife_exp_overall.jpeg", width = 800, height = 600, units = "px")
plot(out.ife.exp.overall, ylab = "Effect on Exports to the EU (log) - Overall",
     xlab = "Year Since Treatment Started",
     type = "gap", show.points = FALSE, main = "")
dev.off()

# Plot and save the fixed effects results for the lower decile group
jpeg("out_ife_exp_lower.jpeg", width = 800, height = 600, units = "px")
plot(out.ife.exp.lower, ylab = "Effect on Exports to the EU (log) - Lower Decile",
     xlab = "Year Since Treatment Started",
     type = "gap", show.points = FALSE, main = "")
dev.off()

# Plot and save the fixed effects results for the upper decile group
jpeg("out_ife_exp_upper.jpeg", width = 800, height = 600, units = "px")
plot(out.ife.exp.upper, ylab = "Effect on Exports to the EU (log) - Upper Decile",
     xlab = "Year Since Treatment Started",
     type = "gap", show.points = FALSE, main = "")
dev.off()


### EFFECTS ON DEFORESTATION
dr.overall.deforestation <- att_gt(yname = "incremento",
                     gname = "first_treatment_year",
                     idname = "CO_MUN",
                     tname = "year",
                     xformla = ~ 1,
                     biters = 1000,
                     control_group = "notyettreated",
                     data = data_final_eeu)

dr.overall.deforestation.eff <- aggte(dr.overall.deforestation, type = "dynamic", na.rm = TRUE)

ggdid(dr.overall.deforestation.eff)

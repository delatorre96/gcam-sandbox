
library(dplyr)
library(tidyr)
library(stringr)

### inputs
shrw_input <- read.csv('csvs/inputs/A54.globaltranTech_shrwt_revised.csv')
costs_input <- read.csv('csvs/inputs/OTAQ_trn_data_EMF37.csv')
### outputs
output_output <- read.csv('csvs/outputs/output.csv')
costs_output <- read.csv('csvs/outputs/costs.csv')
shrw_output <- read.csv('csvs/outputs/sharewheights.csv')

################################# Join inputs  ################################# 

#-------------------------------------------------------
# 1. LIMPIAR SHRW
#-------------------------------------------------------
shrw_clean <- shrw_input %>%
  select(
    -supplysector, -sce,
    -X1971, -X2010,
    -final.calibration.year
  ) %>%
  mutate(
    Techno = tranTechnology,
    car_type = "Car"
  ) %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "shareweights"
  ) %>%
  mutate(
    Year = as.integer(str_replace(Year, "X", ""))
  ) %>%
  select(Techno, car_type, scenario, Year, shareweights)

# Añadir año 2020 desde initial.future.year
shrw_2020 <- shrw_input %>%
  transmute(
    Techno = tranTechnology,
    car_type = "Car",
    scenario,
    Year = 2020,
    shareweights = initial.future.year
  )

shrw_final <- bind_rows(shrw_clean, shrw_2020)
shrw_final$car_type <- NULL

#-------------------------------------------------------
# 2. LIMPIAR COSTS
#-------------------------------------------------------
costs_clean <- costs_input %>%
  select(
    -UCD_sector, -mode, -UCD_fuel, -unit,
    -X2005, -X2010, -X2015
  ) %>%
  rename(
    Region = UCD_region,
    Techno = UCD_technology
  ) %>%
  mutate(car_type = "Car") %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "value"
  ) %>%
  mutate(
    Year = as.integer(str_replace(Year, "X", ""))
  ) %>%
  # Expandir las 3 variables de cost a columnas separadas
  mutate(
    cost_purchase       = ifelse(variable == "Capital costs (purchase)",       value, NA),
    cost_other          = ifelse(variable == "Capital costs (other)",          value, NA),
    cost_infrastructure = ifelse(variable == "Capital costs (infrastructure)", value, NA)
  ) %>%
  select(
    Region, Techno, scenario, car_type, Year,
    cost_purchase, cost_other, cost_infrastructure
  ) %>%
  group_by(Region, Techno, scenario, car_type, Year) %>%
  summarise(across(starts_with("cost_"), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
costs_clean$car_type <- NULL
#-------------------------------------------------------
# 3. MERGE DIRECTO
#-------------------------------------------------------
input_df <- shrw_final %>%
  inner_join(
    costs_clean,
    by = c("scenario", "Techno", "Year")
  ) %>%
  transmute(
    Region,
    Years = Year,
    Techno,
    shareweights,
    `Capital costs (purchase)`       = cost_purchase,
    `Capital costs (other)`          = cost_other,
    `Capital costs (infrastructure)` = cost_infrastructure,
    scenario
  )


################################# Join outputs  ################################# 
# Función para limpiar y agrupar cada data frame
clean_and_aggregate <- function(df) {
  df %>%
    # Eliminar columnas innecesarias
    select(-Units, -X, -sector) %>%
    # Agrupar por scenario, region, technology, years (las columnas que empiezan con X)
    group_by(scenario, region, technology) %>%
    # Para cada año, hacer promedio de subsector (que es carácter, así que primero convertir a factor y luego promedio por niveles)
    # Pero como subsector es carácter, el promedio no tiene sentido directo, así que se puede:
    # — ignorar subsector (porque se unifica)
    # — o tomar el valor más frecuente (modo)
    # Aquí simplemente vamos a ignorar subsector y solo agrupar para evitar duplicados
    summarise(across(starts_with("X"), mean, na.rm = TRUE), .groups = "drop")
}

# Limpiar data frames
output_clean <- clean_and_aggregate(output_output)
costs_clean  <- clean_and_aggregate(costs_output)
shrw_clean   <- clean_and_aggregate(shrw_output)
to_long <- function(df, value_name) {
  df %>%
    pivot_longer(
      cols = starts_with("X"),
      names_to = "Year",
      names_prefix = "X",
      values_to = value_name,
      values_drop_na = TRUE
    ) %>%
    mutate(Year = as.integer(Year))
}

output_long <- to_long(output_clean, "output")
costs_long  <- to_long(costs_clean, "cost")
shrw_long   <- to_long(shrw_clean, "shareweights")

output_df <- output_long %>%
  inner_join(costs_long, by = c("scenario", "region", "technology", "Year")) %>%
  inner_join(shrw_long,  by = c("scenario", "region", "technology", "Year")) %>%
  select(Year, region, technology, cost, shareweights, output, scenario)

rm(list = setdiff(ls(), c("output_df", "input_df")))
write.csv(output_df, "output_df.csv", row.names = FALSE)
write.csv(input_df, "input_df.csv", row.names = FALSE)
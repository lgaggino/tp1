library(readxl)
library(dplyr)
library(tidyr)

data_tp <- read_excel("data_tp1.xlsx")

# Calcula totales anuales x país
totales <- data_tp %>%
  group_by(Year, ReporterName) %>%
  summarise(Total_TradeValue = sum(`TradeValue in 1000 USD`, na.rm = TRUE)) %>%
  ungroup()

data_totales <- merge(data_tp, totales, by = c("Year", "ReporterName"))

# Calcula la prop del v.c.
data_totales <- data_totales %>%
  mutate(prop = (`TradeValue in 1000 USD` / Total_TradeValue))

# Calcula proporción global
prop_all_countries <- data_totales %>%
  filter(ReporterName == "All countries  All --- All") %>%
  select(Year, ProductCode, prop) %>%
  rename(prop_1 = prop)

data_final <- merge(data_totales, prop_all_countries, by = c("Year", "ProductCode"), all.x = TRUE)

# Calcula RCA y RCA Normalizado
data_final <- data_final %>%
  mutate(RCA = prop / prop_1)

data_final <- data_final %>%
  mutate(RCA_Normalizado = (RCA - 1) / (RCA + 1))

# Ordenar datos y obtener top5
data_final <- data_final %>%
  group_by(Year, ProductCode, ReporterName) %>%
  summarise(RCA_Normalizado = mean(RCA_Normalizado, na.rm = TRUE), .groups = "drop")

productos_con_ventaja <- data_final %>%
  filter(ReporterName %in% c("Argentina", "Malaysia")) %>%
  pivot_wider(names_from = ReporterName, values_from = RCA_Normalizado) %>%
  filter(Argentina > Malaysia & Argentina > 0) %>%
  select(Year, ProductCode, Argentina, Malaysia)
write.xlsx(productos_con_ventaja, "productos_con_ventaja.xlsx")
top_5_productos <- productos_con_ventaja %>%
  group_by(Year) %>%
  arrange(desc(Argentina)) %>%
  slice_max(order_by = Argentina, n = 5, with_ties = FALSE) %>%
  ungroup()

print(top_5_productos)

#crear una tabla con top 5 productos
write.xlsx(top_5_productos, "top_5_productos.xlsx")

library(readxl)
library(dplyr)
library(openxlsx)

data_tp <- read_excel("data_tp1_2.xlsx")

# Calcular expo
expo_t <- data_tp %>%
  filter(ReporterName == "Argentina", TradeFlowName == "Export") %>%
  group_by(Year) %>%
  summarise(totales_argentina = sum(`TradeValue in 1000 USD`, na.rm = TRUE))

# Calcular impo
impo_t <- data_tp %>%
  filter(ReporterName == "Malaysia", TradeFlowName == "Import") %>%
  group_by(Year) %>%
  summarise(totales_malasia = sum(`TradeValue in 1000 USD`, na.rm = TRUE))

# Unir los datos
data_tp <- data_tp %>%
  left_join(expo_t, by = "Year") %>%
  left_join(impo_t, by = "Year")

data_tp <- data_tp %>%
  mutate(totales = case_when(
    ReporterName == "Argentina" & TradeFlowName == "Export" ~ totales_argentina,
    ReporterName == "Malaysia" & TradeFlowName == "Import" ~ totales_malasia,
    TRUE ~ NA_real_
  ))

data_tp <- data_tp %>%
  mutate(ratio = case_when(
    ReporterName == "Argentina" & TradeFlowName == "Export" ~ `TradeValue in 1000 USD` / totales,
    ReporterName == "Malaysia" & TradeFlowName == "Import" ~ `TradeValue in 1000 USD` / totales,
    TRUE ~ NA_real_
  ))

# Filtrar los ratios para M-Impo A-Expo
malasia_ratios <- data_tp %>%
  filter(ReporterName == "Malaysia" & TradeFlowName == "Import") %>%
  arrange(Year)

argentina_ratios <- data_tp %>%
  filter(ReporterName == "Argentina" & TradeFlowName == "Export") %>%
  arrange(Year)

combined_ratios <- malasia_ratios %>%
  inner_join(argentina_ratios, by = c("Year", "ProductCode"), suffix = c("_malasia", "_argentina"))

# Calcular el ICC
icc <- combined_ratios %>%
  group_by(Year) %>%
  summarise(
    ICC = 100 * (1 - sum(abs(ratio_malasia - ratio_argentina) / 2, na.rm = TRUE))
  )

print(icc)
write.xlsx(icc, "icc.xlsx")

# Kütüphaneleri yükle
library(ggplot2)
library(dplyr)

# CSV dosyasını oku
data <- read.csv(choose.files(), fileEncoding = "UTF-8")

# 2023 ve World filtresi
data_2023 <- subset(data, Year == 2023 & Entity == "World")

# Üretim türleri ve değerleri
types <- c("Ruzgar", "Hidro", "Gunes", "Diger")
values <- c(
  data_2023$Electricity.from.wind...TWh,
  data_2023$Electricity.from.hydro...TWh,
  data_2023$Electricity.from.solar...TWh,
  data_2023$Other.renewables.including.bioenergy...TWh
)

# Yüzde hesapla ve etiketleri oluştur
total <- sum(values)
percentages <- round(100 * values / total, 1)
labels <- paste0(types, " (", percentages, "%)")

# Data frame oluştur
production <- data.frame(
  Type = types,
  Value = values,
  Label = labels
)

# Pasta grafiği
ggplot(production, aes(x = "", y = Value, fill = Label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "2023 Yilinda Yenilenebilir Elektrik Uretim Turleri") +
  theme_void() +
  theme(legend.title = element_blank())


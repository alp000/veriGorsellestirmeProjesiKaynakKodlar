library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# CSV dosyasını oku
df <- read_csv(choose.files())

# Dünya verisini filtrele ve toplam elektrik üretimini hesapla
dunya_verisi <- df %>% 
  filter(Entity == "World", Year >= 1990, Year <= 2023) %>%
  mutate(Total = `Electricity from wind - TWh` + 
           `Electricity from hydro - TWh` + 
           `Electricity from solar - TWh` + 
           `Other renewables including bioenergy - TWh`) %>%
  select(Year, Total)

# Türkiye verisini filtrele ve toplam elektrik üretimini hesapla
turkiye_verisi <- df %>% 
  filter(Entity == "Turkey", Year >= 1990, Year <= 2023) %>%
  mutate(Total = `Electricity from wind - TWh` + 
           `Electricity from hydro - TWh` + 
           `Electricity from solar - TWh` + 
           `Other renewables including bioenergy - TWh`) %>%
  select(Year, Total)

# Yılları 5'er 5'er arttırarak verileri filtrele
dunya_verisi <- dunya_verisi %>% filter(Year %% 5 == 0)
turkiye_verisi <- turkiye_verisi %>% filter(Year %% 5 == 0)

# Dünya ve Türkiye verilerini birleştir
combined_data <- bind_rows(
  dunya_verisi %>% mutate(Entity = "Dunya"),
  turkiye_verisi %>% mutate(Entity = "Turkiye")
)

# Grafik oluştur
ggplot(combined_data, aes(x = Year, y = Total, color = Entity)) +
  geom_line(size = 1.2) +
  labs(title = "Dunya ve Turkiye Genelinde Toplam\nYenilenebilir Elektrik Uretimi (TWh)",
       x = "Yil",
       y = "Uretim (TWh)",
       color = "Ulke") +
  theme_minimal()



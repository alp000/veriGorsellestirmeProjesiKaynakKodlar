# Gerekli kütüphaneleri yükle
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# CSV dosyasını oku
df <- read_csv(file.choose())

# Sadece Dünya geneli ve 1990-2023 arası veriyi filtrele
world_data <- df %>% 
  filter(Entity == "World", Year >= 1995, Year <= 2023) %>%
  select(Year, 
         Ruzgar = `Electricity from wind - TWh`, 
         Hidro = `Electricity from hydro - TWh`, 
         Gunes = `Electricity from solar - TWh`, 
         Diger = `Other renewables including bioenergy - TWh`)

# Veriyi uzun formata çevir
world_long <- world_data %>%
  pivot_longer(cols = -Year, names_to = "Source", values_to = "TWh")

# Grafik oluştur
ggplot(world_long, aes(x = Year, y = TWh, color = Source)) +
  geom_line(size = 1.2) +
  labs(title = "Dunya Genelinde Uretim Kaynagina Gore\nYenilenebilir Elektrik Uretimi (1995–2023)",
       x = "Yil",
       y = "Uretim (TWh)",
       color = "Enerji Kaynagi") +
  theme_minimal()



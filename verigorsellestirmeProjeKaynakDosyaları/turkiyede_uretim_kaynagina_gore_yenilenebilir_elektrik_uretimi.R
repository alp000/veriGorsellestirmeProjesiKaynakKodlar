
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# CSV dosyasını oku
df <- read_csv(file.choose())

# Sadece Türkiye verisini ve 1995 sonrası yılları filtrele
turkey_data <- df %>% 
  filter(Entity == "Turkey", Year >= 2000) %>%
  select(Year, 
         Ruzgar = `Electricity from wind - TWh`, 
         Hidro = `Electricity from hydro - TWh`, 
         Gunes = `Electricity from solar - TWh`, 
         Diger = `Other renewables including bioenergy - TWh`)

# Veriyi uzun formata çevir
turkey_long <- turkey_data %>%
  pivot_longer(cols = -Year, names_to = "Source", values_to = "TWh")

# Kutu grafiği oluştur
ggplot(turkey_long, aes(x = Source, y = TWh, fill = Source)) +
  geom_boxplot() +
  labs(title = "Turkiye'de Uretim Kaynagina Gore\nYenilenebilir Elektrik Uretimi (TWh)",
       x = "Enerji Kaynagi",
       y = "Uretim (TWh)") +
  theme_minimal() +
  theme(legend.position = "none")

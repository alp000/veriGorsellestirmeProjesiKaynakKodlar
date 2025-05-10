# Gerekli kutuphaneler
library(dplyr)
library(ggplot2)
library(readr)

# Dosyayi sec
df <- read_csv(file.choose())

# Yil ve hidro enerji filtresi
hydro_df <- df %>%
  filter(Year >= 1995, Year <= 2023) %>%
  select(Entity, Year, `Electricity from hydro - TWh`)

# Gereksiz gruplari filtrele
exclude_keywords <- c("World", "OECD", "G20", "Asia", "Africa", "Europe", "America", 
                      "income", "countries", "region", "Ember", "EI", "total", "Middle East", "Pacific")

hydro_df_clean <- hydro_df %>%
  filter(!grepl(paste(exclude_keywords, collapse = "|"), Entity, ignore.case = TRUE))

# Belirli ulkeler
selected_countries <- c("Brazil", "Canada", "China", "Russia", "Turkey", "United States")

# Secilen ulkelerin verisi
plot_data <- hydro_df_clean %>%
  filter(Entity %in% selected_countries)

# Ulke isimlerini Turkce karakter olmadan cevir
country_names_simple <- c(
  "Brazil" = "Brezilya",
  "Canada" = "Kanada",
  "China" = "Cin",
  "Russia" = "Rusya",
  "Turkey" = "Turkiye",
  "United States" = "Amerika"
)

plot_data <- plot_data %>%
  mutate(Ulke = recode(Entity, !!!country_names_simple))

# Grafik
ggplot(plot_data, aes(x = Year, y = `Electricity from hydro - TWh`, color = Ulke)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c(
    "Brezilya" = "#e41a1c",      # Kırmızı
    "Kanada" = "#377eb8",      # Mavi
    "Cin" = "#4daf4a",       # Yeşil
    "Rusya" = "#ff7f00",      # Turuncu
    "Turkiye" = "#000000",      # Siyah
    "Amerika" = "#ff99cc" # Pembe
  )) +
  labs(title = "1995 - 2023 Yillari Arasinda Hidroelektrik Enerjisinden\nEn Cok Elektrik Ureten Ulkeler (TWh)",
       x = "Yil", y = "Elektrik Uretimi (TWh)", color = "Ulke") +
  theme_minimal()


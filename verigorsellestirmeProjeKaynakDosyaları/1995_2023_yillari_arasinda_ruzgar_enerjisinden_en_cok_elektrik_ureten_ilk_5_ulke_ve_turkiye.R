
# Gerekli kutuphaneler
library(dplyr)
library(ggplot2)
library(readr)

# Dosyayi sec
df <- read_csv(file.choose())

# Yil ve ruzgar enerjisi filtresi
wind_df <- df %>%
  filter(Year >= 1995, Year <= 2023) %>%
  select(Entity, Year, `Electricity from wind - TWh`)

# Gereksiz gruplari filtrele
exclude_keywords <- c("World", "OECD", "G20", "Asia", "Africa", "Europe", "America", 
                      "income", "countries", "region", "Ember", "EI", "total", "Middle East", "Pacific")

wind_df_clean <- wind_df %>%
  filter(!grepl(paste(exclude_keywords, collapse = "|"), Entity, ignore.case = TRUE))

# İlgili ülkeler ve Türkiye'yi seçme
selected_countries <- c("China", "Germany", "India", "Spain", "United States", "Turkey")

# Secilen ulkelerin verisi
plot_data <- wind_df_clean %>%
  filter(Entity %in% selected_countries)

# Ulke isimlerini Türkçe'ye çevirme
country_names_simple <- c(
  "China" = "Cin",
  "Germany" = "Almanya",
  "India" = "Hindistan",
  "Spain" = "İspanya",
  "United States" = "Amerika Birleşik Devletleri",
  "Turkey" = "Turkiye"
)

plot_data <- plot_data %>%
  mutate(Ulke = recode(Entity, !!!country_names_simple))

# Grafik
ggplot(plot_data, aes(x = Year, y = `Electricity from wind - TWh`, color = Ulke)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c(
    "Cin" = "#e41a1c",          # Kırmızı
    "Almanya" = "#377eb8",      # Mavi
    "Hindistan" = "#4daf4a",    # Yeşil
    "İspanya" = "#ff7f00",      # Turuncu
    "Amerika Birleşik Devletleri" ="#ff99cc", # Pembe
    "Turkiye" = "#000000"       # Siyah (Türkiye)
  )) +
  labs(title = "1995 - 2023 Yillari Arasinda Ruzgar Enerjisinden En Cok\nElektrik Ureten 5 Ulke ve Turkiye (TWh)",
       x = "Yil", y = "Elektrik Uretimi (TWh)", color = "Ulke") +
  theme_minimal()


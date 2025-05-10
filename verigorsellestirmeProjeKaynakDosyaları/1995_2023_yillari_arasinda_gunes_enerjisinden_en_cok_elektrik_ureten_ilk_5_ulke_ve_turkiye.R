# gunes enerjisinden elektrik uretimi ilk 5 ve tr
# Gerekli kutuphaneler
library(dplyr)
library(ggplot2)
library(readr)

# Dosyayi sec
df <- read_csv(file.choose())

# Yil ve gunes enerjisi filtresi
solar_df <- df %>%
  filter(Year >= 1995, Year <= 2023) %>%
  select(Entity, Year, `Electricity from solar - TWh`)

# Gereksiz gruplari filtrele
exclude_keywords <- c("World", "OECD", "G20", "Asia", "Africa", "Europe", "America", 
                      "income", "countries", "region", "Ember", "EI", "total", "Middle East", "Pacific")

solar_df_clean <- solar_df %>%
  filter(!grepl(paste(exclude_keywords, collapse = "|"), Entity, ignore.case = TRUE))

# En cok ureten 5 ulke
top_countries <- solar_df_clean %>%
  group_by(Entity) %>%
  summarise(Total_Solar = sum(`Electricity from solar - TWh`, na.rm = TRUE)) %>%
  arrange(desc(Total_Solar)) %>%
  slice_head(n = 5)

# Turkiye'yi ekle
if (!"Turkey" %in% top_countries$Entity) {
  turkey_data <- solar_df_clean %>%
    filter(Entity == "Turkey") %>%
    summarise(Total_Solar = sum(`Electricity from solar - TWh`, na.rm = TRUE)) %>%
    mutate(Entity = "Turkey")
  
  top_countries <- bind_rows(top_countries, turkey_data)
}

# Secilen ulkelerin verisi
selected_countries <- top_countries$Entity
plot_data <- solar_df_clean %>%
  filter(Entity %in% selected_countries)

# Ulke isimlerini Turkce karakter olmadan cevir
country_names_simple <- c(
  "China" = "Cin",
  "Germany" = "Almanya",
  "India" = "Hindistan",
  "Japan" = "Japonya",
  "United States" = "Amerika Birlesik Devletleri",
  "Turkey" = "Turkiye"
)

plot_data <- plot_data %>%
  mutate(Ulke = recode(Entity, !!!country_names_simple))

# Grafik
ggplot(plot_data, aes(x = Year, y = `Electricity from solar - TWh`, color = Ulke)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c(
    "Cin" = "#e41a1c",
    "Almanya" = "#377eb8",
    "Hindistan" = "#4daf4a",
    "Japonya" = "#114ea3",
    "Amerika Birlesik Devletleri" = "#ff7f00",
    "Turkiye" = "#000000"
  )) +
  labs(title = "1995 - 2023 Yillari Arasinda Gunes Enerjisinden En Cok\nElektrik Ureten Ilk 5 Ulke ve Turkiye (TWh)",
       x = "Yil", y = "Elektrik Uretimi (TWh)", color = "Ulke") +
  theme_minimal()


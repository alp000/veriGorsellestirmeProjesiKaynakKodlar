# Gerekli kütüphaneler
library(ggplot2)
library(dplyr)
library(readr)
library(ggrepel)

# Veriyi oku
df <- read_csv(choose.files())

# Hariç tutulacak bölge isimleri
exclude_list <- c(
  'Africa', 'Asia', 'Europe', 'European Union (27)', 'High-income countries',
  'North America', 'Central America', 'South America', 'Oceania',
  'Low-income countries', 'Upper-middle-income countries',
  'Lower-middle-income countries', 'World'
)

# Ülke isimlerini Türkçeye çeviren sözlük
turkish_names <- c(
  "United States" = "ABD",
  "Indonesia" = "Endonezya",
  "Philippines" = "Filipinler",
  "Turkey" = "Turkiye",
  "New Zealand" = "Yeni Zelanda",
  "Mexico" = "Meksika",
  "Italy" = "İtalya",
  "Iceland" = "İzlanda",
  "Kenya" = "Kenya",
  "Japan" = "Japonya",
  "Germany" = "Almanya",
  "El Salvador" = "El Salvador"
)

# Sadece ülke bazlı veriler
country_df <- df %>% 
  filter(!(Entity %in% exclude_list))

# En son yıl
latest_year <- max(country_df$Year, na.rm = TRUE)

# Son yıldaki veriler
latest_data <- country_df %>% 
  filter(Year == latest_year)

# En çok üreten 5 ülke
top5 <- latest_data %>%
  arrange(desc(`Geothermal capacity (total)`)) %>%
  head(5)

# Türkiye ilk 5'te mi?
turkey_in_top5 <- "Turkey" %in% top5$Entity

# Grafik için ülke listesini oluştur
if (turkey_in_top5) {
  selected_entities <- top5$Entity
  title <- "En Cok Jeotermal Enerji Ureten Ilk 5 Ulke (Turkiye dahil)"
} else {
  selected_entities <- c(top5$Entity, "Turkey")
  title <- "En Cok Jeotermal Enerji Ureten Ilk 5 Ulke ve Türkiye"
}

# Seçilen ülkelerin verisini al ve Türkçe isimleri ekle
plot_data <- country_df %>%
  filter(Entity %in% selected_entities) %>% 
  mutate(Entity_tr = turkish_names[Entity])

# Etiketler için son yıl verisi
label_data <- plot_data %>%
  group_by(Entity_tr) %>%
  filter(Year == max(Year, na.rm = TRUE))

# Çizgi grafiği + Türkçe ülke isimleri
ggplot(plot_data, aes(x = Year, y = `Geothermal capacity (total)`, color = Entity_tr)) +
  geom_line(size = 1.2) +
  geom_text_repel(data = label_data,
                  aes(label = Entity_tr),
                  hjust = 0,
                  nudge_x = 2,
                  direction = "y",
                  segment.color = "grey50",
                  show.legend = FALSE) +
  scale_color_manual(values = c(
    "Turkiye" = "#000000",   # Türkiye siyah
    "ABD" = "#e41a1c",       # Kırmızı
    "Endonezya" = "#377eb8", # Mavi
    "Filipinler" = "#4daf4a", # Yeşil
    "Yeni Zelanda" = "#ff7f00", # Turuncu
    "Meksika" = "#ff99cc",    # Pembe
    "İtalya" = "#ff4500",    # Koyu turuncu
    "İzlanda" = "#00bfff",    # Açık mavi
    "Kenya" = "#8a2be2",     # Mor
    "Japonya" = "#f08080",   # Soluk kırmızı
    "Almanya" = "#a52a2a",    # Kahverengi
    "El Salvador" = "#00fa9a"  # Açık yeşil
  )) +
  labs(
    title = title,
    x = "Yil",
    y = "Kurulu Jeotermal Kapasite (MW)",
    color = "Ulke"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  xlim(min(plot_data$Year), max(plot_data$Year) + 5)


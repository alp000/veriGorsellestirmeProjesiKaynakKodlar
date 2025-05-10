library(tidyverse)

# Veri yükle
data <- read.csv(file.choose())

# 2000 ve sonrasi veriler
filtered_data <- data %>%
  filter(Year >= 2000)

# OECD ulkeleri listesi
oecd_ulkeleri <- c(
  "Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Costa Rica", "Czechia",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland",
  "Ireland", "Israel", "Italy", "Japan", "South Korea", "Latvia", "Lithuania", "Luxembourg",
  "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic",
  "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States"
)

# En fazla ureten ilk 5 OECD ulkesini al
top5_oecd <- filtered_data %>%
  filter(Entity %in% oecd_ulkeleri) %>%
  group_by(Entity) %>%
  summarise(toplam_uretim = sum(Biofuels.production...TWh, na.rm = TRUE)) %>%
  arrange(desc(toplam_uretim)) %>%
  slice_head(n = 5) %>%
  pull(Entity)

# Turkce isim eslestirmeleri
turkce_isimler <- c(
  "United States" = "Amerika",
  "Germany" = "Almanya",
  "France" = "Fransa",
  "Canada" = "Kanada",
  "United Kingdom" = "Birlesik Krallik",
  "Italy" = "Italya",
  "Spain" = "Ispanya",
  "Netherlands" = "Hollanda"
)

# Grafik verisi
grafik_verisi <- filtered_data %>%
  filter(Entity %in% top5_oecd) %>%
  mutate(Ulke = recode(Entity, !!!turkce_isimler))

# Belirgin renk paleti
ozel_renkler <- c(
  "Amerika" = "#1b9e77",
  "Almanya" = "#d95f02",
  "Fransa" = "#7570b3",
  "Kanada" = "#e7298a",
  "Birlesik Krallik" = "#66a61e",
  "Italya" = "#e6ab02",
  "Ispanya" = "#a6761d",
  "Hollanda" = "#666666"
)

# Grafik
ggplot(grafik_verisi, aes(x = Year, y = Biofuels.production...TWh, color = Ulke)) +
  geom_line(size = 1) +
  geom_point(size = 0) +
  scale_color_manual(values = ozel_renkler) +
  scale_x_continuous(breaks = seq(2000, 2025, by = 5), expand = expansion(mult = c(0.01, 0.1))) +
  labs(
    title = "2000–2023 Arasi En Fazla Biyoyakit Ureten 5 OECD Ulkesi",
    x = "Yil",
    y = "Biyoyakit Uretimi (TWh)",
    color = "Ulke"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )



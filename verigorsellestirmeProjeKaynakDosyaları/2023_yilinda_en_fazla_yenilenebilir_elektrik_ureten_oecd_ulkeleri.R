library(dplyr)
library(ggplot2)
library(readr)

# Veriyi oku
df <- read_csv(choose.files())

# 2023 yılına filtrele
df_2023 <- df %>%
  filter(Year == 2023)

# Toplam yenilenebilir üretimi hesapla
df_2023 <- df_2023 %>%
  mutate(Total_Renewable = `Hydro generation - TWh` +
           `Wind generation - TWh` +
           `Solar generation - TWh` +
           `Other renewables (including geothermal and biomass) electricity generation - TWh`)

# OECD ülkeleri listesi
oecd_countries <- c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Czech Republic", 
                    "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", 
                    "Ireland", "Israel", "Italy", "Japan", "Korea", "Latvia", "Lithuania", "Luxembourg", 
                    "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", 
                    "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States")

# Sadece OECD ülkeleri
df_2023_oecd <- df_2023 %>%
  filter(Entity %in% oecd_countries)

# En çok üreten ilk 10 ülke
top10 <- df_2023_oecd %>%
  arrange(desc(Total_Renewable)) %>%
  slice_head(n = 10)

# Türkiye yoksa ekle
if (!"Turkey" %in% top10$Entity) {
  turkey_row <- df_2023_oecd %>% filter(Entity == "Turkey")
  top10 <- bind_rows(top10, turkey_row)
}

# Ülke adlarini Turkce cevir
translate <- c(
  "United States" = "Amerika",
  "Canada" = "Kanada",
  "Germany" = "Almanya",
  "Japan" = "Japonya",
  "Norway" = "Norvec",
  "Spain" = "Ispanya",
  "France" = "Fransa",
  "Turkey" = "Turkiye",
  "United Kingdom" = "Birlesik Krallik",
  "Sweden" = "Isvec"
)

top10 <- top10 %>%
  mutate(Entity_tr = ifelse(Entity %in% names(translate), translate[Entity], Entity),
         renk = ifelse(Entity == "Turkey", "Turkiye", "Diger"))

# Grafik icin faktor siralamasini ayarla
top10$Entity_tr <- factor(top10$Entity_tr, levels = top10$Entity_tr[order(top10$Total_Renewable)])

# Grafik
ggplot(top10, aes(x = Entity_tr, y = Total_Renewable, fill = renk)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Turkiye" = "orange", "Diger" = "steelblue")) +
  labs(
    title = "2023 Yilinda En Fazla Yenilenebilir Elektrik Ureten\nOECD Ulkeleri",
    x = "Ulke",
    y = "Toplam Yenilenebilir Elektrik Uretimi (TWh)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")




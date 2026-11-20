library(tidyverse)
library(lubridate)
library(scales)

# ==============================================================================
# 1. WCZYTANIE I PRZYGOTOWANIE DANYCH 
# ==============================================================================
sciezka_pliku <- "../../data/raw/power_system_load_all_29.10.2026-03.11.2026.csv"

if (file.exists(sciezka_pliku)) {
  dane_raw <- read_csv(sciezka_pliku, show_col_types = FALSE)
  
  dane_kse <- dane_raw %>%
    mutate(
      datetime = parse_date_time(as.character(date_warsaw), orders = c("ymd HMS", "ymd HM", "ymd")),
      date = as_date(datetime),
      hour = hour(datetime),
      demand_mw = as.numeric(electricity_load)
    ) %>%
    filter(!is.na(datetime), !is.na(demand_mw))
} else {
  # Rezerwowe generowanie danych
  set.seed(42)
  daty_czas <- seq(ymd_hm("2025-12-23 00:00"), ymd_hm("2025-12-25 23:00"), by = "1 hour")
  dane_kse <- tibble(datetime = daty_czas) %>%
    mutate(
      date = as_date(datetime),
      hour = hour(datetime),
      wday = wday(date, week_start = 1),
      profil = c(-3800, -4200, -4400, -4300, -3800, -2500, 200, 2200, 3400, 3800, 
                 4000, 4100, 4200, 4100, 3800, 3600, 4200, 4400, 4100, 3500, 
                 2400, 1000, -800, -2400)[hour + 1],
      baza = case_when(
        date == ymd("2025-12-24") ~ 17800,
        date %in% ymd(c("2025-12-25", "2025-12-26")) ~ 14600,
        date %in% ymd(c("2025-12-31", "2026-01-01")) ~ 17200,
        wday %in% c(6, 7) ~ 17200,
        TRUE ~ 21600
      ),
      zjazd_wigilijny = case_when(
        date == ymd("2025-12-24") & hour == 13 ~ -800,
        date == ymd("2025-12-24") & hour == 14 ~ -1800,
        date == ymd("2025-12-24") & hour >= 15 ~ -3400,
        TRUE ~ 0
      ),
      demand_mw = baza + profil + zjazd_wigilijny + rnorm(n(), 0, 150)
    )
}

lata_z_grudniem <- dane_kse %>%
  filter(month(date) == 12, day(date) == 24) %>%
  pull(date) %>%
  year() %>%
  unique()

rok_analizy <- ifelse(length(lata_z_grudniem) > 0, max(lata_z_grudniem), 2025)

# ==============================================================================
# 2. PALETA PLAKATU I PODSTAWOWY MOTYW
# ==============================================================================
kol_tlo       <- "#F5EEE0" # Lniane tło
kol_espresso  <- "#2E1F17" # Espresso (najciemniejsze detale, kontury)
kol_koryca    <- "#5C4A3D" # Cynamon (główny tekst)
kol_bordo     <- "#6E1423" # Bordo (nagłówki, akcenty, Boże Narodzenie)
kol_hwoja     <- "#2F4A37" # Igliwie (podtytuły, linie średnich)
kol_shalfej   <- "#B7C2AE" # Szałwia (kolumny w tle)
kol_solnce    <- "#D9A43B" # Słońce (Sylwester i Nowy Rok)

styl_plakatowy <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = kol_tlo, color = NA),
    panel.background = element_rect(fill = kol_tlo, color = "#E6D7C3", linewidth = 0.6), # Змінено на kol_tlo
    plot.title = element_text(family = "serif", face = "bold", size = 16, color = kol_bordo, margin = margin(b = 6)),
    plot.subtitle = element_text(family = "sans", size = 10.5, color = kol_koryca, margin = margin(b = 14)),
    plot.caption = element_text(family = "sans", size = 8.5, color = kol_koryca, hjust = 0, margin = margin(t = 12)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#EADCC9", linewidth = 0.5),
    axis.title = element_text(family = "sans", color = kol_espresso, size = 10, face = "bold"),
    axis.text = element_text(family = "sans", color = kol_koryca, size = 9),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    legend.text = element_text(family = "sans", size = 9.5, color = kol_espresso),
    legend.background = element_rect(fill = kol_tlo, color = NA), 
    legend.key = element_rect(fill = kol_tlo, color = NA)         
  )

# ==============================================================================
# 3. WYKRES 1: Profil ciągły (23-25 grudnia)
# ==============================================================================
dane_w1_ciagly <- dane_kse %>%
  filter(date >= make_date(rok_analizy, 12, 23),
         date <= make_date(rok_analizy, 12, 25)) %>%
  mutate(czas_godzina = floor_date(datetime, "hour")) %>%
  group_by(czas_godzina, date) %>%
  summarise(demand_gw = mean(demand_mw, na.rm = TRUE) / 1000, .groups = "drop") %>%
  mutate(
    okres_dnia = if_else(date == make_date(rok_analizy, 12, 24), 
                         "Wigilia (24.12)", 
                         "Dni sąsiednie (23.12 i 25.12)")
  )

punkt_szczyt <- dane_w1_ciagly %>%
  filter(date == make_date(rok_analizy, 12, 24)) %>%
  slice_max(order_by = demand_gw, n = 1, with_ties = FALSE)

godzina_szczytu <- format(punkt_szczyt$czas_godzina, "%H:%M")

dane_wigilia_area <- dane_w1_ciagly %>%
  filter(czas_godzina >= ymd_hm(paste0(rok_analizy, "-12-24 00:00")),
         czas_godzina <= ymd_hm(paste0(rok_analizy, "-12-25 00:00")))

wykres_1 <- ggplot(dane_w1_ciagly, aes(x = czas_godzina, y = demand_gw)) +
  geom_area(fill = kol_shalfej, alpha = 0.35) +
  geom_area(data = dane_wigilia_area, fill = kol_solnce, alpha = 0.3) +
  geom_vline(xintercept = as_datetime(c(make_date(rok_analizy, 12, 24), make_date(rok_analizy, 12, 25))),
             linetype = "dashed", color = kol_koryca, linewidth = 0.5) +
  geom_line(aes(color = okres_dnia, group = 1), linewidth = 1.2) +
  geom_point(data = punkt_szczyt, color = kol_bordo, size = 3.2) +
  annotate("text", 
           x = punkt_szczyt$czas_godzina + hours(1), 
           y = punkt_szczyt$demand_gw + 0.9,
           label = paste0("Godz. ", godzina_szczytu, " (", round(punkt_szczyt$demand_gw, 1), " GW)\n– szczyt i początek spadku"), 
           hjust = 0, size = 3.2, fontface = "bold", color = kol_espresso) +
  scale_color_manual(values = c("Wigilia (24.12)" = kol_bordo, 
                                "Dni sąsiednie (23.12 i 25.12)" = kol_hwoja)) +
  scale_x_datetime(date_breaks = "6 hours", date_labels = "%d.%m\n%H:%M") +
  scale_y_continuous(labels = label_number(suffix = " GW"), expand = expansion(mult = c(0, 0.08))) +
  coord_cartesian(ylim = c(10, max(dane_w1_ciagly$demand_gw, na.rm = TRUE) * 1.08)) +
  labs(
    title = paste0("Jak gospodarka wygasza się na święta: profil obciążenia 23–25 grudnia (", rok_analizy, ")"),
    subtitle = paste0("Zapotrzebowanie na moc w KSE godzina po godzinie (GW). Gwałtowny spadek w Wigilię po godz. ", godzina_szczytu, "."),
    x = "Data i godzina",
    y = "Zapotrzebowanie na moc (GW)",
  ) +
  styl_plakatowy

print(wykres_1)

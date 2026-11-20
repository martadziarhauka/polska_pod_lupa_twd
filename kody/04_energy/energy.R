library(tidyverse)
library(lubridate)
library(scales)

# wspólne rozmiary czcionek; działa z katalogu projektu i z kody/04_energy
source(if (file.exists("kody/paleta.R")) "kody/paleta.R" else "../paleta.R")

# ==============================================================================
# 1. WCZYTANIE I PRZYGOTOWANIE DANYCH 
# ==============================================================================
plik_kse <- "power_system_load_all_29.10.2026-03.11.2026.csv"
# działa zarówno z katalogu projektu, jak i z kody/04_energy
sciezka_pliku <- if (file.exists(file.path("data/raw", plik_kse))) {
  file.path("data/raw", plik_kse)
} else {
  file.path("../../data/raw", plik_kse)
}

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
# kolory ze wspólnej palety plakatu (kody/paleta.R) – tak samo jak na innych wykresach
kol_espresso  <- ESPRESSO   # najciemniejszy tekst
kol_koryca    <- CYNAMON    # osie, podpisy, linia przerywana
kol_bordo     <- WIGILIA    # Wigilia
kol_hwoja     <- "#315442"  # igliwie – dni sąsiednie (jak na wykresie zatrzymań)
kol_shalfej   <- INNE       # szałwia – pole pod krzywą
kol_solnce    <- NOWY_ROK   # złoto – pole Wigilii

# styl jak w weather.R / retail.R: przezroczyste tło, tytuł Helvetica w kolorze CZERWON
styl_plakatowy <- theme_minimal() +
  theme(
    plot.background    = element_rect(fill = "transparent", colour = NA),
    panel.background   = element_rect(fill = "transparent", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = SIATKA),
    
    plot.title    = element_text(family = "Helvetica", face = "bold",
                                 colour = CZERWON, hjust = 0, lineheight = 0.9,
                                 margin = margin(b = 6, unit = "pt")),
    plot.title.position = "plot",
    plot.subtitle = element_text(colour = CZERWON),
    plot.caption  = element_text(colour = CYNAMON, hjust = 0),
    axis.text     = element_text(colour = CYNAMON),
    axis.title    = element_text(colour = CYNAMON),
    
    legend.position      = "top",
    legend.justification = "left",
    legend.title         = element_blank(),
    legend.text          = element_text(colour = CYNAMON),
    legend.background    = element_rect(fill = "transparent", colour = NA),
    legend.key           = element_rect(fill = "transparent", colour = NA)
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
    title = paste0("Jak gospodarka wygasza się na święta:\nprofil obciążenia 23–25 grudnia (", rok_analizy, ")"),
    subtitle = paste0("Zapotrzebowanie na moc w KSE godzina po godzinie (GW).\nGwałtowny spadek w Wigilię po godz. ", godzina_szczytu, "."),
    x = "Data i godzina",
    y = "Zapotrzebowanie na moc (GW)",
  ) +
  styl_plakatowy +
  motyw_plakatu   # wspólne rozmiary czcionek (kody/paleta.R)

print(wykres_1)

folder_output <- if (dir.exists("output")) "output" else "../../output"
ggsave(file.path(folder_output, "energia_p.png"), plot = wykres_1,
       width = ROZMIAR_ENERGIA["w"], height = ROZMIAR_ENERGIA["h"], units = "cm",
       dpi = 300, bg = "transparent")

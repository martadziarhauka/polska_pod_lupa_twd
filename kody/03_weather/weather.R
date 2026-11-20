# =============================================================================
# Wykres: „Czy święta są jeszcze białe?”
# Odsetek stacji IMGW z pokrywą śnieżną (min. 1 cm) w noc wigilijną i noworoczną
# (pomiar rano 25.12 i 1.01), sezony 1996/97–2025/26.
# Dane: data/raw/swieta_snieg.csv – przygotowane skryptem download_weather.R
# Źródło: IMGW-PIB, dane synoptyczne dobowe
#https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/synop/
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

source("kody/paleta.R")


# --- 1. Wczytanie danych -----------------------------------------------------

swieta <- read.csv("data/raw/swieta_snieg.csv", fileEncoding = "UTF-8")

# stacje wysokogórskie – śnieg leży tam prawie zawsze i zawyżałby wynik
gory <- c(349190650,   # Kasprowy Wierch
          350150510,   # Śnieżka
          349200628)   # Hala Gąsienicowa


# --- 2. Przygotowanie danych -------------------------------------------------

# bez gór i bez braków pomiaru; „biała” stacja = co najmniej 1 cm śniegu
swieta <- swieta %>%
  filter(!kod %in% gory,
         !is.na(snieg_cm)) %>%
  mutate(biala = snieg_cm >= 1)

# odsetek białych stacji w każdym sezonie, osobno dla Wigilii i Nowego Roku
sezony <- swieta %>%
  group_by(sezon, dzien) %>%
  summarise(stacje = n(),
            biale  = sum(biala),
            proc   = round(biale / stacje * 100),
            .groups = "drop")

write.csv(sezony, "data/clean/swieta_sezony.csv", row.names = FALSE)

# jeden wiersz na sezon (Wigilia i Nowy Rok obok siebie) – do kresek gantelek
gantele <- sezony %>%
  select(sezon, dzien, proc) %>%
  pivot_wider(names_from = dzien, values_from = proc)

# średnia z obu dni dla każdej dekady; od/do – początek i koniec linii przerywanej
# (±0,45, żeby linia obejmowała skrajne gantelki, ale nie stykała się z sąsiednią dekadą)
dekady <- sezony %>%
  mutate(dekada = case_when(sezon <= 2005 ~ "1996–2005",
                            sezon <= 2015 ~ "2006–2015",
                            TRUE          ~ "2016–2025")) %>%
  group_by(dekada) %>%
  summarise(od      = min(sezon) - 0.45,
            do      = max(sezon) + 0.45,
            srednia = mean(proc),
            .groups = "drop")


# --- 3. Wykres ---------------------------------------------------------------

p <- ggplot() +
  # granice dekad i średnie dla dekad
  geom_vline(xintercept = c(2005.5, 2015.5), colour = LINIA, linewidth = 0.4) +
  geom_segment(data = dekady,
               aes(x = od, xend = do, y = srednia, yend = srednia),
               colour = LINIA, linewidth = 1, linetype = "dashed") +
  geom_text(data = dekady,
            aes(x = (od + do) / 2, y = 100,
                label = paste0("średnio ", round(srednia), "%")),
            colour = ESPRESSO, size = 4.5, fontface = "bold", vjust = -0.6) +

  # gantelki: kreska łączy Wigilię i Nowy Rok tego samego sezonu
  geom_segment(data = gantele,
               aes(x = sezon, xend = sezon, y = Wigilia, yend = `Nowy Rok`),
               colour = CYNAMON, linewidth = 0.6) +
  geom_point(data = sezony,
             aes(x = sezon, y = proc, fill = dzien),
             shape = 21, size = 3.5, colour = ESPRESSO, stroke = 0.4) +

  # skale
  scale_fill_manual(values = c("Wigilia" = WIGILIA, "Nowy Rok" = NOWY_ROK),
                    breaks = c("Wigilia", "Nowy Rok"),
                    labels = c("Wigilia (pomiar rano 25.12)", "Nowy Rok (pomiar rano 1.01)"),
                    name = NULL) +
  scale_x_continuous(breaks = 1996:2025,
                     labels = sprintf("%02d", 1996:2025 %% 100),   # 96, 97, …, 25
                     expand = expansion(add = 0.2)) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 10),
                     labels = function(x) paste0(x, "%")) +

  # opisy
  labs(title    = "Czy święta są jeszcze białe?",
       subtitle = "Odsetek stacji IMGW z pokrywą śnieżną (min. 1 cm) w noc wigilijną i noworoczną\n(pomiar rano 25.12 i 1.01). Kreska łączy oba dni tych samych świąt. Przerywana linia – średnia dla 10 lat.",
       x = "Rok (Wigilia danego roku i Nowy Rok następnego)",
       y = "Stacje ze śniegiem, %") +

  # wygląd
  theme_minimal() +
  theme(
    plot.background    = element_rect(fill = "transparent", colour = NA),
    panel.background   = element_rect(fill = "transparent", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = SIATKA),

    plot.title    = element_text(family = "Helvetica", face = "bold",
                                 size = 34.2, colour = CZERWON,
                                 hjust = 0, lineheight = 0.9,
                                 margin = margin(b = 6, unit = "pt")),
    plot.title.position = "plot",
    plot.subtitle = element_text(colour = CZERWON),
    axis.text     = element_text(colour = CYNAMON),
    axis.text.x   = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title    = element_text(colour = CYNAMON),

    legend.position      = "top",
    legend.justification = "left",
    legend.text          = element_text(colour = CYNAMON)
  ) +
  motyw_plakatu   # wspólne rozmiary czcionek (kody/paleta.R)


# --- 4. Zapis ----------------------------------------------------------------

dir.create("output", showWarnings = FALSE)
ggsave("output/pogoda_p.png", plot = p,
       width = ROZMIAR_POGODA["w"], height = ROZMIAR_POGODA["h"], units = "cm",
       dpi = 300, bg = "transparent")

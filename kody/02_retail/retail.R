# =============================================================================
# Wykres: „Na co wydajemy więcej przed świętami?”
# Zmiana sprzedaży detalicznej względem poprzedniego miesiąca, 2021–2025,
# w czterech kategoriach towarów; grudzień wyróżniony kolorem.
# Źródło: GUS, Bank Danych Lokalnych (indeksy sprzedaży, poprzedni miesiąc = 100)
#https://bdl.stat.gov.pl/bdl/dane/podgrup/tablica
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

source("kody/paleta.R")


# --- 1. Wczytanie danych -----------------------------------------------------

retail <- read.csv("data/raw/retail_raw.csv",
                   sep = ";",
                   fileEncoding = "UTF-8")

# cztery kategorie = cztery panele wykresu
kategorie <- c("newspapers, books, other sale in specialized stores",
               "food, beverages and tobacco products",
               "furniture, radio, TV and household appliances",
               "solid, liquid and gaseous fuels")


# --- 2. Przygotowanie danych -------------------------------------------------

# zostawiamy pełne lata 2021–2025 (bez 2020 i niepełnego 2026),
# potrzebne kolumny i kategorie; Value = 0 to dane utajnione przez GUS
retail <- retail %>%
  filter(Year %in% 2021:2025) %>%
  select(Name, Months, Activity.types, Year, Value) %>%
  filter(Activity.types %in% kategorie) %>%
  filter(Value != 0)

# dane są dla 16 województw – dla każdego miesiąca bierzemy medianę województw
cala_polska <- retail %>%
  group_by(Activity.types, Year, Months) %>%
  summarise(Value = median(Value), .groups = "drop")

# dla każdego miesiąca: mediana z lat 2021–2025 oraz min i max (rozrzut między latami);
# odejmujemy 100, żeby indeks zamienić na zmianę w %
wynik <- cala_polska %>%
  group_by(Activity.types, Months) %>%
  summarise(mediana = median(Value) - 100,
            min     = min(Value) - 100,
            max     = max(Value) - 100,
            .groups = "drop")

# polskie nazwy kategorii, kolejność paneli i miesięcy, wyróżnienie grudnia
wynik <- wynik %>%
  mutate(
    Months = factor(Months, levels = month.name),
    Activity.types = recode(Activity.types,
      "newspapers, books, other sale in specialized stores" = "Książki, prasa i prezenty",
      "food, beverages and tobacco products"                = "Żywność i napoje",
      "furniture, radio, TV and household appliances"       = "Meble, RTV, AGD",
      "solid, liquid and gaseous fuels"                     = "Paliwa"),
    Activity.types = factor(Activity.types,
      levels = c("Książki, prasa i prezenty", "Żywność i napoje",
                 "Meble, RTV, AGD", "Paliwa")),
    wyroznienie = ifelse(Months == "December", "Grudzień", "Pozostałe miesiące")
  )

# pionowe rozdzielacze między kategoriami: w każdym panelu oprócz ostatniego,
# na pustym 13. „miesiącu” (factor z tymi samymi poziomami – inaczej panele
# ułożą się alfabetycznie)
rozdzielacze <- data.frame(
  Activity.types = factor(levels(wynik$Activity.types),
                          levels = levels(wynik$Activity.types))
)


# --- 3. Wykres ---------------------------------------------------------------

p <- ggplot(wynik, aes(x = Months, y = mediana, fill = wyroznienie)) +
  # słupki – mediana zmian, kreski – rozrzut między latami
  geom_col(width = 0.7) +
  geom_linerange(aes(ymin = min, ymax = max), colour = CYNAMON, linewidth = 0.4) +
  geom_hline(yintercept = 0, colour = ZERO) +
  geom_vline(data = rozdzielacze, aes(xintercept = 13),
             colour = LINIA, linewidth = 0.4) +
  facet_wrap(~ Activity.types, nrow = 2) +

  # skale
  scale_fill_manual(values = c("Grudzień" = WIGILIA, "Pozostałe miesiące" = INNE),
                    breaks = c("Grudzień", "Pozostałe miesiące"),
                    name = NULL) +
  scale_x_discrete(limits = c(month.name, "przerwa"),   # 13. pusty „miesiąc” na rozdzielacz
                   labels = c("I", "", "", "IV", "", "", "VII", "", "", "X", "", "XII", "")) +

  # opisy
  labs(title    = "Na co wydajemy więcej przed świętami?",
       subtitle = "Zmiana sprzedaży względem poprzedniego miesiąca (słupek – mediana, kreska – min–max), 2021–2025",
       x = "Miesiąc",
       y = "Zmiana sprzedaży, %") +

  # wygląd (rozmiar na plakacie: ROZMIAR_ZAKUPY w kody/paleta.R)
  theme_minimal(base_size = 9) +
  theme(
    plot.background    = element_rect(fill = "transparent", colour = NA),
    panel.background   = element_rect(fill = "transparent", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = SIATKA),

    plot.title    = element_text(family = "Helvetica", face = "bold",
                                 size = 18, colour = CZERWON,
                                 hjust = 0, lineheight = 0.9,
                                 margin = margin(b = 4, unit = "pt")),
    plot.title.position = "plot",
    plot.subtitle = element_text(colour = CZERWON, size = 8),
    strip.text    = element_text(colour = ESPRESSO, size = 9, face = "bold"),
    axis.text     = element_text(colour = CYNAMON, size = 7),
    axis.title    = element_text(colour = CYNAMON),
    legend.text   = element_text(colour = CYNAMON),

    legend.position      = "top",
    legend.justification = "left"
  ) +
  motyw_plakatu   # wspólne rozmiary czcionek (kody/paleta.R)


# --- 4. Zapis ----------------------------------------------------------------

dir.create("output", showWarnings = FALSE)
ggsave("output/wykres_zakupy_p.png", plot = p,
       width = ROZMIAR_ZAKUPY["w"], height = ROZMIAR_ZAKUPY["h"], units = "cm", dpi = 300, bg = "transparent")

# =============================================================================
# Wspólna paleta kolorów wykresów plakatu „Zimowe święta pod lupą”
# Wczytywana w skryptach wykresów:  source("kody/paleta.R")
# =============================================================================

# --- kolory świąt (te same znaczenia na wszystkich wykresach) ---
WIGILIA  <- "#7A1E2B"   # bordo – Wigilia / grudzień
NOWY_ROK <- "#D9A43B"   # złoto – Nowy Rok
INNE     <- "#C5D0C0"   # szałwia – pozostałe dni / miesiące

# --- teksty ---
CZERWON  <- "#A8323E"   # tytuł i podtytuł
CYNAMON  <- "#3B2A1E"     # osie, podpisy, kreski min–max, gantelki
ESPRESSO <- "#2E1F17"   # najciemniejszy tekst, obrys punktów, nazwy paneli

# --- linie pomocnicze ---
LINIA    <- "#B3A48D"   # rozdzielacze (dekady, kategorie) i linie średnich
SIATKA   <- "#E6DDCB"   # siatka pozioma
ZERO     <- "#4D4D4D"   # linia y = 0

# --- tło plakatu (wykresy zapisujemy z przezroczystym tłem) ---
KARTA    <- "#F5EEE0"

# =============================================================================
# Wspólne rozmiary czcionek (w punktach) – takie same na wszystkich wykresach.
# Działa tylko wtedy, gdy:
#   1) wykres zapisujemy ggsave(..., units = "cm") DOKŁADNIE w rozmiarze z Canvy,
#   2) w Canvie nie skalujemy obrazka (Position → Width = ta sama liczba cm).
# Użycie: dodać  + motyw_plakatu  NA KOŃCU wykresu (po własnym theme()).
# =============================================================================

ROZMIAR_TYTUL     <- 18
ROZMIAR_PODTYTUL  <- 10
ROZMIAR_OSIE_TYT  <- 10
ROZMIAR_OSIE_TEKST <- 9
ROZMIAR_LEGENDA   <- 10
ROZMIAR_PANEL     <- 10   # nazwy paneli (facet)
ROZMIAR_PODPIS    <- 8    # caption

motyw_plakatu <- ggplot2::theme(
  plot.title    = ggplot2::element_text(size = ROZMIAR_TYTUL, face = "bold"),
  plot.subtitle = ggplot2::element_text(size = ROZMIAR_PODTYTUL),
  plot.caption  = ggplot2::element_text(size = ROZMIAR_PODPIS),
  axis.title    = ggplot2::element_text(size = ROZMIAR_OSIE_TYT),
  axis.text     = ggplot2::element_text(size = ROZMIAR_OSIE_TEKST),
  axis.text.x   = ggplot2::element_text(size = ROZMIAR_OSIE_TEKST),
  axis.text.y   = ggplot2::element_text(size = ROZMIAR_OSIE_TEKST),
  legend.text   = ggplot2::element_text(size = ROZMIAR_LEGENDA),
  strip.text    = ggplot2::element_text(size = ROZMIAR_PANEL)
)

# rozmiary miejsc na wykresy w Canvie (cm)
ROZMIAR_ZATRZYMANIA <- c(w = 18.01, h = 13.93)
ROZMIAR_POGODA      <- c(w = 21.48, h = 11.01)
ROZMIAR_ZAKUPY      <- c(w = 22.11, h = 14.20)
ROZMIAR_ENERGIA     <- c(w = 18.04, h = 13.24)

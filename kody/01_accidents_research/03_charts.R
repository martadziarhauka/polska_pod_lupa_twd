library(dplyr)
library(ggplot2)

source("kody/paleta.R")   # motyw_plakatu i ROZMIAR_ZATRZYMANIA

# Paleta plakatu — natywne kolory ggplot, bez rastrowego tła.
KARTA <- "#F5EEE0"
BORDO <- "#7A1E2B"
ZLOTO <- "#D9A43B"
SZALWIA <- "#C5D0C0"
IGLIWIE <- "#315442"
TYTUL <- "#A8323E"
TEKST <- "#5C4A3D"
SIATKA <- "#E6DDCB"
SEZONY <- c("2022" = BORDO, "2023" = ZLOTO,
            "2024" = SZALWIA, "2025" = IGLIWIE)

df <- read.csv("data/clean/accidents.csv")
df$date <- as.Date(df$date)

df <- df %>%
  arrange(date) %>%
  mutate(
    day_and_month = format(date, "%m-%d"),
    year = as.integer(format(date, "%Y")),
    season = year - as.integer(format(date, "%m") == "01"),
    plot_date = as.Date(paste0(
      if_else(format(date, "%m") == "01", "2001-", "2000-"),
      day_and_month
    ))
  )

table <- df %>%
  filter(day_and_month >= "12-15" | day_and_month <= "01-02",
         season >= 2022) %>%
  mutate(year = as.character(season)) %>%
  group_by(plot_date) %>%
  mutate(
    # Nie pokazujemy niepełnych sum, jeśli brakuje pomiaru w którymś sezonie.
    total_arrested = if (anyNA(wanted_arrested)) NA_real_ else sum(wanted_arrested),
    share = if_else(!is.na(total_arrested) & total_arrested > 0,
                    wanted_arrested / total_arrested, NA_real_)
  ) %>%
  ungroup() %>%
  arrange(year, plot_date)

year_labels <- setNames(
  paste0(sort(unique(table$season)), "/", sprintf("%02d", (sort(unique(table$season)) + 1) %% 100)),
  as.character(sort(unique(table$season)))
)

plot_table <- table %>%
  filter(!is.na(total_arrested))

p <- ggplot(plot_table, aes(x = plot_date, y = wanted_arrested, fill = year, group = year)) +
  annotate("rect", xmin = as.Date("2000-12-24") - 0.5,
           xmax = as.Date("2000-12-24") + 0.5, ymin = -Inf, ymax = Inf,
           fill = ZLOTO, alpha = 0.16) +
  annotate("rect", xmin = as.Date("2000-12-25") - 0.5,
           xmax = as.Date("2000-12-26") + 0.5, ymin = -Inf, ymax = Inf,
           fill = SZALWIA, alpha = 0.35) +
  annotate("rect", xmin = as.Date("2001-01-01") - 0.5,
           xmax = as.Date("2001-01-01") + 0.5, ymin = -Inf, ymax = Inf,
           fill = ZLOTO, alpha = 0.16) +
  annotate("rect", xmin = as.Date("2000-12-31") - 0.5,
           xmax = as.Date("2000-12-31") + 0.5, ymin = -Inf, ymax = Inf,
           fill = SZALWIA, alpha = 0.35) +
  geom_col(width = 0.88, colour = KARTA, linewidth = 0.4, na.rm = TRUE) +
  geom_text(aes(label = scales::number(wanted_arrested, accuracy = 1, big.mark = " "),
                colour = if_else(year %in% c("2022", "2025"), KARTA, "#2E1F17")),
            position = position_stack(vjust = 0.5), size = 3, show.legend = FALSE) +
  scale_colour_identity() +
  annotate("text", x = as.Date("2000-12-24"),
           y = max(plot_table$total_arrested) * 1.07,
           label = "Wigilia", fontface = "bold", colour = BORDO, size = 3.5) +
  annotate("text", x = as.Date("2000-12-25") + 0.5,
           y = max(plot_table$total_arrested) * 1.07,
           label = "Boże\nNarodzenie", fontface = "bold", colour = BORDO,
           size = 3.5, lineheight = 0.95) +
  annotate("text", x = as.Date("2001-01-01"),
           y = max(plot_table$total_arrested) * 1.07,
           label = "Nowy\nRok", fontface = "bold", colour = BORDO, size = 3.5) +
  annotate("text", x = as.Date("2000-12-31"),
           y = max(plot_table$total_arrested) * 1.07,
           label = "Sylwester", fontface = "bold", colour = BORDO, size = 3.5) +
  scale_x_date(
    limits = as.Date(c("2000-12-15", "2001-01-02")) + c(-0.5, 0.5),
    breaks = seq(as.Date("2000-12-15"), as.Date("2001-01-02"), by = "day"),
    date_labels = "%d.%m", expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = " ", accuracy = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  scale_fill_manual(values = SEZONY, labels = year_labels, name = NULL) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title = "W święta poszukiwani rzadziej wpadają",
    subtitle = "Zatrzymania osób poszukiwanych · 15 grudnia – 2 stycznia",
    x = NULL, y = "Liczba osób (suma 4 sezonów)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = KARTA, colour = NA),
    panel.background = element_rect(fill = KARTA, colour = NA),
    legend.background = element_rect(fill = KARTA, colour = NA),
    legend.key = element_rect(fill = KARTA, colour = NA),
    text = element_text(colour = TEKST),
    axis.text = element_text(colour = TEKST),
    axis.title = element_text(colour = TEKST),
    plot.subtitle = element_text(colour = TYTUL),
    panel.grid.major.y = element_line(colour = SIATKA, linewidth = 0.4),
    panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 11),
    legend.key.size = grid::unit(0.4, "cm"),
    legend.spacing.x = grid::unit(0.25, "cm"),
    plot.title = element_text(face = "bold", colour = TYTUL, size = 34.2),
    plot.caption = element_text(hjust = 0)
  ) +
  motyw_plakatu   # wspólne rozmiary czcionek (kody/paleta.R)

p

# Eksport zachowuje ten sam kremowy kolor co plakat.
dir.create("output", showWarnings = FALSE)
ggsave("output/zatrzymani_swieta.png", p,
       width = ROZMIAR_ZATRZYMANIA["w"], height = ROZMIAR_ZATRZYMANIA["h"],
       units = "cm", dpi = 300, bg = KARTA)

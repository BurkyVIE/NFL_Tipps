require(tidyverse)

RamsSeason <- tribble(~Season, ~Part, ~City, ~Win, ~Loss, ~Tie,
                      2025, "reg", "la", 2, 0, 0,
                      2024, "post", "la", 1, 1, 0,
                      2024, "reg", "la", 10, 7, 0,
                      2023, "post", "la", 0, 1, 0,
                      2023, "reg", "la", 10, 7, 0,
                      2022, "reg", "la", 5, 12, 0,
                      2021, "post", "la", 4, 0, 0,
                      2021, "reg", "la", 12, 5, 0,
                      2020, "post", "la", 1, 1, 0,
                      2020, "reg", "la", 10, 6, 0,
                      2019, "reg", "la", 9, 7, 0,
                      2018, "post", "la", 2, 1, 0,
                      2018, "reg", "la", 13, 3, 0,
                      2017, "post", "la", 0, 1, 0,
                      2017, "reg", "la", 11, 5, 0,
                      2016, "reg", "la", 4, 12, 0,
                      2015, "reg", "stl", 7, 9, 0,
                      2014, "reg", "stl", 6, 10, 0,
                      2013, "reg", "stl", 7, 9, 0,
                      2012, "reg", "stl", 7, 8, 1,
                      2011, "reg", "stl", 2, 14, 0,
                      2010, "reg", "stl", 7, 9, 0,
                      2009, "reg", "stl", 1, 15, 0,
                      2008, "reg", "stl", 2, 14, 0,
                      2007, "reg", "stl", 3, 13, 0,
                      2006, "reg", "stl", 8, 8, 0,
                      2005, "reg", "stl", 6, 10, 0,
                      2004, "post", "stl", 1, 1, 0,
                      2004, "reg", "stl", 8, 8, 0,
                      2003, "post", "stl", 0, 1, 0,
                      2003, "reg", "stl", 12, 4, 0,
                      2002, "reg", "stl", 7, 9, 0,
                      2001, "post", "stl", 2, 1, 0,
                      2001, "reg", "stl", 14, 2, 0,
                      2000, "post", "stl", 0, 1, 0,
                      2000, "reg", "stl", 10, 6, 0,
                      1999, "post", "stl", 3, 0, 0,
                      1999, "reg", "stl", 13, 3, 0,
                      1998, "reg", "stl", 4, 12, 0,
                      1997, "reg", "stl", 5, 11, 0,
                      1996, "reg", "stl", 6, 10, 0,
                      1995, "reg", "stl", 7, 9, 0,
                      1994, "reg", "la", 4, 12, 0,
                      1993, "reg", "la", 5, 11, 0,
                      1992, "reg", "la", 6, 10, 0,
                      1991, "reg", "la", 3, 13, 0,
                      1990, "reg", "la", 5, 11, 0,
                      1989, "post", "la", 2, 1, 0,
                      1989, "reg", "la", 11, 5, 0,
                      1988, "post", "la", 0, 1, 0,
                      1988, "reg", "la", 10, 6, 0,
                      1987, "reg", "la", 6, 9, 0,
                      1986, "post", "la", 0, 1, 0,
                      1986, "reg", "la", 10, 6, 0,
                      1985, "post", "la", 1, 1, 0,
                      1985, "reg", "la", 11, 5, 0,
                      1984, "post", "la", 0, 1, 0,
                      1984, "reg", "la", 10, 6, 0,
                      1983, "post", "la", 1, 1, 0,
                      1983, "reg", "la", 9, 7, 0,
                      1982, "reg", "la", 2, 7, 0,
                      1981, "reg", "la", 6, 10, 0,
                      1980, "post", "la", 0, 1, 0,
                      1980, "reg", "la", 11, 5, 0,
                      1979, "post", "la", 2, 1, 0,
                      1979, "reg", "la", 9, 7, 0,
                      1978, "post", "la", 1, 1, 0,
                      1978, "reg", "la", 12, 4, 0,
                      1977, "post", "la", 0, 1, 0,
                      1977, "reg", "la", 10, 4, 0,
                      1976, "post", "la", 1, 1, 0,
                      1976, "reg", "la", 10, 3, 1,
                      1975, "post", "la", 1, 1, 0,
                      1975, "reg", "la", 12, 2, 0,
                      1974, "post", "la", 1, 1, 0,
                      1974, "reg", "la", 10, 4, 0,
                      1973, "post", "la", 0, 1, 0,
                      1973, "reg", "la", 12, 2, 0,
                      1972, "reg", "la", 6, 7, 1,
                      1971, "reg", "la", 8, 5, 1,
                      1970, "reg", "la", 9, 4, 1,
                      1969, "post", "la", 0, 1, 0,
                      1969, "reg", "la", 11, 3, 0,
                      1968, "reg", "la", 10, 3, 1,
                      1967, "post", "la", 0, 1, 0,
                      1967, "reg", "la", 11, 1, 2,
                      1966, "reg", "la", 8, 6, 0,
                      1965, "reg", "la", 4, 10, 0,
                      1964, "reg", "la", 5, 7, 2,
                      1963, "reg", "la", 5, 9, 0,
                      1962, "reg", "la", 1, 12, 1,
                      1961, "reg", "la", 4, 10, 0,
                      1960, "reg", "la", 4, 7, 1,
                      1959, "reg", "la", 2, 10, 0,
                      1958, "reg", "la", 8, 4, 0,
                      1957, "reg", "la", 6, 6, 0,
                      1956, "reg", "la", 4, 8, 0,
                      1955, "post", "la", 0, 1, 0,
                      1955, "reg", "la", 8, 3, 1,
                      1954, "reg", "la", 6, 5, 1,
                      1953, "reg", "la", 8, 3, 1,
                      1952, "post", "la", 0, 1, 0,
                      1952, "reg", "la", 9, 3, 0,
                      1951, "post", "la", 1, 0, 0,
                      1951, "reg", "la", 8, 4, 0,
                      1950, "post", "la", 1, 1, 0,
                      1950, "reg", "la", 9, 3, 0,
                      1949, "post", "la", 0, 1, 0,
                      1949, "reg", "la", 8, 2, 2,
                      1948, "reg", "la", 6, 5, 1,
                      1947, "reg", "la", 6, 6, 0,
                      1946, "reg", "la", 6, 4, 1,
                      1945, "post", "cle", 1, 0, 0,
                      1945, "reg", "cle", 9, 1, 0,
                      1944, "reg", "cle", 4, 6, 0,
                      1943, "reg", "cle", NA, NA, NA,
                      1942, "reg", "cle", 5, 6, 0,
                      1941, "reg", "cle", 2, 9, 0,
                      1940, "reg", "cle", 4, 6, 1,
                      1939, "reg", "cle", 5, 5, 1,
                      1938, "reg", "cle", 4, 7, 0,
                      1937, "reg", "cle", 1, 10, 0
                   #  1936, "reg", "cle", 5, 2, 2, # not NFL!
                      )

# Datentypen und Berechnungen; Berücksichtige Qualifikation für Playoffs ohne (bisherige) Spielergebnisse
RamsSeason <- RamsSeason |> 
  mutate_if(is.numeric, as.integer) |> 
  mutate(Part = factor(Part, levels = c("reg", "post")),
         City = factor(City, levels = c("cle", "la", "stl"), labels = c("Cleveland", "Los Angeles", "St. Louis")),
         Pct = ((Win + 1/2 * Tie) / (Win + Loss + Tie)) %>% round(3))
RamsSeason$Pct[is.nan(RamsSeason$Pct)] <- 0 # i.e. qualified for POST but not yet played (0 divided by 0)

# Eine Zeile pro Season; Keine ties in den Playoffs
RamsSeason <- RamsSeason |>
  pivot_wider(names_from = Part, values_from = Win:Pct, names_vary = "slowest", names_sep = ".")
RamsSeason <- select(RamsSeason, -Tie.post) |> 
  arrange(Season)

# Füge Perioden ein
RamsSeason <- RamsSeason |> mutate(Periode = case_when(
  City == "Cleveland" ~ "CLE",
  City == "Los Angeles" & Season < 2016 ~ "LA1",
  City == "St. Louis" ~ "STL",
  City == "Los Angeles" & Season >= 2016 ~ "LA2"))

# Erreichen Postseason; Gewinn der Championship/Superbowl
RamsSeason <- RamsSeason |> 
  mutate(Playoff = case_when(Pct.post == 1 ~ "Champion",
                             !is.na(Pct.post) ~ "Yes",
                             TRUE ~ "No") |> 
           factor(levels = c("No", "Yes", "Champion")))

# City-Hintergrund; Mittelwertlinien (gewichtet)
background <- RamsSeason  |> 
  filter(!(is.na(Pct.reg))) |> 
  group_by(Periode) |> 
  summarise(City = first(City),
            Mean = (sum(Win.reg) + sum(Tie.reg) * 1/2) / sum(Win.reg + Loss.reg + Tie.reg),
            from = min(Season),
            to = max(Season),
            .groups = "drop")

# Größte Anderungen
change <- RamsSeason |> 
  transmute(Time = Season + 1/2,
            bef = Pct.reg,
            aft = lead(Pct.reg),
            change = aft - bef,
            show = change == max(change, na.rm = TRUE) | change == min(change, na.rm = TRUE))

# Plot
p <- ggplot() +
  geom_rect(data = background, mapping = aes(xmin = from - 1/2, xmax = to + 1/2, fill = City), ymin = -Inf, ymax = Inf, alpha = 1/10) +
  geom_vline(xintercept = c(1943, 1970 - 1/2), linetype = 2, color = "grey50") +
  geom_label(aes(x = c(1943, 1970 - 1/2), y = 0, label = c("Suspended\nOperation", "NFL-AFL\nmerger")), vjust = 0, color = "grey50", size = 3) +
  geom_segment(data = background, mapping = aes(x = from - 1/2, xend = to + 1/2, y = Mean, yend = Mean , group = Periode), color = "grey50", linewidth = 1.5, linetype = 1, alpha = 2/5) +
  geom_segment(data = change %>% filter(show), mapping = aes(x = Time, xend = Time, y = bef, yend = aft), arrow = arrow(angle = 20, length = unit(0.15, "inches")), color = "grey75", linewidth = 1.5) +
  geom_segment(data = change %>% filter(show), mapping = aes(x = Time, xend = Time, y = bef, yend = aft), arrow = arrow(angle = 20, length = unit(0.15, "inches")), color = "white", linewidth = 1) +
  geom_line(data = RamsSeason, mapping = aes(x = Season, y = Pct.reg), linetype = 3, color = "grey75", lineend = "round") +
  geom_point(data = RamsSeason, mapping = aes(x = Season, y = Pct.reg, color = Pct.reg, size = Playoff, shape = Playoff)) +
  scale_fill_manual(values = rgb(c(0, 0, 179), c(0, 34, 153), c(0, 68, 93), names = c("Cleveland", "Los Angeles", "St. Louis"), maxColorValue = 255)) +
  scale_color_gradient2(name = "Regular\nSeason Pct", low = "red3", mid = "gold", high = "green3", midpoint = 0.5, labels = function(x) format(x, nsmall = 3), limits = c(0, 1)) +
  scale_shape_manual(name = "Playoff Berth", values = c(19, 15, 17)) +
  scale_size_manual(name = "Playoff Berth", values = c(3, 3, 5)) +
  labs(title = "Rams NFL Franchise History") +
  scale_x_continuous(name = "Season", breaks = function(x) seq(x[1] %/% 20 * 20, to = x[2], by = 10), expand = c(0, 0)) +
  scale_y_continuous(name = "Percentage", labels = function(x) format(x, nsmall = 3), limits = c(0, 1), expand = c(.01, .01)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size=14, hjust = 0.5))

windows(16, 7)
plot(p)
rm(background, change, p)

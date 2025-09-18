# GLOBALE WERTE ----
## Libraries ----
library(tidyverse)

# Modern Era Franchise Information ----
source("nfl_teams.r")

## Abbreviations for use of nfplotR-package - most recent ----
teamabbr <- tribble(~Franchise, ~Abbr,
                    "49ers", "SF", "Bears", "CHI", "Bengals", "CIN",
                    "Bills", "BUF", "Broncos", "DEN", "Browns", "CLE",
                    "Buccaneers", "TB", "Cardinals", "ARI", "Chargers", "LAC",
                    "Chiefs", "KC", "Colts", "IND", "Commanders", "WAS",
                    "Cowboys", "DAL", "Dolphins", "MIA", "Eagles", "PHI",
                    "Falcons", "ATL", "Giants", "NYG", "Jaguars", "JAC",
                    "Jets", "NYJ", "Lions", "DET", "Packers", "GB",
                    "Panthers", "CAR", "Patriots", "NE", "Raiders", "LV",
                    "Rams", "LAR", "Ravens", "BAL", "Saints", "NO",
                    "Seahawks", "SEA", "Steelers", "PIT", "Texans", "HOU",
                    "Titans", "TEN", "Vikings", "MIN")

# CLASS ----
## Definition ----
setClass("WLT",
         slots = c(WLT = "vector", Twt = "numeric"),
         prototype = list(WLT = NULL, Twt = 1/2))

## Funktion summary ----
summary.WLT <- function(object) {
  raw = factor(object@WLT, levels = c("W", "L", "T")); raw <- raw[!is.na(raw)]
  WLT_vec = as.integer(table(raw))
  WLT_form = paste0("(", paste(WLT_vec[1:2], collapse = "-"), ifelse(WLT_vec[3] != 0, paste0("-", WLT_vec[3], ")"), ")"))
  Gs = sum(WLT_vec)
  Pct = as.numeric(t(WLT_vec) %*% c(1, 0, object@Twt) / Gs)
  
  return(list(
    Gs = Gs,
    WLT = WLT_form,
    Pct = Pct,
    W = WLT_vec[1],
    L = WLT_vec[2],
    T = WLT_vec[3],
    Twt = object@Twt))
}

## Method show ----
setMethod(f = show,
          signature = "WLT",
          definition = function(object) {
            tmp <- summary(object)
            print(
              paste0(tmp$WLT, " = ", sprintf(tmp$Pct, fmt = "%#.3f"))
            )
          })

# IMPORT DATEN ----
## Message Start
cat("NFL> Importing Raw Data ...")

## Hilfsfunktion 'import' ----
import <- function(x) {
  x %>%
    separate(filename, into = c("Season", "League", "Residual"), sep = "_", remove = FALSE) %>% 
    select(-Residual) %>% 
    mutate(Season = as.integer(Season),
           Data = map(.x = filename,
                      .f = ~ read_csv(file = paste0("games/nfl/", .),
                                      skip = 1,
                                      col_names = c("Week", "Day", "Date", "Time", "Winner/tie", "Matchup", "Loser/tie",
                                                    "PreBox", "PtsW", "PtsL", "YdsW", "TOW", "YdsL", "TOL"),
                                      col_types = cols(Week = col_character(),
                                                       Date = col_character(),
                                                       Time = col_character(),
                                                       PtsW = col_integer(),
                                                       PtsL = col_integer(),
                                                       YdsW = col_integer(),
                                                       TOW = col_integer(),
                                                       YdsL = col_integer(),
                                                       TOL = col_integer()),
                                      comment = "#",
                                      progress = FALSE,
                                      lazy = FALSE))) %>% 
    return()
}

## Erstmalige Initialisierung ----
if(!exists("data_raw")) {
  data_raw <- tibble(filename = dir("games/nfl/")) %>% 
    mutate(chk = tools::md5sum(paste0("games/nfl/", filename))) %>%
    import()
}

## Suchen möglicherweise veränderter Dateien anhand 'chk' ----
tmp <- tibble(filename = dir("games/nfl/")) %>% 
  mutate(chk = tools::md5sum(paste0("games/nfl/", filename))) %>% 
  anti_join(data_raw, by = c("filename", "chk")) %>% 
  import()

## Es gibt veränderte Files ----
if(dim(tmp)[1] > 0) {
  cat(paste0(" reimporting (", dim(tmp)[1], " files) ..."))
  data_raw <- data_raw %>%
    filter(!(filename %in% tmp$filename)) %>%  # Entferen veraltete Zeile, ...
    bind_rows(tmp) %>%                         # füge Neuimport an und ...
    arrange(filename)                          # stelle Reihenfolge wieder her
}

## Aufräumen
rm(import, tmp)

## Nested Daten erweitern und Anpassungen ----
raw <- data_raw |> 
  select(Season, League, Data) |> 
  unnest(cols = Data) |> 
  mutate(Year = case_when(substr(Date, 1, 3) %in% c("Jan", "Feb") ~ as.integer(Season + 1),
                          TRUE ~ Season),
         Date = case_when(Season < 2021 ~ paste(Year, Date),
                          TRUE ~ Date),
         Date = lubridate::ymd(Date),
         Time = parse_time(Time, format = "%I:%M%p"),
         Game = case_when(str_detect(Week, "^[0-9]{1,2}\\b") ~ paste0("Week_", str_pad(Week, 2, "left", "0")),
                          TRUE ~ Week),
         Week = case_when(Week == "WildCard" ~ "31",
                          Week == "Division" ~ "32",
                          Week == "Champ" ~ "33",
                          Week == "ConfChamp" ~ "34",
                          Week == "SuperBowl" ~ "35",
                          TRUE ~ Week),
         Week = as.integer(Week)) |> 
  replace_na(list(Matchup = "vs")) |> 
  select(-Day, -PreBox, -Year)

## Vertausche, wenn nötig, Winner und Loser (bei händischer preview-Erfassung) ----
tmp <- raw |> 
  filter(PtsL > PtsW) |> 
  mutate(Matchup = "vs") |> 
  rename(c('Winner/tie' = 'Loser/tie', 'Loser/tie' = 'Winner/tie', PtsW = PtsL, PtsL = PtsW, YdsW = YdsL, YdsL = YdsW, TOW = TOL, TOL = TOW))
if(dim(tmp)[1] != 0)                                                                # Nur wenn es zu Vertauschungen kommt werden ...
  raw <- anti_join(raw, tmp, by = c("Date", "Time", "Winner/tie" = "Loser/tie")) |> # die entsprechenden Zeilen mit anti_join entfernt ...
  bind_rows(tmp)                                                                    # und anschließend wieder angefügt

## Aufräumen
rm(tmp)

## Message Ende
cat(paste0(" done (", format(dim(raw)[1], big.mark = ","), " lines)\n"))

# RESULTS ----
## Message Start
cat("NFL> Generating Results ...")

## Darstellung nach Teams ----
results <- raw %>%
  rename(Winner = 'Winner/tie', Loser = 'Loser/tie') %>% 
  transmute(Season,
            Week,
            Game,
            Date,
            Time,
            Team = Winner,
            Road = case_when(Matchup == "N" ~ NA,
                             Matchup == "@" ~ TRUE,
                             TRUE ~ FALSE),
            PF = PtsW,
            PA = PtsL,
            YdsF = YdsW,
            YdsA = YdsL,
            TOF = TOW,
            TOA = TOL,
            Opponent = Loser) %>%
  bind_rows(
    raw %>%
      rename(Winner = 'Winner/tie', Loser = 'Loser/tie') %>% 
      transmute(Season,
                Week,
                Game,
                Date,
                Time,
                Team = Loser,
                Road = case_when(Matchup == "N" ~ NA,
                                 Matchup == "@" ~ FALSE,
                                 TRUE ~ TRUE),
                PF = PtsL,
                PA = PtsW,
                YdsF = YdsL,
                YdsA = YdsW,
                TOF = TOL,
                TOA = TOW,
                Opponent = Winner)
  )

# rm(raw)

## Add WLT and Franchise to data ----
results <- results %>%
  mutate(Result = case_when(is.na(Date) ~ NA_character_,
                            PF > PA ~ "W",
                            PF < PA ~ "L",
                            TRUE ~ "T")) %>% 
  left_join(teaminfo %>% select(Season, Team, Franchise, League, Conference, Division),
            by = c("Season", "Team"), multiple = "all", relationship = "many-to-many") %>% 
  left_join(teaminfo %>% select(Season, Team, Opp_Fr = Franchise, Opp_League = League, Opp_Conf = Conference, Opp_Div = Division),
            by = c("Season", "Opponent" = "Team"), multiple = "all", relationship = "many-to-many") %>%
  select(Season, Week, Game, Date, Time, Team, Franchise:Division, Result, PF, PA, YdsF, YdsA, TOF, TOA, Road, Opponent, Opp_Fr:Opp_Div) %>%
  mutate(Season = as.integer(Season)) %>% 
  arrange(Date, Time, Week, Franchise)

## Add Intradivisional flag and nest Team- and Opponent-info ----
results <- results %>%
  mutate(Div = case_when(Week > 30 ~ NA, # No Intradivisional games in postseason
                         TRUE ~ as.logical(League == Opp_League & Conference == Opp_Conf & Division == Opp_Div)),
         Conf = case_when(Week > 30 ~ NA,
                          TRUE ~ as.logical(League == Opp_League & Conference == Opp_Conf))) %>% 
  nest(Team_data = c(Team, League, Conference, Division)) %>% 
  nest(Opponent_data = c(Opponent, Opp_League, Opp_Conf, Opp_Div))

## Message Ende
cat(paste0(" done (", format(dim(results)[1], big.mark = ","), " lines)\n")) # The result is > 2 times raw, because of 1943 Eagles/Steelers and 1944 Cards/Steelers which are counted as games or both Franchises

# STANDINGS ----
## Message Start
cat("NFL> Generating Standings ...")

## Standings ----
# standings <- results %>%
#   mutate(Team = map_chr(Team_data, ~.$Team),
#          he = Season) %>%  # use Variable 'he' to group because Season will be used for nesting 
#   select(-Franchise, -Time, -Team_data, -Opp_Fr, -Opponent_data) %>%
#   unique() %>% # unique because of 1943, 1944 two Franchises = one Teams
#   group_by(he) %>%
#   complete(Week = 1:max(Week[Week < 30]), nesting(Season, Team),# add bye-weeks (officially 1960:1966 and from 1990 on)
#            fill = list(Game = "Week_", Result = "bye", PF = 0, PA = 0, YdsF = 0, YdsA = 0, TOF = 0, TOA = 0)) %>% # ^
#   ungroup() %>% #                                                                                                   |
#   filter(!((Season < 1960 | Season %in% 1967:1989) & Result == "bye")) %>% #  --------------------------------------┘
#   mutate(Game = case_when(Game == "Week_" ~ paste0(Game, str_pad(Week, 2, "left", "0")), # add week number in 'Game'
#                           TRUE ~ Game),
#          W = (Result == "W") * 1,
#          L = (Result == "L") * 1,
#          T = (Result == "T") * 1,
#          Post = Week > 30) %>%
#   #  group_by(Season) %>% # add last reg week indicator: for Season 1924 and 1934 GB had two games in the last week; and many others: results %>% group_by(Season, Week, Team) %>% tally() %>% filter(n > 1) %>% View()
#   #  mutate(LastReg = Week == max(Week[Week < 30])) %>% 
#   #  ungroup() %>% 
#   group_by(Season, Team) %>% # add last game indicator: bye weeks have no date and are ignored
#   mutate(LastReg = Date == max(Date[Week < 30], na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   group_by(Season, Team, Post) %>%
#   mutate_at(.vars = vars(PF:TOA, W:T), .funs = cumsum) %>% 
#   select(Season, Week, Game, Team, Result:Road, Result, Post, LastReg, PFc = PF, PAc = PA, YdsFc = YdsF, YdsAc = YdsA, TOFc = TOF, TOAc = TOA, W:T) %>%
#   ungroup() %>%
#   mutate(Pct = case_when(Season < 1972 ~ W / (W + L),
#                          TRUE ~ ((W + 1/2 * T) / (W + L + T))) %>% round(3),
#          WLT = case_when(T == 0 ~ paste0("(", W, "-", L, ")"),
#                          TRUE ~ paste0("(", W, "-", L, "-", T, ")"))) %>% 
#   left_join(teaminfo, by = c("Season", "Team")) %>% # add remaining Infos
#   select(Season:Team, Franchise:Division, Result:WLT) %>% 
#   arrange(Season, Week, -Pct, -PFc, PAc)

standings <- results |>
  mutate(Team = map_chr(Team_data, ~.$Team),
         he = Season) |>  # use Variable 'he' to group because Season will be used for nesting 
  select(-Franchise, -Time, -Team_data, -Opp_Fr, -Opponent_data) |> 
  unique() |>  # unique because of 1943, 1944 two Franchises = one Teams
  group_by(he) |> 
  complete(Week = 1:max(Week[Week < 30]), nesting(Season, Team), # add bye-weeks (officially 1960:1966 and from 1990 on)
           fill = list(Game = "Week_", Result = "bye", PF = 0, PA = 0, YdsF = 0, YdsA = 0, TOF = 0, TOA = 0)) |> # ^
  ungroup() |>  #                                                                                                  |
  select(-he) |> 
  filter(!((Season < 1960 | Season %in% 1967:1989) & Result == "bye")) |> # ---------------------------------------┘
  mutate(Game = case_when(Game == "Week_" ~ paste0(Game, str_pad(Week, 2, "left", "0")), # add week number in 'Game'
                          TRUE ~ Game),
         Post = Week > 30) |> 
  group_by(Season, Team)|> # add last game indicator: bye weeks have no date and are ignored
  mutate(LastReg = Date == max(Date[Week < 30], na.rm = TRUE))|> 
  ungroup() |> 
  group_by(Season, Team, Post) |> 
  mutate(across(.cols = PF:TOA, .fns = ~cumsum(.), .names = "{.col}c"),
         Results = map2(as.list(Reduce("c", Result, accumulate = TRUE)), Season, ~new("WLT", WLT = .x, Twt = ifelse(.y < 1971, 0, 1/2)))) |> 
  ungroup() |> 
  select(-(PF:TOA))  |> 
  mutate(map_df(Results, ~summary(.)[2:3])) |> 
  left_join(teaminfo, by = c("Season", "Team"), multiple = "all", relationship = "many-to-many") |> # add remaining Infos
  arrange(Season, Week, -Pct, -PFc, PAc)

## Message Ende
cat(paste0(" done (", format(dim(standings)[1], big.mark = ","), " lines)\n"))
#
# ## Message
# cat("NFL> Show League Standings after last import?\n")
# 
# if(readline("Show Standings after import? [y/n] ") == "y") {
#   standings %>%
#     filter(Season == max(standings$Season), LastReg) %>%
#     select(Franchise, Season, Week, PFc:WLT) %>%
#     arrange(-Pct, -PFc, PAc) %>%
#     as.data.frame() %>% 
#     print()
# }

# MATCHUPS function ----
matchups <- function (a, b, starting = 1970) {
  library(tidyverse)
  cat(paste0("Seasons since ", starting, "!\n"))
  results %>%
    filter(Franchise == a, Opp_Fr == b, Season >= starting) %>%
    mutate(Situation = case_when(Week < 30 & Road ~ "RS Road",
                                 Week < 30 & !Road ~ "RS Home",
                                 TRUE ~ "PS")) %>% 
    mutate(Result = factor(Result, levels = c("W", "L", "T")),
           RS = Week < 20, One = 1) %>%
    complete(Result = Result) %>%
    select(Franchise, Opp_Fr, Situation, Result, One) %>%
    pivot_wider(names_from = Result, values_from = One, values_fn = sum, values_fill = 0) %>% 
    filter(!is.na(Franchise)) -> he
  he %>%
    bind_rows(he %>%
                group_by(Franchise, Opp_Fr, Situation = "Overall") %>%
                summarise_all(~ sum(.))) %>%
    mutate(Pct = ((W + 1/2 * T) / (W + L + T)) %>% round(3),
           WLT = case_when(
             T == 0 ~ paste0("(", W, "-", L, ")"),
             TRUE ~ paste0("(", W, "-", L, "-", T, ")"))) %>% 
    arrange(desc(Situation)) %>% 
    print()
}
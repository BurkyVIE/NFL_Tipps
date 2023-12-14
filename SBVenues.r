library(tidyverse)
library(lubridate)

SBVenue <- tribble(~No, ~Date, ~Venue, ~Lat, ~Long, ~Area, ~Municipality, ~State,
                    "I", "15011967", "Los Angeles Memorial Coliseum", 34.014167,-118.287778, "Los Angeles met.", "Los Angeles", "California", 
                    "II", "14011968", "Orange Bowl", 25.778,-80.22, "Miami met.", "Miami", "Florida",
                    "III", "12011969", "Orange Bowl", 25.778,-80.22, "Miami met.", "Miami", "Florida",
                    "IV", "11011970", "Tulane Stadium", 29.942778,-90.1175, "New Orleans", "New Orleans", "Louisiana",
                    "V", "17011971", "Orange Bowl", 25.778,-80.22, "Miami met.", "Miami", "Florida",
                    "VI", "16011972", "Tulane Stadium", 29.942778,-90.1175, "New Orleans", "New Orleans", "Louisiana",
                    "VII", "14011973", "Los Angeles Memorial Coliseum", 34.014167,-118.287778, "Los Angeles met.", "Los Angeles", "California",
                    "VIII", "13011974", "Rice Stadium", 29.716389,-95.409167, "Houston", "Houston", "Texas",
                    "IX", "12011975", "Tulane Stadium", 29.942778,-90.1175, "New Orleans", "New Orleans", "Louisiana",
                    "X", "18011976", "Orange Bowl", 25.778,-80.22, "Miami met.", "Miami", "Florida",
                    "XI", "09011977", "Rose Bowl", 34.161,-118.168, "Los Angeles met.", "Pasadena", "California",
                    "XII", "15011978", "Louisiana Superdome", 29.950833,-90.081111, "New Orleans", "New Orleans", "Louisiana",
                    "XIII", "21011979", "Orange Bowl", 25.778,-80.22, "Miami met.", "Miami", "Florida",
                    "XIV", "20011980", "Rose Bowl", 34.161,-118.168, "Los Angeles met.", "Pasadena", "California",
                    "XV", "25011981", "Louisiana Superdome", 29.950833,-90.081111, "New Orleans", "New Orleans", "Louisiana",
                    "XVI", "24011982", "Pontiac Silverdome", 42.645833,-83.255, "Detroit met.", "Pontiac", "Michigan",
                    "XVII", "30011983", "Rose Bowl", 34.161,-118.168, "Los Angeles met.", "Pasadena", "California",
                    "XVIII", "22011984", "Tampa Stadium", 27.978889,-82.503611, "Tampa", "Tampa", "Florida",
                    "XIX", "20011985", "Stanford Stadium", 37.434444,-122.161111, "San Francisco Bay Area", "Stanford", "California",
                    "XX", "26011986", "Louisiana Superdome", 29.950833,-90.081111, "New Orleans", "New Orleans", "Louisiana",
                    "XXI", "25011987", "Rose Bowl", 34.161,-118.168, "Los Angeles met.", "Pasadena", "California",
                    "XXII", "31011988", "Jack Murphy Stadium", 32.783056,-117.119444, "San Diego", "San Diego", "California",
                    "XXIII", "22011989", "Joe Robbie Stadium", 25.958056,-80.238889, "Miami met.", "Miami", "Florida",
                    "XXIV", "28011990", "Louisiana Superdome", 29.950833,-90.081111, "New Orleans", "New Orleans", "Louisiana",
                    "XXV", "27011991", "Tampa Stadium", 27.978889,-82.503611, "Tampa", "Tampa", "Florida",
                    "XXVI", "26011992", "Metrodome", 44.973889,-93.258056, "Minneapolis", "Minneapolis", "Minnesota",
                    "XXVII", "31011993", "Rose Bowl", 34.161,-118.168, "Los Angeles met.", "Pasadena", "California",
                    "XXVIII", "30011994", "Georgia Dome", 33.758,-84.401, "Atlanta", "Atlanta", "Georgia",
                    "XXIX", "29011995", "Joe Robbie Stadium", 25.958056,-80.238889, "Miami met.", "Miami", "Florida",
                    "XXX", "28011996", "Sun Devil Stadium", 33.426389,-111.9325, "Phoenix met.", "Tempe", "Arizona",
                    "XXXI", "26011997", "Louisiana Superdome", 29.950833,-90.081111, "New Orleans", "New Orleans", "Louisiana",
                    "XXXII", "25011998", "Qualcomm Stadium", 32.783056,-117.119444, "San Diego", "San Diego", "California",
                    "XXXIII", "31011999", "Pro Player Stadium", 25.958056,-80.238889, "Miami met.", "Tampa", "Florida",
                    "XXXIV", "30012000", "Georgia Dome", 33.758,-84.401, "Atlanta", "Atlanta", "Georgia",
                    "XXXV", "28012001", "Raymond James Stadium", 27.975833,-82.503333, "Tampa", "Tampa", "Florida",
                    "XXXVI", "03022002", "Louisiana Superdome", 29.950833,-90.081111, "New Orleans", "New Orleans", "Louisiana",
                    "XXXVII", "26012003", "Qualcomm Stadium", 32.783056,-117.119444, "San Diego", "San Diego", "California",
                    "XXXVIII", "01022004", "Reliant Stadium", 29.684722,-95.410833, "Houston", "Houston", "Texas",
                    "XXXXIX", "06022005", "Alltel Stadium", 30.323889,-81.6375, "Jacksonville", "Jacksonville", "Florida",
                    "XL", "05022006", "Ford Field", 42.34,-83.045556, "Detroit met.", "Detroit", "Michigan",
                    "XLI", "04022007", "Pro Player Stadium", 25.958056,-80.238889, "Miami met.", "Miami Gardens", "Florida",
                    "XLII", "03022008", "University of Phoenix Stadium", 33.5275,-112.2625, "Phoenix met.", "Glendale", "Arizona",
                    "XLIII", "01022009", "Raymond James Stadium", 27.975833,-82.503333, "Tampa", "Tampa", "Florida",
                    "XLIV", "07022010", "Sun Life Stadium", 25.958056,-80.238889, "Miami met.", "Miami Gardens", "Florida",
                    "XLV", "06022011", "Cowboys Stadium", 32.747778,-97.092778, "Dallas-Fort Worth met.", "Arlington", "Texas",
                    "XLVI", "05022012", "Lucas Oil Stadium", 39.760056,-86.163806, "Indianapolis", "Indianapolis", "Indiana",
                    "XLVII", "03022013", "Mercedes Benz Superdome", 29.950833,-90.081111, "New Orleans", "New Orleans", "Louisiana",
                    "XLVIII", "02022014", "MetLife Stadium", 40.813611,-74.074444, "New York met.", "East Rutherford", "New Jersey",
                    "XLIX", "01022015", "University of Phoenix Stadium", 33.5275,-112.2625, "Phoenix met.", "Glendale", "Arizona",
                    "50", "07022016", "Levi's Stadium", 37.403,-121.97, "San Francisco Bay Area", "Santa Clara", "California",
                    "LI", "05022017", "NRG Stadium", 29.684722,-95.410833, "Houston", "Houston", "Texas",
                    "LII", "04022018", "U.S. Bank Stadium", 44.974,-93.258, "Minneapolis", "Minneapolis", "Minnesota",
                    "LIII", "03022019", "Mercedes-Benz Stadium", 33.755,-84.401, "Atlanta", "Atlanta", "Georgia",
                    "LIV", "02022020", "Hard Rock Stadium", 25.958056,-80.238889, "Miami met.", "Miami Gardens", "Florida",
                    "LV", "07022021", "Raymond James Stadium", 27.975833,-82.503333, "Tampa", "Tampa", "Florida",
                    "LVI", "13022022", "SoFi Stadium", 33.95,-118.338, "Los Angeles met.", "Inglewood", "California",
                    "LVII", "12022023", "State Farm Stadium", 33.5275,-112.2625, "Phoenix met.", "Glendale", "Arizona",
                    "LVIII", "11022024", "Allegiant Stadium", 36.090833,-115.183611, "Las Vegas Valley", "Paradise", "Nevada",
                    "LIX", "09022025", "Caesars Superdome", 29.950833,-90.081111, "New Orleans", "New Orleans", "Louisiana",
                    "LX", "08022026", "Levi's Stadium", 37.403,-121.97, "San Francisco Bay Area", "Santa Clara", "California",
                    "LXI", "14022027", "SoFi Stadium", 33.95,-118.338, "Los Angeles met.", "Inglewood", "California") %>%
  mutate(Date = dmy(Date))

US_cens_Reg_Div <- tribble(~Abbreviation, ~State, ~Region, ~Division,
                           'AL', 'Alabama', 'South', 'East South Central',
                           'AK', 'Alaska', 'West', 'Pacific',
                           'AZ', 'Arizona', 'West', 'Mountain',
                           'AR', 'Arkansas', 'South', 'West South Central',
                           'CA', 'California', 'West', 'Pacific',
                           'CO', 'Colorado', 'West', 'Mountain',
                           'CT', 'Connecticut', 'Northeast', 'New England',
                           'DE', 'Delaware', 'South', 'South Atlantic',
                           'FL', 'Florida', 'South', 'South Atlantic',
                           'GA', 'Georgia', 'South', 'South Atlantic',
                           'HI', 'Hawaii', 'West', 'Pacific',
                           'ID', 'Idaho', 'West', 'Mountain',
                           'IL', 'Illinois', 'Midwest', 'East Nort Central',
                           'IN', 'Indiana', 'Midwest', 'East Nort Central',
                           'IA', 'Iowa', 'Midwest', 'West North Centtral',
                           'KS', 'Kansas', 'Midwest', 'West North Centtral',
                           'KY', 'Kentucky', 'South', 'East South Central',
                           'LA', 'Louisiana', 'South', 'West South Central',
                           'ME', 'Maine', 'Northeast', 'New England',
                           'MD', 'Maryland', 'South', 'South Atlantic',
                           'MA', 'Massachusetts', 'Northeast', 'New England',
                           'MI', 'Michigan', 'Midwest', 'East Nort Central',
                           'MN', 'Minnesota', 'Midwest', 'West North Centtral',
                           'MS', 'Mississippi', 'South', 'East South Central',
                           'MO', 'Missouri', 'Midwest', 'West North Centtral',
                           'MT', 'Montana', 'West', 'Mountain',
                           'NE', 'Nebraska', 'Midwest', 'West North Centtral',
                           'NV', 'Nevada', 'West', 'Mountain',
                           'NH', 'New Hampshire', 'Northeast', 'New England',
                           'NJ', 'New Jersey', 'Northeast', 'Mid-Atlantic',
                           'NM', 'New Mexico', 'West', 'Mountain',
                           'NY', 'New York', 'Northeast', 'Mid-Atlantic',
                           'NC', 'North Carolina', 'South', 'South Atlantic',
                           'ND', 'North Dakota', 'Midwest', 'West North Centtral',
                           'OH', 'Ohio', 'Midwest', 'East Nort Central',
                           'OK', 'Oklahoma', 'South', 'West South Central',
                           'OR', 'Oregon', 'West', 'Pacific',
                           'PA', 'Pennsylvania', 'Northeast', 'Mid-Atlantic',
                           'RI', 'Rhode Island', 'Northeast', 'Mid-Atlantic',
                           'SC', 'South Carolina', 'South', 'South Atlantic',
                           'SD', 'South Dakota', 'Midwest', 'West North Centtral',
                           'TN', 'Tennessee', 'South', 'East South Central',
                           'TX', 'Texas', 'South', 'West South Central',
                           'UT', 'Utah', 'West', 'Mountain',
                           'VT', 'Vermont', 'Northeast', 'New England',
                           'VA', 'Virginia', 'South', 'South Atlantic',
                           'WA', 'Washington', 'West', 'Pacific',
                           'WV', 'West Virginia', 'South', 'South Atlantic',
                           'WI', 'Wisconsin', 'Midwest', 'East Nort Central',
                           'WY', 'Wyoming', 'West', 'Mountain')
SBVenue <- left_join(SBVenue, US_cens_Reg_Div, by = "State")
rm(US_cens_Reg_Div)

he <- sum((today(tzone = "CET") - SBVenue$Date) > 0) # past SB
he <- c(he - 3, 3, dim(SBVenue)[1] - he)
SBVenue <- bind_cols(SBVenue, Timeline = rep(c("past", "last 3", "future"), he))
rm(he)

clusters <- list(
  tibble(Name = "Pacific", State = c("Arizona", "California", "Nevada")),
  tibble(Name = "Gulf", State = c("Georgia", "Florida", "Louisiana", "Texas")),
  tibble(Name = "North", State = c("Indiana", "Michigan", "Minnesota", "New Jersey"))) %>%
  bind_rows() %>%
  left_join(SBVenue %>%
              select(State, Area, Lat, Long) %>%
              group_by(State, Area) %>%
              summarise(Long = mean(Long), Lat = mean(Lat), .groups = "drop"),
    by = c("State" = "State"))

p <- ggplot(data = SBVenue, mapping = aes(x = Long, y = Lat)) +
  geom_polygon(data = map_data("state"), mapping = aes(x = long, y = lat, group = group), fill = "grey75", color = "white") +
  geom_polygon(data = map_data("state") %>%
                      filter(region %in% (clusters %>%
                                          select(State) %>%
                                          unique() %>%
                                          mutate(State = State %>%
                                          tolower()) %>%
                                          pull())),
    mapping = aes(x = long, y = lat, group = group), fill = "#c8e6c9", color = "white") +
  geom_polygon(data = map_data("usa"), mapping = aes(x = long, y = lat, group = group), fill = NA, color = "grey50", linewidth = 1.25) +
  geom_point(mapping = aes(color = Timeline, size = Timeline), shape = 19) +
  ggrepel::geom_label_repel(data = clusters , mapping = aes(label = Area), size = 3, alpha = .75,
                            label.r = .25, box.padding = 1, point.padding = 0, force = 5) +
  ggalt::geom_encircle(data = clusters, mapping = aes(group = Name), s_shape = .5, expand = .025, linetype = 2, size = 2, color = "#ea4335", alpha = 1/2) +
#  geom_density2d(n = 250, size = 1.05, color = "#ea4335", alpha = 1/2) +
  scale_color_manual(values = c(future = "#f4c20d", "last 3" = "#3cba54", past = "#4885ed")) +
  scale_size_manual(values = c(future = 3, "last 3" = 7, past = 7)) +
  labs(title = "NFL Championship Game Venues",
       subtitle = paste("Super Bowls", first(SBVenue$No), "-", last(SBVenue$No))) +
  coord_map("bonne", 40) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = .5),
        legend.title = element_text(face = "bold"),
        legend.box.margin = margin(r = 10, unit = "mm"))
# 
windows(16, 9)
plot(p)
rm(p, clusters)
### OR
# pdf(file = "SBvenues.pdf", width = 16, height = 9, paper = "a4r")
# plot(p)
# dev.off()
# rm(p, clusters)

###

he <- sum((today(tzone = "CET")-SBVenue$Date)>0) # past SB
he <- c(he, dim(SBVenue)[1] - he)
SBVenue <- mutate(SBVenue, Timeline = rep(c("past", "future"), he))
rm(he)

tmp <- SBVenue %>%
  group_by(Lat, Long, Timeline) %>%
  summarise(No = list(No), .groups = "keep") %>% 
  mutate(No = map_chr(No, ~ paste0(unlist(No), collapse = "\n")))

p <- ggplot(data = SBVenue, mapping = aes(x = Long, y = Lat)) +
  geom_polygon(data = map_data("state"), mapping = aes(x = long, y = lat, group = group), fill = "grey75", color = "white", size = 1) +
  geom_polygon(data = map_data("state") %>% filter(region %in% (tolower(unique(SBVenue$State)))),
               mapping = aes(x = long, y = lat, group = group), fill = "#c8e6c9", color = "white", linewidth = 1.25, alpha = .85) +
  geom_polygon(data = map_data("usa"), mapping = aes(x = long, y = lat, group = group), fill = NA, color = "grey50", linewidth = 1.5) +
#  geom_point(size = 5, color = "black", fill = "red3", shape = 21, alpha = .8) +
  ggimage::geom_image(image = "fb_32.png", size = .02) +
  ggrepel::geom_label_repel(data = tmp, mapping = aes(label = No, color = Timeline), size = 3, fontface = "bold", fill ="beige", alpha = .85,
                            label.r = .25, box.padding = .5, point.padding = 0, force = 5) +
  scale_color_manual(values = c("past" = "navy", "future" = "red3")) +
  guides(color = guide_legend(override.aes = list(label = "SB"))) +
  labs(title = "NFL Championship Game Venues",
       subtitle = paste("Super Bowls", first(SBVenue$No), "-", last(SBVenue$No))) +
  coord_map("bonne", 40) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = .5),
        legend.title = element_text(face = "bold"),
        legend.box.margin = margin(r = 10, unit = "mm"))

windows(16, 9)
plot(p)

rm(p, tmp)

# LIBRARIES ----
library(tidyverse)

# DEFINIERE Trip ----
trip <- 80

# FILTERE Trip Daten ----
data <- filter(mytrips, Trip == trip)

# FILTERE Stops
stops <- filter(myflights, Trip == trip)
stops <- bind_rows(select(stops, IATA = Dep, DepDet) |> unnest(cols = DepDet),
                   select(stops, IATA = Arr, ArrDet) |> unnest(cols = ArrDet)) |> 
  unique()

# EXISTIERT TruePath
tpnex <- any(dim(data$TruePath[[1]]) == 0)

# DEFINIERE bounding box
latrange <- range(if(tpnex) data$GCPath[[1]]$Lat else c(data$GCPath[[1]]$Lat, data$TruePath[[1]]$Lat), na.rm = TRUE) 
latrange <- latrange + (log(diff(latrange) * 2, 5) * c(-1, 1))
latrange <- pmin(abs(latrange), 89) * sign(latrange)
longrange <- range(if(tpnex) data$GCPath[[1]]$Long else c(data$GCPath[[1]]$Long, data$TruePath[[1]]$Long), na.rm = TRUE) 
longrange <- longrange + (log(diff(longrange), 5) * c(-1, 1))
longrange <- pmin(abs(longrange), 179) * sign(longrange)
bgmapbbox <- c(longrange[1], latrange[1], longrange[2], latrange[2])

# HOLE Kartendaten ----
library(ggmap)
# https://client.stadiamaps.com
# ...Stadia...
register_stadiamaps("523d7972-fb94-4a02-a351-5ab2443b2f92")
bgmap <- get_map(bgmapbbox, source = "stadia", maptype = "stamen_toner")
rm(latrange, longrange, bgmapbbox)

# PLOT
library(ggrepel)
if(tpnex) { # No TruePath branch
  p <- ggmap(bgmap, darken = c(1 / 2, "linen")) +
    geom_path(data = data$GCPath[[1]], mapping = aes(x = Long, y = Lat, group = Rel), lineend = "round", linewidth = 3, color = "dodgerblue") +
    geom_path(data = data$GCPath[[1]], mapping = aes(x = Long, y = Lat, group = Rel), lineend = "round", linewidth = .75, color = "green3", linetype = "dashed") +
    geom_point(data = stops, mapping = aes(x = Long, y = Lat), size = 3, color = "red3") +
    geom_label_repel(data = stops, mapping = aes(x = Long, y = Lat, label = IATA), point.padding = 10, size = 3, color = "red3", fontface = "bold", alpha = 3/4) +
    labs(x = "Longitude [°E]", y = "Latitude [°N]",
         title = paste0("GC path of ", data$Route, " (trip ", trip, " ", comment(data), ")"),
         subtitle = filter(myflights, Trip == trip) |> pull(Date) |> range() |> paste(collapse = " to ")) +
    coord_map() +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "top")
} else
{
  waypoints <- data$TruePath[[1]] |>
    group_by(Callsign) |>
    filter(Altitude > max(Altitude, na.rm = TRUE) * 2/3) |>
    summarise_all(~first(.)) |> 
    arrange(UTC) |> 
    rowid_to_column(var = "Leg")
  
  p <- ggmap(bgmap, darken = c(1 / 2, "linen")) +
    geom_path(data = data$TruePath[[1]], mapping = aes(x = Long, y = Lat, group = Callsign, color = Altitude), lineend = "round", linewidth = 3) +
    geom_path(data = data$TruePath[[1]], mapping = aes(x = Long, y = Lat, group = Callsign), lineend = "round", linewidth = .5, color = "linen") +
    geom_point(data = waypoints, mapping = aes(x = Long, y = Lat, color = Altitude), size = 5) +
    geom_text(data = waypoints, mapping = aes(x = Long, y = Lat, label = Leg), color = "linen", size = 3) +
    scale_color_gradient(name = "Altitude [1,000 ft]", labels = function(x)x/1e3, low = "green3", high = "dodgerblue", limits = c(0, 42e3)) +
    geom_point(data = stops, mapping = aes(x = Long, y = Lat), size = 3, color = "red3") +
    geom_label_repel(data = stops, mapping = aes(x = Long, y = Lat, label = IATA), point.padding = 17, size = 3, color = "red3", fontface = "bold", alpha = 3/4) +
    labs(x = "Longitude [°E]", y = "Latitude [°N]",
         title = paste0("Flight path of ", data$Route, " (trip ", trip, " ", comment(data), ")"),
         subtitle = paste0(filter(myflights, Trip == trip) |> pull(Date) |> range() |> paste(collapse = " to "), " / Airtime = ",
                           group_by(data$TruePath[[1]], Callsign) |> summarise(Airt = difftime(last(UTC), first(UTC), units = "hours")) |> pull(Airt) |> sum() |> round(digits = 1), " hours")) +
    coord_map() +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          legend.position = "top")
  
  rm(waypoints)
}

# DIN A4 in mm: 210 x 297; in inch: 8.2 x 11.6
pdfsize <- rev(dim(bgmap))
pdfsize <- round(pdfsize / min(pdfsize) * 8.2, 1)
# Aufschlag für Überschriftenbereich
pdfsize[2] <- pdfsize[2] + 1.5
if(min(pdfsize) > 8.2) pdfsize <- round(pdfsize / min(pdfsize) * 8.2, 1)
if(max(pdfsize) > 11.6) pdfsize <- round(pdfsize / max(pdfsize) * 11.6, 1)

# AUSAGBE in pdf-File ----
pdf(paste0("Detaildaten/trip", str_sub(comment(data), 1, 1), str_pad(trip, width = 3, pad = "0"), ".pdf"), pdfsize[1], pdfsize[2] + 1 / 2)
plot(p)
dev.off()

# AUFRÄUMEN ----
rm(trip, data, stops, tpnex, bgmap, p, pdfsize)

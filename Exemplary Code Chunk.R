#This section takes the WorldPhones data matrix and converts it to a format similar to my presentation

df = WorldPhones |>
  as.data.frame() |>
  rownames_to_column(var = "Year") |>
  pivot_longer(
      cols = c("N.Amer", "Europe", "Asia", "S.Amer", "Oceania", "Africa", "Mid.Amer"),
      values_to = 'Phones',
      names_to = 'Region'
  ) |>
  group_by(Region) |>
  mutate(
    Mean = mean(Phones),
    Competition = case_when(Phones > Mean ~ "Full Competition", .default = "Monopoly")
  ) |> #Creates mean of "Phones" grouped by Region to help me create my 'Series' column
  select(-Mean) #I don't need the Mean column anymore

#Reposting my poster_theme (I'd normally import it from a Github repository)
poster_theme = theme_minimal(base_size = 24) +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(family = "Times", hjust = 0.5),
    plot.subtitle = element_text(family = "Times", hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  )

#Function Time!
    #Data is df
    #r = (name of region)

case_plot = function(data, r) {
  require(dplyr); require(knitr); require(ggplot2); require(tidyverse)
  data |>
    filter(
      Year > 1951,
      Region == {{r}}
    ) |>
    group_by(Region, Competition) |>
    mutate(
      xmin_monopoly = min(Year[Competition == "Monopoly"]),
      xmax_monopoly = max(Year[Competition == "Monopoly"]),
      xmin_full = min(Year[Competition == "Full Competition"]),
      xmax_full = max(Year[Competition == "Full Competition"])
    ) |>
    ungroup() |>
    ggplot(aes(x = Year, y = Phones)) +
    geom_point(shape = 21, color = "black", size = 3, show.legend = TRUE) +
    geom_rect(aes(xmin = xmin_monopoly, xmax = xmax_monopoly, ymin = -Inf, ymax = Inf),
              fill = "red",
              alpha = 0.05
    ) + #The above geom_rect creates a red rectangle over years where there is a monopoly
    geom_rect(aes(xmin = xmin_full, xmax = xmax_full, ymin = -Inf, ymax = Inf),
              fill = "green",
              alpha = 0.05
    ) + #The above geom_rect creates a green rectangle over years where there is full competition
    labs(
      y = "$USD (Billions)",
      caption = "Annual Investment in Telecommunications.") +
    poster_theme
}

#To test, do unique(df$Region) and make a case_plot(df, "regionName"). Make sure to put quotes "" around the regionName.

case_plot(df, "Europe") #there's a small gap between Monopoly/Full Competition. Just imagine the gap represents Monopoly... It was too tricky for me to fix when I tried.
case_plot(df, "Africa")
case_plot(df, "S.Amer")
case_plot(df, "Oceania")



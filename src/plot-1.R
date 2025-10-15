library(dplyr)
library(lubridate)
library(pins)
library(tidyr)
library(purrr)
library(glue)
library(ggplot2)
library(ggiraph)
library(tidyhydro)

plot_co_pal <- list(
  "obs" = "#000000",
  "qmap" = "#DC3220",
  "raw" = "#009E73"
)

# databoard <- pins::board_folder("data/datasets", versioned = TRUE)

# anadyr_obs <-
#   pins::pin_read(databoard, "anadyr_obs") |>
#   dplyr::filter(between(year, 1979, 1996))

# anadyr_sim <-
#   pins::pin_read(databoard, "glofas_anadyr") |>
#   collapse::unlist2d(idcols = FALSE) |>
#   dplyr::mutate(
#     year = lubridate::year(datetime),
#     date = lubridate::as_date(datetime) - 1
#   ) |>
#   dplyr::filter(id %in% unique(anadyr_obs$id))

# anadyr_all <-
#   anadyr_obs |>
#   dplyr::rename(obs = q) |>
#   dplyr::left_join(
#     dplyr::select(anadyr_sim, id, year, date, raw = dis24),
#     by = dplyr::join_by(id, year, date)
#   ) |>
#   dplyr::filter(!is.na(raw)) |>
#   dplyr::filter(lubridate::month(date) %in% c(5:9)) |>
#   filter(id == 1496, year == 1981)

anadyr_all <-
  qs::qread("data/eropol_1981.qs") |>
  rename(raw = sim)

anadyr <-
  anadyr_all |>
  mutate(date = lubridate::as_datetime(date, tz = "UTC")) |>
  tidyr::complete(date = seq.POSIXt(min(date), max(date), by = "1 hour")) |>
  mutate(
    # id = zoo::na.locf(id),
    year = lubridate::year(date),
    obs = imputeTS::na_interpolation(obs),
    raw = imputeTS::na_interpolation(raw),
    qmap = imputeTS::na_interpolation(qmap),
  ) |>
  dplyr::mutate(
    tooltip = glue::glue(
      "
      <div style='background:white;box-shadow: rgba(0, 0, 0, 0.1) 0px 4px 6px -1px, rgba(0, 0, 0, 0.06) 0px 2px 4px -1px;'>
        <div style='padding:8px; font-weight: bold; margin-bottom: 8px; font-size: 16px;background:black;color:white;'>{lubridate::as_date(date)}</div>
        <div style='padding:0px 8px 8px'>
        <table style='border-collapse: collapse; width: 100%;'>
          <tr>
            <td style='text-align: left; padding: 2px 8px 2px 0; border: none;'>
              <span style='display: inline-block; width: 10px; height: 10px; background-color: {plot_co_pal$obs}; margin-right: 6px; vertical-align: middle;'></span>Observed
            </td>
            <td style='text-align: right; padding: 2px 4px; border: none; font-family: Roboto Mono'>
              {format(round(obs))}
            </td>
          </tr>
          <tr>
            <td style='text-align: left; padding: 2px 8px 2px 0; border: none;'>
              <span style='display: inline-block; width: 10px; height: 10px; background-color: {plot_co_pal$raw}; margin-right: 6px; vertical-align: middle;'></span>GloFAS
            </td>
            <td style='text-align: right; padding: 2px 4px; border: none; font-family: Roboto Mono'>
              {format(round(raw))}
            </td>
          </tr>
        </table>
        </div>
      </div>
    "
    )
  ) |>
  pivot_longer(
    cols = c(obs, raw, qmap),
    names_to = "type",
    values_to = "q"
  ) |>
  dplyr::mutate(type = factor(type, levels = names(plot_co_pal)))

glofas_plot <-
  ggplot() +
  geom_line(
    data = filter(anadyr, type != "qmap"),
    mapping = aes(
      x = date,
      y = q,
      group = type,
      color = type
    ),
    linewidth = 1
  ) +
  #interactive cross hair, hide with alpha
  ggiraph::geom_vline_interactive(
    data = filter(anadyr, type != "qmap"),
    mapping = aes(
      data_id = date,
      xintercept = date,
      tooltip = tooltip
    ),
    alpha = 0.001,
    color = "black",
    linewidth = 0.4
  ) +
  #interactive point, hide with alpha
  ggiraph::geom_point_interactive(
    data = filter(anadyr, type != "qmap"),
    mapping = aes(
      data_id = date,
      x = date,
      y = q,
      color = type
    ),
    alpha = 0.001,
    shape = 19,
    size = 4,
    show.legend = FALSE
  ) +
  #recolor lines
  scale_color_manual(
    values = unlist(plot_co_pal),
    labels = c("Observed", "GloFAS")
  ) +
  #labels
  labs(
    title = "Anadyr — Novyy Eropol (1981)",
    subtitle = 'Mean daily streamflow, m³/s'
  ) +
  guides(
    color = guide_legend(override.aes = list(linewidth = 1))
  ) +
  #theme
  theme(
    legend.position = c(0.75, 0.96),
    legend.direction = "horizontal",
    text = element_text(family = "Roboto"),
    plot.title = element_text(face = "bold", size = 18),
    plot.caption = element_text(color = "#AAAAAA", margin = margin(t = 10)),
    plot.subtitle = element_text(color = "#AAAAAA", size = 14),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.line.x = element_line(color = "black"),
    axis.text.y = element_text(family = "Roboto Mono", size = 12),
    axis.text.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.25)
  )

# ggiraph::girafe(
#   ggobj = glofas_plot,
#   width_svg = 8,
#   height_svg = 6,
#   options = list(
#     ggiraph::opts_toolbar(saveaspng = FALSE),
#     opts_tooltip(css = "font-family:Roboto"),
#     opts_hover(
#       css = "stroke-opacity:100%;fill-opacity:100%",
#       nearest_distance = NULL
#     )
#   )
# )

# Corrected -----------------------------------------------------------
qmap_plot <-
  ggplot() +
  geom_line(
    data = anadyr,
    mapping = aes(
      x = date,
      y = q,
      group = type,
      color = type
    ),
    linewidth = 1
  ) +
  #interactive cross hair, hide with alpha
  ggiraph::geom_vline_interactive(
    data = anadyr,
    mapping = aes(
      data_id = date,
      xintercept = date,
      tooltip = tooltip
    ),
    alpha = 0.001,
    color = "black",
    linewidth = 0.4
  ) +
  #interactive point, hide with alpha
  ggiraph::geom_point_interactive(
    data = anadyr,
    mapping = aes(
      data_id = date,
      x = date,
      y = q,
      color = type
    ),
    alpha = 0.001,
    shape = 19,
    size = 4,
    show.legend = FALSE
  ) +
  #recolor lines
  scale_color_manual(
    values = unlist(plot_co_pal),
    labels = c("Observed", "DQM", "GloFAS")
  ) +
  #labels
  labs(
    title = "Anadyr — Novyy Eropol (1981)",
    subtitle = 'Mean daily streamflow, m³/s'
  ) +
  guides(
    color = guide_legend(override.aes = list(linewidth = 1))
  ) +
  #theme
  theme(
    legend.position = c(0.75, 0.96),
    legend.direction = "horizontal",
    text = element_text(family = "Roboto"),
    plot.title = element_text(face = "bold", size = 18),
    plot.caption = element_text(color = "#AAAAAA", margin = margin(t = 10)),
    plot.subtitle = element_text(color = "#AAAAAA", size = 14),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.line.x = element_line(color = "black"),
    axis.text.y = element_text(family = "Roboto Mono", size = 12),
    axis.text.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.25)
  )

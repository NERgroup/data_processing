library(ggplot2)
library(ggforce)



################################################################################
#20 rings

outer_radius <- 20
arm_length <- 22

sheets <- c("A", "B")

rings <- expand.grid(
  sheet = sheets,
  r = 1:outer_radius
)

rings$linewidth <- ifelse(rings$r %% 5 == 0, 0.7, 0.15)
rings$alpha <- ifelse(rings$r %% 5 == 0, 1, 0.25)

arms <- data.frame(
  sheet = rep(sheets, each = 2),
  x = rep(c(-arm_length, 0), times = 2),
  xend = rep(c(arm_length, 0), times = 2),
  y = rep(c(0, -arm_length), times = 2),
  yend = rep(c(0, arm_length), times = 2)
)

headers <- data.frame(
  sheet = sheets,
  x = 0,
  y = 25.5,
  label = "Site name __________________    Date __________"
)

instructions <- data.frame(
  sheet = sheets,
  x = 0,
  y = 23.5,
  label = 'Indicate location of all kelps >1 m tall with an "X" and write the heading along each transect.'
)

p <- ggplot() +
  geom_circle(
    data = rings,
    aes(
      x0 = 0,
      y0 = 0,
      r = r,
      linewidth = linewidth,
      alpha = alpha
    ),
    color = "grey35",
    fill = NA
  ) +
  geom_segment(
    data = arms,
    aes(x = x, xend = xend, y = y, yend = yend),
    linewidth = 0.8,
    color = "grey20"
  ) +
  geom_text(
    data = headers,
    aes(x = x, y = y, label = label),
    size = 3.2,
    fontface = "bold"
  ) +
  geom_text(
    data = instructions,
    aes(x = x, y = y, label = label),
    size = 2.4
  ) +
  facet_wrap(~sheet, ncol = 1) +
  scale_linewidth_identity() +
  scale_alpha_identity() +
  coord_fixed(
    xlim = c(-22.5, 22.5),
    ylim = c(-22.5, 26.5),
    clip = "off"
  ) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    panel.spacing = unit(0.15, "in"),
    plot.margin = margin(0.15, 0.15, 0.15, 0.15, unit = "in")
  )

p

outfile <- file.path(
  path.expand("~/Downloads"),
  "survey_target_sheet.pdf"
)

ggsave(
  outfile,
  p,
  width = 8.5,
  height = 11,
  units = "in"
)

cat("Saved to:", outfile)


################################################################################
#10 rings



outer_radius <- 10
arm_length <- 11

sheets <- c("A", "B")

rings <- expand.grid(
  sheet = sheets,
  r = 1:outer_radius
)

rings$linewidth <- ifelse(rings$r %in% c(5, 10), 0.8, 0.18)
rings$alpha <- ifelse(rings$r %in% c(5, 10), 1, 0.35)

arms <- data.frame(
  sheet = rep(sheets, each = 2),
  x = rep(c(-arm_length, 0), times = 2),
  xend = rep(c(arm_length, 0), times = 2),
  y = rep(c(0, -arm_length), times = 2),
  yend = rep(c(0, arm_length), times = 2)
)

headers <- data.frame(
  sheet = sheets,
  x = 0,
  y = 13,
  label = "Site name __________________    Date __________"
)

instructions <- data.frame(
  sheet = sheets,
  x = 0,
  y = 12,
  label = 'Indicate location of all kelps >1 m tall with an "X" and write the heading along each transect.'
)

p <- ggplot() +
  geom_circle(
    data = rings,
    aes(x0 = 0, y0 = 0, r = r,
        linewidth = linewidth, alpha = alpha),
    color = "grey35",
    fill = NA
  ) +
  geom_segment(
    data = arms,
    aes(x = x, xend = xend, y = y, yend = yend),
    linewidth = 0.8,
    color = "grey20"
  ) +
  geom_text(data = headers, aes(x = x, y = y, label = label),
            size = 3.0, fontface = "bold") +
  geom_text(data = instructions, aes(x = x, y = y, label = label),
            size = 2.2) +
  facet_wrap(~sheet, ncol = 1) +
  scale_linewidth_identity() +
  scale_alpha_identity() +
  coord_fixed(xlim = c(-11.5, 11.5), ylim = c(-11.5, 13.5)) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    panel.spacing = unit(0.15, "in"),
    plot.margin = margin(0.15, 0.15, 0.15, 0.15, unit = "in")
  )

p

outfile <- file.path(path.expand("~/Downloads"), "survey_target_sheet_10m.pdf")

ggsave(outfile, p, width = 8.5, height = 11, units = "in")

cat("Saved to:", outfile)
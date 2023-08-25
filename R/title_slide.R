library(dutchmasters)
library(tidyverse)

dutch_white <- dutchmasters$pearl_earring["white(colar)"]

harem <- list(
  prussian_blue      = "#0B273C",
  cadet_gray         = "#989F9C",
  carribbean_current = "#01606E",
  pale_dogwood       = "#E3C0B6",
  cadet_gray2        = "#A1B4B9",
  bistre             = "#2E180F",
  chinese_violet     = "#856084",
  gold               = "#A68247"
)

# make palettes

harem_pals <- purrr::map(harem,
                         function(x){
                           colorRampPalette(c(x, dutch_white))
                         })

create_palette_ramp <- function(x = 50,
                                palette_colors,
                                palette_functions) {
  tibble::tibble(
    family = c(
      rep(names(palette_colors)[[1]], x),
      rep(names(palette_colors)[[2]], x),
      rep(names(palette_colors)[[3]], x),
      rep(names(palette_colors)[[4]], x),
      rep(names(palette_colors)[[5]], x),
      rep(names(palette_colors)[[6]], x),
      rep(names(palette_colors)[[7]], x),
      rep(names(palette_colors)[[8]], x)

    ),
    level = c(rep(1:x, 8)),
    code = c(
      palette_functions[[1]](x),
      palette_functions[[2]](x),
      palette_functions[[3]](x),
      palette_functions[[4]](x),
      palette_functions[[5]](x),
      palette_functions[[6]](x),
      palette_functions[[7]](x),
      palette_functions[[8]](x)

    )
  ) %>%
    dplyr::arrange(-level)
}


harem_ramp_50 <- create_palette_ramp(palette_colors = harem,
                                     palette_functions = harem_pals)


split_palette_bubble <-  create_palette_ramp(200,
                                             palette_colors = harem,
                                             palette_functions = harem_pals
) %>%
  filter(family != "white") %>%
  arrange(family, -level) %>%
  group_by(family) %>%
  group_split()


purrr::map(split_palette_bubble, function(x) {
  max_level <- max(x$level)
  ggplot(
    x,
    aes(
      x = family,
      y = level,
      color = code
    )
  ) +
    geom_jitter(
      size = 25,
      width = 0.5,
      height = 0.2,
      alpha = 0.5
    ) +
    scale_color_identity() +
    geom_hline(
      yintercept = max_level * 0.55,
      color = x$code[max_level * 0.45],
      size = 8
    ) +
    geom_hline(
      yintercept = max_level * 0.5,
      color = x$code[max_level * 0.5],
      size = 10
    ) +
    geom_hline(
      yintercept = max_level * 0.45,
      color = x$code[max_level * 0.55],
      size = 8
    ) +
    coord_cartesian(
      clip = "off",
      # xlim = c(-0.2, 0.2),
      ylim = c(-5, 205)
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      plot.background = element_rect(
        fill = x$code[max_level * 0.5],
        colour = NA
      ),
      plot.caption.position = "plot",
      plot.margin = margin(5, 5, 10, 5, "pt")
    )
})

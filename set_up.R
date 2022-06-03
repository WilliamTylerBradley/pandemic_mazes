library(data.table)
library(ggplot2)
library(ggimage)

source("get_maze.R")
source("update_maze.R")
source("maze_to_path.R")
source("get_image_data.R")

set.seed(1)
for (structure_parameter in seq(0, 1, by = .25)) {
  for (num in 1:2) {
    edges <- get_maze(structure_parameter)
    edges <- update_maze(edges)
    path <- maze_to_path(edges)
    path <- get_image_data(path, structure_parameter)

    # set up background floor
    floor <- CJ(
      x = seq(.5, 10.5, length.out = 360),
      y = seq(-.5, 10.5, length.out = 360)
    )
    floor[, color := sample(c(seq(5, 9), c("A", "B", "C", "D", "E", "F")), 1),
      by = seq_len(nrow(floor))
    ]
    floor[, color := paste0("#", color, color, color, color, color, color)]
    floor[, ":="(xmin = x - runif(1, 0, .25),
      ymin = y - runif(1, 0, .25),
      xmax = x + runif(1, 0, .25),
      ymax = y + runif(1, 0, .25)),
    by = seq_len(nrow(floor))
    ]

    ggplot() +
      geom_rect(
        data = floor,
        aes(
          xmin = xmin, ymin = ymin,
          xmax = xmax, ymax = ymax,
          fill = color
        ),
        color = NA,
        alpha = .01
      ) +
      scale_fill_identity() +
      geom_image(
        data = path,
        aes(x, y, image = image)
      ) +
      theme_void() +
      theme(aspect.ratio = 1)
    ggsave(paste0("output/image_", structure_parameter * 100, 
                  "_", num, ".jpeg"),
      width = 8,
      height = 8,
      bg = "#F3F3F3"
    )
  }
}

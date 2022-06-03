library(data.table)
library(ggplot2)

# do not enter
ggplot() +
  geom_polygon(aes(
    x = cos(seq(0, 2 * pi, pi / 4) + pi / 8) * 1.082,
    y = sin(seq(0, 2 * pi, pi / 4) + pi / 8) * 1.082
  ), color = "#90091E", fill = "#90091E") +
  geom_text(aes(x = 0, y = 0, label = "DO NOT\nENTER"), 
            color = "white", size = 3) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(-1, 1)) +
  theme_void() +
  coord_equal()
ggsave(paste0("signs/do_not_enter_straight_south.png"),
  height = 1,
  width = 1
)

directions <- data.table(
  direction = c(
    "east",
    "north",
    "west",
    "south"
  ),
  angle = c(270, 0, 90, 180)
)

circle <- data.table(
  x = cos(seq(0, 2 * pi, length.out = 360)),
  y = sin(seq(0, 2 * pi, length.out = 360))
)

# square
square <- data.table(
  x = c(-1, 1, 1, -1),
  y = c(-1, -1, 1, 1)
)

# diamond
diamond <- data.table(
  x = c(0, 1, 0, -1),
  y = c(-1, 0, 1, 0)
)

for (i in 1:nrow(directions)) {
  current_direction <- directions[i, direction]
  current_angle <- directions[i, angle]

  # light green dot
  ggplot() +
    geom_polygon(data = circle, aes(x = x * .5, y = y * .5), 
                 color = "#6FBD4B", fill = "#6FBD4B") +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_y_continuous(limits = c(-1, 1)) +
    theme_void() +
    coord_equal()
  ggsave(paste0("signs/light_green_dot_straight_", 
                current_direction, ".png"),
    height = 1,
    width = 1
  )

  # dark green dot
  ggplot() +
    geom_polygon(data = circle, aes(x = x * .75, y = y * .75), 
                 color = "#235C09", fill = "#235C09") +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_y_continuous(limits = c(-1, 1)) +
    theme_void() +
    coord_equal()
  ggsave(paste0("signs/dark_green_dot_straight_", 
                current_direction, ".png"),
    height = 1,
    width = 1
  )

  # yellow tape
  if (current_direction %in% c("north", "south")) {
    ggplot() +
      geom_rect(aes(
        xmin = -.5, ymin = -.075,
        xmax = .5, ymax = .075
      ),
      color = "#EDE24C",
      fill = "#EDE24C"
      ) +
      scale_x_continuous(limits = c(-1, 1)) +
      scale_y_continuous(limits = c(-1, 1)) +
      theme_void() +
      coord_equal()
    ggsave(paste0("signs/yellow_tape_straight_", 
                  current_direction, ".png"),
      height = 1,
      width = 1
    )
  } else {
    ggplot() +
      geom_rect(aes(
        xmin = -.075, ymin = -.5,
        xmax = .075, ymax = .5
      ),
      color = "#EDE24C",
      fill = "#EDE24C"
      ) +
      scale_x_continuous(limits = c(-1, 1)) +
      scale_y_continuous(limits = c(-1, 1)) +
      theme_void() +
      coord_equal()
    ggsave(paste0("signs/yellow_tape_straight_", 
                  current_direction, ".png"),
      height = 1,
      width = 1
    )
  }

  # wait here
  ggplot() +
    geom_polygon(data = square, aes(x = x, y = y), 
                 color = "#4D8235", fill = "#4D8235") +
    geom_text(aes(x = 0, y = 0, label = "WAIT\nHERE"),
      color = "white", size = 6,
      angle = current_angle
    ) +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_y_continuous(limits = c(-1, 1)) +
    theme_void() +
    coord_equal()
  ggsave(paste0("signs/wait_here_straight_", 
                current_direction, ".png"),
    height = 1,
    width = 1
  )

  for (turn in c("right", "left")) {
    ggplot() +
      geom_polygon(data = square, aes(x = x, y = y), 
                   color = "#4D8235", fill = "#4D8235") +
      geom_text(aes(x = 0, y = 0, label = "WAIT\nHERE"),
        color = "white", size = 6,
        angle = current_angle
      ) +
      scale_x_continuous(limits = c(-1, 1)) +
      scale_y_continuous(limits = c(-1, 1)) +
      theme_void() +
      coord_equal()
    ggsave(paste0("signs/wait_here_", turn, "_", 
                  current_direction, ".png"),
      height = 1,
      width = 1
    )
  }

  # wash your hands
  ggplot() +
    geom_polygon(data = circle, aes(x = x, y = y), 
                 color = "#4D4D7A", fill = "#4D4D7A") +
    geom_text(aes(x = 0, y = 0, label = "WASH YOUR\nHANDS"),
      color = "white", size = 3,
      angle = current_angle
    ) +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_y_continuous(limits = c(-1, 1)) +
    theme_void() +
    coord_equal()
  ggsave(paste0("signs/wash_your_hands_straight_", 
                current_direction, ".png"),
    height = 1,
    width = 1
  )

  for (turn in c("right", "left")) {
    ggplot() +
      geom_polygon(data = circle, aes(x = x, y = y), 
                   color = "#4D4D7A", fill = "#4D4D7A") +
      geom_text(aes(x = 0, y = 0, label = "WASH YOUR\nHANDS"),
        color = "white", size = 3,
        angle = current_angle
      ) +
      scale_x_continuous(limits = c(-1, 1)) +
      scale_y_continuous(limits = c(-1, 1)) +
      theme_void() +
      coord_equal()
    ggsave(paste0("signs/wash_your_hands_", turn, "_", 
                  current_direction, ".png"),
      height = 1,
      width = 1
    )
  }

  # wear a mask
  ggplot() +
    geom_polygon(data = circle, aes(x = x, y = y), 
                 color = "#538479", fill = "#538479") +
    geom_text(aes(x = 0, y = 0, label = "WEAR A\nMASK"),
      color = "white", size = 3,
      angle = current_angle
    ) +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_y_continuous(limits = c(-1, 1)) +
    theme_void() +
    coord_equal()
  ggsave(paste0("signs/wear_a_mask_straight_", 
                current_direction, ".png"),
    height = 1,
    width = 1
  )

  for (turn in c("right", "left")) {
    ggplot() +
      geom_polygon(data = circle, aes(x = x, y = y), 
                   color = "#538479", fill = "#538479") +
      geom_text(aes(x = 0, y = 0, label = "WEAR A\nMASK"),
        color = "white", size = 3,
        angle = current_angle
      ) +
      scale_x_continuous(limits = c(-1, 1)) +
      scale_y_continuous(limits = c(-1, 1)) +
      theme_void() +
      coord_equal()
    ggsave(paste0("signs/wear_a_mask_", turn, "_", 
                  current_direction, ".png"),
      height = 1,
      width = 1
    )
  }

  # wait six feet
  ggplot() +
    geom_polygon(data = circle, aes(x = x, y = y), 
                 color = "#54707C", fill = "#54707C") +
    geom_text(aes(x = 0, y = 0, label = "WAIT \nSIX FEET"),
      color = "white", size = 3,
      angle = current_angle
    ) +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_y_continuous(limits = c(-1, 1)) +
    theme_void() +
    coord_equal()
  ggsave(paste0("signs/wait_six_feet_straight_", 
                current_direction, ".png"),
    height = 1,
    width = 1
  )

  for (turn in c("right", "left")) {
    ggplot() +
      geom_polygon(data = circle, aes(x = x, y = y), 
                   color = "#54707C", fill = "#54707C") +
      geom_text(aes(x = 0, y = 0, label = "WAIT \nSIX FEET"),
        color = "white", size = 3,
        angle = current_angle
      ) +
      scale_x_continuous(limits = c(-1, 1)) +
      scale_y_continuous(limits = c(-1, 1)) +
      theme_void() +
      coord_equal()
    ggsave(paste0("signs/wait_six_feet_", turn, "_", 
                  current_direction, ".png"),
      height = 1,
      width = 1
    )
  }

  # 6 feet
  if (current_direction %in% c("north", "south")) {
    ggplot() +
      geom_polygon(data = diamond, aes(x = x, y = y), 
                   color = "#E1AD0F", fill = "#E1AD0F") +
      geom_text(aes(x = 0, y = 0, label = "6 FEET"),
        color = "black", size = 3,
        angle = current_angle
      ) +
      geom_segment(aes(
        x = 0,
        y = c(.25, -.25),
        xend = 0,
        yend = c(.8, -.8)
      ),
      lineend = "butt",
      linejoin = "mitre",
      color = "black",
      arrow = arrow(length = unit(0.05, "npc"), 
                    angle = 45, type = "closed"),
      size = 2
      ) +
      scale_x_continuous(limits = c(-1, 1)) +
      scale_y_continuous(limits = c(-1, 1)) +
      theme_void() +
      coord_equal()
    ggsave(paste0("signs/six_feet_straight_", 
                  current_direction, ".png"),
      height = 1,
      width = 1
    )
  } else {
    ggplot() +
      geom_polygon(data = diamond, aes(x = x, y = y), 
                   color = "#E1AD0F", fill = "#E1AD0F") +
      geom_text(aes(x = 0, y = 0, label = "6 FEET"),
        color = "black", size = 3,
        angle = current_angle
      ) +
      geom_segment(aes(
        x = c(.25, -.25),
        y = 0,
        xend = c(.8, -.8),
        yend = 0
      ),
      lineend = "butt",
      linejoin = "mitre",
      color = "black",
      arrow = arrow(length = unit(0.05, "npc"), 
                    angle = 45, type = "closed"),
      size = 2
      ) +
      scale_x_continuous(limits = c(-1, 1)) +
      scale_y_continuous(limits = c(-1, 1)) +
      theme_void() +
      coord_equal()
    ggsave(paste0("signs/six_feet_straight_", 
                  current_direction, ".png"),
      height = 1,
      width = 1
    )
  }

  # one way
  ggplot() +
    geom_polygon(data = square, aes(x = x, y = y), 
                 color = "#AB4343", fill = "#AB4343") +
    geom_text(aes(x = 0, y = 0, label = "ONE\nWAY"),
      color = "white", size = 6,
      angle = current_angle
    ) +
    scale_x_continuous(limits = c(-1, 1)) +
    scale_y_continuous(limits = c(-1, 1)) +
    theme_void() +
    coord_equal()
  ggsave(paste0("signs/one_way_straight_", 
                current_direction, ".png"),
    height = 1,
    width = 1
  )

  for (turn in c("right", "left")) {
    ggplot() +
      geom_polygon(data = square, aes(x = x, y = y), 
                   color = "#AB4343", fill = "#AB4343") +
      geom_text(aes(x = 0, y = 0, label = "ONE\nWAY"),
        color = "white", size = 6,
        angle = current_angle
      ) +
      scale_x_continuous(limits = c(-1, 1)) +
      scale_y_continuous(limits = c(-1, 1)) +
      theme_void() +
      coord_equal()
    ggsave(paste0("signs/one_way_", turn, "_", 
                  current_direction, ".png"),
      height = 1,
      width = 1
    )
  }
}

# arrow
ggplot() +
  geom_polygon(aes(
    x = c(-.25, .25, .25, .5, 0, -.5, -.25),
    y = c(-.9, -.9, .25, .25, .9, .25, .25)
  ), color = "#444444", fill = "#444444") +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(-1, 1)) +
  theme_void() +
  coord_equal()
ggsave(paste0("signs/arrow_straight_north.png"),
  height = 1,
  width = 1
)
ggsave(paste0("signs/arrow_right_north.png"),
  height = 1,
  width = 1
)
ggsave(paste0("signs/arrow_left_north.png"),
  height = 1,
  width = 1
)

ggplot() +
  geom_polygon(aes(
    x = c(-.25, .25, .25, .5, 0, -.5, -.25),
    y = c(.9, .9, -.25, -.25, -.9, -.25, -.25)
  ), color = "#444444", fill = "#444444") +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(-1, 1)) +
  theme_void() +
  coord_equal()
ggsave(paste0("signs/arrow_straight_south.png"),
  height = 1,
  width = 1
)
ggsave(paste0("signs/arrow_right_south.png"),
  height = 1,
  width = 1
)
ggsave(paste0("signs/arrow_left_south.png"),
  height = 1,
  width = 1
)

ggplot() +
  geom_polygon(aes(
    x = c(-.9, -.9, .25, .25, .9, .25, .25),
    y = c(-.25, .25, .25, .5, 0, -.5, -.25)
  ), color = "#444444", fill = "#444444") +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(-1, 1)) +
  theme_void() +
  coord_equal()
ggsave(paste0("signs/arrow_straight_east.png"),
  height = 1,
  width = 1
)
ggsave(paste0("signs/arrow_right_east.png"),
  height = 1,
  width = 1
)
ggsave(paste0("signs/arrow_left_east.png"),
  height = 1,
  width = 1
)

ggplot() +
  geom_polygon(aes(
    x = c(.9, .9, -.25, -.25, -.9, -.25, -.25),
    y = c(-.25, .25, .25, .5, 0, -.5, -.25)
  ),
  color = "#444444", fill = "#444444"
  ) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(-1, 1)) +
  theme_void() +
  coord_equal()
ggsave(paste0("signs/arrow_straight_west.png"),
  height = 1,
  width = 1
)
ggsave(paste0("signs/arrow_right_west.png"),
  height = 1,
  width = 1
)
ggsave(paste0("signs/arrow_left_west.png"),
  height = 1,
  width = 1
)

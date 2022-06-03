get_image_data <- function(path, structure_parameter) {

  # Set up variables used to determine which images can be used
  path <- path[order(order), ]
  path[, ":="(previous_x = shift(x, type = "lag"),
    previous_y = shift(y, type = "lag"),
    next_x = shift(x, type = "lead"),
    next_y = shift(y, type = "lead"))]

  path[, ":="(course = fifelse(
    previous_x == next_x | previous_y == next_y, "straight",
    fifelse((y > previous_y & next_x > x) |
      (x > previous_x & next_y < y) |
      (y < previous_y & next_x < x) |
      (x < previous_x & next_y > y), "right", "left")
  ),
  direction = fifelse(
    x < next_x, "east",
    fifelse(
      y < next_y, "north",
      fifelse(
        x > next_x, "west",
        "south"
      )
    )
  ))]

  # set up bag to hold the images
  bag_pull <- function(this_course, this_direction, bag) {
    bag[course == this_course & direction == this_direction, 
        ][sample(.N, 1), file]
  }

  # fill bag
  bag <- data.table(file = list.files("signs", full.names = TRUE))
  bag <- bag[file != "signs/do_not_enter_straight_south.png", ]

  bag[, c("sign", "direction") := 
        tstrsplit(gsub("signs/|.png", "", file), "_(?!.*_)", perl = TRUE)]
  bag[, c("sign", "course") := tstrsplit(sign, "_(?!.*_)", perl = TRUE)]
  bag[, ":="(sub_course = fifelse(course == "straight", "straight", "turn"))]

  # filter down to a smaller amount if the structure_parameter is large
  bag_subset <- unique(bag[, c("sign", "sub_course")])
  bag_subset <- unique(bag_subset[
    , .SD[sample(.N, ceiling(
      (1 - (structure_parameter)^(1 / 4)) * (.N - 1) + 1))]
    , by = sub_course][, c("sign", "sub_course")])
  bag <- merge(bag, bag_subset, by = c("sign", "sub_course"))

  # add images from bag
  path[, image := bag_pull(course, direction, bag), by = seq_len(nrow(path))]

  # Start and end
  path[y == 0 & x == 6, image := "signs/wait_here_straight_north.png"]
  path[y == 0 & x == 5, image := "signs/do_not_enter_straight_south.png"]
}

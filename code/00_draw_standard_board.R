## Imports ----
library(tidyverse)
source("./code/utils.R")

## Draw a default board ----
board_df <- create_board() %>%
    add_case(sq_number = 133,
             sq_color = "black",
             sq_move = 1000) %>%
    left_join(return_board_layout()) %>%
    mutate(
        fill_color = case_when(
            sq_color == "red" ~ "firebrick3",
            sq_color == "purple" ~ "purple4",
            sq_color == "yellow" ~ "darkgoldenrod1",
            sq_color == "blue" ~ "dodgerblue3",
            sq_color == "orange" ~ "darkorange",
            sq_color == "green" ~ "darkolivegreen4",
            sq_number == 0 ~ "grey40",
            TRUE ~ "hotpink1"
        ),
        label_text = case_when(
            sq_number == 0 ~ "S",
            sq_number == 133 ~ "E",
            TRUE ~ as.character(sq_number)
        )
    ) %>%
    mutate(fill_color = ifelse(sq_number == 133, "black", fill_color))

p1_board <- ggplot(board_df,
                   aes(
                       x = x,
                       y = y,
                       fill = fill_color,
                       label = label_text
                   )) +
    geom_tile(color = "white",
              size = .8) +
    geom_text(color = "white",
              size = 3.5) +
    ## Bridge from 4 to 60
    geom_curve(
        aes(
            y = 23,
            x = 8,
            yend = 14,
            xend = 1
        ),
        curvature = -.35,
        arrow = arrow(length = unit(.2, "cm"))
    ) +
    ## Bridge from 29 to 41
    geom_curve(
        aes(
            y = 16,
            x = 14,
            yend = 13,
            xend = 13
        ),
        curvature = -.35,
        arrow = arrow(length = unit(.2, "cm"))
    ) +
    ## Licorice at 45
    geom_point(aes(x = 9, y = 12.25),
               shape = 25) +
    ## Licorice at 76
    geom_point(aes(x = 3, y = 7.25),
               shape = 25) +
    coord_equal() +
    theme_void() +
    scale_y_reverse() +
    scale_fill_identity()

ggsave(
    "./plots/board_setup.jpg",
    p1_board,
    width = 3,
    height = 3.5,
    scale = 1.65,
    dpi = 300
)

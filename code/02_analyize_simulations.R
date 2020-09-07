## Imports ----
library(tidyverse)
source('./code/mk_nytimes.R')
source("./code/utils.R")

## Data ----
simulations <- bind_rows(
    readRDS("./simulations/regular_sims.RDS"),
    readRDS("./simulations/forward_only_sims.RDS"),
    readRDS("./simulations/best_choice_sims.RDS"),
    readRDS("./simulations/best_worst_choice_sims.RDS")
) %>% 
    select(game_move, player, move, card, current_pos, sim, sim_type) %>%
    mutate(sim_cat = factor(
        sim_type,
        levels = c("regular", "forward_only", "best_choice", "best_worst"),
        labels = c(
            "Standard",
            "Youngest Forward-Only",
            "All Best Choice",
            "Youngest Best Choice"
        ),
        ordered = TRUE
    )) %>%
    mutate(player_cat = factor(
        player,
        levels = paste0("player_", 1:3),
        labels = c("Player 1\n(youngest)", "Player 2", "Player 3"),
        ordered = TRUE
    ))

## Make a reference board ----
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

## Example of player trajectories ----
trajectories_df <- simulations %>%
    filter(sim %in% 1:20) %>%
    mutate(
        current_pos = ifelse(current_pos == -1, 45, current_pos),
        current_pos = ifelse(current_pos == -2, 76, current_pos),
        current_pos = ifelse(current_pos == 1000, 133, current_pos)
    )

trajectories_df <- trajectories_df %>% 
    bind_rows(
        expand.grid(
            move = 0,
            card = NA,
            current_pos = 0,
            sim = unique(trajectories_df$sim),
            sim_type = unique(trajectories_df$sim_type),
            player = unique(trajectories_df$player),
            stringsAsFactors = FALSE
        )
    ) %>%
    mutate(sim_cat = factor(
        sim_type,
        levels = c("regular", "forward_only", "best_choice", "best_worst"),
        labels = c(
            "Standard",
            "Youngest Forward-Only",
            "All Best Choice",
            "Youngest Best Choice"
        ),
        ordered = TRUE
    )) %>%
    mutate(player_cat = factor(
        player,
        levels = paste0("player_", 1:3),
        labels = c("Player 1\n(youngest)", "Player 2", "Player 3"),
        ordered = TRUE
    ))

p_trajectories <- ggplot(trajectories_df,
       aes(
           x = move,
           y = current_pos,
           group = interaction(player, sim, sim_type),
           color = player_cat
       )) +
    geom_point(
        data = trajectories_df %>%
            filter(current_pos == 133),
        aes(
            x = move,
            y = current_pos,
            group = interaction(player, sim, sim_type),
            color = player_cat
        )
    ) +
    geom_step(alpha = .2) +
    mk_nytimes(legend.position = "none") +
    facet_grid(player_cat ~ sim_cat) +
    scale_x_continuous("Move number", expand = c(0, 0)) +
    scale_y_continuous("Board position",
                       expand = c(0, 0),
                       limits = c(0, 135)) +
    scale_color_brewer(NULL, palette = "Dark2")

## Distribution of number of moves until a win ----
winning_moves <- simulations %>%
    filter(current_pos == 1000)

simulations %>%
    filter(current_pos == 1000) %>%
    group_by(sim_cat) %>%
    summarize(
        mean = mean(move),
        mean_game = mean(game_move),
        sd = sd(move)
    )

p_winning_dist <- ggplot(winning_moves,
       aes(
           x = move,
           color = sim_cat,
           fill = sim_cat,
           group = sim_cat
       )) +
    geom_density(bw = .6, alpha = .3, size = .8) +
    mk_nytimes(legend.position = c(.99, .99),
               legend.justification = c(1, 1)) +
    scale_color_brewer("Rule set", palette = "Set1") +
    scale_fill_brewer("Rule set", palette = "Set1") +
    scale_x_continuous("Number of moves for winning player",
                       expand = c(0, 0)) +
    scale_y_continuous(NULL, expand = c(0, 0))

winning_moves %>%
    group_by(sim_cat) %>%
    summarize(
        mean = mean(move),
        median = median(move),
        p99 = quantile(move, .99),
        p90 = quantile(move, .90),
        sd = sd(move)
    ) %>%
    arrange(sim_cat)

## Proportion of games won by player ----
prop_wins <- simulations %>%
    filter(current_pos == 1000) %>%
    count(sim_cat, player) %>%
    group_by(sim_cat) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup() %>%
    mutate(player_cat = factor(
        player,
        levels = paste0("player_", 1:3),
        labels = c("Player 1\n(youngest)", "Player 2", "Player 3"),
        ordered = TRUE
    ))

p_win_props <- ggplot(prop_wins, 
       aes(x = player_cat, y = prop, color = player_cat, fill = player_cat)) + 
    geom_col() +
    facet_wrap( ~ sim_cat) + 
    mk_nytimes(legend.position = "none") + 
    scale_x_discrete(NULL, expand = c(0, 0)) + 
    scale_y_continuous("Proportion of games won", expand = c(0, 0)) + 
    scale_color_brewer(NULL, palette = "Dark2") +
    scale_fill_brewer(NULL, palette = "Dark2")

## Number of moves vs proportion of games won ----
p1_wins_prop <- simulations %>%
    filter(current_pos == 1000) %>%
    group_by(move, sim_type, sim_cat) %>% 
    summarize(
        n_matches = n(), 
        n_p1_wins = sum(player == "player_1"),
        n_p2_wins = sum(player == "player_2"),
        n_p3_wins = sum(player == "player_3")
    ) %>%
    ungroup() %>% 
    mutate(prop_p1_wins = n_p1_wins / n_matches)

p_wins_vs_moves <- ggplot(
    p1_wins_prop,
    aes(
        x = move,
        y = prop_p1_wins,
        color = sim_cat,
        fill = sim_cat,
        size = n_matches,
        weight = n_matches,
    )
) +
    geom_ribbon(stat = "smooth",
                alpha = .2,
                color = NA) +
    geom_line(
        stat = "smooth",
        alpha = 1,
        color = "white",
        size = 1.5,
    ) +
    geom_line(stat = "smooth",
              alpha = .75,
              size = 1) +
    geom_point(alpha = .75) +
    mk_nytimes(
        legend.direction = "vertical",
        legend.position = "bottom",
        legend.box = "horizontal"
    ) +
    scale_size_area("Number of simulations") +
    scale_color_brewer("Rule set", palette = "Set1") +
    scale_fill_brewer("Rule set", palette = "Set1") +
    scale_x_continuous("Number of moves for winning player",
                       expand = c(0, 0)) +
    scale_y_continuous("Proportion of games won by Player 1 (youngest)",
                       expand = c(0, .01)) +
    coord_cartesian(ylim = c(0, 1))

## Location after set number of moves ----
position_df <- expand.grid(
    sim_type = unique(simulations$sim_type),
    current_pos = 0:133,
    move = c(1, 3, 7),
    stringsAsFactors = FALSE
)

position_df <- position_df %>%
    left_join(
        simulations %>%
            filter(player == "player_1",
                   move %in% c(1, 3, 7)) %>%
            mutate(
                current_pos = ifelse(current_pos == -1, 45, current_pos),
                current_pos = ifelse(current_pos == -2, 76, current_pos)
            ) %>%
            count(sim_type, current_pos, move)
    )

position_df <- position_df %>%
    mutate(sim_cat = factor(
        sim_type,
        levels = c("regular", "forward_only", "best_choice", "best_worst"),
        labels = c(
            "Standard",
            "Youngest\nForward-Only",
            "All Best\nChoice",
            "Youngest\nBest Choice"
        ),
        ordered = TRUE
    ))

position_df <- position_df %>%
    mutate(
        current_pos = ifelse(current_pos == 1000, 133, current_pos)
    ) %>%
    group_by(sim_cat, move) %>%
    mutate(prop = n / sum(n, na.rm = TRUE) * 100) %>%
    left_join(board_df,
              by = c("current_pos" = "sq_number"))

p_positions_by_rules <- ggplot(position_df,
       aes(
           x = x,
           y = y,
           fill = prop,
           label = label_text
       )) +
    geom_tile(color = "black") +
    scale_fill_viridis_c(
        "Probability of being in this location (%)",
        na.value = "white",
        trans = "log1p",
        direction = -1,
        guide = guide_colorbar(
            barheight = unit(.5, "cm"),
            barwidth = unit(10, "cm"),
            title.position = "top"
        )
    ) +
    coord_equal() +
    mk_nytimes(
        panel.grid.major = element_blank(),
        legend.position = "bottom",
        axis.text = element_blank()
    ) +
    scale_y_reverse(NULL) +
    scale_x_continuous(NULL) +
    facet_grid(move ~ sim_cat)

## Transition matrix for each simulation type ----
transitions <- simulations %>%
    group_by(sim_type, sim, sim_cat, player) %>% 
    mutate(previous_pos = lag(current_pos, default = 0)) %>%
    mutate(
        current_pos = ifelse(current_pos == 1000, 133, current_pos),
        current_pos = ifelse(current_pos == -1, 45, current_pos),
        current_pos = ifelse(current_pos == -2, 76, current_pos)
    ) %>%
    mutate(
        previous_pos = ifelse(previous_pos == -1, 45, previous_pos),
        previous_pos = ifelse(previous_pos == -2, 76, previous_pos)
    )

transitions <- transitions %>% 
    group_by(sim_type, sim_cat, current_pos, previous_pos) %>% 
    summarize(n = n())

transitions <- transitions %>% 
    group_by(sim_type, sim_cat, previous_pos) %>% 
    mutate(prop = n / sum(n)) %>% 
    ungroup()

p_transitions <- ggplot(transitions %>% 
                            filter(current_pos < 133),
       aes(x = current_pos, y = previous_pos, fill = prop)) +
    geom_tile() +
    facet_wrap(~ sim_cat) +
    coord_equal() +
    mk_nytimes(legend.position = "bottom") +
    scale_fill_viridis_c(
        "Probability of transition from i to j", 
        trans = "sqrt",
        direction = -1,
        option = "A",
        guide = guide_colorbar(
            barheight = unit(.5, "cm"),
            barwidth = unit(10, "cm"),
            title.position = "top"
        )
    ) + 
    scale_y_continuous("Position i",
                       expand = c(0, 0)) + 
    scale_x_continuous("Position j",
                       expand = c(0, 0))

## Distribution of move lengths
# move_lengths <- simulations %>%
#     filter(current_pos != 1000) %>%
#     mutate(
#         current_pos = ifelse(current_pos == -1, 45, current_pos),
#         current_pos = ifelse(current_pos == -2, 76, current_pos)
#     ) %>%
#     group_by(sim_type, sim, sim_cat, player, player_cat) %>%
#     mutate(previous_pos = lag(current_pos, default = 0)) %>%
#     mutate(move_length = current_pos - previous_pos) %>%
#     ungroup()
# 
# ggplot(move_lengths,
#        aes(x = move_length, fill = player_cat)) +
#     geom_density(color = NA, alpha = .9, bw = .5) + 
#     facet_grid(player_cat ~ sim_cat, scales = "free_y") + 
#     mk_nytimes(legend.position = "none") + 
#     scale_color_brewer(NULL, palette = "Dark2") + 
#     scale_y_continuous(expand = c(0, 0))

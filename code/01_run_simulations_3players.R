## Imports ----
library(tidyverse)
library(doParallel)
library(foreach)
source("./code/utils.R")

## Constants ----
N_SIMS <- 100000
GAME_BOARD <- create_board()
N_CORES <- 12

## Simulate regular games with 3 players ----
if (!file.exists("./simulations/regular_sims.RDS")) {
    doParallel::registerDoParallel(cores = N_CORES)
    regular_sims <- foreach(i = 1:N_SIMS, .inorder = FALSE) %dopar% {
        play_game_regular(n_players = 3,
                          game_board = GAME_BOARD) %>%
            mutate(sim = i,
                   sim_type = "regular")
    }
    doParallel::stopImplicitCluster()
    regular_sims <- bind_rows(regular_sims)
    saveRDS(regular_sims,
            "./simulations/regular_sim.RDS",
            compress = "xz")
}

## Simulate forward-only with 3 players ----
if (!file.exists("./simulations/forward_only_sims.RDS")) {
    doParallel::registerDoParallel(cores = N_CORES)
    forward_only_sims <- foreach(i = 1:N_SIMS, .inorder = FALSE) %dopar% {
        play_game_forward_only(n_players = 3,
                               game_board = GAME_BOARD) %>%
            mutate(sim = i,
                   sim_type = "forward_only")
    }
    doParallel::stopImplicitCluster()
    forward_only_sims <- bind_rows(forward_only_sims)
    saveRDS(forward_only_sims,
            "./simulations/forward_only_sims.RDS",
            compress = "xz")
}

## Simulate best choice with 3 players ----
if (!file.exists("./simulations/best_choice_sims.RDS")) {
    doParallel::registerDoParallel(cores = N_CORES)
    best_choice_sims <- foreach(i = 1:N_SIMS, .inorder = FALSE) %dopar% {
        play_game_best_choice(n_players = 3,
                              game_board = GAME_BOARD) %>%
            mutate(sim = i,
                   sim_type = "best_choice")
    }
    doParallel::stopImplicitCluster()
    best_choice_sims <- bind_rows(best_choice_sims)
    saveRDS(best_choice_sims,
            "./simulations/best_choice_sims.RDS",
            compress = "xz")
}

## Simulate best-worst choice with 3 players ----
if (!file.exists("./simulations/best_worst_choice_sims.RDS")) {
    doParallel::registerDoParallel(cores = N_CORES)
    best_worst_sims <- foreach(i = 1:N_SIMS, .inorder = FALSE) %dopar% {
        play_game_best_worst_choice(n_players = 3,
                                    game_board = GAME_BOARD) %>%
            mutate(sim = i,
                   sim_type = "best_worst")
    }
    doParallel::stopImplicitCluster()
    best_worst_sims <- bind_rows(best_worst_sims)
    saveRDS(best_worst_sims,
            "./simulations/best_worst_choice_sims.RDS",
            compress = "xz")
}

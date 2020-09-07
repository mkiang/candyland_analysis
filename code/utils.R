return_board_layout <- function() {
    ## Note, you can modify/replicate this layout by running the following code
    ## after modifying ./data/board_layout.csv:
    ## 
    ##    dput(read_csv("./data/board_layout.csv") %>% 
    ##             gather(x, sq_number, `1`:`18`) %>% 
    ##             rename(y = X1) %>% 
    ##             filter(!is.na(sq_number)) %>% 
    ##             mutate(x = as.numeric(x)) %>% 
    ##             arrange(sq_number))
    
    structure(list(
        y = c(23, 23, 23, 23, 23, 22, 21, 20, 20, 20, 
              21, 22, 22, 22, 22, 22, 22, 22, 21, 20, 19, 19, 19, 19, 19, 18, 
              17, 16, 16, 16, 16, 16, 16, 16, 15, 14, 13, 13, 13, 13, 13, 13, 
              13, 13, 13, 13, 14, 15, 16, 17, 17, 17, 17, 17, 17, 17, 17, 17, 
              16, 15, 14, 14, 14, 14, 14, 14, 13, 12, 11, 11, 11, 11, 11, 10, 
              9, 8, 8, 8, 8, 8, 8, 8, 9, 10, 11, 11, 11, 11, 11, 11, 11, 11, 
              11, 11, 10, 9, 8, 7, 6, 6, 6, 5, 4, 3, 3, 3, 3, 4, 4, 5, 5, 5, 
              5, 5, 5, 6, 6, 6, 6, 6, 5, 4, 3, 2, 1, 1, 1, 1, 2, 3, 3, 3, 3
        ), 
        x = c(4, 5, 6, 7, 8, 8, 8, 8, 9, 10, 10, 10, 11, 12, 13, 14, 
              15, 16, 16, 16, 16, 15, 14, 13, 12, 12, 12, 12, 13, 14, 15, 16, 
              17, 18, 18, 18, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 9, 9, 
              9, 9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 2, 3, 4, 5, 6, 6, 6, 6, 
              5, 4, 3, 2, 2, 2, 2, 3, 4, 5, 6, 7, 8, 8, 8, 8, 9, 10, 11, 12, 
              13, 14, 15, 16, 17, 17, 17, 17, 17, 17, 16, 15, 15, 15, 15, 14, 
              13, 12, 12, 11, 11, 10, 9, 8, 7, 6, 6, 5, 4, 3, 2, 2, 2, 2, 2, 
              2, 3, 4, 5, 5, 5, 6, 7, 8), 
        sq_number = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 
                      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 
                      30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 
                      44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 
                      58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 
                      72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 
                      86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 
                      100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 
                      111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 
                      123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133)), 
        row.names = c(NA, -133L), 
        class = c("tbl_df", 
                  "tbl", "data.frame"))
}

create_deck <- function(n_decks = 1, shuffled = TRUE) {
    ## Modify this table if you've lost some cards
    cards <- tribble(
        ~color,    ~moves, ~cards,
        "purple",       2,      4,
        "purple",       1,      5,
        "orange",       2,      3,
        "orange",       1,      6,
        "red",          2,      4,
        "red",          1,      6,
        "yellow",       2,      4,
        "yellow",       1,      6,
        "green",        2,      3,
        "green",        1,      6,
        "blue",         2,      4, 
        "blue",         1,      6,
        "cupcake",      1,      1,
        "icecream",     1,      1,
        "gummystar",    1,      1,
        "gingerbread",  1,      1,
        "lollipop",     1,      1,
        "popsicle",     1,      1,
        "chocolate",    1,      1
        )
    
    deck <- cards %>% 
        unite(card_name, color, moves) %>% 
        uncount(weights = cards) %>% 
        pull(card_name) 
    
    if (shuffled) {
        as.vector(replicate(n_decks, sample(deck), simplify = TRUE))
    } else {
        rep(deck, n_decks)
    }
}

create_board <- function() {
    ## Base pattern for the board
    c_pattern <- c("red", "purple", "yellow", "blue", "orange", "green")
    board <- rep(c_pattern, 21)
    
    ## Insert the named spots (
    ## Note: The location of each spot on the board is x + 1.
    board <- append(board, "cupcake", 8)
    board <- append(board, "icecream", 19)
    board <- append(board, "gummystar", 41)
    board <- append(board, "gingerbread", 68)
    board <- append(board, "lollipop", 91)
    board <- append(board, "popsicle", 101)
    board[117] <- "chocolate" ## This replaces a yellow
    
    ## Add a starting square
    board <- c("start", board)
    
    ## Prepend two "holding" spots for when you jump on the licorice (45, 76)
    board <- c("licorice_1", "licorice_2", board)
    
    ## Append each color twice to a final "finished" space 
    ## NOTE: we add it twice so if you get a "move 2 X" at the end, you'll
    ## fall into one of the "finished" spaces. 
    board <- c(board, c_pattern, c_pattern)
    
    tibble(sq_number = -2:(NROW(board) - 3),
           sq_color = board) %>%
        mutate(
            sq_move = case_when(
                ## Add bridges
                sq_number == 4 ~ 60L,
                sq_number == 29 ~ 41L,
                
                ## Add skipped turns
                sq_number == 45 ~ -1L,
                sq_number == 76 ~ -2L,
                
                ## After skipping a turn, go back to original
                sq_number == -1L ~ 45L,
                sq_number == -2L ~ 76L,
                
                ## Finished!
                sq_number > 132 ~ 1000L, 
                TRUE ~ sq_number
            ),
            sq_number = ifelse(sq_number > 132, 1000, sq_number)
        )
}

draw_cards <- function(n_players = 3) {
        shuffled_deck <- create_deck(n_decks = 5 * n_players)
    
    draws <- tibble(
        game_move = 1:NROW(shuffled_deck),
        move = rep(1:(NROW(shuffled_deck) / n_players), each = n_players),
        player = rep(paste0("player_", 1:n_players), 
                     NROW(shuffled_deck) / n_players),
        card = shuffled_deck
    )
    
    draws
}

draw_cards_double <- function(n_players = 3) {
    ## In real life, we would pull two cards in a row so we can't use two
    ## independent decks. Instead, we draw from one deck, twice as many times.
    shuffled_deck <- create_deck(n_decks = 5 * n_players * 2)
    
    draws <- tibble(
        game_move = 1:(NROW(shuffled_deck) / 2),
        move = rep(1:(NROW(shuffled_deck) / n_players / 2), each = n_players),
        player = rep(paste0("player_", 1:n_players), 
                     NROW(shuffled_deck) / n_players / 2),
        card1 = shuffled_deck[c(TRUE, FALSE)],
        card2 = shuffled_deck[c(FALSE, TRUE)]
    )
    
    draws
}

play_card <- function(card, 
                      current_pos = 0, 
                      forward_only = FALSE, 
                      game_board = NA) {
    if (is.na(game_board)[1]) {
        board <- create_board()
    } else {
        board <- game_board
    }

    card_color <- strsplit(card, "_")[[1]][1]
    card_num <- as.numeric(strsplit(card, "_")[[1]][2])
    
    ## Next move by color or special card
    if (card_color %in% c("red", "purple", "yellow", "blue", "orange", "green")) {
        next_move <- board %>%
            filter(sq_number > current_pos,
                   sq_color == card_color) %>%
            slice(card_num) %>%
            pull(sq_move)
    } else {
        next_move <- board %>%
            filter(sq_color == card_color) %>%
            pull(sq_move)
    }
        
    ## If special cards can only send you forward
    if (forward_only) {
        next_move <- max(next_move, current_pos)
    }
    
    next_move
}

play_best_card <- function(card1,
                           card2,
                           current_pos = 0,
                           forward_only = FALSE,
                           game_board = NA) {
    card1_position <- play_card(card1,
                                current_pos = current_pos,
                                forward_only = forward_only,
                                game_board = game_board)
    card2_position <- play_card(card2,
                                current_pos = current_pos,
                                forward_only = forward_only,
                                game_board = game_board)
    
    cards <- c(card1_position, card2_position)
    names(cards) <- c(card1, card2)
    sort(cards, decreasing = TRUE)[1]
}

play_worst_card <- function(card1,
                            card2,
                            current_pos = 0,
                            forward_only = FALSE,
                            game_board = game_board) {
    card1_position <- play_card(card1,
                                current_pos = current_pos,
                                forward_only = forward_only,
                                game_board = game_board)
    card2_position <- play_card(card2,
                                current_pos = current_pos,
                                forward_only = forward_only,
                                game_board = game_board)
    
    cards <- c(card1_position, card2_position)
    names(cards) <- c(card1, card2)
    sort(cards)[1]
}

play_game_regular <- function(n_players = 3, game_board = NA) {
    draws <- draw_cards(n_players) %>% 
        mutate(current_pos = NA)
    
    MAX_POSITION <- 0
    i <- 0
    
    while (MAX_POSITION < 1000) {
        i <- i + 1
        
        if (i <= n_players) {
            current_p <- 0
        } else {
            current_p <- draws$current_pos[i - n_players]
        }
        draws$current_pos[i] <- play_card(draws$card[i],
                                          current_pos = current_p,
                                          game_board = game_board)
        
        MAX_POSITION <- max(draws$current_pos, na.rm = TRUE)
    }
    
    draws %>% 
        filter(!is.na(current_pos)) %>% 
        arrange(player, move) 
}

play_game_forward_only <- function(n_players = 3, game_board = NA) {
    draws <- draw_cards(n_players) %>% 
        mutate(current_pos = NA)
    
    MAX_POSITION <- 0
    i <- 0
    
    while (MAX_POSITION < 1000) {
        i <- i + 1
        
        if (i <= n_players) {
            current_p <- 0
        } else {
            current_p <- draws$current_pos[i - n_players]
        }
        
        if (draws$player[i] == "player_1") {
            draws$current_pos[i] <- play_card(draws$card[i],
                                              current_pos = current_p, 
                                              forward_only = TRUE,
                                              game_board = game_board)
        } else {
            draws$current_pos[i] <- play_card(draws$card[i],
                                              current_pos = current_p, 
                                              forward_only = FALSE,
                                              game_board = game_board)
        }
        
        MAX_POSITION <- max(draws$current_pos, na.rm = TRUE)
    }
    
    draws %>% 
        arrange(player, move) %>% 
        filter(!is.na(current_pos))
}

play_game_best_choice <- function(n_players = 3, game_board = NA) {
    draws <- draw_cards_double(n_players) %>% 
        mutate(current_pos = NA, 
               card = NA)
    
    MAX_POSITION <- 0
    i <- 0
    
    while (MAX_POSITION < 1000) {
        i <- i + 1
        
        if (i <= n_players) {
            current_p <- 0
        } else {
            current_p <- draws$current_pos[i - n_players]
        }
        best_card <- play_best_card(draws$card1[i],
                                    draws$card2[i],
                                    current_pos = current_p, 
                                    forward_only = TRUE,
                                    game_board = game_board)
        draws$current_pos[i] <- unname(best_card)
        draws$card[i] <- names(best_card)
        
        MAX_POSITION <- max(draws$current_pos, na.rm = TRUE)
    }
    
    draws %>% 
        filter(!is.na(current_pos)) %>% 
        arrange(player, move) 
}

play_game_best_worst_choice <- function(n_players = 3, game_board = NA) {
    draws <- draw_cards_double(n_players) %>% 
        mutate(current_pos = NA,
               card = NA)
    
    MAX_POSITION <- 0
    i <- 0
    
    while (MAX_POSITION < 1000) {
        i <- i + 1
        
        if (i <= n_players) {
            current_p <- 0
        } else {
            current_p <- draws$current_pos[i - n_players]
        }
        
        if (draws$player[i] == "player_1") {
            best_card <- play_best_card(draws$card1[i],
                                        draws$card2[i],
                                        current_pos = current_p,
                                        forward_only = TRUE,
                                        game_board = game_board)
            draws$current_pos[i] <- unname(best_card)
            draws$card[i] <- names(best_card)
        } else {
            worst_card <- play_worst_card(draws$card1[i],
                                          draws$card2[i],
                                        current_pos = current_p,
                                        forward_only = FALSE,
                                        game_board = game_board)
            draws$current_pos[i] <- unname(worst_card)
            draws$card[i] <- names(worst_card)
        }
        
        MAX_POSITION <- max(draws$current_pos, na.rm = TRUE)
    }
    
    draws %>% 
        arrange(player, move) %>% 
        filter(!is.na(current_pos))
}

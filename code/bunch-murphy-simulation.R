# Bunch and Murphy simulation

# Packet 1 contains 10 fires from the same weapon
# Packet 2 contains 1 from each of the other 9 glocks, plus one non-glock
# Packets 3-8 contain
#    - 0, 1, or 2 non-glocks
#    - 10, 9, or 8 randomly sampled from each of the other glocks

# After packet 2 is assembled, then, the following cartridges remain:

bunch_test_kits <- function() {
  # Accounting for packet 2
  unknown_2 <- sample(c("S", "B"), size = 1)
  unknown_left <- c(rep(unknown_2, 9), rep(setdiff(c("S", "B"), unknown_2), 10))
  
  # Number of unknowns
  n_unk <- purrr::map_int(3:8, ~sample(0:2, 1))
  n_k <- 10 - n_unk
  
  unk_order <- sample(size = 19, unknown_left, replace = F)
  known_order <- sample(size = 81, rep(2:10, each = 9)) %>% as.character()
  
  kits <- purrr::map(1:8, 
                     function(x) {
                       if (x < 7) {
                         start_unk <- ifelse(x == 1, 1, sum(n_unk[1:(x-1)]) + 1)
                         idxs <- if(n_unk[x] > 0) start_unk:(start_unk + n_unk[x] - 1) else NULL
                         # print(idxs)
                         unk <- unk_order[idxs]
                         start_k <- ifelse(x == 1, 1, sum(n_k[1:(x-1)]) + 1)
                         idxk <- start_k:(start_k + n_k[x] - 1)
                         res <- c(unk, known_order[idxk])
                       } else if (x == 7) {
                         res <- as.character(rep(1, times = 10))
                       } else {
                         res <- c(2:10, unknown_2)
                       }
                       
                       sample(res, size = 10)
                     })
}

prev_exists <- function(idx, x) {
  if (idx < 10) return(FALSE)
  x[idx] %in% unique(x[1:(idx - 1)])
}

bunch_eval <- function(set) {
  idx <- 1:length(set)
  setf <- factor(set, levels = unique(set), ordered = T)
  comparisons <- crossing(i1 = idx, i2 = idx) %>%
    # This is the max number of comparisons
    filter(i1 < i2) %>% 
    mutate(set1 = setf[i1], set2 = setf[i2],
           f1 = as.numeric(set1), f2 = as.numeric(set2),
           idx = row_number(),
           match = set1 == set2,
           orderedf = map2_chr(as.numeric(set1), as.numeric(set2), 
                               function(x, y) sort(unique(c(x, y))) %>% paste(collapse = ",")),
           max_match = ifelse(match, f1, 0) %>% cummax %>% pmin(., i1 - 1),
           compared_before = map_lgl(idx, partial(prev_exists, x = orderedf))
    ) %>%
    group_by(i1) %>%
    mutate(min_match = min(max_match)) %>%
    ungroup() %>%
    mutate(full_row = pmin(f1, f2) <= min_match,
           inferred = full_row * compared_before)
 
  comparisons %>%
    summarize(matches_all = sum(match), matches_min = sum(match[!inferred]),
              nonmatches_all = sum(!match), nonmatches_min = sum(!match[!inferred]),
              optional = sum(inferred))
}

res <- purrr::map_df(1:1000, function(i) {
    tibble(kit = bunch_test_kits()) %>%
      mutate(eval = purrr::map(kit, bunch_eval)) %>%
      unnest(eval) %>%
      summarize(iteration = i, 
                all_pairwise_matches = sum(matches_all), min_pairwise_matches = sum(matches_min), 
                all_pairwise_nonmatches = sum(nonmatches_all), min_pairwise_nonmatches = sum(nonmatches_min))
    
  })

res %>%
  select(-iteration) %>%
  pivot_longer(cols = 1:4, names_to = "type", values_to = "comparisons") %>%
  extract(type, into = c("strategy", "source"), "(all|min)_pairwise_(matches|nonmatches)") %>%
  mutate(source = str_replace_all(source, c("^matches" = "same source", "nonmatches" = "different source"))) %>%
  ggplot(aes(x = comparisons)) + geom_histogram() + facet_grid(strategy~source, scales = "free_x")
res %>%
  # select(-iteration) %>%
  pivot_longer(cols = 2:5, names_to = "type", values_to = "comparisons") %>%
  extract(type, into = c("strategy", "source"), "(all|min)_pairwise_(matches|nonmatches)") %>%
  mutate(source = str_replace_all(source, c("^matches" = "same source", "nonmatches" = "different source"))) %>%
  pivot_wider(id_cols = c(iteration, strategy), names_from = source, values_from = comparisons) %>%
  filter(strategy == "min") %>%
  ggplot(aes(x = `same source`, y = `different source`)) + geom_jitter() + coord_fixed(ratio = .2) + 
  scale_y_continuous(breaks = seq(150, 275, by = 5)) 

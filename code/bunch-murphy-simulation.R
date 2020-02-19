# Bunch and Murphy simulation
library(tidyverse)
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
           # Set of ordered pairwise comparisons (by source)
           orderedf = map2_chr(as.numeric(set1), as.numeric(set2), 
                               function(x, y) sort(unique(c(x, y))) %>% paste(collapse = ",")),
           # Calculate the highest source-source match found
           max_match = ifelse(match, f1, 0) %>% cummax %>% pmin(., i1 - 1),
           # has the orderedf comparison been performed before?
           compared_before = map_lgl(idx, partial(prev_exists, x = orderedf))
    ) %>%
    # Once a new source-source match is found, you still have to finish out the row
    # with new comparisons, so group by row and take the min value
    group_by(i1) %>%
    mutate(min_match = min(max_match)) %>%
    ungroup() %>%
    mutate(full_row = pmin(f1, f2) <= min_match,
           # If you've completed the full row of comparisons for that match
           # and you have made the comparison before, you can infer the result
           # from previous work
           inferred = full_row * compared_before)
 
  comparisons %>%
    summarize(matches_all = sum(match), matches_min = sum(match[!inferred]),
              nonmatches_all = sum(!match), nonmatches_min = sum(!match[!inferred]),
              optional = sum(inferred))
}

res <- purrr::map_df(1:10000, function(i) {
    tibble(kit = bunch_test_kits()) %>%
      mutate(eval = purrr::map(kit, bunch_eval)) %>%
      unnest(eval) %>%
      summarize(iteration = i, 
                all_pairwise_matches = sum(matches_all), min_pairwise_matches = sum(matches_min), 
                all_pairwise_nonmatches = sum(nonmatches_all), min_pairwise_nonmatches = sum(nonmatches_min))
    
  })

save(res, file = here::here("data/bunch-murphy-results.Rdata"))

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
  mutate(source = str_replace_all(source, c("^matches" = "same source", "nonmatches" = "different source")),
         strategy = str_replace_all(strategy, c("all" = "All possible", "min" = "Minimum possible"))) %>%
  pivot_wider(id_cols = c(iteration, strategy), names_from = source, values_from = comparisons) %>%
  ggplot(aes(x = `same source`, y = `different source`, color = strategy)) + 
  geom_jitter(alpha = .1) + 
  scale_color_manual("Examination\nStrategy", values = c("purple4", "orange3")) + 
  guides(color = guide_legend(override.aes = list(alpha = 1))) + 
  ylab("Different Source Comparisons") + 
  xlab("Same Source Comparisons") + 
  ggtitle("Bunch and Murphy # Comparisons\nDistribution Breakdown") + 
  coord_fixed(ratio = .5) + 
  theme_bw() + 
  theme(legend.position = c(1, 0), legend.justification = c(1.02, -.02))

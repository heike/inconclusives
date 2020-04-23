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

fbi_elim_calc <- function(x, y) {
  x1 <- ifelse(x %in% c("S", "B"), x, "A")
  y1 <- ifelse(y %in% c("S", "B"), y, "A")
  x1 != y1
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
           fbi_elim = map2_lgl(set1, set2, fbi_elim_calc),
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
              fbi_elim_all = sum(fbi_elim), fbi_elim_min = sum(fbi_elim[!inferred]),
              optional = sum(inferred))
}

res <- purrr::map_df(1:500000, function(i) {
    tibble(kit = bunch_test_kits()) %>%
      mutate(eval = parallel::mclapply(kit, bunch_eval)) #%>%
      # unnest(eval) %>%
      # summarize(iteration = i, 
      #           all_pairwise_matches = sum(matches_all), min_pairwise_matches = sum(matches_min), 
      #           all_pairwise_nonmatches = sum(nonmatches_all), min_pairwise_nonmatches = sum(nonmatches_min))
    
  })
res <- unnest(res, eval)

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

res
res %>%
  mutate(total_comparisons = all_pairwise_matches + all_pairwise_nonmatches, min_comparisons = min_pairwise_matches + min_pairwise_nonmatches)

# res2 <- res %>% rename(matches = matches_all, matches_indep = matches_min, nonmatches = nonmatches_all, nonmatches_indep = nonmatches_min, fbi_elim = fbi_elim_all, fbi_elim_indep = fbi_elim_min, optional = optional)

load("data/bunch-murphy-15000-sim.Rdata") # res2

# Add in FBI rules - can't exclude on class characteristic matches

res3 <-res2[(0:(nrow(res2)-1))%%8 <=5, ] #%>% unnest(eval)
res3 <- rename(res3, matches_indep = matches_min, nonmatches_indep = nonmatches_min)
res3$set <- floor((1:nrow(res3) - 1)/6)

res3 %>% group_by(set) %>% select(-1) %>% summarize_all(sum) %>% filter(matches_all == 25) -> our_res
res3[res3$set == 1, ]$kit
our_res %>% select(-set) %>% select(matches_indep, nonmatches_indep) %>% summarize_each(list(~quantile(., c(0.025)), ~mean(.), ~quantile(., .975)))


res3 %>% group_by(set) %>% select(-1) %>% summarize_all(sum) %>% filter(matches_all == 30) -> our_res
res3[res3$set == 1, ]$kit
our_res %>% select(-set) %>% select(matches_indep, nonmatches_indep) %>% summarize_each(list(~quantile(., c(0.025)), ~mean(.), ~quantile(., .975)))

# 95% bootstrap interval for # required DS comparisons by # reported SS comparisons
res3 %>% group_by(set) %>% select(-1) %>% summarize_all(sum) %>% filter(matches_all < 30, matches_all > 20) %>%
  group_by(matches_all) %>% 
  summarize(lb = quantile(nonmatches_indep, 0.025), mean = mean(nonmatches_indep), ub = quantile(nonmatches_indep, 0.975)) %>%
  ggplot(aes(x = matches_all)) + 
  geom_point(aes(y = mean)) + 
  geom_segment(aes(x = matches_all, xend = matches_all, y = lb, yend = ub)) + 
  scale_x_continuous(breaks = seq(20, 30, 2))

res3 %>% group_by(set) %>% select(-1) %>% summarize_all(sum) %>% filter(matches_all == 25) %>%
  select(matches_indep, nonmatches_indep) %>% summarize_each(list(~quantile(., c(0.025)), ~mean(.), ~quantile(., .975))) %>%
  select(matches("nonmatches"), matches("^matches")) %>%
  pivot_longer(cols = 1:6, names_to = "match_type", values_to = "value") %>%
  separate(match_type, into = c("match", "val"), sep = "_indep_") %>%
  pivot_wider(id_cols = "match", names_from = "val", values_from = "value")

## How to handle distribution of inconclusives and eliminations? Assume same fraction? 
## Allocate based on fraction of redundant comparisons that meet FBI definitions?

compute_consec_comparisons <- function(x) {
  y <- suppressWarnings(as.numeric(x))
  yidx <- which(!is.na(y))
  tmp <- expand.grid(i = yidx, j = yidx) 
  tmp2 <- tmp[tmp$i > tmp$j,]
  tmp2$i <- y[tmp2$i]
  tmp2$j <- y[tmp2$j]
  sum(abs(tmp2$i - tmp2$j) == 1)
  # crossing(i = which(!is.na(y)), j = which(!is.na(y))) %>% 7
  #   filter(i > j) %>%
  #   mutate(i = y[i], j = y[j]) %>%
  #   mutate_each(as.numeric) %>% 
  #   na.omit() %>%
  #   mutate(diff = abs(i - j)) %>%
  #   filter(diff == 1) %>%
  #   count() %>%
  #   as.numeric()
}

res2 <- res %>% 
  mutate(sim_num = rep(1:500000, each = 8),
         kit_num = rep(1:8, times = 500000)) %>%
  mutate(consec_manuf = purrr::map_dbl(kit, compute_consec_comparisons))

res3 <- res2 %>%
  # select(kit_num <= 5) %>% # Select kits that don't have all matches or all nonmatches
  group_by(sim_num) %>% 
  select(-kit, -kit_num) %>%
  summarise_all(sum) %>% 
  filter(matches_all == 70, nonmatches_all == 290, consec_manuf == 42) %>%
  select(sim_num) %>% 
  left_join(res2) %>% # Keep only trials with 70 min matches, 290 nonmatches, and 
                      # 42 consecutively manufactured comparisons
  select(sim_num, kit_num, kit) %>%
  mutate(eval = purrr::map(kit, bunch_eval)) %>%
  unnest(eval)

res3 %>% group_by(sim_num) %>% select(-kit) %>% summarize_all(sum) %>%
  select(matches_min, nonmatches_min, fbi_elim_min) %>% 
  summarize_each(list(~quantile(., c(0.025)), ~mean(.), ~quantile(., .975))) %>%
  select(matches("nonmatches"), matches("^matches"), matches("^fbi")) %>%
  pivot_longer(cols = 1:9, names_to = "match_type", values_to = "value") %>%
  separate(match_type, into = c("match", "val"), sep = "_min_") %>%
  pivot_wider(id_cols = "match", names_from = "val", values_from = "value")

res4 <- res2 %>%
  # select(kit_num <= 5) %>% # Select kits that don't have all matches or all nonmatches
  group_by(sim_num) %>% 
  select(-kit, -kit_num) %>%
  summarise_all(sum) %>% 
  filter(matches_all == 70, nonmatches_all == 290) %>%
  select(sim_num) %>% 
  left_join(res2) %>% # Keep only trials with 70 min matches, 290 nonmatches
  select(sim_num, kit_num, kit) %>%
  mutate(eval = purrr::map(kit, bunch_eval)) %>%
  unnest(eval)


res4 %>% group_by(sim_num) %>% select(-kit) %>% summarize_all(sum) %>%
  select(matches_min, nonmatches_min, fbi_elim_min) %>% 
  summarize_each(list(~quantile(., c(0.025)), ~mean(.), ~quantile(., .975))) %>%
  select(matches("nonmatches"), matches("^matches"), matches("^fbi")) %>%
  pivot_longer(cols = 1:9, names_to = "match_type", values_to = "value") %>%
  separate(match_type, into = c("match", "val"), sep = "_min_") %>%
  pivot_wider(id_cols = "match", names_from = "val", values_from = "value")

range(res4$fbi_elim_all)

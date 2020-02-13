library(tidyverse)
clopper <- function(alpha, success, trials) {
  lower <- qbeta(alpha/2, success, trials-success+1)
  upper <- qbeta(1-alpha/2, success+1, trials-success)
  c(lower, upper)
}

studies <- read.csv("data/studies.csv")
cis <- studies %>% group_by(Study, Decision) %>% 
  summarize(
    pred_ss = sum(Number[Ground.truth=="Same Source"])/sum(Number),
    pred_ss_lower = clopper(0.05, sum(Number[Ground.truth=="Same Source"]), sum(Number))[1],
    pred_ss_upper = clopper(0.05, sum(Number[Ground.truth=="Same Source"]), sum(Number))[2]
  )

cis %>% 
  mutate(
    Decision = factor(Decision, levels=c("Identification", "Inconclusive", "Elimination"))
  ) %>%
  ggplot(aes(x = pred_ss, y = Study)) +
  facet_grid(Decision~.) +
  geom_vline(xintercept=c(0,1), colour = "grey20", size=0.25) +
  geom_errorbarh(
    aes(xmin = pred_ss_lower, xmax=pred_ss_upper, y=Study), 
    size=0.5, height = 0.5, colour = "grey20") +
  geom_point() +
  xlim(c(0,1)) +
  xlab("Probability for same source given examiner's decision") +
  theme_bw()

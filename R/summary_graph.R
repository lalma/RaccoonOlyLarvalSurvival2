library(tidyverse)
library(here)
library(janitor)

d <- read_csv(here("data", "RaccoonLikelyhoodLarvaeInJar", "all500cox_coeff_HR_CI_122721.csv")) %>%
  clean_names() %>%
  mutate(comparison = paste(site_1, treatment_2, "x", site_4, treatment_5, sep = "_")) %>%
  mutate(exp_mean = exp(mean_expected_hr),
         exp_lower = exp(x0_025quantile), 
         exp_upper = exp(x0_975quantile)) %>%
  {.}

 
p <- d %>%
  ggplot(aes(comparison, exp_mean)) +
    geom_point(aes(colour = site_1)) +
    geom_errorbar(aes(ymin = exp_lower, ymax = exp_upper)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(filename = here("figs", "hr_interactions.png"), plot = p)



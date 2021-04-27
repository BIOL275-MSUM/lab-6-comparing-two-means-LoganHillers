
# load packages -----------------------------------------------------------

library(tidyverse)


# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")


# put data in tidy format ------------------------------------------------

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()


# do stuff ----------------------------------------------------------------

fish_ttest <- t.test(species ~ location, data = fish_long)
fish_ttest$estimate

fish_upstream <- filter(fish_long, location == "Upstream")

fish_downstream <- filter(fish_long, location == "Downstream")

ggplot(data = fish_upstream) +
  geom_histogram(mapping = aes(x = species), binwidth = 3,
                 boundary = 0, closed = "left", 
                 fill = "#C5351B", color = "black") +
  labs(x = "species", y = "count") +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 4), 
                     expand = expansion(mult = 0)) +
  scale_x_continuous(breaks = seq(0, 600, 100)) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black", size = rel(1))
  )

# ANOVA -------------------------------------------------------------------

crab <- read_csv("chap15q27FiddlerCrabFans.csv")

crab %>% 
  filter(!is.na(crabType)) %>% 
  ggplot(aes(x = bodyTemperature)) +
  geom_histogram(
    aes(fill = crabType), 
    bins = 8, 
    alpha = 0.5, 
    position = "identity"
  ) +
  facet_wrap( ~ crabType, ncol = 1)

crab_means <-
  crab %>% 
  filter(!is.na(crabType)) %>%      # remove missing values
  group_by(crabType) %>% 
  summarize(
    mean = mean(bodyTemperature),
    sd = sd(bodyTemperature),
    n = n(),
    sem = sd / sqrt(n),
    upper = mean + 1.96 * sem,
    lower = mean - 1.96 * sem
  ) %>% 
  print()

ggplot(data = crab, aes(x = crabType, y = bodyTemperature)) +
  geom_jitter(aes(color = crabType),
              width = 0.1,
              alpha = 0.7,
              show.legend = FALSE,
              na.rm = TRUE) +
  geom_errorbar(aes(y = mean, ymin = lower, ymax = upper), 
                data = crab_means,
                width = .1, position = position_nudge(.3)) +
  geom_point(aes(y = mean), data = crab_means,
             position = position_nudge(.3)) +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4","red"))

aov_crab <-
  aov(bodyTemperature ~ crabType, data = crab)

aov_crab

summary(aov_crab)








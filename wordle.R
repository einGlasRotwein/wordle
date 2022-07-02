
library(tidyverse)
library(geomtextpath)
library(lmerTest)
library(broom.mixed)
library(openxlsx)

wordle <- read.xlsx("wordle.xlsx", 1)
wordle$date <- convertToDate(wordle$date)

start_date <- min(wordle$date)
end_date <- max(wordle$date)

# Penalty: Not solving a wordle counts as 8 tries
wordle <- 
  wordle %>% 
  mutate(guess_no = ifelse(guess_no == 7, 8, guess_no))

# Get final guess (either the correct answer or the 6th unsuccessful try)
final_guesses <- 
  wordle %>% 
  group_by(word, guesser) %>% 
  filter(guess_no == max(guess_no)) %>% 
  ungroup()

# Percentage of # of guesses
guess_summary <- 
  final_guesses %>% 
  group_by(guesser, guess_no) %>%
  summarise(n = n()) %>% 
  mutate(percent = n / sum(n))

# Fill up missing values (the count function omits entries that are 0)
full_counts <- 
  expand.grid(
    guesser = c("jan", "juli"),
    guess_no = c(1:6, 8),
    stringsAsFactors = FALSE
  )

full_counts <-
  merge.data.frame(
    full_counts, guess_summary, 
    by = c("guesser", "guess_no"),
    all.x = TRUE
  )

full_counts <- 
  full_counts %>% 
  mutate(percent = ifelse(is.na(percent), 0, percent))

full_counts %>% 
  mutate(guess_no = factor(guess_no)) %>% 
  ggplot(aes(x = guess_no, y = percent, fill = guesser)) +
  geom_col(position = "dodge", colour = "black") +
  scale_x_discrete(labels = c(1:6, "X")) +
  labs(x = "guess number") +
  theme_classic() +
  theme(legend.position = "top")

# Mean all: Mean across all trials (including failed trials, which count as
# 8 tries).
# Mean won: Mean across all won games (games that were not solved don't count)
mean_all_jan <- 
  round(
    mean(final_guesses$guess_no[final_guesses$guesser == "jan"]),
    2
  )

mean_won_jan <- 
  round(
    mean(final_guesses$guess_no[final_guesses$guesser == "jan" &
                                  final_guesses$guess_no != 8]),
    2
  )

mean_all_juli <- 
  round(
    mean(final_guesses$guess_no[final_guesses$guesser == "juli"]),
    2
  )

mean_won_juli <- 
  round(
    mean(final_guesses$guess_no[final_guesses$guesser == "juli" &
                                  final_guesses$guess_no != 8]),
    2
  )

final_guesses %>% 
  ggplot() +
  geom_violin(
    aes(x = guesser, y = guess_no, fill = guesser, colour = guesser), 
    alpha = .3, size = 1
  ) +
  geom_texthline(yintercept = mean_all_jan, label = mean_all_jan, 
                 hjust = .21, color = "red1", size = 6) +
  geom_texthline(yintercept = mean_won_jan, label = mean_won_jan, 
                 hjust = .3, color = "red4", size = 6) +
  geom_texthline(yintercept = mean_all_juli, label = round(mean_all_juli, 2), 
                 hjust = .69, color = "blue1", size = 6) +
  geom_texthline(yintercept = mean_won_juli, label = round(mean_won_juli, 2), 
                 hjust = .81, color = "blue4", size = 6) +
  scale_y_continuous(breaks = 1:8, limits = c(1, 8), labels = c(1:6, "", "X")) +
  labs(
    y = "# of guesses", title = "Wordle guesses", 
    subtitle = paste("data collected from", start_date, "-", end_date)
  ) +
  theme_classic() +
  theme(legend.position = "top")

wordle_mm <- 
  lmer(
    guess_no ~ guesser + (1|word), 
    data = final_guesses
  )

options(scipen = 999)

tidy(wordle_mm) %>%
  mutate(p.value = round(p.value, 3))


# Setup -------------------------------------------------------------------
library(tidyverse)
library(lme4)


# Fixed effect vs Random effect -------------------------------------------
# simulation data
fish.n = 200
fish.sd = 0.1
cohort.n = 10
aac = 10
env.sd = 1
env.eff = 0.5
log.age.eff = -1
grand.i = 6
error.sd = 0.25 

year.n <- cohort.n + aac - 1
env <- rnorm(n = year.n, mean = 0, sd = env.sd)
year.env <- tibble(year = seq(1,year.n), env = env)

fish <- tibble(
  fish.id = 1:fish.n,
  fish.i  = rnorm(fish.n, 0, fish.sd), # random intercept
  cohort = rep(seq(1, cohort.n), each = 20)
)

dat <- crossing(
  fish.id = fish$fish.id, # get fishid from fish data
  age = seq(1, aac)) %>%
  left_join(fish, by = "fish.id") %>%
  mutate(year = age + cohort - 1) %>%
  left_join(year.env, by = "year") %>% 
  mutate(log.age = log(age)) %>%
  mutate(
    # calculate error term (normally distributed residual with SD set above)
    err = rnorm(nrow(.), 0, error.sd),
    # calculate DV from intercepts, effects, and error
    log.increment = grand.i + log.age.eff*log.age + fish.i + err + env.eff*env 
  )

# example too few grouping levels
dat_12 <- dat %>% filter(fish.id %in% c(1, 20, 30))
ggplot(data = dat_12, aes(x = log.age, y = log.increment, color = factor(fish.id) )) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)

lm <- lm(log.increment ~ 1 + log.age + factor(fish.id), data = dat_12)
summary(lm)
# log.increment(fish.id.20) ~ 5.7 + -0.8*log.age + 0.12
# log.increment(fish.id.1) ~ 5.7 + -0.8*log.age + 0
# log.increment(fish.id.1) ~ 5.7 + -0.8*log.age - 0.02

lmer <- lmer(log.increment ~ 1 + log.age + (1 | fish.id), data = dat_12)
summary(lmer)
# log.increment ~ 5.7 + -0.7*log.age + alpha_fish.id

# example too many grouping levels
lmer <- lmer(log.increment ~ 1 + log.age + (1 | fish.id) + env, data = dat)
summary(lmer)

ggplot(data = dat, aes(x = year, y = env)) + geom_line()

# NOTE:
# if too few grouping levels (fishid) - rule of thump less than 5 is too few -> variable as fixed effect
# variable as fixed effect -> the model can have good estimation 
# variable as random effect -> the model cannot have good estimation

# if too many grouping levels (fishid) - rule of thump more than 5 is too many -> variable as random effect
# variable as fixed effect -> the model cannot good estimation 
# variable as random effect -> the model can have good estimation


# Cohort vs Year problem --------------------------------------------------

# Note: Should include only Year instead of both Year and Cohort

# VARIABLES
# increment raw increment values
# age       fish age, log and then scaled (x - mean(x)/sd(x)) | age = s.(log(Age)) | raw values: Age 
# pop       population (4bc, 7a, 8ab)
# pop.year  index of year nested in population (e.g. 4bc:1970) 

data <- read_rds(file.path("D:/OneDrive - UGent/FWO/@FWO/01_fwo_phd/fwo_phd_git/ch1_otolith-growth_lmm/lme4-vs-inla/note_lme4-vs-inla_data.rds"))

# only 8ab
# data <- data %>% filter(pop == "8ab", Age == 1) #can try fitting only Age 1 to see the trend vs raw data
data <- data %>% filter(pop == "8ab")

# raw data Age 1
ggplot(data = data %>% filter(Age == 1), aes(x = year, y = increment)) + 
  geom_point() + 
  geom_smooth()

# fit year only
m.year <- lmer(increment ~ 1 + (1 | year), data)

re.year <- as.data.frame(ranef(m.year)$year) 
re.year <- re.year %>% mutate(time = as.numeric(rownames(re.year)),
                              model = "year.only")

ggplot(data = re.year, aes(x = time, y = `(Intercept)`, color = model)) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed")

# fit cohort only
m.cohort <- lmer(increment ~ 1 + (1 | cohort), data)

re.cohort <- as.data.frame(ranef(m.cohort)$cohort) 
re.cohort <- re.cohort %>% mutate(time = as.numeric(rownames(re.cohort)),
                              model = "cohort.only")

ggplot(data = re.cohort, aes(x = time, y = `(Intercept)`, color = model)) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed")


# fit year and cohort together
m.year.cohort <- lmer(increment ~ 1 + (1 | year) +  (1 | cohort), data)
re.year2 <- as.data.frame(ranef(m.year.cohort)$year) 
re.year2 <- re.year2 %>% mutate(time = as.numeric(rownames(re.year2)),
                                  model = "year")

ggplot(data = re.year2, aes(x = time, y = `(Intercept)`, color = model)) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed")


re.cohort2 <- as.data.frame(ranef(m.year.cohort)$cohort) 
re.cohort2 <- re.cohort2 %>% mutate(time = as.numeric(rownames(re.cohort2)),
                                model = "cohort")

ggplot(data = re.cohort2, aes(x = time, y = `(Intercept)`, color = model)) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed")

# add year, cohort from both year and cohort model 
# and year from year only model
re.year.cohort <- bind_rows(re.year2, re.cohort2, re.year)
ggplot(data = re.year.cohort, aes(x = time, y = `(Intercept)`, color = model)) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = "dashed")

# centered: x - mean(x)
ggplot(data = data, aes(x = log(Age), y = log(AnnulusDiameterIncrement.um))) + geom_point()
ggplot(data = data, aes(x = scale(log(Age), scale = F) , y = log(AnnulusDiameterIncrement.um))) + geom_point()



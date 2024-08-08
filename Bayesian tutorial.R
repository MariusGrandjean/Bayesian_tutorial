####### Bayesian example #######

library(tidyverse)

# Set the seed for reproducibility
set.seed(123)

# Define the researchers and days
researchers <- c('Helene','Mrittika','Marius','Valerie')
days <- c('Lundi','Mardi','Mercredi','Jeudi','Vendredi')

# Generate random productivity between 50% and 90%
productivity <- rnorm(length(researchers) * length(days), mean = 70, sd = 10)

# Simulate different coffee quality levels (1- low, 5 - high)
coffee_quality <- sample(1:5, length(researchers) * length(days), replace = TRUE)

# Introduce some researcher effect on baseline productivity
researcher_effect <- rnorm(length(researchers), mean = 0, sd = 2)
productivity <- productivity + rep(researcher_effect, each = length(days))

# Simulate the effect of coffee quality on productivity (stronger for some researchers)
researcher_sensitivity <- rnorm(length(researchers), mean = 0.5, sd = 0.2)
productivity <- productivity + coffee_quality * rep(researcher_sensitivity, each = length(days))

# Create a data frame with researcher and day names
data <- data.frame(researcher = rep(researchers, times = length(days)),
                   day = rep(days, length.out = length(researchers) * length(days)),
                   productivity = productivity,
                   coffee_quality = coffee_quality)

data_sum <- data %>% 
  group_by(researcher) %>% 
  summarise(mean_productivity = mean(productivity))

# Print a glimpse of the data
print(data)

# Load the ggplot2 package
library(ggplot2)

# Create a scatter plot
ggplot(data, aes(x = coffee_quality, y = productivity)) +
  geom_point(aes(color = researcher, shape = day)) +
  geom_abline(intercept = model_intercept, slope = model_slope, size = 1)+
  labs(x = "Coffee Quality", y = "Productivity", color = "Researcher", shape = "Day", 
       title = "Daily productivity based on coffee quality") +
  theme_minimal()

### BRMS ####

# Load package
library(brms)
library(parallel)

# Model formula with random effects
model_fit <- brm(productivity ~ 1 + coffee_quality + (1 + coffee_quality|researcher),
                 data = data,
                 prior = c(set_prior("normal(70,7)", class = "Intercept"),
                           set_prior("normal(0, 1)", class = "b"),
                           set_prior("normal(10.3,2)", class = "sigma")),
                 family = gaussian(),
                 chains = 4, cores = parallel::detectCores(),
                 control = list(adapt_delta = 0.95),
                 warmup = 1000,iter = 2000)

get_prior(productivity ~ 1 + coffee_quality + (1 + coffee_quality|researcher),
          data = data)

summary(model_fit)                     

posterior_summary(x=model_fit, probs=c(0.025, 0.975), pars = c("^b_", "sigma"))

# Visually inspect the chain behavior of a few semi_randomly selected parameters
pars <- variables(model_fit)
pars_sel <- c(sample(pars[1:10], 3), sample(pars[-(1:10)], 3))
plot(model_fit, variable = pars_sel, N = 6, 
     ask = FALSE, exact_match = TRUE, newpage = TRUE, plot = TRUE)

# directly plot all the parameters
plot(model_fit)

# Checking posterior distribution
pp_check(object = model_fit, ndraws = 1e2)

# to see the effects of the main effects
conditional_effects(model_fit)

model_fit2 <- brm(formula = productivity ~ 1 + coffee_quality + (1|researcher),
                 data = data,
                 prior = c(set_prior("normal(70,7)", class = "Intercept"),
                           set_prior("normal(0, 1)", class = "b"),
                           set_prior("normal(10.3,2)", class = "sigma")),
                 family = gaussian(),
                 chains = 4, cores = parallel::detectCores(),
                 warmup = 1000,iter = 2000)

posterior_summary(x=model_fit2, probs=c(0.025, 0.975), pars = c("^b_", "sigma"))

model_fit3 <- brm(formula = productivity ~ 1 + day + coffee_quality + (1 + day + coffee_quality|researcher),
                  data = data,
                  prior = c(set_prior("normal(70,7)", class = "Intercept"),
                            set_prior("normal(0, 1)", class = "b"),
                            set_prior("normal(10.3,2)", class = "sigma")),
                  family = gaussian(),
                  chains = 4, cores = parallel::detectCores(),
                  control = list(adapt_delta = 0.95),
                  warmup = 2000,iter = 4000)

posterior_summary(x=model_fit3, probs=c(0.025, 0.975), pars = c("^b_", "sigma"))
bayes_df <- tidybayes::summarise_draws(model_fit3)
summary(model_fit3)
# We will use the WAIC to compare the models, we therefore need to add it to each model
# so we can compare them afterwards
model_fit <- add_criterion(model_fit, "waic")
model_fit2 <- add_criterion(model_fit2,"waic")
model_fit3 <- add_criterion(model_fit3,"waic")

comparaison_model <- loo_compare(model_fit, model_fit2, model_fit3, criterion = "waic")
print(comparaison_model, digits = 2, simplify = FALSE)

pp_check(object = model_fit2, ndraws = 1e2)
pp_check(object = model_fit3, ndraws = 1e2)


get_prior(formula = productivity ~ 1 + day + coffee_quality + (1|researcher),
          data = data)

# Plot the model

predictions <- data.frame(predict(model_fit3))  # Make predictions
combined_data <- cbind(data, predictions)


# Define the order of days
day_order <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi")

# Convert the "day" variable to a factor with specified levels
combined_data$day <- factor(combined_data$day, levels = day_order)

# ggplot with geom_smooth using predicted values
ggplot(combined_data, aes(x = coffee_quality, y = productivity)) +
  geom_point(aes(color = researcher), size = 3) +
  facet_grid(~day, axis.labels = "margins")+
  geom_line(aes(group = day), alpha = .5)+
  #geom_ribbon(aes(ymin = `Q2.5`, ymax = `Q97.5`, fill = researcher), alpha =.1, colour = NA) +
  labs(x = "Coffee Quality", y = "Productivity", color = "Researcher", shape = "Day",
       title = "Daily productivity based on coffee quality") +
  theme_classic()

# How to visualize posterior distribution to the original data
posterior <- model_fit3 %>% 
  tidybayes::spread_draws(b_Intercept, b_coffee_quality, ndraws = 100) %>% 
  select(b_Intercept, b_coffee_quality)

ggplot(data, aes(x = coffee_quality, y = productivity)) +
  geom_point(aes(color = researcher, shape = day)) +
  geom_abline(data = posterior,
              aes(intercept = b_Intercept, slope = b_coffee_quality),
              alpha = 0.1, size = 1)+
  labs(x = "Coffee Quality", y = "Productivity", color = "Researcher", shape = "Day", 
       title = "Daily productivity based on coffee quality") +
  theme_minimal()

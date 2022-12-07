#I copied the code from the notes but I understand the purpose of it

# load packages
library(tidyverse)
library(stringr)
library(kableExtra)

# set seed for reproducibility
set.seed(1234)

# choose informative  prior distribution
# - I expect govts to last, on avg, 3 years, so fail at a 
#   rate of lambda = 1/3 per year, give-or-take about 1/10.
# - I know that the mean of the gamma is a/b and the sd is 
#   sqrt{a/(b^2)}, so I set a/b = 1/3 and sqrt{a/(b^2)} = 1/10
#   and solve. You could also just experiment with different 
#   values
alpha_inf <- 100/9
beta_inf <-  3*alpha_inf

# plot informative prior
ggplot() + xlim(0, 1) +
  stat_function(fun = dgamma, args = list(shape = alpha_inf, rate = beta_inf)) + 
  labs(title = "prior distribution", x = "lambda; expected failures/year", y = "prior density")

# choose **weakly informative** prior distribution
# - For this, I repeated the same process, but doubled my
#   give-or-take from 1/10 to 1/5,
alpha_weak <- 25/9
beta_weak <- 3*alpha_weak

# plot weakly informative prior
ggplot() + xlim(0, 1) +
  stat_function(fun = dgamma, args = list(shape = alpha_weak, rate = beta_weak)) + 
  labs(title = "prior distribution", x = "lambda; expected failures/year", y = "prior density")

# choose flattest prior distribution
# - I just want to make this prior as close to uniform as possible.
# - the failure rate (failures/year) is *surely* between 0 and 100
#   (lambda = 100 means that 100 governments are failing per year), 
#   so I plot the prior from zero to 100--it's basically flat   
alpha_flat <- 1
beta_flat <- 0.01

# plot flattish prior
ggplot() + xlim(0, 100) + ylim(0, NA) + 
  stat_function(fun = dgamma, args = list(shape = alpha_flat, rate = beta_flat)) + 
  labs(title = "prior distribution", x = "lambda; expected failures/year", y = "prior density")

# data
x <- c(142, 773, 333, 432, 1823)/365  # rescale to years

# make a data frame with the posterior and prior parameters
posts <- data.frame(prior_type = c("Informative", 
                                   "Weakly Informative",
                                   "Flat-ish"),
                    alpha_prior = c(alpha_inf, 
                                    alpha_weak,
                                    alpha_flat),
                    beta_prior = c(beta_inf, 
                                   beta_weak,
                                   beta_flat)) %>%
  # add posterior parameters
  mutate(alpha_posterior = alpha_prior + length(x),
         beta_posterior = beta_prior + sum(x)) %>%
  # create one row per parameter
  pivot_longer(cols = alpha_prior:beta_posterior) %>%
  # separate parameter from distribution type
  separate(name, into = c("parameter", "distribution_type")) %>%
  # put parameters into separate columns
  pivot_wider(names_from = parameter, values_from = value) %>%
  mutate(distribution_type = str_to_title(distribution_type),
         distribution_type = factor(distribution_type, levels = c("Prior", "Posterior"))) %>%
  mutate(prior_type = factor(prior_type, levels = c("Informative", "Weakly Informative", "Flat-ish")))

# compute point estimates and make a table
point_estimates <- posts %>% 
  #filter(distribution_type == "posterior") %>%
  group_by(prior_type, distribution_type) %>%
  mutate(mean = alpha/beta,
         median = median(rgamma(100000, alpha, beta)),
         mode = (alpha - 1)/beta,
         mean_expected_duration = mean(1/rgamma(100000, shape = alpha, rate = beta))) 

point_estimates %>%
  select(-alpha, -beta) %>%
  arrange(distribution_type, prior_type) %>%
  rename(`Prior` = prior_type, 
         `Distribution` = distribution_type,
         `Mean of lambda` = mean,
         `Median of lambda` = median,
         `Mode of lambda` = mode,
         `Mean of 1/lambda` = mean_expected_duration) %>%
  kable(format = "markdown", digits = 2)

# plot distributions
gg_posts <- posts %>%
  # add in lambda values for which to compute the prior and posterior density
  full_join(data_frame(lambda = seq(0, 2, length.out = 100)), 
            by = character()) %>%
  # compute the posterior and prior density for each lambda
  mutate(density = dgamma(lambda, shape = alpha, rate = beta))

ggplot(gg_posts, aes(x = lambda, y = density, 
                     color = prior_type, 
                     linetype = distribution_type)) + 
  geom_line()

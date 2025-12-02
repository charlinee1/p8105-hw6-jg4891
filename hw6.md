hw6
================
2025-12-02

## Q1

``` r
homi <- homi %>%
  mutate(
    city_state = paste(city, state, sep = ", "),
    solved = if_else(str_detect(disposition, "Closed"), 1, 0)
  ) %>%
  filter(!city_state %in% c("Dallas, TX", "Phoenix, AZ",
                            "Kansas City, MO", "Tulsa, AL")) %>%
  filter(victim_race %in% c("White", "Black")) %>%
  mutate(
    victim_age = na_if(victim_age, "Unknown"),
    victim_age = as.numeric(victim_age)
  )
```

``` r
# 1. Filter to Baltimore
balt <- homi %>%
  filter(city_state == "Baltimore, MD")

# 2. Fit logistic regression and save model object
fit_balt <- glm(
  solved ~ victim_age + victim_race + victim_sex,
  data = balt,
  family = binomial()
)

# 3. Tidy results with ORs and 95% CI
results_balt <- broom::tidy(
  fit_balt,
  exponentiate = TRUE,
  conf.int = TRUE
)

# 4. Display OR for all predictors (or filter)
results_balt %>%
  filter(term == "victim_sexMale") %>%   
  select(term, OR = estimate, conf.low, conf.high, p.value) %>%
  knitr::kable(digits = 3)
```

| term           |    OR | conf.low | conf.high | p.value |
|:---------------|------:|---------:|----------:|--------:|
| victim_sexMale | 0.355 |    0.267 |     0.468 |       0 |

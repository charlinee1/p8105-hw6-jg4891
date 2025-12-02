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

``` r
city_results <- homi %>%
  group_by(city_state) %>%
  nest() %>%
  mutate(
    fit = map(data, ~ glm(
      solved ~ victim_age + victim_race + victim_sex,
      data = .x,
      family = binomial()
    )),
    tidy_fit = map(fit, ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE))
  ) %>%
  unnest(tidy_fit) %>%
  filter(str_detect(term, "victim_sexMale")) %>%
  select(city_state, term, OR = estimate, conf.low, conf.high, p.value)
```

    ## Warning: There were 43 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `tidy_fit = map(fit, ~tidy(.x, exponentiate = TRUE, conf.int =
    ##   TRUE))`.
    ## ℹ In group 1: `city_state = "Albuquerque, NM"`.
    ## Caused by warning:
    ## ! glm.fit: fitted probabilities numerically 0 or 1 occurred
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 42 remaining warnings.

``` r
city_results
```

    ## # A tibble: 47 × 6
    ## # Groups:   city_state [47]
    ##    city_state      term              OR conf.low conf.high  p.value
    ##    <chr>           <chr>          <dbl>    <dbl>     <dbl>    <dbl>
    ##  1 Albuquerque, NM victim_sexMale 0.430   0.132      1.16  1.22e- 1
    ##  2 Atlanta, GA     victim_sexMale 0.512   0.323      0.789 3.22e- 3
    ##  3 Baltimore, MD   victim_sexMale 0.355   0.267      0.468 3.74e-13
    ##  4 Baton Rouge, LA victim_sexMale 0.319   0.160      0.596 5.87e- 4
    ##  5 Birmingham, AL  victim_sexMale 0.704   0.444      1.09  1.25e- 1
    ##  6 Boston, MA      victim_sexMale 0.674   0.353      1.28  2.26e- 1
    ##  7 Buffalo, NY     victim_sexMale 0.438   0.239      0.793 6.78e- 3
    ##  8 Charlotte, NC   victim_sexMale 0.301   0.143      0.572 6.16e- 4
    ##  9 Chicago, IL     victim_sexMale 0.391   0.321      0.476 9.40e-21
    ## 10 Cincinnati, OH  victim_sexMale 0.206   0.0977     0.390 5.98e- 6
    ## # ℹ 37 more rows

``` r
city_results %>%
  mutate(city_state = fct_reorder(city_state, OR)) %>%   # order by OR
  ggplot(aes(x = OR, y = city_state)) +
  geom_point(size = 2, color = "darkblue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, color = "gray40") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Adjusted Odds Ratios (Male vs Female Victims) by City",
    x = "Adjusted Odds Ratio (Male vs Female)",
    y = "City"
  ) +
  theme_minimal()
```

    ## Warning: `geom_errobarh()` was deprecated in ggplot2 4.0.0.
    ## ℹ Please use the `orientation` argument of `geom_errorbar()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## `height` was translated to `width`.

![](hw6_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Compared to female, males on average are less likely to have their cases
solved. As from the plot, we can see most cities’ odds ratios fall under
1.

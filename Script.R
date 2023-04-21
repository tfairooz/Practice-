install.packages("vars")
library("vars")
install.packages("fpp2")
library("fpp2")
install.packages("fpp3")
library("fpp3")

# forecasting Corticosteroid Drug sales in Australia using ARIMA


pbs <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost=sum(Cost/1e6))

pbs1 <- PBS %>%
  filter(ATC2 == "H02") %>%
  filter_index("1991 Jul" ~ "2006 Jun") %>%
  summarise(Cost=sum(Cost/1e6))


pbs1 %>%
  mutate(log(Cost)) %>%
  pivot_longer(-Month) %>%
  ggplot(aes(x=Month, y=value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(y="", title = "Corticosteroid drug script (H02)")

pbs1 %>%
  ACF(difference(Cost, lag=12)) %>%
  autoplot()

pbs1 %>%
  gg_tsdisplay((Cost), plot_type = "partial")

# after seasonally differencing the data

pbs1 %>%
  gg_tsdisplay(difference(Cost, 12), plot_type= "partial", lag = 36) +
  labs(title = "Seasonally Differenced", y = "")

pbs1 |>
  mutate(differ = difference(Cost, lag=12)) |>
  features(differ, unitroot_kpss)

pbs1 |>
  features(Cost, unitroot_nsdiffs)


### trying to fit the model

cot_fit <- pbs1 |>
  model(arima300211 = ARIMA(Cost ~ pdq(3,0,0) + PDQ(2,1,1)))

cot_fit1 <- pbs1 |>
  model(auto = ARIMA(Cost, stepwise = FALSE, approx = FALSE)
  )

glance(cot_fit) |> arrange(AICc)
glance(cot_fit1) |> arrange(AICc)

cot_fit |>
  gg_tsresiduals()

augment(cot_fit) |>
  features(.innov, ljung_box, lag=10, df=3)

cot_fit1 |>
  gg_tsresiduals()

augment(cot_fit1) |>
  features(.innov, ljung_box, lag=10, df=4)

## to forecast

fc1 <- cot_fit |>
  forecast(h=23) 
fc1 |>
  autoplot(pbs1)

fc1 |> accuracy(pbs)

fc2 <- cot_fit1 |>
  forecast(h=23) 
fc2 |>
  autoplot(pbs1)

fc2 |> accuracy(pbs)



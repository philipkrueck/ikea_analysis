library(tidyverse)
library(readr)
ikea <- read_delim("./ikea.csv", ";", 
                   escape_double = FALSE, 
                   trim_ws = TRUE,
                   col_types = cols(
                     name = col_factor(),
                     category = col_factor(),
                     designer = col_factor()
                   )
                  )

# tidy data

tidy_ikea <- ikea

## tidy category

tidy_ikea <- tidy_ikea %>%
  group_by(category) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  arrange(-n) %>%
  select(-n) %>%
  distinct(item_id, .keep_all = TRUE)

## tidy designer

tidy_ikea$designer[grepl("\\d",tidy_ikea$designer)] <- NA

tidy_ikea <- tidy_ikea %>%
  separate(designer, c("d1", "d2", "d3", "d4", "d5", "d6", "d7"), sep = "/")

#tidy_ikea[ , 11:17][tidy_ikea[ , 11:17] == "IKEA of Sweden" ] <- NA
tidy_ikea[ , 11:17] <- t(apply(tidy_ikea[ , 11:17], 1, function(x) c(sort(x[x!='']), x[x==''])))

tidy_ikea <- tidy_ikea %>%
  unite(col = "designer", d1, d2, d3, d4, d5, d6, d7, sep = ", ", na.rm = TRUE)

tidy_ikea$designer[tidy_ikea$designer == ""] <- NA
tidy_ikea$designer <- factor(tidy_ikea$designer)

## tidy other_colors
tidy_ikea <- tidy_ikea %>%
  mutate(other_colors = other_colors == "Yes")

## tidy_old_price
tidy_ikea <- tidy_ikea %>%
  mutate(old_price = str_replace(old_price, "SR ", "")) %>%
  mutate(old_price = str_replace_all(old_price, ",", "")) %>%
  mutate(old_price = strtoi(old_price)) %>%
  mutate(old_price = old_price * 10)
  
# transform data columns

## add size in m^3
tidy_ikea <- tidy_ikea %>%
  mutate(size_m3 = round(depth/100 * width/100 * height/100, 2))

# transform price and old price in eur
sr_to_eur_conversion_factor <- 0.24537 # conversion factor from 20.04.2020 (https://www.xe.com/de/currencyconverter/convert/?Amount=1&From=EUR&To=SAR)

## transform price and old_price to euro
tidy_ikea <- tidy_ikea %>%
  mutate(price_eur = round(price * sr_to_eur_conversion_factor / 10, 2), old_price_eur = round(old_price * sr_to_eur_conversion_factor / 10, 2))

# select relevant data for further exploration
tidy_ikea <- tidy_ikea %>%
  select(name, category, price_eur, old_price_eur, sellable_online, other_colors, designer, size_m3)
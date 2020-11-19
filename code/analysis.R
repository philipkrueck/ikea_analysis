# ANALYSIS

tidy_ikea # make sure that ikea.R is executed and tidy_ikea is loaded into memory

tidy_ikea %>% 
  lm(price_eur ~ name, data = .) %>%
  summary()

# many categories -> overfitting -> r^2 high

tidy_ikea %>% 
  lm(price_eur ~ category, data = .) %>%
  summary()

# maybe cluster non-significant categories in "other_categories"
# price range varies heavily in category -> low r^2

tidy_ikea %>% 
  lm(price_eur ~ old_price_eur, data = .) %>%
  summary()

tidy_ikea %>%
  filter(!is.na(old_price_eur))%>%
  summarize(mean = median(price_eur) / median(old_price_eur))

# correlation high, but not useful predictory analysis : no old_price for new product, unlikely for existing product

tidy_ikea %>% 
  lm(price_eur ~ sellable_online, data = .) %>%
  summary()

# low r^2 : price range too high + not sellable very rare

tidy_ikea %>% 
  lm(price_eur ~ other_colors, data = .) %>%
  summary()

# products of every price range have both options

tidy_ikea %>% 
  lm(price_eur ~ designer, data = .) %>%
  summary()

# designers with high number of products produce products in wide price range
# designers with low number of products -> overfitting -> relatively high r^2, but low generalization

tidy_ikea %>% 
  lm(price_eur ~ size_m3, data = .) %>%
  summary()

# material cost





# model, tbd
tidy_ikea %>%
  lm(price_eur ~ designer + size_m3 + category, data = .) %>%
  summary()





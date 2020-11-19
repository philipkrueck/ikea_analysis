library(randomForest)
library(dplyr)
library(ggplot2) 

tidy_ikea # make sure that tidy.R is executed and tidy_ikea is loaded into memory

# ANALYSIS

# correlation price + old_price

cor.test(tidy_ikea$price_eur, tidy_ikea$old_price_eur, 
         method = "pearson")

# importance - drop_na
rf_ikea <- tidy_ikea %>%
  select(-old_price_eur) %>%
  drop_na()

rf_ikea$designer <- fct_lump_n(rf_ikea$designer, 51)
rf_ikea$name <- fct_lump_n(rf_ikea$name, 51)

ikea.rf <- randomForest(price_eur ~ ., rf_ikea, ntree=2000,
                          keep.forest=FALSE, importance=TRUE)
imp_drop_na <- importance(ikea.rf)

# importance - dummy value instead of na

rf_ikea <- tidy_ikea 

rf_ikea$size_m3[is.na(rf_ikea$size_m3)] <- -1000

rf_ikea <- rf_ikea %>%
  select(-old_price_eur) %>%
  drop_na()

rf_ikea$designer <- fct_lump_n(rf_ikea$designer, 51)
rf_ikea$name <- fct_lump_n(rf_ikea$name, 51)

ikea.rf <- randomForest(price_eur ~ ., rf_ikea, ntree=2000,
                        keep.forest=FALSE, importance=TRUE)
imp_size_minus_1000 <- importance(ikea.rf)


# importance - na roughfix

rf_ikea <- tidy_ikea %>%
  select(-old_price_eur)

rf_ikea$designer <- fct_lump_n(rf_ikea$designer, 51)
rf_ikea$name <- fct_lump_n(rf_ikea$name, 51)

ikea.rf <- randomForest(price_eur ~ ., rf_ikea, ntree=2000,
                        keep.forest=FALSE, importance=TRUE, na.action = na.roughfix)
imp_na_roughfix <- importance(ikea.rf)


# mean importance
mean_imp <- as.data.frame((imp_na_roughfix+imp_size_minus_1000+imp_drop_na)/3)

mean_imp$varnames <- rownames(mean_imp)
rownames(mean_imp) <- NULL
mean_imp <- mean_imp %>%
  rename('PercentIncMSE' = '%IncMSE')

ggplot(mean_imp, aes(y=reorder(varnames, PercentIncMSE), x=PercentIncMSE)) + 
  geom_point() +
  labs(x = "% Increase MSE", y = "Feature", title = "Mean Feature Importance")




# ANALYSIS_OLD

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
  lm(price_eur ~ name + category + price_eur + old_price_eur + sellable_online + other_colors + designer + size_m3, data = .) %>%
  summary()







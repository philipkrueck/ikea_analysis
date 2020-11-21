library(randomForest)
library(ggplot2)

tidy_ikea # make sure that tidy.R is executed and tidy_ikea is loaded into memory

###############
## ANALYSIS ###
###############

# correlation price + old_price

cor.test(tidy_ikea$price_eur, tidy_ikea$old_price_eur, 
         method = "pearson")

# importance - drop_na

rf_ikea <- tidy_ikea %>%
  select(-old_price_eur) %>%
  drop_na()

rf_ikea$designer <- fct_lump_n(rf_ikea$designer, 50)
rf_ikea$name <- fct_lump_n(rf_ikea$name, 49)

result_rf <- randomForest(price_eur ~ ., rf_ikea, ntree=2000,
                          keep.forest=FALSE, importance=TRUE)
imp_drop_na <- importance(result_rf)

# importance - dummy value instead of na

rf_ikea <- tidy_ikea 

rf_ikea$size_m3[is.na(rf_ikea$size_m3)] <- -1000

rf_ikea <- rf_ikea %>%
  select(-old_price_eur) %>%
  drop_na()

rf_ikea$designer <- fct_lump_n(rf_ikea$designer, 50)
rf_ikea$name <- fct_lump_n(rf_ikea$name, 49)

ikea.rf <- randomForest(price_eur ~ ., rf_ikea, ntree=2000,
                        keep.forest=FALSE, importance=TRUE)
imp_size_minus_1000 <- importance(ikea.rf)


# importance - na roughfix

rf_ikea <- tidy_ikea %>%
  select(-old_price_eur)

rf_ikea$sellable_online <- factor(rf_ikea$sellable_online)
rf_ikea$other_colors <- factor(rf_ikea$other_colors)

rf_ikea$designer <- fct_lump_n(rf_ikea$designer, 50)
rf_ikea$name <- fct_lump_n(rf_ikea$name, 49)

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


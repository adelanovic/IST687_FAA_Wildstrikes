source("source_code/9_14_2026_damage_model_prep.R")
library(rpart)
library(rpart.plot)

rpart_predictors <- c("SIZE", "HEIGHT", "SPEED", "TIME_OF_DAY", "NUM_STRUCK")
indicators_train <- make_indicators(x_train, c("SPECIES_GROUP", "PHASE_GROUP"))
indicators_test <- make_indicators(x_test, c("SPECIES_GROUP", "PHASE_GROUP"))

# Short names keep the plotted tree readable.
short_names <- gsub("\\.", "_", toupper(sub("^(SPECIES|PHASE)_", "",
                                            colnames(indicators_train))))
colnames(indicators_train) <- short_names
colnames(indicators_test) <- short_names

train_frame <- data.frame(DAMAGED = train$DAMAGED, x_train[, rpart_predictors],
                          indicators_train)
test_frame <- data.frame(x_test[, rpart_predictors], indicators_test)

# A missed damaging strike costs 12x more than a false damage warning.
loss_matrix <- matrix(c(0, 1, 12, 0), nrow = 2, byrow = TRUE)

model <- rpart(DAMAGED ~ ., data = train_frame, method = "class",
               parms = list(loss = loss_matrix),
               control = rpart.control(cp = 0.0025, minbucket = 100,
                                       maxdepth = 4, xval = 0))
prediction <- predict(model, test_frame, type = "class")
evaluate(test$DAMAGED, prediction)

dir.create("graphs/9-14-2026", showWarnings = FALSE, recursive = TRUE)
png("graphs/9-14-2026/rpart_damage_tree.png", width = 2000, height = 1200, res = 150)
rpart.plot(model, extra = 104, box.palette = 0, shadow.col = "gray",
           box.col = ifelse(model$frame$yval == 2, "lightcoral", "palegreen3"),
           faclen = 0, tweak = 0.75, yesno = 2, main = "rpart Damage Model")
dev.off()

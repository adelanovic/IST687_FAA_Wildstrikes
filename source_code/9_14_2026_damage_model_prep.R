set.seed(687)
INPUT <- "data/clean/faa_strikes_clean.rds"
MAX_TRAIN <- getOption("faa.train_size", 12000L)
.libPaths(c(file.path(getwd(), "data", "r_library"), .libPaths()))

numeric_fields <- c("HEIGHT", "SPEED", "NUM_ENGS")
categorical_fields <- c("SIZE", "SPECIES_GROUP", "PHASE_GROUP", "TIME_OF_DAY",
                        "TYPE_ENG", "NUM_STRUCK", "WARNED", "INCIDENT_MONTH")
predictors <- c(numeric_fields, categorical_fields)

d <- readRDS(INPUT)
d <- d[!is.na(d$INCIDENT_YEAR) & d$INDICATED_DAMAGE %in% c(0, 1), ]
d$DAMAGED <- factor(ifelse(d$INDICATED_DAMAGE == 1, "Yes", "No"), c("No", "Yes"))
train <- d[d$INCIDENT_YEAR <= 2020, ]
test <- d[d$INCIDENT_YEAR %in% 2024:2025, ]
if (nrow(train) > MAX_TRAIN) train <- train[sample.int(nrow(train), MAX_TRAIN), ]

# Fill-in values come from training years only, so test years stay honest.
medians <- lapply(train[numeric_fields], function(x) median(as.numeric(x), na.rm = TRUE))
rules <- lapply(train[categorical_fields], function(x) {
  value <- as.character(x[!is.na(x)])
  list(mode = names(which.max(table(value))), levels = sort(unique(value)))
})

prepare <- function(data) {
  result <- data[, predictors, drop = FALSE]
  for (field in numeric_fields) {
    value <- as.numeric(result[[field]])
    value[is.na(value)] <- medians[[field]]
    result[[field]] <- value
  }
  for (field in categorical_fields) {
    rule <- rules[[field]]
    value <- as.character(result[[field]])
    value[is.na(value) | !value %in% rule$levels] <- rule$mode
    result[[field]] <- factor(value, levels = rule$levels)
  }
  # Strike-count bands have a natural order: 1, 2-10, 11-100, over 100.
  result$NUM_STRUCK <- match(as.character(result$NUM_STRUCK),
                             c("1", "2-10", "11-100", "More than 100"))
  result
}
x_train <- prepare(train)
x_test <- prepare(test)

make_indicators <- function(data, fields) {
  factors <- fields[vapply(data[fields], is.factor, logical(1))]
  result <- model.matrix(reformulate(fields, intercept = FALSE), data,
                         contrasts.arg = lapply(data[factors], contrasts, contrasts = FALSE))
  colnames(result) <- make.names(sub("_GROUP", "_", colnames(result)))
  result
}

evaluate <- function(actual, predicted) {
  confusion <- table(Actual = actual, Predicted = predicted)
  print(confusion)
  recall <- confusion["Yes", "Yes"] / sum(confusion["Yes", ])
  specificity <- confusion["No", "No"] / sum(confusion["No", ])
  print(c(accuracy = mean(predicted == actual),
          balanced_accuracy = (recall + specificity) / 2,
          damage_recall = recall,
          precision = confusion["Yes", "Yes"] / sum(confusion[, "Yes"])))
}

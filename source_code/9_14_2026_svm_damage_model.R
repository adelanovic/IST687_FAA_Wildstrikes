source("source_code/9_14_2026_damage_model_prep.R")
library(e1071)
library(ggplot2)

svm_train <- make_indicators(x_train, names(x_train))
svm_test <- make_indicators(x_test, names(x_test))

# Settings selected in the earlier validation run.
class_weights <- c(No = 1, Yes = sum(train$DAMAGED == "No") / sum(train$DAMAGED == "Yes"))
model <- svm(x = svm_train, y = train$DAMAGED, kernel = "radial",
             cost = 1, gamma = 1 / ncol(svm_train), scale = TRUE,
             class.weights = class_weights)
prediction <- predict(model, svm_test)
evaluate(test$DAMAGED, prediction)

confusion <- as.data.frame(table(Actual = test$DAMAGED, Predicted = prediction))
confusion$Result <- ifelse(confusion$Actual == confusion$Predicted, "Correct", "Incorrect")
axis_labels <- c(No = "No damage", Yes = "Damage")

confusion_plot <- ggplot(confusion, aes(Predicted, Actual, fill = Result)) +
  geom_tile(color = "white", linewidth = 2) +
  geom_text(aes(label = paste0(scales::comma(Freq), " strikes")), size = 5) +
  scale_fill_manual(values = c(Correct = "palegreen3", Incorrect = "lightcoral"),
                    guide = "none") +
  scale_x_discrete(position = "top", expand = c(0, 0), labels = axis_labels) +
  scale_y_discrete(limits = c("Yes", "No"), expand = c(0, 0), labels = axis_labels) +
  coord_equal() +
  labs(title = "SVM Confusion Matrix", subtitle = "2024-2025 test data",
       x = "Predicted outcome", y = "Actual outcome") +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

dir.create("graphs/9-15-2026", recursive = TRUE, showWarnings = FALSE)
ggsave("graphs/9-15-2026/svm_confusion_matrix.png", confusion_plot,
       width = 9, height = 8, dpi = 300)

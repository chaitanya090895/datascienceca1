
students <- c(1:17) 


scores_without_visual_aids <- c(50, 60, 58, 72, 36, 51, 49, 49, 25, 52, 41, 32, 58, 39, 25, 40, 61)


scores_with_visual_aids <- c(58, 70, 60, 73, 40, 63, 54, 60, 29, 57, 66, 37, 50, 48, 80, 65, 70)


quality_scores <- data.frame(
  Student = students,
  Without_Visual_Aids = scores_without_visual_aids,
  With_Visual_Aids = scores_with_visual_aids
)


print(quality_scores)




if (!require(psych, quietly = TRUE)) {
  install.packages("psych", dependencies=TRUE)
  library(psych)
}

# Descriptive statistics
describe_stats <- describe(quality_scores[, c("Without_Visual_Aids", "With_Visual_Aids")])
print(describe_stats) 

# Boxplot visualization
boxplot(quality_scores$With_Visual_Aids, quality_scores$Without_Visual_Aids,
        names = c("With Visual Aids", "Without Visual Aids"),
        main = "Boxplot of Lecture Quality Scores",
        ylab = "Quality Score",
        col = c("blue", "red"))

# Performing normality tests and printing the results
normality_with <- shapiro.test(quality_scores$With_Visual_Aids)
print(normality_with)  

normality_without <- shapiro.test(quality_scores$Without_Visual_Aids)
print(normality_without)  

# testing
if (normality_with$p.value > 0.05 && normality_without$p.value > 0.05) {
  test_result <- t.test(quality_scores$With_Visual_Aids, quality_scores$Without_Visual_Aids, paired = TRUE)
} else {
  test_result <- wilcox.test(quality_scores$With_Visual_Aids, quality_scores$Without_Visual_Aids, paired = TRUE)
}


# checking results
if (exists("test_result")) {
  print(test_result)
} else {
  cat("test_result was not created \n")
}
```

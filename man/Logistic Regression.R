library("ggplot2")
pima <- read.csv("inst/extdata/pima-indians-diabetes.csv", header = TRUE)
colnames(pima) <- c("Pregnancies", "Plasma.glucose", "Diastolic.pressure",
                    "Triceps.skinfold", "Serum.insulin", "BMI",
                    "Diabetes.pedigree", "Age", "Diabetes")

pima$Diabetes <- factor(pima$Diabetes, labels = c("No", "Yes"))

str(pima)
head(pima)

# Age, Plasma, Diabates
ggplot(pima, aes(x = Age, y = Plasma.glucose, colour = Diabetes)) + 
  geom_point(size = 1.8, alpha = 0.7) + 
  labs(title = "Scatterplot: Plasma glucose vs. Age",
       subtitle = "Ground Truth: Distribution of diabetic cases in the training set",
       x = "Age (years)",
       y = "Plasma glucose concentration (mg/dL)",
       color = "Diabetes status") +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  xlim(range(pima$Age)) + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, colour = "gray30", size = 13),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


# logistic regression model
m1 <- glm(Diabetes ~ Plasma.glucose + Age, data = pima, family = binomial)

summary(m1)$coefficients

# predict
p_all <- predict(m1, newdata = pima, type = "response")

# use r = 0.5 convert predicted probabilities into class labels
yhat_05 <- ifelse(p_all >= 0.5, "Yes", "No")
pima$Predicted_05 <- factor(yhat_05, levels = c("No", "Yes"))


# Confusion matrices
cm_05 <- table(pred = pima$Predicted_05, actual = pima$Diabetes)
cm_05

# errors
err_05 <- mean(pima$Predicted_05 != pima$Diabetes)
err_05

ggplot(pima, aes(x = Age, y = Plasma.glucose, colour = Predicted_05)) + 
  geom_point(size = 1.8, alpha = 0.7) + 
  labs(title = "Logistic Regression Classification (Threshold r = 0.5)",
       x = "Age (years)", 
       y = "Plasma glucose concentration (mg/dL)", 
       color = "Predicted Class") + 
  scale_color_manual(values = c("No" = "steelblue", "Yes" = "firebrick")) + 
  xlim(range(pima$Age)) + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# decision boundary
coefs <- coef(m1)
b0 <- coefs[1]
b1 <- coefs["Plasma.glucose"]
b2 <- coefs["Age"]

# y = intercept + slope * x (x = Age, y = Plasma.glucose)
intercept <- - b0 / b1
slope <- - b2 / b1
intercept
slope

ggplot(pima, aes(x = Age, y = Plasma.glucose, colour = Predicted_05)) + 
  geom_point(size = 1.8, alpha = 0.7) + 
  geom_abline(intercept = intercept, slope = slope, 
              colour = "black", linetype = "dashed", linewidth = 1) + 
  labs(title = "Logistic Regression Classification (Threshold r = 0.5)", 
       subtitle = paste("Linear Decision Boundary | Misclassification Error:", round(err_05, 3)),
       x = "Age (years)", 
       y = "Plasma glucose concentration (mg/dL)", 
       color = "Predicted Class") + 
  scale_color_manual(values = c("No" = "steelblue", "Yes" = "firebrick")) + 
  xlim(range(pima$Age)) + 
  theme_bw() + 
  theme( 
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18), 
    plot.subtitle = element_text(hjust = 0.5, colour = "gray30", size = 13), 
    axis.title = element_text(face = "bold"), 
    legend.position = "top", legend.text = element_text(face = "bold"), 
    panel.grid.minor = element_blank()
  )


# r = 0.2
r2 <- 0.2
yhat_02 <- ifelse(p_all >= r2, "Yes", "No")
pima$Predicted_02 <- factor(yhat_02, levels = c("No", "Yes"))

err_02 <- mean(pima$Predicted_02 != pima$Diabetes)
err_02

# logit(r) = log(r / (1-r))
logit_r2 <- log(r2 / (1 - r2))
intercept2 <- (logit_r2 - b0) / b1
slope2 <- - b2 / b1
intercept2
slope2


ggplot(pima, aes(x = Age, y = Plasma.glucose, colour = Predicted_02)) + 
  geom_point(size = 1.8, alpha = 0.7) +
  geom_abline(intercept = intercept2,
              slope = slope2,
              colour = "black",
              linetype = "dashed",
              linewidth = 1) +
  labs(title = "Logistic Regression Classification (Threshold r = 0.2)",
       subtitle = paste("Lower threshold increases sensitivity (more 'Yes' predictions) | Error:", round(err_02, 3)),
       x = "Age (years)",
       y = "Plasma glucose concentration (mg/dL)",
       color = "Predicted Class") +
  scale_color_manual(values = c("No" = "steelblue", "Yes" = "firebrick")) +
  xlim(range(pima$Age)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, colour = "gray30", size = 13),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )



# r = 0.8
r8 <- 0.8
yhat_08 <- ifelse(p_all >= r8, "Yes", "No")
pima$Predicted_08 <- factor(yhat_08, levels = c("No", "Yes"))

err_08 <- mean(pima$Predicted_08 != pima$Diabetes)
err_08

# logit(r) = log(r / (1-r))
logit_r8 <- log(r8 / (1 - r8))
intercept8 <- (logit_r8 - b0) / b1
slope8 <- - b2 / b1
intercept8
slope8


ggplot(pima, aes(x = Age, y = Plasma.glucose, colour = Predicted_08)) + 
  geom_point(size = 1.8, alpha = 0.7) +
  geom_abline(intercept = intercept8,
              slope = slope8,
              colour = "black",
              linetype = "dashed",
              linewidth = 1) +
  labs(title = "Logistic Regression Classification (Threshold r = 0.8)",
       subtitle = paste("Higher threshold increases specificity (fewer 'Yes' predictions) | Error:", round(err_08, 3)),
       x = "Age (years)",
       y = "Plasma glucose concentration (mg/dL)",
       color = "Predicted Class") +
  scale_color_manual(values = c("No" = "steelblue", "Yes" = "firebrick")) +
  xlim(range(pima$Age)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, colour = "gray30", size = 13),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )





# Perform a basis function expansion trick by computing new features
# data
pima$z1 <- pima$Plasma.glucose ^ 4
pima$z2 <- pima$Plasma.glucose ^ 3 * pima$Age
pima$z3 <- pima$Plasma.glucose ^ 2 * pima$Age ^ 2
pima$z4 <- pima$Plasma.glucose * pima$Age ^ 3
pima$z5 <- pima$Age ^ 4


str(pima[, c("Plasma.glucose", "Age", "z1", "z2", "z3", "z4", "z5")])

# model
m2 <- glm(
  Diabetes ~ Plasma.glucose + Age + z1 + z2 + z3 + z4 + z5,
  data = pima,
  family = binomial
)

summary(m2)$coefficients

# predict
p_poly <- predict(m2, newdata = pima, type = "response")
yhat_poly <- ifelse(p_poly >= 0.5, "Yes", "No")
pima$Predicted_poly <- factor(yhat_poly, levels = c("No", "Yes"))

# Confusion matrix
cm_poly <- table(pred = pima$Predicted_poly, actual = pima$Diabetes)
cm_poly

# error
err_poly <- mean(pima$Predicted_poly != pima$Diabetes)
err_poly

# decision boundary
age_seq <- seq(min(pima$Age), max(pima$Age), length.out = 200)
glu_seq <- seq(min(pima$Plasma.glucose), max(pima$Plasma.glucose), length.out = 200)

grid <- expand.grid(
  Age = age_seq,
  Plasma.glucose = glu_seq
)

grid$z1 <- grid$Plasma.glucose^4
grid$z2 <- grid$Plasma.glucose^3 * grid$Age
grid$z3 <- grid$Plasma.glucose^2 * grid$Age^2
grid$z4 <- grid$Plasma.glucose * grid$Age^3
grid$z5 <- grid$Age^4

grid$prob <- predict(m2, newdata = grid, type = "response")

# plot
ggplot() +
  geom_point(data = pima,
             aes(x = Age, y = Plasma.glucose, colour = Predicted_poly),
             size = 1.8, alpha = 0.7) +
  geom_contour(data = grid,
               aes(x = Age, y = Plasma.glucose, z = prob),
               breaks = 0.5,
               colour = "black",
               linetype = "dashed",
               linewidth = 1) +
  labs(
    title = "Decision Boundary with Basis Function Expansion",
    subtitle = paste("Non-linear boundary captures complex data structure | Error:", round(err_poly, 3)),
    x = "Age (years)",
    y = "Plasma glucose concentration (mg/dL)",
    color = "Predicted Class"
  ) +
  scale_color_manual(values = c("No" = "steelblue", "Yes" = "firebrick")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )


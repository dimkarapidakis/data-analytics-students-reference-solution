################################################################################
# FINAL PROJECT SCENARIO 2: UNDERSTANDING STUDENT PERFORMANCE
# Analysis file - solution
#
#
#   - the 38 students with a final grade of 0 are examined as a data-quality issue
#     and the linear model is refitted without them as a sensitivity check
#   - the predictors are chosen from the exploratory analysis
#     (failures, Medu, goout, studytime, sex, higher, schoolsup)
#   - the logistic model is also read as predicted probabilities
#   - every figure is saved to a file so it can go into the report
#
################################################################################

# ---------------------------------------------------------------------------- #
# 0. SETUP
# ---------------------------------------------------------------------------- #

needed_packages <- c("ggplot2", "dplyr", "tidyr", "broom")
missing_packages <- needed_packages[!(needed_packages %in% rownames(installed.packages()))]
if (length(missing_packages) > 0) install.packages(missing_packages)

library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)

options(digits = 4)
theme_set(theme_minimal(base_size = 11))

dir.create("figures", showWarnings = FALSE)
save_fig <- function(name, plot, width = 6, height = 4) {
  ggsave(file.path("figures", name), plot, width = width, height = height, dpi = 200)
}

# ============================================================================ #
# QUESTION 1. UNDERSTAND AND PREPARE THE DATA
# ============================================================================ #

local_file <- "student-mat.csv"
if (!file.exists(local_file)) local_file <- file.choose()

students <- read.csv2(local_file, stringsAsFactors = FALSE)

head(students)
dim(students)          # 395 students, 33 variables
str(students)

# Data-quality checks
sum(is.na(students))           # missing values
sum(duplicated(students))      # duplicated rows
summary(students[c("age", "absences", "G1", "G2", "G3")])

factor_variables <- c(
  "school", "sex", "address", "famsize", "Pstatus",
  "Mjob", "Fjob", "reason", "guardian", "schoolsup", "famsup",
  "paid", "activities", "nursery", "higher", "internet", "romantic"
)
students[factor_variables] <- lapply(students[factor_variables], factor)

students <- students %>%
  mutate(pass = factor(ifelse(G3 >= 10, "Pass", "Fail"), levels = c("Fail", "Pass")))
table(students$pass)

# --- A data-quality issue the summary hides: final grades of exactly zero ----
table(students$G3)                                   # 38 students have G3 = 0; nobody has 1, 2 or 3
zero_students <- students %>% filter(G3 == 0)
summary(zero_students$absences)                      # every one of them has 0 recorded absences
table(G2_was_positive = zero_students$G2 > 0)        # most had a positive second-period grade
# exam or left the course) and not like a measured performance of 0 out of 20.

# ============================================================================ #
# QUESTION 2. EXPLORE THE DATA
# ============================================================================ #

describe_numeric <- function(x) {
  c(n = sum(!is.na(x)), mean = mean(x), median = median(x), sd = sd(x),
    min = min(x), q1 = unname(quantile(x, 0.25)), q3 = unname(quantile(x, 0.75)),
    max = max(x), iqr = IQR(x))
}

descriptives <- sapply(students[c("G3", "age", "absences", "failures", "studytime", "goout", "Medu")],
                       describe_numeric)
round(t(descriptives), 2)

# G3 without the zeros, for comparison
round(describe_numeric(students$G3[students$G3 > 0]), 2)

# Frequency tables
table(students$pass)
round(prop.table(table(students$pass)) * 100, 1)
table(students$sex)
table(students$higher)
table(students$schoolsup)
table(students$failures)

# Figure 1: distribution of the final grade
p_g3 <- ggplot(students, aes(x = G3, fill = G3 == 0)) +
  geom_histogram(binwidth = 1, boundary = -0.5, colour = "white") +
  geom_vline(xintercept = 9.5, linetype = 2) +
  scale_fill_manual(values = c("FALSE" = "#7a3b69", "TRUE" = "#d9a441"), guide = "none") +
  labs(title = "Distribution of the final Mathematics grade",
       subtitle = "Dashed line = pass mark (10). The 38 grades of zero are highlighted.",
       x = "Final grade (G3)", y = "Students")
save_fig("fig1_g3_hist.png", p_g3)

# Figure 2: final grade by number of previous class failures
p_fail <- ggplot(students, aes(x = factor(failures), y = G3)) +
  geom_boxplot(fill = "#e6d3e0") +
  labs(title = "Final grade by previous class failures",
       x = "Previous class failures", y = "Final grade (G3)")
save_fig("fig2_g3_by_failures.png", p_fail)

# Figure 3: final grade by how often the student goes out
p_goout <- ggplot(students, aes(x = factor(goout), y = G3)) +
  geom_boxplot(fill = "#e6d3e0") +
  labs(title = "Final grade by going out with friends",
       x = "Going out (1 = very low, 5 = very high)", y = "Final grade (G3)")
save_fig("fig3_g3_by_goout.png", p_goout)

# Group means behind the two boxplots
students %>% group_by(failures) %>% summarise(n = n(), mean_G3 = mean(G3), median_G3 = median(G3))
students %>% group_by(goout) %>% summarise(n = n(), mean_G3 = mean(G3), median_G3 = median(G3))

# ============================================================================ #
# QUESTION 3. INVESTIGATE RELATIONSHIPS
# ============================================================================ #

# ---- Correlations (several variables are ordered categories: read with care) -
cor_vars <- students %>%
  select(age, Medu, Fedu, traveltime, studytime, failures,
         famrel, freetime, goout, Dalc, Walc, health, absences, G3)
cor_matrix <- cor(cor_vars)
round(cor_matrix, 2)

g3_correlations <- sort(cor_matrix[, "G3"], decreasing = TRUE)
round(g3_correlations, 3)

round(c(G1 = cor(students$G1, students$G3), G2 = cor(students$G2, students$G3)), 3)

# Absences: the correlation changes sign once the zero grades are set aside
round(c(all_students = cor(students$absences, students$G3),
        without_zero_grades = with(filter(students, G3 > 0), cor(absences, G3))), 3)

# ---- Contingency tables with conditional (column) percentages ----------------
pass_table <- function(variable) {
  counts <- table(students$pass, students[[variable]])
  list(counts = counts, column_percent = round(prop.table(counts, margin = 2) * 100, 1))
}
pass_table("failures")
pass_table("higher")
pass_table("schoolsup")
pass_table("sex")
pass_table("goout")

# Figure 4: pass rate by previous failures
pass_by_failures <- students %>%
  group_by(failures) %>%
  summarise(n = n(), pass_rate = 100 * mean(pass == "Pass"))
pass_by_failures

p_passfail <- ggplot(pass_by_failures, aes(x = factor(failures), y = pass_rate)) +
  geom_col(fill = "#7a3b69") +
  geom_text(aes(label = paste0(round(pass_rate), "%\n(n = ", n, ")")), vjust = -0.2, size = 3.2) +
  ylim(0, 100) +
  labs(title = "Pass rate by previous class failures",
       x = "Previous class failures", y = "Students passing (%)")
save_fig("fig4_pass_by_failures.png", p_passfail)

students %>% group_by(schoolsup) %>% summarise(n = n(), mean_G1 = mean(G1), mean_G3 = mean(G3))

# ============================================================================ #
# QUESTION 4. BUILD AND INTERPRET STATISTICAL MODELS
# ============================================================================ #

#   goout     : lower grades and pass rates for students who go out a lot
#   studytime : a weak association, kept because it is the one behaviour a school
#               can encourage directly
#   sex, higher, schoolsup : categorical characteristics with visible differences
# and that figure is distorted by the zero grades (see above).

# ---- Multiple linear regression (outcome: G3) --------------------------------
linear_model <- lm(G3 ~ failures + Medu + goout + studytime + sex + higher + schoolsup,
                   data = students)
summary(linear_model)
tidy(linear_model, conf.int = TRUE)
glance(linear_model)

starter_linear <- lm(G3 ~ studytime + failures + absences + sex + higher, data = students)
bind_rows(glance(starter_linear) %>% mutate(model = "Starter"),
          glance(linear_model)  %>% mutate(model = "Chosen")) %>%
  select(model, r.squared, adj.r.squared, sigma)

# ---- Logistic regression (outcome: pass) -------------------------------------
# 'Fail' is the first level, so glm() models the probability of 'Pass'.
logistic_model <- glm(pass ~ failures + Medu + goout + studytime + sex + higher + schoolsup,
                      data = students, family = binomial)
summary(logistic_model)

logistic_odds_ratios <- tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE)
print(logistic_odds_ratios)

# for a reference student and for one change at a time.
# level 3, study time 2, wants higher education, no extra school support.
reference <- data.frame(failures = 0, Medu = 3, goout = 3, studytime = 2,
                        sex = factor("F", levels = levels(students$sex)),
                        higher = factor("yes", levels = levels(students$higher)),
                        schoolsup = factor("no", levels = levels(students$schoolsup)))
scenarios <- bind_rows(
  reference %>% mutate(profile = "Reference student"),
  reference %>% mutate(failures = 1, profile = "One previous failure"),
  reference %>% mutate(failures = 2, profile = "Two previous failures"),
  reference %>% mutate(goout = 5, profile = "Goes out very often (5)"),
  reference %>% mutate(sex = factor("M", levels = levels(students$sex)), profile = "Male student")
)
scenarios$probability_of_passing <- predict(logistic_model, newdata = scenarios, type = "response")
scenarios %>% select(profile, probability_of_passing)

predicted_class <- ifelse(predict(logistic_model, type = "response") > 0.5, "Pass", "Fail")
classification <- table(actual = students$pass, predicted = predicted_class)
classification
round(sum(diag(classification)) / sum(classification), 3)     # accuracy
round(mean(students$pass == "Pass"), 3)                       # accuracy of always predicting 'Pass'

# ============================================================================ #
# QUESTION 5. EVALUATE THE ANALYSIS
# ============================================================================ #

# ---- Linear-model residuals --------------------------------------------------
diagnostic_data <- augment(linear_model) %>% mutate(zero_grade = students$G3 == 0)

p_resid <- ggplot(diagnostic_data, aes(x = .fitted, y = .resid, colour = zero_grade)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_colour_manual(values = c("FALSE" = "#7a3b69", "TRUE" = "#d9a441"),
                      labels = c("Grade above 0", "Grade of 0"), name = NULL) +
  labs(title = "Residuals against fitted values (linear model)",
       x = "Fitted values", y = "Residuals") +
  theme(legend.position = "bottom")
save_fig("fig5_residuals_vs_fitted.png", p_resid)

summary(diagnostic_data$.resid)
diagnostic_data %>% group_by(zero_grade) %>% summarise(n = n(), mean_residual = mean(.resid))

# ---- Sensitivity check: the same linear model without the zero grades --------
linear_no_zero <- lm(G3 ~ failures + Medu + goout + studytime + sex + higher + schoolsup,
                     data = filter(students, G3 > 0))
sensitivity <- full_join(
  tidy(linear_model, conf.int = TRUE) %>% select(term, all_students = estimate, p_all = p.value),
  tidy(linear_no_zero, conf.int = TRUE) %>% select(term, without_zeros = estimate, p_without = p.value),
  by = "term")
print(sensitivity)
bind_rows(glance(linear_model) %>% mutate(model = "All 395 students"),
          glance(linear_no_zero) %>% mutate(model = "357 students with G3 > 0")) %>%
  select(model, r.squared, adj.r.squared, sigma)

linear_coef_table <- tidy(linear_model, conf.int = TRUE)
print(linear_coef_table)
print(logistic_odds_ratios)

# Figure 6: odds ratios with 95% confidence intervals (log scale)
or_plot_data <- logistic_odds_ratios %>%
  filter(term != "(Intercept)") %>%
  mutate(label = recode(term, failures = "Each previous failure", Medu = "Mother's education (+1 level)",
                        goout = "Going out (+1 level)", studytime = "Study time (+1 level)",
                        sexM = "Male (vs female)", higheryes = "Wants higher education",
                        schoolsupyes = "Receives school support"))
p_or <- ggplot(or_plot_data, aes(x = estimate, y = reorder(label, estimate))) +
  geom_vline(xintercept = 1, linetype = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(size = 3, colour = "#7a3b69") +
  scale_x_log10() +
  labs(title = "Odds ratios for passing (logistic model)",
       subtitle = "Below 1 = lower odds of passing; above 1 = higher odds",
       caption = "Bars show 95% confidence intervals; horizontal axis on a log scale",
       x = "Odds ratio", y = NULL)
save_fig("fig6_odds_ratios.png", p_or, width = 6.5, height = 3.8)

# ---- Overlap between the numerical predictors --------------------------------
round(cor(students %>% select(failures, Medu, goout, studytime)), 2)

# ============================================================================ #
# QUESTION 6. DRAW APPROPRIATE CONCLUSIONS
# ============================================================================ #
#   Scenario2_Student_Performance_Report_SOLUTION_.pdf

################################################################################
# END OF ANALYSIS FILE
################################################################################

# Diabetes progression

Final project, Data Analytics and Statistics, Scenario 1.

**Question:** Which baseline measures are associated with disease progression after one year?

## Files

| File | What it is |
|---|---|
| `Scenario2_Student_Performance_Solution.R` | The analysis script; reads the data from this folder and writes the figures |
| `Scenario2_Student_Performance_Report.pdf` | The 5-page report |
| `Scenario2_Student_Performance_Report.Rmd` | The R Markdown source of the report |
| `Scenario2_Student_Performance_Presentation.pptx` | The 6-slide presentation |
| `figures/` | All figures used in the report and slides |
| Data | `diabetes.tab.txt`: 442 patients, 10 baseline measures and disease progression after one year |

## Note on the analysis

Uses five predictors (BMI, blood pressure, s5, s3, sex) rather than the starter's three. The report explains why the full ten-predictor model is worse despite a higher R-squared.

## Running

Open the `.R` file in RStudio or Posit Cloud, set the working directory to this folder, and run it. Uses the `tidyverse` and `broom` packages. To rebuild the report, knit the `.Rmd` file.

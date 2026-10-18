# Student performance

Final project, Data Analytics and Statistics, Scenario 2.

**Question:** Which student and family factors are associated with the final grade and with passing?

## Files

| File | What it is |
|---|---|
| `*_Analysis_*.R` | The analysis script; reads the data from this folder and writes the figures |
| `*_Report_*.pdf` | The 5-page report |
| `*_Report_*.Rmd` | The R Markdown source of the report |
| `*_Presentation_*.pptx` | The 6-slide presentation |
| `figures/` | All figures used in the report and slides |
| Data | `student-mat.csv`: 395 Portuguese secondary-school maths students |

## Note on the analysis

38 students have a final grade of exactly zero and no recorded absences. The report treats this as a data-quality issue, keeps them in the main analysis as the brief defines it, and shows which conclusions change without them.

## Running

Open the `.R` file in RStudio or Posit Cloud, set the working directory to this folder, and run it. Uses the `tidyverse` and `broom` packages. To rebuild the report, knit the `.Rmd` file.

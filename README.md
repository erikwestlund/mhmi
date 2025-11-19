# Missing Data and Imputation 1

## Overview
This repository contains materials from a presentation on missing data concepts, challenges, and strategies for addressing incomplete data in statistical analyses. The presentation uses simulation-based demonstrations with directed acyclic graphs (DAGs) and linear models to build intuition for missing data mechanisms and imputation approaches.

## Repository Structure

The project includes:
- **presentation.qmd**: Main Quarto presentation file
- **functions/**: Helper R functions auto-sourced by framework
- **settings.yml**: Project configuration and package management

## Presentation Topics

The presentation covers:

- **MCAR (Missing Completely at Random)**: Random missingness with no bias, only precision loss
- **MAR (Missing at Random)**: Missingness driven by observed variables
- **MNAR (Missing Not at Random)**: Missingness driven by unobserved values
- **Inferior imputation methods**: Mean imputation, mean + indicator, single regression imputation
- **Multiple Imputation**: An introduction to to the concept; a future presentation will cover how to implement it.

## Technical Stack

**Software:** R and Quarto for reproducible presentations
**Framework:** Built using the `framework` R package for standardized project structure
**Required R packages:** dplyr, ggplot2, purrr, tidyr, ggdag, dagitty, kableExtra, mice, and miceadds

## Rendering
To render the presentation:
```r
quarto render presentation.qmd
```

The output is a self-contained HTML file using Revealjs for slide transitions.

## Author
Erik Westlund
Johns Hopkins Bloomberg School of Public Health
ewestlund@jhu.edu

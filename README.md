# Outlier Detection Shiny App

## Overview

This is a Shiny web application for outlier detection and visualization. It allows users to upload a CSV file, select numeric and categorical variables, and generates plots and summary tables based on the user's selections.

## Table of Contents

- [Features](#features)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
- [Usage](#usage)
- [Authors](#authors)

## Features

- Upload a CSV file for outlier detection.
- Select a numeric variable and an optional second variable (categorical or numeric).
- Visualize data using scatter plots, box plots, or ridgeline plots.
- View summary tables for selected variables.
- User manual and about page included.

## Getting Started

### Prerequisites

Before running the Shiny app, make sure you have R and RStudio installed on your system. You will also need to install the following R packages:

- `shiny`
- `shinythemes`
- `data.table`
- `ggplot2`
- `labeling`
- `ggridges`
- `gridExtra`

You can install these packages using R's package manager, `install.packages("package_name")`.

### Installation

1. Clone or download the repository to your local machine.

2. Open the project in RStudio.

3. Run the Shiny app by executing the following code in R:

```R
shiny::runApp("path_to_app_directory")
```
### Authors

This Shiny web application was developed by the following authors:

- **Tushar Badhwar**
  - tb3070@columbia.edu, https://github.com/tushar-badhwar

- **Harmeet Singh Bagga**
  - hb2783@columbia.edu https://github.com/harmith-singh

If you have any questions, feedback, or need assistance with the app, feel free to reach out to the authors through their GitHub profiles or contact them via email.

We welcome collaboration and contributions to this project. If you'd like to get involved, please don't hesitate to get in touch with us.


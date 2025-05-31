# UIUC Enrollment Visualization App

## Overview

This R Shiny web application provides interactive visualizations and summaries of student enrollment data at the University of Illinois Urbana-Champaign (UIUC), filtered by semester, college, degree, major, and concentration. The app helps users explore enrollment trends across demographic dimensions such as race/ethnicity, sex, and residency.

The data covers enrollment from Fall 2004 to Spring 2025, with key changes over time:

- In Summer 2010, a new race/ethnicity classification system was adopted.

- Starting in Fall 2020, data on Underrepresented Minorities (URM) was introduced.

These changes are reflected in the dashboard, which supports dynamic exploration across this full time range.

## Project Objective

The goal of this project is to:

- Make UIUC student enrollment data more accessible and visually intuitive.

- Provide summary statistics and charts to support data-driven decision-making.

- Highlight enrollment distributions by race/ethnicity, underrepresented minority status, sex, and Illinois residency.

## Data Sources

- This dashboard utilizes semester enrollment data sourced from the [Division of Management Information - UIUC](https://dmi.illinois.edu/stuenr/#race). The data sets contain demographic records of students by curriculum.


## Process 

1. **Data Ingestion:** Web scraping was used to automatically downloads the enrollment data in Excel format from the DMI site. 
2. **Data Cleaning:** Data is cleaned and preprocessed to ensure accuracy and relevance. 
3. **Dynamic Filtering**: Users can filter data by semester, college, degree, major, and concentration.
4. **Visualizations**: Displays various plots and graphs for enrollment data across multiple dimensions.
5. **Data Summary**: Provides a textual summary of enrollment statistics, including totals and breakdowns based on selected filters.
6. **Dashboard Development:** An interactive R shiny app is created to display the analysis results.

## Technologies Used

- R: Main programming language for data processing and Shiny app.

- rvest: Web scraping for downloading Excel files.

- tidyverse: Data manipulation, plotting, and summarization.

- Shiny: Framework for building the web-based application.

- scales: For formatting numbers with commas (e.g., 1,000 instead of 1000).

- DT: Data tables for displaying filtered datasets.


## Dashboard
Access the interactive [dashboard here]()

![Shiny App Interface for Exploring University of Illinois Enrollment Data](EnrollmentDashboard.gif)

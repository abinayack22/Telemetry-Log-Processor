# Sensor Log Processor - R Shiny Web Application

## Overview
Sensor Log Processor is an R Shiny based web application developed during an internship project at Airports Authority of India (AAI). The application processes telemetry sensor log CSV files, cleans the data, identifies warning and alarm events, and generates structured Excel reports.

## Technologies Used
- R Programming
- R Shiny
- CSV Data Processing
- Excel Report Generation

## Features
- Upload telemetry CSV files through a web interface
- Process and clean raw telemetry data
- Handle inconsistent CSV formats and delimiters
- Detect warning and alarm events
- Apply configurable time threshold filtering
- Ignore temporary warnings that return to normal within the selected threshold
- Export processed results as Excel files

## Workflow
1. Upload telemetry log CSV file
2. Read and preprocess the data
3. Identify warnings and alarms
4. Apply filtering rules
5. Generate cleaned Excel report

## How to Run

Install required R packages:

```r
install.packages("shiny")
install.packages("dplyr")
install.packages("readxl")
install.packages("writexl")
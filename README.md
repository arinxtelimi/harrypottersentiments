# Harry Potter Text Analysis

## Overview
Interactive Shiny application that performs sentiment analysis and word trend tracking across all seven Harry Potter books. The app uses the AFINN lexicon for sentiment scoring and provides visualizations of textual patterns throughout the series.

## Features

### Analysis 1: Sentiment Analysis
- **Line Graph**: Visualizes sentiment score trends across book sections
- **Data Table**: Shows average sentiment scores per book
- **Customization Options**:
  - Book selection (individual or all books)
  - Stopword inclusion/exclusion
  - Highlight top emotional sections

### Analysis 2: Word Trend Analysis
- **Line Graph**: Tracks frequency of user-specified words
- **Data Table**: Displays total word counts per book
- **Customization Options**:
  - Any word search (case-insensitive)
  - Highlight most frequent sections
  - Compare across all books

## Data Sources
- `harry_potter_books.csv`: Full text of all seven Harry Potter novels
- `afinn.csv`: AFINN lexicon for sentiment scoring (words rated -5 to +5)

## How to Run
Ensure required R packages are installed:
```R
install.packages(c("shiny", "tidyverse", "tidytext", "DT", "plotly"))

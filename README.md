
# Bradford Protein Assay Analyzer Shiny App

This Shiny app provides a graphical user interface for analyzing protein concentrations using the Bradford protein assay. The app includes protocols for standard preparation and measuring protein concentration in unknown samples.

## Table of Contents
1. [Prerequisites](#prerequisites)
2. [Installing R and RStudio](#installing-r-and-rstudio)
3. [Installing Required R Packages](#installing-required-r-packages)
4. [Running the Shiny App](#running-the-shiny-app)
5. [Using the App](#using-the-app)
6. [Example CSV Files](#example-csv-files)
7. [Troubleshooting](#troubleshooting)

## Prerequisites
- **Operating System**: Windows, macOS, or Linux
- **Internet Connection**: Required to download R, RStudio, and the necessary R packages

## Installing R and RStudio

### 1. Download and Install R
- Visit the [R Project website](https://cran.r-project.org/).
- Select your operating system:
  - **Windows**: Click on "Download R for Windows" and then click "base" -> "Download R x.x.x for Windows".
  - **macOS**: Click on "Download R for macOS" and select the appropriate version for your macOS.
  - **Linux**: Follow the instructions specific to your Linux distribution provided on the website.
- Download and run the installer file.
- Follow the on-screen instructions to complete the installation.

### 2. Download and Install RStudio
- Visit the [RStudio website](https://www.rstudio.com/products/rstudio/download/).
- Download the free version of RStudio Desktop for your operating system.
- Run the installer file and follow the on-screen instructions to complete the installation.

## Installing Required R Packages

Before running the app, you need to install the required R packages (`shiny`, `ggplot2`, `dplyr`, `DT`, `readr`). Follow these steps:

1. Open RStudio.
2. In the **Console** (bottom left panel), type the following commands and press Enter:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "DT", "readr"))
```

This command will download and install the necessary packages. It might take a few minutes depending on your internet connection.

## Running the Shiny App

### 1. Download the Shiny App Script
- Save the `app.R` file to a known location on your computer. For this example, let's assume you save it to a folder named `BradfordAssayApp`.

### 2. Open the Shiny App in RStudio
- Open RStudio.
- Go to `File` -> `Open File...`.
- Navigate to the `app.R` file you downloaded and open it.

### 3. Run the Shiny App
- In RStudio, click on the **Run App** button located in the top right corner of the `app.R` script window.
- A new window will open displaying the Shiny app interface. You can now use the app to analyze your Bradford assay data.

### 4. Run the App from the Console (Alternative)
If you prefer to run the app without using the RStudio interface, you can execute the following command in the Console:

```r
shiny::runApp("path/to/your/app.R")
```

Replace `"path/to/your/app.R"` with the actual path to the `app.R` file on your computer.

## Using the App

1. **CSV Upload Tab**:
   - Upload your standards and unknown samples CSV files.
   - Process and analyze the data to generate the standard curve and determine protein concentrations.

2. **Manual Input Tabs**:
   - Manually input standard curve data and unknown sample absorbance values.
   - Generate a standard curve and analyze unknown samples.

3. **Protocols Tab**:
   - Follow the detailed protocols for preparing standards and measuring protein concentrations.

## Example CSV Files

The Shiny app requires CSV files in a specific format for the standards and unknown samples. Example CSV files (`standards.csv` and `unknown_sample.csv`) are provided in this repository to guide you:

- `standards.csv`: This file contains the standard curve data with columns for standard ID, concentration, and absorbance. Format your standards data in this way for successful processing.
- `unknown_sample.csv`: This file contains data for unknown samples with columns for sample ID and absorbance. Format your unknown samples data according to this example for correct analysis.

Use these example files as templates to format your data correctly before uploading them to the app.

## Troubleshooting

- **Error in Installing Packages**: If you encounter an error while installing packages, ensure that you have a stable internet connection and that R is correctly installed.
- **Shiny App Not Running**: If the app does not run, check the Console for error messages. Ensure all required packages are installed and loaded without errors.
- **Missing `Run App` Button**: If you do not see the `Run App` button in RStudio, make sure you have the `shiny` package installed and the `app.R` file is open in the editor.

## Additional Notes

- **Updating R and RStudio**: Occasionally, updates are released for R and RStudio. Ensure that you are using the latest versions to avoid compatibility issues.
- **Saving Your Work**: If you make changes to the `app.R` file, save your work using `File` -> `Save` in RStudio.
- **Learning Resources**: If you are new to R and RStudio, the [RStudio Education](https://education.rstudio.com/) website provides helpful tutorials and documentation.

With this app, you can easily perform protein concentration analysis using the Bradford assay, with a user-friendly interface to guide you through the process.

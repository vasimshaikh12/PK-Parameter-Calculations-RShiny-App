PK Parameter Calculations and Visualizations - RShiny App

This RShiny application provides an interactive platform for calculating pharmacokinetic (PK) parameters and visualizing the results. Users can upload their concentration-time data, compute key PK metrics, and generate insightful visualizations for analysis and reporting.


---

Features

PK Parameter Calculations:

Maximum Concentration (Cmax)

Area Under the Curve (AUCt, AUCinf)

Elimination Rate Constant (kel)

Half-life (t1/2)

Time of Maximum Concentration (Tmax)

Time to Last Quantifiable Concentration (LQCT)

Threshold Time (TLIN)


Interactive Visualizations:

Boxplots for PK parameters by treatment groups

Test/Reference ratio plots with 80%-125% bioequivalence limits

Combined visual representations for in-depth comparisons


Data Handling Capabilities:

Upload custom datasets in CSV format

Built-in sample dataset for demonstration purposes

Real-time calculation of summary statistics




---

Installation

1. Ensure R and RStudio are installed on your system.


2. Install the required R packages:



install.packages(c("shiny", "ggplot2", "dplyr", "tidyverse", "readxl", "patchwork"))

3. Clone this repository or download the project files:



git clone https://github.com/your-username/PK-Parameter-Calculations-RShiny-App.git

4. Set the working directory in R:



setwd("path/to/PK-Parameter-Calculations-RShiny-App")


---

Usage

1. Run the Shiny App:



shiny::runApp()

2. Upload Data:

Click "Upload CSV File" to select your dataset (ensure it contains sub, TRT, time, and concentration columns).



3. Select Parameters:

Choose the desired PK parameter from the dropdown menu.

Filter by treatment group (Test or Reference).



4. Analyze and Visualize:

View summary statistics for selected parameters.

Examine test/reference ratios with visual indicators.





---

Project Structure

PK-Parameter-Calculations-RShiny-App/
│-- conc_data.xlsx          # Sample dataset  
│-- app.R                   # Main RShiny application script  
│-- README.md               # Project documentation  
│-- LICENSE                 # Project license  
│-- data/                   # Directory for data files  
│-- plots/                   # Directory for generated plots


---

Example Plots

Below are some example plots generated by the application:

Cmax by Treatment:


AUC Test/Reference Ratios:




---

Known Issues

Ensure that uploaded datasets do not contain missing values or incorrect formats.

The calculation of kel may require adjustment based on specific dataset characteristics.



---

Contributing

Contributions are welcome! If you have suggestions or improvements, please follow these steps:

1. Fork the repository


2. Create a new feature branch (git checkout -b feature-branch)


3. Commit your changes (git commit -m "Add new feature")


4. Push to the branch (git push origin feature-branch)


5. Open a Pull Request

---

Contact

For questions or suggestions, feel free to reach out via GitHub issues.


---

Enhance your pharmacokinetic data analysis with this interactive and user-friendly RShiny application!


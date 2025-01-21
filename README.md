# PK-Parameter-Calculations-RShiny-App
A basic RShiny app for calculating PK parameters and displaying visualizations
# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Sample dataset (replace with actual dataset or upload functionality)
library(readxl)
conc_data <- read_excel("C:/Users/91898/Desktop/conc_data.xlsx")

str(conc_data)
colnames(conc_data)


# Convert 'time' to numeric after reshaping
conc_long <- conc_long %>%
  mutate(time = as.numeric(time))


# Check the reshaped data

head(conc_long)


# Calculate Cmax
cmax <- conc_long %>%
  group_by(sub, TRT) %>%
  summarize(Cmax = max(concentration, na.rm = TRUE))

# Calculate auc_t using the trapezoidal rule
auc_t <- conc_long %>%
  group_by(sub, TRT) %>%
  arrange(time) %>% # Sort by time
  summarize(
    AUCt = sum((lead(time) - time) * (lead(concentration) + concentration) / 2, na.rm = TRUE)
  )


head(auc_t)

# Calculate kel 
kel <- conc_long %>%
  group_by(sub, TRT) %>%
  arrange(desc(time)) %>% #order by descending time
  mutate(log_conc = log(concentration)) %>%
  filter(!is.infinite(log_conc)) %>%
  slice(1:3) %>% #take the last 3 points. This should be adjusted based on the data. More sophisticated methods exist.
  summarize(kel = -coef(lm(log_conc ~ time))[2], .groups = "drop")


# Calculate AUCinf (AUC to infinity)
aucinf <- left_join(auc_t, kel, by = c("sub", "TRT")) %>%
  group_by(sub, TRT) %>%
  summarize(
    Clast = last(conc_long$concentration),
    AUCinf = AUCt + (Clast / kel), .groups = "drop"
  )
# Calculate t1/2
t_half <- kel %>%
  mutate(t1_2 = log(2) / kel)

# Calculate Tmax
tmax <- conc_long %>%
  group_by(sub, TRT) %>%
  filter(concentration == max(concentration, na.rm = TRUE)) %>%
  summarize(Tmax = time[1])

# Calculate threshold
tlin <- conc_long %>%
  filter(time > 2 & time < 5) %>% # Adjust range based on visualization
  group_by(sub, TRT) %>%
  summarize(TLIN = min(time))



# Calculate LQCT  

LOQ <- 0.5 # Example limit of quantification
lqct <- conc_long %>%
  filter(concentration > LOQ) %>%
  group_by(sub, TRT) %>%
  summarize(LQCT = max(time))


pk_summary <- cmax %>%
  left_join(tmax, by = c("sub", "TRT")) %>%
  left_join(auc_t, by = c("sub", "TRT")) %>%
  left_join(aucinf, by = c("sub", "TRT")) %>% # Use AUCinf
  left_join(kel, by = c("sub", "TRT")) %>%
  left_join(t_half, by = c("sub", "TRT")) %>%
  left_join(tlin, by = c("sub", "TRT")) %>%
  left_join(lqct, by = c("sub", "TRT"))
print(pk_summary)

#install.packages("ggplot2")
library(ggplot2)

# Plot Cmax by treatment
ggplot(pk_summary, aes(x = TRT, y = Cmax, fill = TRT)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Cmax by Treatment", y = "Cmax", x = "Treatment")


library(tidyverse)

p <- pk_summary %>%
  group_by(sub) %>%
  mutate(
    Cmax_Test = ifelse(TRT == "T", Cmax, NA),
    Cmax_Reference = ifelse(TRT == "R", Cmax, NA)
  ) %>%
  ungroup()

p2 <- p %>%
  mutate(Cmax_ratio = Cmax_Test / Cmax_Reference)

p3 <- p %>%
  group_by(sub) %>%
  mutate(
    Cmax_Test = ifelse(TRT == "T", Cmax, NA),
    Cmax_Reference = ifelse(TRT == "R", Cmax, NA)
  ) %>%
  tidyr::fill(Cmax_Test, Cmax_Reference, .direction = "downup") %>%
  ungroup() %>%
  mutate(Cmax_ratio = Cmax_Test / Cmax_Reference)


summary(p3$Cmax_ratio)
library(dplyr)

p3 <- p3 %>%
  group_by(sub) %>%
  mutate(
    AUCt_Test = ifelse(TRT == "T", AUCt, NA),
    AUCt_Reference = ifelse(TRT == "R", AUCt, NA)
  ) %>%
  tidyr::fill(AUCt_Test, AUCt_Reference, .direction = "downup") %>%
  ungroup()

p3 <- p3 %>%
  mutate(AUCt_ratio = AUCt_Test / AUCt_Reference)

head(p3)

p3 <- p3 %>%
  group_by(sub) %>%
  mutate(
    AUCinf_Test = ifelse(TRT == "T", AUCinf, NA),
    AUCinf_Reference = ifelse(TRT == "R", AUCinf, NA)
  ) %>%
  tidyr::fill(AUCinf_Test, AUCinf_Reference, .direction = "downup") %>%
  ungroup()

p3 <- p3 %>%
  mutate(AUCt_ratio = AUCinf_Test / AUCinf_Reference)

head(p3)


# Calculate Ratios
ratio_data <- p3 %>%
  pivot_wider(names_from = TRT, values_from = c(Cmax, AUCt, AUCinf)) %>%
  mutate(
    Cmax_ratio = Cmax_Test / Cmax_Reference,
    AUCt_ratio = AUCt_Test / AUCt_Reference,
    AUCinf_ratio = AUCinf_Test / AUCinf_Reference
  )

# Generate Plots
cmax_plot <- plot_ratios(ratio_data, "Cmax")
auct_plot <- plot_ratios(ratio_data, "AUCt")
aucinf_plot <- plot_ratios(ratio_data, "AUCinf")

# Display Plots (You can arrange them using patchwork if needed)
print(cmax_plot)
print(auct_plot)
print(aucinf_plot)

#install.packages("patchwork")
##install.packages("ggplot2")


library(ggplot2)
library(patchwork)


combined_plot <- cmax_plot + auct_plot + aucinf_plot + plot_layout(ncol = 1)
print(combined_plot)

# Define the UI
ui <- fluidPage(
  titlePanel("PK Parameter Calculations & Visualizations"),
  
  sidebarLayout(
    sidebarPanel(
      # Input for selecting PK parameter
      selectInput("param", "Select PK Parameter:", choices = c("Cmax", "AUCt", "AUCinf")),
      
      # Input for selecting Treatment group
      selectInput("treatment", "Select Treatment Group:", choices = c("Test", "Reference")),
      
      # Input for custom data upload (CSV)
      fileInput("file1", "Upload CSV File", accept = ".csv")
    ),
    
    mainPanel(
      # Output for summary statistics
      tableOutput("summaryStats"),
      
      # Output for Test/Reference ratio plot
      plotOutput("ratioPlot")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive data input (uploading the file or using sample data)
  dataset <- reactive({
    req(input$p3)  # Ensure file is uploaded
    uploaded_data <- read.csv(input$file1$datapath)
    if (any(is.na(uploaded_data))) {
      return(NULL)  # Return NULL if there are NA values
    }
    uploaded_data
  })
  
  # Default dataset if no file is uploaded
  default_dataset <- reactive({
    if (is.null(input$p3)) {
      pk_data  # Use the sample data if no file is uploaded
    } else {
      dataset()
    }
  })
  
  # Calculate and display summary statistics dynamically
  output$summaryStats <- renderTable({
    param_data <- default_dataset()[[input$param]]
    
    summary_data <- data.frame(
      Min = min(param_data, na.rm = TRUE),
      Max = max(param_data, na.rm = TRUE),
      Mean = mean(param_data, na.rm = TRUE),
      Median = median(param_data, na.rm = TRUE),
      SD = sd(param_data, na.rm = TRUE),
      CV = (sd(param_data, na.rm = TRUE) / mean(param_data, na.rm = TRUE)) * 100
    )
    
    return(summary_data)
  })
  
  # Plot Test/Reference ratio with reference lines
  output$ratioPlot <- renderPlot({
    req(default_dataset())  # Ensure that dataset is not NULL
    
    data_subset <- default_dataset() %>%
      filter(TRT %in% c("Test", "Reference"))
    
    if (nrow(data_subset) == 0) {
      return(NULL)  # If the subset is empty, return NULL to avoid errors
    }
    
    # Calculate the Test/Reference ratio
    data_subset <- data_subset %>%
      mutate(ratio = case_when(
        input$param == "Cmax" ~ Cmax / lag(Cmax),
        input$param == "AUCt" ~ AUCt / lag(AUCt),
        input$param == "AUCinfnf" ~ AUCinfnf / lag(AUCinfnf)
      ))
    
    # Plot the Test/Reference ratio with reference lines at 0.8 and 1.25
    ggplot(data_subset, aes(x = sub, y = ratio, color = TRT)) +
      geom_line() +
      geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 1.25, linetype = "dashed", color = "blue") +
      labs(title = paste(input$param, "Test/Reference Ratios"),
           x = "Subject", y = paste(input$param, "Ratio")) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)


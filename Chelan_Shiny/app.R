
# General Data Tidying
library(tidyverse)
library(janitor)
library(data.table)
library(scales)
library(stats)

# For Animations
library(ggiraph)
library(gganimate)

# For Dashboard
library(shinydashboard)
library(shiny)
library(rsconnect)

chelan_df <- read_csv(file= "PreAssignmentDataSet_StatorTemp.csv") %>%
  clean_names()


chelan_df_long <- chelan_df %>%
  pivot_longer(cols = -c(1,2), 
               names_to = c("unit_num", ".value"), 
               names_pattern = "c_(\\d+)_(\\w+)_") %>%
  mutate(unit_num = as.factor(as.numeric(unit_num)))


chelan_df_long$unit_num <- paste("Unit C", chelan_df_long$unit_num, sep = "")

colSums(is.na(chelan_df_long))
sum(is.na(chelan_df_long))

chelan_df_long[is.na(chelan_df_long)] <- 0

chelan_df_long <- chelan_df_long %>%
  mutate(unit_status = ifelse(total_current > 10, "On", "Off")) %>%
  mutate(timestamp_utc = as.POSIXct(timestamp_utc)) %>%
  mutate(unit_num = as.factor(unit_num),
         seasons = ifelse(timestamp_utc < "2018-03-20",
                          "Winter",
                          ifelse(timestamp_utc < "2018-06-21",
                                 "Spring",
                                 ifelse(timestamp_utc < "2018-09-22",
                                        "Summer",
                                        ifelse(timestamp_utc < "2018-12-21",
                                               "Autumn", "Winter")))),
         seasons = as.factor(seasons))

plot_chelan_loess_may <- function(data,
                                 time_var,
                                 y_var,
                                 subset_var = NULL,
                                 subset_level = NULL,
                                 start_date = NULL,
                                 end_date = NULL) {
  
  plot_title <- ""
  
  if (!is.null(subset_level)) {
    plot_title <- paste0(plot_title, "", subset_level)
  }
  
  if (!is.null(subset_var) && !is.null(subset_level)) {
    data <- data %>% filter(!!as.name(subset_var) == subset_level)
  }
  
  if (!is.null(start_date) && !is.null(end_date)) {
    data <- data %>% 
      filter(as.Date(data[[time_var]]) >= as.Date(start_date) & as.Date(data[[time_var]]) <= as.Date(end_date))
  }
  
  min_idx <- which.min(data[[y_var]])
  max_idx <- which.max(data[[y_var]])
  
  if (y_var == "avg_winding_temp") {
    y_axis_title <- "Average Stator Winding Temperature (°C)"
    legend_title <- "(°C)"
  } else if (y_var == "total_current") {
    y_axis_title <- "Total Current (A)"
    legend_title <- "(A)"
  } else if (y_var == "avg_cooling_water_flow_gal") {
    y_axis_title <- "Average Cooling Water Flow (gal/min)"
    legend_title <- "(gal/min)"
  } else if (y_var == "avg_cooling_water_temp") {
    y_axis_title <- "Average Cooling Water Temperature (°C)"
    legend_title <- "(°C)"
  } else if (y_var == "avg_cooling_air_out_temp") {
    y_axis_title <- "Average Cooling Air Out Temperature (°C)"
    legend_title <- "(°C)"
  }
  
  ggplot(data %>% filter(month(data[[time_var]]) == 5),
         aes_string(x = time_var, y = y_var, color = y_var)) +
    geom_point(aes(col = ..y..)) +
    geom_smooth(color = "grey", size = 1.5) +
    geom_point(aes(x = data[[time_var]][min_idx], y = data[[y_var]][min_idx]), color = "blue", size = 3) +
    geom_point(aes(x = data[[time_var]][max_idx], y = data[[y_var]][max_idx]), color = "red", size = 3) +
    scale_color_gradient2(low = "blue", high = "red", mid = "yellow",
                          midpoint = median(data[[y_var]]),
                          name = legend_title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = .5),
          axis.title.x = element_blank()) +
    labs(y = y_axis_title, title = plot_title) +
    scale_x_datetime(date_labels = "%b %d",
                     breaks = date_breaks("2 days"),
                     labels = format(as.POSIXct(data[[time_var]]), "%b %d")) +
    geom_hline(yintercept = ifelse(y_var == "avg_cooling_air_out_temp",
                                   27,
                                   NA_real_),
               linetype = "dashed",
               alpha = 1,
               colour = "green")
  
}

sum_table <- function(data,
                      time_var,
                      y_var,
                      subset_var = NULL,
                      subset_level = NULL,
                      start_date = NULL,
                      end_date = NULL) {
  
  
  if (!is.null(subset_var) && !is.null(subset_level)) {
    data <- data %>% filter(!!as.name(subset_var) == subset_level)
  }
  
  if (!is.null(start_date) && !is.null(end_date)) {
    data <- data %>% 
      filter(as.Date(data[[time_var]]) >= as.Date(start_date) & as.Date(data[[time_var]]) <= as.Date(end_date))
  }
  
  if (y_var == "avg_cooling_air_out_temp") {
    data %>%
      select(!!sym(time_var), !!sym(y_var)) %>%
      summarize(Min = round(min(!!sym(y_var)), 2),
                Max = round(max(!!sym(y_var)), 2),
                Median = round(median(!!sym(y_var)), 2),
                N = nrow(data),
                `Proportion too Hot` = round(mean(!!sym(y_var) > 27), 4))
  } else {
    data %>%
      select(!!sym(time_var), !!sym(y_var)) %>%
      summarize(Min = round(min(!!sym(y_var)), 2),
                Max = round(max(!!sym(y_var)), 2),
                Median = round(median(!!sym(y_var)), 2),
                N = nrow(data))
    
  }
  
  
}

ui <- dashboardPage(
  dashboardHeader(title = "Unit Diagnostics"),
  dashboardSidebar(
    selectInput("Unit", 
                label = "Unit", 
                choices = c("All Units", 
                            "Unit C2", 
                            "Unit C4",
                            "Unit C5",
                            "Unit C6",
                            "Unit C7"),
                selected = NULL,
                width = "100%"),
    selectInput("Variable", 
                label = "Measurement", 
                choices = list(
                  "Avg Winding Temp" = "avg_winding_temp", 
                  "Total Current" = "total_current",
                  "Avg Cooling Water Flow" = "avg_cooling_water_flow_gal",
                  "Avg Cooling Water Temp" = "avg_cooling_water_temp",
                  "Avg Cooling Air Out Temp" = "avg_cooling_air_out_temp"),
                width = "100%"),
    dateRangeInput("dates", 
                   "Date Range",
                   start = "2018-05-01", 
                   end = "2018-05-31",
                   min = "2018-05-01",
                   max = "2018-05-31")
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 400, width = 400)
      )),
    fluidRow(
      box(tableOutput("table1")
      )
    )
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    if (input$Unit == "All Units") {
      plot_chelan_loess_may(chelan_df_long,
                            "timestamp_utc",
                            input$Variable,
                            NULL,
                            NULL,
                            input$dates[1],
                            input$dates[2])
    } else {
      plot_chelan_loess_may(chelan_df_long,
                            "timestamp_utc",
                            input$Variable,
                            "unit_num",
                            input$Unit,
                            input$dates[1],
                            input$dates[2])
    }
  })
  
  output$table1 <- renderTable({
    if (input$Unit == "All Units") {
      sum_table(chelan_df_long,
                "timestamp_utc",
                input$Variable,
                NULL,
                NULL,
                input$dates[1],
                input$dates[2])
    } else {
      sum_table(chelan_df_long,
                "timestamp_utc",
                input$Variable,
                "unit_num",
                input$Unit,
                input$dates[1],
                input$dates[2])
    }
  })
}

shinyApp(ui, server)

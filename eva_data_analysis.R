library(tidyverse)
library(jsonlite)
library(lubridate)

# Files
input_file  <- "eva_data.json"
output_file <- "eva_data.csv"
graph_file  <- "cumulative_eva_graph.png"

# 1) Read JSON array into a tibble
read_json_todatafame <- function(input_file){
  jsonlite::fromJSON(input_file) |>
  as_tibble()
}
# 2) Convert types and drop missing duration/date
#' Title Converts 
#'
#' @param df A data frame or tibble containing EVA records
#' @param output_file name of output
#'
#' @returns The cleaned dataframe
#' @export
#'
#' @examples 
#' eva_tbl <- write_dataframe_to_csv(eva_tbl,output_file)
write_dataframe_to_csv <- function(df,output_file){
df <- df |>
  mutate(
    eva  = as.numeric(eva),
    date = ymd_hms(date, quiet = TRUE)
  ) |>
  filter(!is.na(duration), duration != "", !is.na(date))

# 3) Write CSV (index=False equivalent)
  readr::write_csv(df, output_file)
  df
}


#' Title Plot cummulative EVA time in space and save figure
#'
#' @param df A dataframe or tibble containing EVA records
#' @param graph_file output file name and path
#'
#' @returns Invisibly returns ggplot object
#' @export
#'
#' @examples
#' plot_cumulative_time_in_space(df=eva_tbl,graph_file)
plot_cumulative_time_in_space <- function(df,graph_file) {
  # 4) Sort by date
  df <- df |>
    arrange(date)
  
  # 5) duration_hours + cumulative_time
  df <- df |>
    mutate(
      duration_hours = {
        parts <- str_split(duration, ":", n = 2, simplify = TRUE)
        as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60
      },
      cumulative_time = cumsum(duration_hours)
    )
  
  
# 6) Plot + save
  p <- ggplot(df, aes(x = date, y = cumulative_time)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Total time spent in space to date (hours)"
  ) +
  theme_minimal()

 ggsave(graph_file, plot = p, width = 9, height = 5, dpi = 300)
 #print(p)
}


#Pipeline 
eva_tbl <- read_json_todatafame(input_file)
eva_tbl <- write_dataframe_to_csv(eva_tbl,output_file)
plot_cumulative_time_in_space(df=eva_tbl,graph_file)

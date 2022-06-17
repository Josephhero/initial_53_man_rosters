library(tidyverse)
library(gt)
library(gtExtras)

# dataset ---------------------------------------------------------------------

my_team = "KC"

init_data_raw <- read_csv("https://raw.githubusercontent.com/ajreinhard/NFL-public/main/misc-data/2013_to_2021_init53.csv")

init_data <- init_data_raw |> 
  filter(team == my_team) |> 
  mutate(position_pff = case_when(position_pff == "HB" ~ "RB", 
                                  TRUE ~ position_pff)) |> 
  mutate(position_pff = factor(position_pff, c(
                                 "QB", "WR", "TE", "RB", "FB", "T", "G", "C", 
                                 "ED", "DI", "LB", "S", "CB", "K", "P", "LS")
                               )
         ) |> 
  mutate(side = case_when(position_pff %in% c("QB", "WR", "TE", "RB", "FB", "T", "G", "C") ~ "Offense", 
                          position_pff %in% c("ED", "DI", "LB", "S", "CB") ~ "Defense", 
                          TRUE ~ "Specialists"))
  
init_df <- init_data |> 
  count(season, side, position_pff, name = "pos_count") |> 
  arrange(season, position_pff) |> 
  pivot_wider(names_from = season, values_from = pos_count) 
  
  
init_df1 <- init_df |> 
  mutate(Avg = select(init_df, "2013":"2021") |>  rowMeans(na.rm = TRUE)) 




# plot -------------------------------------------------------------------------

init_tab <- gt(init_df1, 
          rowname_col = "position_pff", 
          groupname_col = "side" 
          ) |> 
        gt_theme_538()


final_tab <- init_tab |> 
  tab_header(title = paste(my_team, " Initial 53 under Andy Reid")) |> 
  fmt_number(columns = Avg, decimals = 1) |> 
  tab_style(
    style = list(
      cell_borders("left"), 
      cell_fill(color = "grey90")
                ), 
    locations = list(
      cells_body(Avg), 
      cells_column_labels(Avg))
    ) |> 
  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center")
                ),
    locations = cells_title("title")
    )

gtsave(final_tab, paste(my_team, " Initial 53.png"), expand = 10)

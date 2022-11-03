library(dplyr)
library(tidyr)
library(gt)
library(tidyverse)
player_df <- tibble(
  player = c(
    "Evan Mobley",
    "Sandro Mamukelashvili",
    "Charles Bassey",
    "Luke Garza",
    "Moses Wright",
    "Neemias Queta",
    "Isaiah Jackson",
    "Day'Ron Sharpe"
  ),
  team = c(
    "USC", "Seton Hall", "Western Kentucky",
    "Iowa", "Georgia Tech", "Utah St", "Kentucky",
    "North Carolina"
  ),
  ht = c(
    "7'0\"",
    "6'10\"",
    "6'10\"",
    "6'11\"",
    "6'9\"",
    "7'1\"",
    "6'11\"",
    "6'10\""
  ),
  dk_pct_time = c(40, 48, 50, 50, 51, 55, 60, 66),
  dk_pps = c(1.62, 1.02, 1.54,1.33,1.46,1.37,1.33,1.18),
  tip_pct_time = c(26, 10, 19, 15, 25, 27, 15, 24),
  tip_pps = c(0.88, .97,1,1.05, .63, .85, .76, .84),
  jmp_pct_time = c(33, 42, 31, 35, 25, 18, 25, 10),
  jmp_pps = c(.91, .91, .78, 1.04, .86, .74, .71, .42)
) %>%
  left_join(
    tibble(
      player = c(
        "Evan Mobley",
        "Sandro Mamukelashvili",
        "Charles Bassey",
        "Luke Garza",
        "Moses Wright",
        "Neemias Queta",
        "Isaiah Jackson",
        "Day'Ron Sharpe"
      ) %>% rep(each = 3),
      shot_type = c("Dunks + Lays", "Hooks + Floats", "Jumpers") %>% rep(8)
    ) %>%
      mutate(
        shot_type = factor(shot_type, levels = c("Jumpers", "Hooks + Floats", "Dunks + Lays")),
        shot_mix = c(
          40, 26, 33,
          48, 10, 42,
          50, 19, 31,
          50, 15, 35,
          51, 25, 25,
          55, 27, 18,
          60, 15, 25,
          66, 24, 10
        )
      ),
    by = "player"
  )

basic_tb <- player_df %>%
  group_by(player) %>%
  dplyr::summarize(dunks = shot_mix[1], list_data = list(shot_mix)) %>%
  arrange(dunks) %>%
  gt()


full.percentage_trail <- as.data.frame(full.percentage2)
colnames(full.percentage_trail) <- c("Never or very rarely true","Rarely true",
                                "Sometimes true","Often true","Very often or always true")

full.percentage_trail.2 <-full.percentage_trail  %>%

  tidyr::separate(col = `Never or very rarely true`,  sep = "%",
                  into= c("Never or very rarely true", "A")) %>%
  tidyr::separate(col = `Rarely true`,  sep = "%",
                  into= c("Rarely true", "B")) %>%
tidyr::separate(col = `Sometimes true`,  sep = "%",
                into= c("Sometimes true", "C")) %>%
tidyr::separate(col = `Often true`,  sep = "%",
                into= c("Often true", "D")) %>%
  tidyr::separate(col = `Very often or always true`,  sep = "%",
                  into= c("Very often or always true", "E"))

full.percentage_trail.3 <- full.percentage_trail.2 %>%
  select(`Never or very rarely true`,`Rarely true`,`Sometimes true`,`Often true`,`Very often or always true`)
Items <- rownames(full.percentage_trail.3)

full.percentage_trail.4 <- cbind(Items,full.percentage_trail.3)

full.percentage_trail.5 <- full.percentage_trail.4 %>%
  pivot_longer( cols = 2:6,
    names_to = "Response",
               values_to = "Value")
full.percentage_trail.5$Value= as.numeric(full.percentage_trail.5$Value)

basic_tb <- full.percentage_trail.5 %>%
  group_by(Items) %>%
  dplyr::summarize(list_data = list(Value))



gt.tab <- gt.long %>%
  group_by(Items) %>%
  # calculate summary stats & create data for the histogram and density plot
  dplyr::summarise(
    nr = n(),
    mean = mean(value, na.rm = TRUE),
    # med = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    hist_data = list(value),
    dens_data = list(value),
    .groups = "drop"
  )


full_tab <- inner_join(gt.tab, basic_tb, by = "Items") %>%
  gt()



full_tab  %>%
  # histogram and density plots
  gtExtras::gt_plt_dist(
    hist_data,
    type = "histogram",
    line_color = "black",
    fill_color = "#00A08799",
    bw = 1,
    same_limit = TRUE)%>%
  gtExtras::gt_plt_dist(
    dens_data,
    type = "density",
    line_color = "black",
    fill_color = "grey",
    bw = 0.75,
    same_limit = TRUE
  )%>%
  gtExtras::gt_plt_bar_stack(list_data, width = 65,
                   labels = c("1",  "3",  "5"),
                   palette= c("#ff4343", "#bfbfbf", "#0a1c2b")) %>%
  # format decimals
  fmt_number(columns = mean:sd, decimals = 1) %>%
  # header
  tab_header(
    title = md("Summary Descriptives (n=532)"),
  ) %>%
  #create groups of columns

  tab_spanner(
    label = "Summary Statistics",
    columns = mean:sd
  ) %>%
  tab_spanner(
    label = "Graphics",
    columns = hist_data:list_data
  ) %>%
  # tab_footnote(
  #   footnote = md("**Shapiro–Wilk test**"),
  #   locations = cells_column_labels(columns = `S-W Statistics`)
  # ) %>%
  # change column names to appear in the table
  cols_label(
    Items = ("Items"),
    mean = ("Mean"),
    sd = ("SD"),
    # `Item-Total Correlation` = ("Item-Total Correlation"),
    # `S-W Statistics` = ("SW"),
    hist_data = "Histogram",
    dens_data = "Density"
  )  %>%
  # set alignment as per wish
  cols_align(
    align = "center",
    columns = mean: dens_data
  ) %>%
  opt_align_table_header(align = "left")

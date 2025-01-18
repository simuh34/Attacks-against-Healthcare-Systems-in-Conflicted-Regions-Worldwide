library(ggplot2)
library(viridis) 
library(dplyr)
library(lubridate)
library(tidyr)
library(tableone)
library(scales)
library(treemap)
library(grid)
library(officer)

####load data from SSA (Jan 2020 to Dec 2024)####
df <- readxl::read_excel("D:\\R project\\war\\Data_202001-202412.xlsx")

####group the conflict by region####
attack_counts <- df %>%
  group_by(`Country / Territory`) %>%
  summarize(Attack_Count = n()) %>%
  arrange(desc(Attack_Count))

df <- df %>%
  mutate(conflict = case_when(
    `Country / Territory` %in% c("Israel", "Lebanon", "occupied Palestinian territory") ~ "Arab–Israeli",
    `Country / Territory` %in% c("Ukraine") ~ "Russo-Ukrainian",  
    `Country / Territory` %in% c("Myanmar") ~ "Myanmar",
    `Country / Territory` %in% c("Afghanistan") ~ "Afghanistan",
    `Country / Territory` %in% c("Democratic Republic of the Congo") ~ "DRC",
    `Country / Territory` %in% c("Syrian Arab Republic") ~ "Syrian",
    `Country / Territory` %in% c("Yemen") ~ "Middle eastern",
    `Country / Territory` %in% c("Nigeria") ~ "Sahel",
    `Country / Territory` %in% c("Libya") ~ "Middle eastern",
    `Country / Territory` %in% c("Mali") ~ "Sahel",
    `Country / Territory` %in% c("Burkina Faso") ~ "Sahel",
    `Country / Territory` %in% c("Sudan") ~ "Sudan and South Sudan",
    `Country / Territory` %in% c("Sudanese") ~ "Sudan and South Sudan",
    `Country / Territory` %in% c("Central African Republic") ~ "CAR",
    TRUE ~ "Others"
  ))
table(df$conflict)

####count the number of attacks by conflict####
attack_counts <- df %>%
  group_by(conflict) %>%
  summarize(Attack_Count = n()) %>%
  arrange(desc(Attack_Count))

#subset War in Afghanistan between 2020-2021
df <- df[!(df$conflict == "Afghanistan" &
             !(format(df$'Attack Date', "%Y") %in% c("2020", "2021"))), ]

#convert the attacks data into month and year
df <- df %>%
  mutate(
    `Attack Date` = as.Date(`Attack Date`),  
    month = floor_date(`Attack Date`, "month"),
    year = format(as.Date(month), "%Y")
  )

#### categorize attack type into military or non military####
unique(df$'Attack Type')
classify <- function(text){
  if (grepl("Violence with heavy weapons|Militarization of a health care asset|Abduction/Arrest/Detention|Obstruction to health care delivery", text)) {
    return(1)
  } else {
    return(0)
  }
}

df$attack_type_overall <- sapply(df$'Attack Type', classify)
table(df$attack_type_overall)

conflicts <- unique(df$conflict) 
result_tables <- list()

for (conflict in conflicts) {
  df_subset <- df[df$conflict == conflict, ] 
  df_subset$attack_type <- sapply(df_subset$'Attack Type', classify) 
  result_tables[[conflict]] <- table(df_subset$attack_type) 
}

for (conflict in names(result_tables)) {
  cat("##", conflict, "\n")
  
  counts <- result_tables[[conflict]]
  total <- sum(counts) 
  proportions <- round((counts / total) * 100, 1) 
  
  for (class in names(counts)) {
    cat("Class", class, ":", counts[class], "(", proportions[class], "%)", "\n")
  }
  cat("\n")
}
table(df_subset$attack_type)

####Figure A####
df$conflict <- ifelse(df$conflict=="Arab–Israeli", "Arab–Israeli", ifelse(df$conflict=="Russo-Ukrainian", "Russo-Ukrainian",ifelse(df$conflict=="Myanmar", "Myanmar","Other regions")))
# df <- df[df$conflict == "Arab–Israeli conflict"|df$conflict == "Russo-Ukrainian war"|df$conflict == "Myanmar conflict",]
# convert attack_date to date format
df$conflict <- factor(df$conflict, 
                      levels = c("Russo-Ukrainian","Arab–Israeli",  
                                 "Myanmar","Other regions"))
df <- df %>%
  mutate(`Attack Date` = as.Date(`Attack Date`),  
         month = floor_date(`Attack Date`, "month")) 

monthly_data <- df %>%
  group_by(month) %>%
  summarise(
    monthly_injured = sum(`Total Injured`, na.rm = TRUE),
    monthly_death = sum(`Total Death`, na.rm = TRUE)
  ) %>%
  mutate(
    injured_cumulative = cumsum(monthly_injured),
    death_cumulative = cumsum(monthly_death),
    casualty_cumulative = injured_cumulative + death_cumulative
  )

monthly_count_data <- df %>%
  group_by(month, conflict) %>%
  summarise(monthly_count = n()) %>%
  ungroup()

monthly_total <- monthly_count_data %>%
  group_by(month) %>%
  summarise(total_count = sum(monthly_count))

# Calculate max values for left and right Y axes
line_max <- max(monthly_data$casualty_cumulative, na.rm = TRUE)
bar_max <- max(monthly_count_data$monthly_count, na.rm = TRUE)

# Define the step for the left Y axis (this can be adjusted)
step_left <- 1500  # Left Y axis step
max_left <- ceiling(line_max / step_left) * step_left  # Adjust max_left to be a multiple of step_left

# Define the step for the right Y axis (this will scale with the left Y axis)
step_right <- 100  # Right Y axis step
max_right <- ceiling(bar_max / step_right) * step_right  # Adjust max_right to be a multiple of step_right

# Create breaks for both Y axes
breaks_left <- seq(0, max_left, by = step_left)
breaks_right <- seq(0, max_right, by = step_right)

# Plotting
p_final <- ggplot() +
  
  # Bars with monthly count, mapped to the right Y axis
  geom_bar(data = monthly_count_data, 
           aes(x = month, y = monthly_count * max_left / max_right, fill = conflict), 
           stat = "identity", position = "stack", alpha = 0.7, width = 20) + 
  
  # Cumulative line for casualties, mapped to the left Y axis
  geom_line(data = monthly_data, 
            aes(x = month, y = casualty_cumulative, color = "casualty Cumulative"), 
            size = 1) +
  
  # Adding vertical dashed lines between the bars
  geom_vline(xintercept = as.Date("2021-02-01") - days(15), linetype = "dotted", color = "black", size = 1) +
  geom_vline(xintercept = as.Date("2023-10-01") - days(15), linetype = "dotted", color = "black", size = 1) +
  
  scale_y_continuous(
    name = "Cumulative number of casualties",  
    limits = c(0, max_left),  # Adjust the limits based on max_left
    breaks = breaks_left,  # Set breaks for the left Y axis
    expand = c(0, 0),  
    labels = scales::comma,  
    sec.axis = sec_axis(
      trans = ~ . * max_right / max_left,  # Adjust transformation for right Y axis
      name = "Monthly number of attacks",  
      breaks = breaks_right,  # Set breaks for the right Y axis
      labels = scales::comma  # Ensure labels show up on the right Y axis
    )
  ) +
  
  scale_x_date(
    name = "",
    date_labels = "%Y-%m",  
    date_breaks = "3 months",  
    expand = c(0.02, 0),  
    limits = c(min(monthly_data$month) - months(1), max(monthly_data$month) + months(1))
  ) +
  
  scale_fill_manual(
    values = c(
      "Russo-Ukrainian" = "#982b2b", 
      "Arab–Israeli" = "#0074b3", 
      "Myanmar" = "#e8c559",
      "Other regions" = "grey"
    ),
    name = "Conflict regions"
  ) +
  
  scale_color_manual(
    values = c("casualty Cumulative" = "black"),  
    name = "Cumulative trend",
    guide = "none"
  ) +
  
  theme_minimal() +  
  theme(
    axis.text = element_text(size = 24),               # Adjust axis text size
    axis.title = element_text(size = 24),              # Adjust axis title size
    legend.text = element_text(size = 24),             # Adjust legend text size
    legend.title = element_text(size = 24, face = "bold"), # Adjust legend title size and make it bold
    plot.subtitle = element_text(size = 24),           # Adjust subtitle size
    axis.text.x = element_text(angle = 45, hjust = 1), # Keep x-axis labels rotated
    legend.position = c(0.01, 0.98),                   # Legend position
    legend.justification = c("left", "top"), 
    legend.background = element_rect(fill = alpha("white", 0.8)),
    plot.title.position = "panel",  # Position the title within the panel area
    plot.title = element_text(hjust = -0.035, size = 25, face = "bold")  # Position the title to x = 0.02
  ) +
  
  labs(
    title = "B",       
    fill = "Conflicts",
    color = ""
  )

print(p_final)

#extract plot
png(file = 'D:\\R project\\war\\plot\\cumulative_total_injured_death.png',
    width = 4800,
    height = 2400,
    units = "px",
    res = 200,
)
print(p_final)
dev.off()

####Figure B####
df <- readxl::read_excel("D:\\R project\\war\\Data_202001-202412.xlsx")
df <- df %>%
  mutate(conflict = case_when(
    `Country / Territory` %in% c("Israel", "Lebanon", "occupied Palestinian territory") ~ "Arab–Israeli",
    `Country / Territory` %in% c("Ukraine") ~ "Russo-Ukrainian",  
    `Country / Territory` %in% c("Myanmar") ~ "Myanmar",
    `Country / Territory` %in% c("Afghanistan") ~ "Afghanistan",
    `Country / Territory` %in% c("Democratic Republic of the Congo") ~ "DRC",
    `Country / Territory` %in% c("Syrian Arab Republic") ~ "Syrian",
    `Country / Territory` %in% c("Yemen") ~ "Middle eastern",
    `Country / Territory` %in% c("Nigeria") ~ "Sahel",
    `Country / Territory` %in% c("Libya") ~ "Middle eastern",
    `Country / Territory` %in% c("Mali") ~ "Sahel",
    `Country / Territory` %in% c("Burkina Faso") ~ "Sahel",
    `Country / Territory` %in% c("Sudan") ~ "Sudan and South Sudan",
    `Country / Territory` %in% c("Sudanese") ~ "Sudan and South Sudan",
    `Country / Territory` %in% c("Central African Republic") ~ "CAR",
    TRUE ~ "Others"
  ))
df <- df[!(df$conflict == "Afghanistan" &
             !(format(df$'Attack Date', "%Y") %in% c("2020", "2021"))), ]
df <- df %>%
  mutate(
    `Attack Date` = as.Date(`Attack Date`),  
    month = floor_date(`Attack Date`, "month"),
    year = format(as.Date(month), "%Y")
  )
df_summary <- df %>%
  group_by(conflict, year) %>%
  summarise(attack_counts = n(), .groups = "drop")

df_sorted <- df_summary %>%
  group_by(conflict) %>%
  arrange(desc(attack_counts), .by_group = TRUE) %>%  
  ungroup()
print(df_sorted)

df_summary <- df_summary %>%
  mutate(
    color_hex = case_when(
      conflict == "Arab–Israeli" & year == 2020 ~ '#cce3ef',  # Arab–Israeli 2020
      conflict == "Arab–Israeli" & year == 2022 ~ '#99c7e0',  # Arab–Israeli conflict 2021
      conflict == "Arab–Israeli" & year == 2021 ~ '#66abd1',  # Arab–Israeli conflict 2022
      conflict == "Arab–Israeli" & year == 2024 ~ '#328fc2',  # Arab–Israeli conflict 2023
      conflict == "Arab–Israeli" & year == 2023 ~ '#0074b3',  # Arab–Israeli conflict 2024
      
      
      conflict == "CAR" & year == 2024 ~ '#ffe8cc',  
      conflict == "CAR" & year == 2020 ~ '#ffd199',   
      conflict == "CAR" & year == 2022 ~ '#ffba66',   
      conflict == "CAR" & year == 2023 ~ '#ffa332',   
      conflict == "CAR" & year == 2021 ~ '#ff8c00', 
      
      conflict == "Middle eastern" & year == 2023 ~ '#fef3e8',   
      conflict == "Middle eastern" & year == 2021 ~ '#fee7d2',   
      conflict == "Middle eastern" & year == 2022 ~ '#fddcbb',   
      conflict == "Middle eastern" & year == 2020 ~ '#fdc58f', 
      
      conflict == "Myanmar" & year == 2020 ~ '#faf3dd',  
      conflict == "Myanmar" & year == 2024 ~ '#f5e7bc',   
      conflict == "Myanmar" & year == 2022 ~ '#f1dc9b',   
      conflict == "Myanmar" & year == 2023 ~ '#ecd07a',   
      conflict == "Myanmar" & year == 2021 ~ '#e8c559', 
      
      conflict == "Russo-Ukrainian" & year == 2020 ~ '#ead4d4',  
      conflict == "Russo-Ukrainian" & year == 2021 ~ '#d5aaaa',   
      conflict == "Russo-Ukrainian" & year == 2024 ~ '#c17f7f',   
      conflict == "Russo-Ukrainian" & year == 2023 ~ '#ac5555',  
      conflict == "Russo-Ukrainian" & year == 2022 ~ '#982b2b', 
      
      conflict == "Sahel" & year == 2024 ~ '#ecf6e9',   
      conflict == "Sahel" & year == 2023 ~ '#daedd3',   
      conflict == "Sahel" & year == 2022 ~ '#c7e4be',   
      conflict == "Sahel" & year == 2020 ~ '#b5dba8', 
      conflict == "Sahel" & year == 2021 ~ '#a3d393', 
      
      conflict == "Others" & year == 2020 ~ '#dfe2e7',   
      conflict == "Others" & year == 2022 ~ '#bfc5d0',   
      conflict == "Others" & year == 2021 ~ '#9fa8b8',   
      conflict == "Others" & year == 2024 ~ '#7f8ba1',   
      conflict == "Others" & year == 2023 ~ '#606f8a',
      
      conflict == "DRC" & year == 2020 ~ '#f0f1ea',   
      conflict == "DRC" & year == 2022 ~ '#e0e3d5',   
      conflict == "DRC" & year == 2021 ~ '#d1d6c0',  
      conflict == "DRC" & year == 2024 ~ '#c1c8aa',   
      conflict == "DRC" & year == 2023 ~ '#b5b8a3',  
      
      conflict == "Sudan and South Sudan" & year == 2024 ~ '#f0d6f4',  
      conflict == "Sudan and South Sudan" & year == 2023 ~ '#e1aadf',   
      conflict == "Sudan and South Sudan" & year == 2021 ~ '#d182ca',   
      conflict == "Sudan and South Sudan" & year == 2022 ~ '#a95c9b',   
      conflict == "Sudan and South Sudan" & year == 2020 ~ '#7a4c9c',  
      
      conflict == "Syrian" & year == 2024 ~ '#eaf4fb',   
      conflict == "Syrian" & year == 2022 ~ '#d6eaf7',  
      conflict == "Syrian" & year == 2023 ~ '#c1dff3',   
      conflict == "Syrian" & year == 2020 ~ '#add5ef',   
      conflict == "Syrian" & year == 2021 ~ '#99cbeb',   
      
      
      conflict == "Afghanistan" & year == 2021 ~ '#c58a7f',  
      conflict == "Afghanistan" & year == 2020 ~ '#b76d60',  
      
      TRUE ~ "#ffffff" 
    )
  )

#extract plot
png("D:\\R project\\war\\plot\\treemap.png", width = 4800, height = 2400, res = 180)

df_summary$custom_labels_year <- ifelse(
  df_summary$conflict %in% c("Arab–Israeli", "Russo-Ukrainian", "Myanmar"),
  paste(df_summary$year, "\n(n=", df_summary$attack_counts, ")", sep = ""),
  as.character(df_summary$year)
)

conflict_total <- aggregate(attack_counts ~ conflict, data = df_summary, sum)
colnames(conflict_total)[2] <- "total_attack_counts"

df_summary <- merge(df_summary, conflict_total, by = "conflict")

df_summary$custom_labels <- ifelse(
  df_summary$conflict %in% c("Arab–Israeli", "Russo-Ukrainian", "Myanmar"),
  paste(df_summary$conflict, "\n(n=", df_summary$total_attack_counts, ")", sep = ""),
  as.character(df_summary$conflict)
)

treemap(
  df_summary,
  index = c("custom_labels", "year"),
  vSize = "attack_counts",
  vColor = "color_hex",
  type = "color",
  fontsize.labels = c(30, 22),    
  fontcolor.labels = "black",
  border.col = "white",
  border.lwd = 2,
  align.labels = list(c("center", "center"), c("right", "top")),
  aspRatio = 2,
  draw = TRUE,
  overlap.labels = 0.1,
  bg.labels = c("transparent"),
  inflate.labels = FALSE,
  title = " ",
  legend = FALSE
)

grid.text(
  "A", 
  x = unit(0.02, "npc"), 
  y = unit(0.997, "npc"), 
  just = c("left", "top"), 
  gp = gpar(fontsize = 30, fontface = "bold")  
)
dev.off()


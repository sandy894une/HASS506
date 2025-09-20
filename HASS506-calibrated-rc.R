
library(readr)
library(dplyr)
library(ggplot2)

# Load calibrated dataset
rc_data_bc <- read.csv("Radiocarbon-dates.csv")

# Clean Site_Name for consistency
rc_data_bc <- rc_data_bc %>%
  mutate(Site_Name = trimws(Site))

# Compute the earliest calibrated date (max Start_BC) per site for ordering
site_order_bc <- rc_data_bc %>%
  group_by(Site_Name) %>%
  summarise(site_earliest_bc = max(Start_BC, na.rm = TRUE)) %>%
  arrange(desc(site_earliest_bc))

# Extract ordered site names
ordered_sites_bc <- site_order_bc$Site_Name

# Factor Site_Name with explicit ordering
rc_data_bc <- rc_data_bc %>%
  mutate(Site_Name = factor(Site_Name, levels = ordered_sites_bc))


rc_data_bc <- rc_data_bc %>%
  mutate(PPN_flag = ifelse(grepl("PPN", Phase_Level, ignore.case = TRUE), "PPN", "PN"))


# Define custom breaks for y-axis 
y_breaks <- seq(from = min(rc_data_bc$End_BC, na.rm = TRUE),
                 to = max(rc_data_bc$Start_BC, na.rm = TRUE),
                 by = 200)  # every 200 years, adjust as needed

ggplot(rc_data_bc, aes(x = Site_Name, color = PPN_flag)) +
  geom_point(aes(y = Start_BC), size = 1) +
  geom_errorbar(aes(ymin = End_BC, ymax = Start_BC), width = 0.3) +
  scale_y_reverse(breaks = y_breaks) +
  scale_color_manual(values = c("PPN" = "#E41A1C", "PN" = "#21409A")) +
  theme_minimal() +
  labs(
    title = "Calibrated Date Ranges by Site",
    x = "",
    y = "Calibrated Dates (BCE)",
    color = "Period"
  ) 

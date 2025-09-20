

library(ggplot2)
library(readr)

# Load data
rc_data <- read_csv("nerds-subset.csv")

# Add a column to flag PPN context
rc_data <- rc_data %>%
  mutate(PPN_flag = ifelse(grepl("PPN", SiteContext, ignore.case = TRUE), "PPN", "PN"))

site_order <- rc_data %>%
  group_by(Site_Name) %>%
  summarise(site_earliest = max(CRA, na.rm = TRUE)) %>%
  arrange(desc(site_earliest))

ordered_sites <- site_order$Site_Name

rc_data <- rc_data %>%
  mutate(Site_Name = factor(Site_Name, levels = ordered_sites))


# Plot with color by PPN_flag
ggplot(rc_data, aes(x = Site_Name, y = CRA, color = PPN_flag)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = CRA - Error, ymax = CRA + Error), width = 0.3) +
  #coord_flip()+
  scale_y_reverse() +

  scale_color_manual(values = c("PPN" = "#E41A1C", "PN" = "#21409A")) +
  theme_minimal() +
  labs(title = "Uncalibrated Radiocarbon Date Ranges by Site",
       x = "",
       y = "Uncalibrated Range BCE",
       color = "Period")

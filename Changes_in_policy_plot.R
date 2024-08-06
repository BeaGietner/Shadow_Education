library(ggplot2)
library(lubridate)
library(tidyverse)
library(reshape2)

# policy change
events <- data.frame(
  date = as.Date(c("2009-01-01", "2011-01-01", "2014-01-01", "2020-03-01",
                   "2013-01-01", "2020-03-01",
                   "2018-01-01", "2021-07-01",
                   "2012-01-01", "2015-01-01", "2020-03-01",
                   "2010-01-01", "2015-01-01", "2018-01-01")),
  event = c("10 p.m. curfew on hagwons (Seoul)", "Curfew adopted by other regions", "Growth in online tutoring", "COVID-19 acceleration",
            "Growth in online juku services", "Sharp increase in online juku",
            "Discussions on stricter regulations", "Ban on for-profit core subject tutoring",
            "Increase in small-group tuition centers", "Rise of online tutoring platforms", "Accelerated online tutoring growth",
            "Emergence of 'celebrity tutors'", "Expansion to kindergarten level", "Rapid growth of online platforms"),
  country = factor(c("South Korea", "South Korea", "South Korea", "South Korea",
                     "Japan", "Japan",
                     "China", "China",
                     "Singapore", "Singapore", "Singapore",
                     "Hong Kong", "Hong Kong", "Hong Kong"),
                   levels = c("South Korea", "Japan", "China", "Singapore", "Hong Kong"))
)

country_colors <- c("South Korea" = "#4e79a7", "Japan" = "#f28e2b", 
                    "China" = "#e15759", "Singapore" = "#76b7b2", 
                    "Hong Kong" = "#59a14f")

pisa_years <- as.Date(paste0(c(2009, 2012, 2015, 2018, 2022), "-01-01"))
timss_years <- as.Date(paste0(c(2011, 2015, 2019, 2023), "-01-01"))

ggplot(events, aes(x=date, y=country, color=country)) +
  geom_rect(data=data.frame(x=pisa_years), aes(xmin=x, xmax=x+years(1), ymin=-Inf, ymax=Inf), 
            fill="lightblue", alpha=0.3, inherit.aes=FALSE) +
  geom_rect(data=data.frame(x=timss_years), aes(xmin=x, xmax=x+years(1), ymin=-Inf, ymax=Inf), 
            fill="lightgreen", alpha=0.3, inherit.aes=FALSE) +
  geom_point(size=4) +
  geom_text(aes(label=event), vjust=2.5, hjust=0.5, size=2.8) +
  theme_minimal() +
  theme(legend.position="none",
        axis.title.y=element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size=10)) +
  labs(title="Timeline of Changes in Shadow Education Systems in East Asia",
       subtitle="Key events from 2009 to 2023\nPISA years shaded in light blue, TIMSS years in light green",
       x="Year") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
               limits = as.Date(c("2009-01-01", "2023-12-31")), 
               expand = c(0.01, 0)) +
  scale_color_manual(values = country_colors) +
  coord_cartesian(clip = 'off')

ggsave("improved_east_asia_shadow_education_timeline_extended.pdf", width = 18, height = 10, dpi = 300)


events <- data.frame(
  date = as.Date(c("2009-01-01", "2011-01-01", "2014-01-01", "2020-03-01",
                   "2013-01-01", "2020-03-01",
                   "2018-01-01", "2021-07-01",
                   "2012-01-01", "2015-01-01", "2020-03-01",
                   "2010-01-01", "2015-01-01", "2018-01-01")),
  event = c("10 p.m. curfew on hagwons (Seoul)", "Curfew adopted by other regions", "Growth in online tutoring", "COVID-19 acceleration",
            "Growth in online juku services", "Sharp increase in online juku",
            "Discussions on stricter regulations", "Ban on for-profit core subject tutoring",
            "Increase in small-group tuition centers", "Rise of online tutoring platforms", "Accelerated online tutoring growth",
            "Emergence of 'celebrity tutors'", "Expansion to kindergarten level", "Rapid growth of online platforms"),
  country = factor(c("South Korea", "South Korea", "South Korea", "South Korea",
                     "Japan", "Japan",
                     "China", "China",
                     "Singapore", "Singapore", "Singapore",
                     "Hong Kong", "Hong Kong", "Hong Kong"),
                   levels = c("South Korea", "Japan", "China", "Singapore", "Hong Kong"))
)

country_colors <- c("South Korea" = "#4e79a7", "Japan" = "#f28e2b", 
                    "China" = "#e15759", "Singapore" = "#76b7b2", 
                    "Hong Kong" = "#59a14f")

pisa_years <- as.Date(paste0(c(2009, 2012, 2015, 2018, 2022), "-01-01"))
timss_years <- as.Date(paste0(c(2011, 2015, 2019), "-01-01"))

ggplot(events, aes(x=date, y=country, color=country)) +
  geom_rect(data=data.frame(x=pisa_years), aes(xmin=x, xmax=x+years(1), ymin=-Inf, ymax=Inf), 
            fill="lightblue", alpha=0.3, inherit.aes=FALSE) +
  geom_rect(data=data.frame(x=timss_years), aes(xmin=x, xmax=x+years(1), ymin=-Inf, ymax=Inf), 
            fill="lightgreen", alpha=0.3, inherit.aes=FALSE) +
  geom_point(size=3) +
  geom_segment(aes(xend=date, yend=as.numeric(country) - 0.2), linetype="dashed") +
  geom_text(aes(label=event, y=as.numeric(country) - 0.2), 
            hjust=0, vjust=1, size=2.5, angle=45, check_overlap = TRUE) +
  theme_minimal() +
  theme(legend.position="none",
        axis.title.y=element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0, size = 10, face = "bold"),
        plot.subtitle = element_text(hjust = 0, size = 8),
        axis.text.y = element_text(size=8, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size=6)) +
  labs(title="Timeline of Changes in Shadow Education Systems in East Asia",
       subtitle="Key events from 2009 to 2023 (PISA years in light blue, TIMSS years in light green)",
       x="Year") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
               limits = as.Date(c("2009-01-01", "2023-12-31")), 
               expand = c(0.01, 0)) +
  scale_color_manual(values = country_colors) +
  coord_cartesian(clip = 'off')

#------------------------------------ PISA ranking

theme_pisa <- theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.margin = unit(c(1, 1, 3, 1), "cm"))  # top, right, bottom, left

china_note <- "Note: Chinese provinces/municipalities of Beijing, Shanghai, Jiangsu and Zhejiang\nwere participants in the 2022 edition, but their results were not published as they\nwere not able to fully collect data because of COVID restrictions."

create_pisa_plot <- function(data, title, max_rank) {
  ggplot(data, aes(x = Year, y = Rank, color = Country, group = Country)) +
    geom_line() +
    geom_point() +
    scale_y_reverse(breaks = seq(1, max_rank, by = ifelse(max_rank > 20, 5, 2)),
                    limits = c(max_rank, 1)) +
    scale_x_continuous(breaks = unique(data$Year)) +
    labs(title = title, x = "Year", y = "Rank") +
    theme_pisa +
    annotate("text", x = mean(range(data$Year)), y = max_rank + 1, 
             label = china_note, hjust = 0.5, vjust = 0, size = 3, color = "darkgray") +
    coord_cartesian(clip = "off")
}


# Science
science_data <- data.frame(
  Year = rep(c(2006, 2009, 2012, 2015, 2022), 6),
  Country = rep(c("Singapore", "Hong Kong", "South Korea", "Japan", "China", "Taiwan"), each = 5),
  Rank = c(
    NA, 3, 2, 1, 1,    # Singapore
    2, 2, 1, 9, 7,     # Hong Kong
    10, 5, 6, 11, 5,   # South Korea
    6, 4, 3, 2, 2,     # Japan
    NA, NA, NA, 10, NA,# China (B-S-J-G)
    4, 11, 11, 4, 4  # Taiwan

  )
)
p_science <- create_pisa_plot(science_data, "PISA Science Ranking Progression", 12)
p_science
ggsave("pisa_science_rankings.png", plot = p_science, width = 12, height = 8)

# Mathematics
math_data <- data.frame(
  Year = rep(c(2000, 2003, 2006, 2009, 2012, 2015, 2022), 6),
  Country = rep(c("Singapore", "Hong Kong", "South Korea", "Japan", "China", "Taiwan"), each = 7),
  Rank = c(
    NA, NA, NA, 1, 1, 1, 1,    # Singapore
    1, 1, 3, 2, 2, 2, 4,       # Hong Kong
    3, 3, 4, 3, 4, 7, 6,       # South Korea
    2, 5, 9, 7, 6, 5, 5,       # Japan
    NA, NA, NA, NA, NA, 6, NA, # China (B-S-J-G)
    NA, NA, NA, NA, NA, 3, 3  # Taiwan
  )
)
p_math <- create_pisa_plot(math_data, "PISA Mathematics Ranking Progression", 10)
p_math
ggsave("pisa_math_rankings.png", plot = p_math, width = 12, height = 8)

# Reading
reading_data <- data.frame(
  Year = rep(c(2000, 2003, 2006, 2009, 2012, 2015, 2022), 6),
  Country = rep(c("Singapore", "Hong Kong", "South Korea", "Japan", "China", "Taiwan"), each = 7),
  Rank = c(
    NA, NA, NA, 4, 2, 1, 1,    # Singapore
    6, 9, 3, 3, 1, 2, 11,      # Hong Kong
    7, 2, 1, 1, 4, 7, 4,       # South Korea
    9, 14, 14, 7, 3, 8, 3,     # Japan
    NA, NA, NA, NA, NA, 27, NA,# China
    NA, NA, NA, NA, NA, NA, 5  # Taiwan (Chinese Taipei)
  )
)
p_reading <- create_pisa_plot(reading_data, "PISA Reading Ranking Progression", 27)
p_reading

ggsave("pisa_reading_rankings.png", plot = p_reading, width = 12, height = 8)

# ------------------------------------- TIMMS ranking position

data_TIMMS <- data.frame(
  Year = rep(c(2003, 2007, 2011, 2015, 2019), each = 20),
  Country = rep(c("Chinese Taipei", "South Korea", "Singapore", "Hong Kong", "Japan"), times = 20),
  Subject = rep(rep(c("Mathematics", "Science"), each = 10), times = 5),
  Grade = rep(rep(c("4th", "8th"), each = 5), times = 10),
  Rank = c(
    #      Maths                      Science
    # 4th       8th               4th          8th
    # CT,SK,SG,HK,JP             CT,SK,SG,HK,JP
   
     # 2003 data
    4,NA,1,2,3,  4,2,1,3,5,      2,NA,1,4,3,  2,3,1,4,5, 
    # 2007 data
    3,NA,2,1,4,  1,2,3,4,5,      2,NA,1,3,4,  2,4,1,9,3,
    # 2011 data
    4,2,1,3,5,  3,1,2,4,5,       5,1,2,9,4,   2,3,1,8,4,
    # 2015 data
    4,3,1,2,5,  3,2,1,4,5,       6,2,1,5,3,   3,4,1,6,2,
    # 2019 data
    4,3,1,2,5,  2,3,1,5,4,       5,2,1,15,4,  2,4,1,17,3
  )
)

ggplot(data_TIMMS, aes(x = Year, y = Rank, color = Country, group = Country)) +
  geom_line(size = 0.5) +
  geom_point(size = 1.5) +
  facet_grid(Subject ~ Grade, scales = "free_y") +
  scale_y_reverse(breaks = seq(1, 20, by = 3)) +
  scale_x_continuous(breaks = c(2003, 2007, 2011, 2015, 2019)) +  
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "TIMSS Rankings of East Asian Countries (2003-2019)",
       x = "Year", y = "Rank") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "gray80")) +  
  guides(color = guide_legend(nrow = 1))

ggsave("east_asian_timss_rankings.png", width = 12, height = 8, dpi = 300)



library(ggplot2)
library(lubridate)

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
# RA stats

require(dplyr)
require(ggplot2)
require(readr)

export <- read_csv("data/ra_data_pull.csv") %>% 
  slice(-c(1:2)) %>% 
  filter(entry_type!="Update on Existing Entry (type in Record ID in text box)") %>% 
  mutate(RecordedDate=lubridate::ymd_hms(RecordedDate),
         record_date_day=lubridate::as_date(RecordedDate))


export %>% 
  filter(record_date_day!=lubridate::today()) %>% 
  group_by(record_date_day) %>% 
  count %>% 
  ggplot(aes(y=n,x=record_date_day)) +
  geom_area(fill="blue",alpha=0.5) +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  ylab("Count of Records (Excluding Corrections)") +
  xlab("") +
  ggtitle("Number of Updates/New Entry Records\nin CoronaNet Database Since March 26th")

ggsave("date_performance.png")

export %>% 
  filter(record_date_day!=lubridate::today()) %>% 
  group_by(record_date_day) %>% 
  count %>% 
  ungroup %>% 
  arrange(record_date_day) %>% 
  mutate(n_cum=cumsum(n)) %>% 
  ggplot(aes(y=n_cum,x=record_date_day)) +
  geom_area(fill="blue",alpha=0.5) +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  ylab("Count of Records (Excluding Corrections)") +
  xlab("") +
  ggtitle("Number of Updates/New Entry Records\nin CoronaNet Database Since March 26th")

ggsave("date_performance_cumsum.png")

# need to produce leader board

export %>% 
  group_by(ra_name) %>% 
  arrange(desc(n)) %>% 
  select(Name="ra_name",`Count of Records`="n") %>% 
  count %>% 
  write_csv("data/ra_leader_board.csv")

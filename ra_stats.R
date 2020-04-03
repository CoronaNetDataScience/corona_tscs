# RA stats

require(dplyr)
require(ggplot2)
require(readr)

export <- read_csv("data/COVID-19 Government Response Tracker Database_April 3, 2020_04.51.csv") %>% 
  slice(-c(1:2)) %>% 
  filter(entry_type!="Correction to Existing Entry (type in Record ID in text box)") %>% 
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

# country coverage by days

export %>% 
  group_by(init_country,record_date_day) %>% 
  mutate(n_exists=ifelse(n()>1,1,NA)) %>% 
  ungroup %>% 
  complete(init_country,record_date_day,fill=list(n_exists=NA)) %>% 
  group_by(init_country) %>% 
  arrange(record_date_day) %>% 
  fill(n_exists,.direction="down") %>% 
  mutate(n_exists=coalesce(n_exists,0)) %>% 
  distinct(record_date_day,init_country,n_exists) %>% 
  group_by(record_date_day) %>% 
  summarize(n=sum(n_exists)) %>% 
  ggplot(aes(y=n,x=record_date_day)) +
  geom_area(fill="blue",alpha=0.5) +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  ylab("Count of Countries") +
  xlab("") +
  ggtitle("Number of Countries Covered\nin CoronaNet Database Since March 26th")

ggsave("country_cov.png")

# need to produce leader board

export %>% 
  group_by(ra_name) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  select(Name="ra_name",`Count of Records`="n") %>% 
  write_csv("data/ra_leader_board.csv")

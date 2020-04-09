# RA stats

require(dplyr)
require(ggplot2)
require(readr)
require(tidyr)
require(googlesheets4)

export <- read_csv("data/CoronaNet/RA/ra_data_pull.csv") %>% 
  slice(-c(1:2)) %>% 
  filter(entry_type!="Correction to Existing Entry (type in Record ID in text box)") %>% 
  mutate(RecordedDate=lubridate::ymd_hms(RecordedDate),
         record_date_day=lubridate::as_date(RecordedDate),
         entry_type=recode(entry_type,`1`="New Entry"))


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

# output to record sheet

sheets_auth()

current_sheet <- sheets_get("https://docs.google.com/spreadsheets/d/183lWnJH7rSVkOTiuCXt9D7uCwS1SdJSOleXPpFkj2us/edit#gid=0")

export %>% 
  select(-target_country,-record_date_day) %>% 
  mutate(link_correct=paste0("https://tummgmt.eu.qualtrics.com/jfe/form/SV_bf6YMWbTpYJAW4l?Q_R=",ResponseId,
                            "&Q_R_DEL=1&record_id=",record_id,"&link_type=C"),
         link_update=paste0("https://tummgmt.eu.qualtrics.com/jfe/form/SV_bf6YMWbTpYJAW4l?Q_R=",ResponseId,
                            "&record_id=",record_id,"&link_type=U")) %>% 
  arrange(init_country,date_announced) %>% 
  sheets_write(ss=current_sheet,sheet="Sheet1")


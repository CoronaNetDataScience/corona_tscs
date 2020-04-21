# RA stats

require(dplyr)
require(ggplot2)
require(readr)
require(tidyr)
require(googlesheets4)
require(lubridate)

start_week <- ymd("2020-04-19")
end_week <- ymd("2020-04-26")

sheets_auth()

export <- read_csv("data/CoronaNet/RA/ra_data_pull.csv") %>% 
  slice(-c(1:2)) %>% 
  mutate(RecordedDate=lubridate::ymd_hms(RecordedDate),
         record_date_day=lubridate::as_date(RecordedDate),
         entry_type=recode(entry_type,`1`="New Entry"))


export %>% 
  filter(entry_type!="Correction to Existing Entry (type in Record ID in text box)") %>% 
  group_by(record_date_day) %>% 
  count %>% 
  ggplot(aes(y=n,x=record_date_day)) +
  geom_area(fill="blue",alpha=0.5) +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  ylab("Count of Records (Excluding Corrections)") +
  xlab("") +
  ggtitle("Number of Updates/New Entry Records\nin CoronaNet Database Since March 26th")

ggsave("date_performance.png",width = 6,height=3)

export %>% 
  filter(entry_type!="Correction to Existing Entry (type in Record ID in text box)") %>% 
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

ggsave("date_performance_cumsum.png",width = 6,height=3)

# country coverage by days

export %>% 
  filter(entry_type!="Correction to Existing Entry (type in Record ID in text box)") %>% 
  group_by(init_country,record_date_day) %>% 
  mutate(n_exists=ifelse(n()>1,1,NA)) %>% 
  ungroup %>% 
  complete(init_country,record_date_day,fill=list(n_exists=NA)) %>% 
  group_by(init_country) %>% 
  arrange(init_country,record_date_day) %>% 
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

ggsave("country_cov.png",width = 6,height=3)

# house competition info

hogwarts <- sheets_get("https://docs.google.com/spreadsheets/d/1nhPGi7GD6RwsI2pZ5SOCRHg4mICqc5nByWZl7UgDjys/edit?usp=sharing") %>% 
  sheets_read(sheet="Sheet1")

# need to produce leader board

leaders <- export %>% 
  group_by(ra_name) %>% 
  filter(entry_type!="Correction to Existing Entry (type in Record ID in text box)",
         RecordedDate>start_week,
         RecordedDate<end_week) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  select(Name="ra_name",`Count of Records`="n") 
  
leaders <- leaders %>% left_join(hogwarts,by=c("Name"="ra_name")) 

leaders %>% write_sheet(ss=sheets_get("https://docs.google.com/spreadsheets/d/1INmpDvIne76qqmlucMABaJxeaZ-xpoypc0s_S0tUyto/edit?usp=sharing"),
                                                                        sheet="Leaderboard")

# add up point totals from leader board

lead_pts <- slice(ungroup(leaders),c(1:10)) %>% 
  mutate(points=c(50,25,15,rep(10,7))) %>% 
  group_by(house) %>% 
  summarize(lead_pts=sum(points,na.rm=T))

# see how many we can match

hogwarts <- left_join(hogwarts,export)

# records by day by house

hogwarts %>% 
  ungroup %>% 
  filter(entry_type!="Correction to Existing Entry (type in Record ID in text box)",
         RecordedDate>start_week,
         RecordedDate<end_week,
         !is.na(house)) %>% 
  group_by(record_date_day,house) %>% 
  count %>% 
  ungroup %>% 
  group_by(house) %>% 
  arrange(record_date_day) %>% 
  mutate(n_cum=cumsum(n)) %>% 
  ggplot(aes(y=n_cum,x=record_date_day)) +
  geom_line(aes(colour=house)) +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  ylab("Count of Records (Excluding Corrections)") +
  xlab("") +
  ggtitle("Number of Records by Hogwarts House")

ggsave('hogwarts.png')

# calculate overall standings

standings <- group_by(hogwarts,house) %>% 
  filter(entry_type!="Correction to Existing Entry (type in Record ID in text box)",
         RecordedDate>start_week,
         RecordedDate<end_week,
         !is.na(house)) %>% 
  summarize(count_policies=length(unique(record_id)))

# addin point bonus

standings <- left_join(standings,lead_pts,by="house") %>% 
  mutate(Total=count_policies+lead_pts)

standings %>% 
  select(House="house",
         `Count of Policies`="count_policies",
         `Player Bonus`="lead_pts",
         Total) %>% 
  write_sheet(ss=sheets_get("https://docs.google.com/spreadsheets/d/1INmpDvIne76qqmlucMABaJxeaZ-xpoypc0s_S0tUyto/edit?usp=sharing"),
              sheet="Overall Standings")


# need 

# output to record sheet

current_sheet <- sheets_get("https://docs.google.com/spreadsheets/d/183lWnJH7rSVkOTiuCXt9D7uCwS1SdJSOleXPpFkj2us/edit#gid=0")

export %>% 
  select(-target_country,-record_date_day) %>% 
  mutate(link_correct=paste0("https://tummgmt.eu.qualtrics.com/jfe/form/SV_bf6YMWbTpYJAW4l?Q_R=",ResponseId,
                            "&Q_R_DEL=1&record_id=",record_id,"&link_type=C"),
         link_update=paste0("https://tummgmt.eu.qualtrics.com/jfe/form/SV_bf6YMWbTpYJAW4l?Q_R=",ResponseId,
                            "&record_id=",record_id,"&link_type=U")) %>% 
  arrange(init_country,date_announced) %>% 
  sheets_write(ss=current_sheet,sheet="Sheet1")


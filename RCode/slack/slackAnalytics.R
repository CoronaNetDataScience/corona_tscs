
# load packages
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)

# load data
pathData = "/cloud/project/data/slack"
slack = read.csv(paste0(pathData, "/slack/corona-govt-response_latest.csv"), stringsAsFactors = FALSE)


# subset to March 28, the day data collection started
slack$Date = as.Date(slack$Date)
slack =  slack %>% dplyr:::filter(Date >= "2020-03-28")


# remove weekly stats
slack = select(slack, -Weekly.active.members, -Weekly.members.posting.messages)
slack = slack %>% dplyr:::rename(Members.present.on.Slack = Daily.active.members ,
							  Members.posting.messages = Daily.members.posting.messages	)

# reshape data to long form
slack = slack %>% gather("var", "value", -Date)


# clean up var names
slack$var = gsub('\\.', ' ', slack$var)

# set order or vars in factor levels
 
 
slack$var = fct_relevel(slack$var, "Members present on Slack", 
						"Members posting messages",
						"Messages posted by members" ,
						"Messages in DMs"   )

 


# make plot of daily active members and daily members posting messges

p1 = ggplot(slack %>% filter(var %in% c("Members present on Slack", "Members posting messages" )),
			aes(x = Date, y = value, color = var))+
			geom_line(size = 1)+ 
			xlab("Date")+
			ylab("Number of Members")+
			scale_x_date(date_labels = "%a %d-%m-%y", date_breaks = "day")+
			scale_color_manual(values=c( "#3B7EA1",  "#FDB515"))+
			theme_minimal()+
			theme(legend.position="bottom",
				  legend.title = element_blank(),
				  axis.text.x = element_text(angle = 90, hjust = 1),
				  panel.grid.minor = element_blank())



p2 = ggplot(slack %>% filter(var %in% c( "Messages posted by members", "Messages in DMs" )),
			aes(x = Date, y = value, color = var))+
			geom_line(size = 1)+ 
			xlab("Date")+
			ylab("Number of Messages")+
			scale_x_date(date_labels = "%a %d-%m-%y", date_breaks = "day")+
			scale_color_manual(values=c( "#3B7EA1",  "#FDB515"))+
			theme_minimal()+
			theme(legend.position="bottom",
				  legend.title = element_blank(),
				  axis.text.x = element_text(angle = 90, hjust = 1),
				  panel.grid.minor = element_blank())


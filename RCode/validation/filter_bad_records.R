# this file contains all records that are removed because they are incorrect
# as these files are fixed, please remove the line of code from the case_when function

# ----------------------------------
## idioscyncratic cleaning
## define a variable validation
## is equal to TRUE if record should stay in analysis, 
## is equal to FALSE if there is some problem we should exclude
# ----------------------------------
# Laura is not an RA; she's a colleague who I asked to test the survey

# validation var
# need to correct these, storing info for now


qualtrics = mutate(qualtrics,
                   validation=case_when(
                                        # RA Cheng-Hao Shen had some pretty complicated issues with Palau, that Cindy can resolve
                                        # at a later date, but for now, lets just remove Palau from the data
                                        init_country == 'Palau'~FALSE,
                                        # ----- Cruise ships 
                                        # lets table/remove this obs for now
                                        # record id: 933728 --; The Government of Canada has chartered a plane to repatriate Canadians on board the Diamond Princess cruise ship docked in Yokohama, Japan. The aircraft will bring passengers from Japan to Canadian Forces Base Trenton, after which they will be assessed and transported to the NAV Canada Training Institute in Cornwall, Ontario, to undergo a further 14-day period of quarantine.
                                        record_id == 933728~FALSE,
                                        # 2890593 ;All American passengers have now disembarked from the Grand Princess.  The following reflects the count as of 8:30 p.m. PDT on March 12: \n \n480 total individuals were disembarked from the ship on March 12, 2020:\n\n    218 individuals were transported via chartered flight to Marine Corps Air Station Miramar near San Diego, California\n    251 individuals were transported via charted flight to Dobbins Air Reserve base near Marietta, Georgia\n    11 individuals were transported for care.
                                        record_id == 2890593~FALSE,
                                        # this entry clearly wrong, remove, validate/fix later
                                        target_who_what_10_TEXT %in% c("Tests","Facilities were meant to increase testing capacity",
                                                                       "Also foreign residents")~FALSE,
                                        TRUE~TRUE))


#filter all, where announced data > entry date (unlogic)
library(lubridate)
q <- qualtrics
q$date_announced<-mdy(q$date_announced)
q$StartDate<-as.Date(q$StartDate)
q<-q[which(q$date_announced >q$StartDate),]
q<-q$record_id 
qualtrics<- qualtrics[!qualtrics$record_id%in% q,]
q

#filter all, where announced data > source published date (unlogic)
q <- qualtrics
q$date_announced<-mdy(q$date_announced)
q$sources_matrix_1_1<-mdy(q$sources_matrix_1_1)

q<-q[which(q$date_announced >q$sources_matrix_1_1),]
q<-q$record_id 
q
qualtrics<- qualtrics[!qualtrics$record_id%in% q,]



# Log of what has been solved

##ra_name %in% "Laura Mahrenbach"~FALSE,----
# RA Mara Forster asked to start fresh on some entries and thus to ignore the following three entries:----
##record_id %in% c(7897317, 5297, 571317)~FALSE,
## 4979089 and 5608933 Veronika asked us to delete these----
## record_id  %in% c('4979089','5608933')~FALSE,
# these entries should not be possible ----
#shouldnt be able to select restriciton of mass gatherings and then type of school
#shouldnt be able to select quarantine and then type of school
# shouldnt be able to select quarantine and then type of external restriction
# in terms of the survey version now, I checked on April 2 and this is not possible for this current version of the dataset
# remove for now, but check what happened later!
# my guess is that some of these RAs coded these cases as one policy type first, and then backtracked to another  policy type, and qualtrics saved the entries for the sub-types 
# in the future, we should think about creating code that automates the search for these survey fallacies
##record_id %in% c('3548864', '9939713', '933728', '9523505')~FALSE,
#----
# test<-coronanet_raw_latest[!is.na(coronanet_raw_latest$target_who_what_10_TEXT),]
# 
# test<-test[,c("record_id","target_who_what_10_TEXT")]
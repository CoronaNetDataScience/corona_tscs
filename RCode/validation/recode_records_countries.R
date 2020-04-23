# we didn't have a text entry option for target_country because it was in a multi-box 
# note that the coder miscoded the country here anyway
# should go back and validate all entries
qualtrics[which(qualtrics$target_country == 'Other (please specify below)'),'target_country'] = 'Bolivia'

# somebody  incorrectly  coded 'Schengen Area' in the other regions box for the target_region when the option is available 
qualtrics[grep('Schengen Area', qualtrics$target_region_14_TEXT),'target_region'] = "Schengen Area (without the UK)"
qualtrics[grep('Schengen Area', qualtrics$target_region_14_TEXT),'target_region_14_TEXT'] = ''

# others  incorrectly  coded regional provinces within a country in the target_region variable (which is for regional groupings)
qualtrics[grep('Hubei|Wuhan|Lombardy', qualtrics$target_region_14_TEXT),'target_province'] =  qualtrics[grep('Hubei|Wuhan|Lombardy', qualtrics$target_region_14_TEXT),'target_region_14_TEXT']
qualtrics[grep('Hubei|Wuhan|Lombardy', qualtrics$target_region_14_TEXT),'target_region_14_TEXT'] =  ''

# somebdoy  incorrectly  coded provinces within a country in the target_other variable
qualtrics[grep('Lombardy; Veneto', qualtrics$target_other),'target_province'] =  gsub('\\;', '\\,', qualtrics[grep("Lombardy; Veneto", qualtrics$target_other),'target_other'])
qualtrics[grep('Lombardy; Veneto', qualtrics$target_other),'target_other'] = ''

# somebdoy incorrectly coded cities within a country in the target_other variable
qualtrics[grep('Codogno;', qualtrics$target_other),'target_city'] =  qualtrics[grep('Codogno;', qualtrics$target_other),'target_other']
qualtrics[grep('Codogno;', qualtrics$target_other),'target_other'] = ''

# somebody  incorrectly  coded this city in target other and also didn't code the corresponding province
qualtrics[which(qualtrics$target_other == 'Tolima (Ibague)'), 'target_city'] = 'Tolima'
qualtrics[which(qualtrics$target_other == 'Tolima (Ibague)'), 'target_province'] = 'Ibague'
qualtrics[which(qualtrics$target_other == 'Tolima (Ibague)'), 'target_other'] = ''



# policy type is curfew. Multiple coders entered the 'target_country' as 'All countries'. Change this to NA
# note that the display logic is now such that this type of mistake no longer happens
qualtrics[which(qualtrics$record_id %in% c(384499,
                                           4101330,
                                           5661584,
                                           645445,
                                           5963120,
                                           226334,
                                           3089485,
                                           384499,
                                           4101330,
                                           5661584,
                                           5963120,
                                           8493502)), 'target_country'] = NA

# policy type is restriction of mass gatherings. One coder entered the 'target country' as 'All countries'. Change this to NA
# note that the display logic is now such that this type of mistake no longer happens
qualtrics[which(qualtrics$record_id %in% c(2010122)), 'target_country'] = NA  


# policy type is New Task Force or Bureau. One coder entered the 'target country' as 'All countries'. Change this to NA
# note that the display logic is now such that this type of mistake no longer happens
qualtrics[which(qualtrics$record_id %in% c(1342942)), 'target_country'] = NA  

# policy type is Declaration of Emergency . One coder entered the 'target country' as 'All countries'. Change this to NA
# note that the display logic is now such that this type of mistake no longer happens
qualtrics[which(qualtrics$record_id %in% c(6064689)), 'target_country'] = NA  



# policy type is closure of schools. Some coder entered the 'target country' as 'All countries' or multiple other countries. Change this to NA
# note that the display logic is now such that this type of mistake no longer happens
qualtrics[which(qualtrics$record_id %in% c(995919,
                                           535618)), 'target_country'] = NA  


# policy type is Public Awareness Campaigns . One coder entered the 'target country' as 'All countries'. Change this to NA
# note it is conceivable that a country might run a public awareness campaign in another country,
# so the that the display logic is unchanged for this policy type
qualtrics[which(qualtrics$record_id %in% c(6641788)), 'target_country'] = NA  


# RA corrected wrong entry; remove for now, properly fix later
qualtrics = qualtrics %>% filter(record_id !=  9938399 )  
qualtrics = qualtrics %>% filter(record_id !=  4418813) 
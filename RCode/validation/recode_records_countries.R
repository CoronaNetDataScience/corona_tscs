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


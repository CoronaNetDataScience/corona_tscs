
# These are records with errors
# Once they are fixed, remove the code from this file and commit to Github


# note RA Adrianna Poppe did not enter in the record id for her correction to 9008490; doing it for her here
qualtrics[which(qualtrics$entry_type_2_TEXT == 'https://bogota.gov.co/mi-ciudad/salud/decreto-081-de-alerta-amarilla-por-coronavirus-en-bogota'), 'entry_type_2_TEXT'] = 9008490

# RA Cheng-Hao Shen unncessarily chose other and NA category for target_who_what but otherwise the record is correct  
qualtrics[which(qualtrics$target_who_what_10_TEXT == "N/A"), c('target_who_what', 'target_who_what_10_TEXT')] = ""

# coder already documents this, correctly , in the target_who_what, removing it from 'target_other'
qualtrics[which(qualtrics$target_other == "this travel ban regards towards government officials and State-Owned Enterprises"),'target_other'] = ''

# add in niklas' corrections
# RA Niklas had systematically made the mistake of entering the date he collected a source instead of the date the source was published
# since he already had 68 entries coded, Cindy decided it would be a better use of time to just allow him to fix
# the mistakes in a spreadsheet rather than having him make corrections on every single one, with the understanding that this was def an exception

# the following code loads his corrections and incorporates them

niklas_sources = read_csv(paste0(pathData, '/niklasSourcesCorrection_corrected.csv'))
niklas_sources = niklas_sources[-1,] # remove first row where i had given him a description of the columns
qualtrics[which(qualtrics$record_id %in% niklas_sources$record_id), c('sources_matrix_1_1', 'sources_matrix_2_1')]  = niklas_sources[, c('correct_date_1', 'correct_date_2')]

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


# coder used 'other' text entry for target_who_what for hopstials; change to 'health infrastructure' for now and we should think about whether to add an extra option for coders
qualtrics[which(qualtrics$target_who_what_10_TEXT == "Health-Infrastructure"),'target_who_what']  = "Health-Infrastructure"
qualtrics[which(qualtrics$target_who_what_10_TEXT == "Infirmiry"),'target_who_what']  = "Health-Infrastructure"

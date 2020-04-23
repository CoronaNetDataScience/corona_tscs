
# These are records with errors
# Once they are fixed, remove the code from this file and commit to Github


# note RA Adrianna Poppe did not enter in the record id for her correction to 9008490; doing it for her here
qualtrics[which(qualtrics$entry_type_2_TEXT == 'https://bogota.gov.co/mi-ciudad/salud/decreto-081-de-alerta-amarilla-por-coronavirus-en-bogota'), 'entry_type_2_TEXT'] = "9008490"

# RA Cheng-Hao Shen unncessarily chose other and NA category for target_who_what but otherwise the record is correct  
qualtrics[which(qualtrics$target_who_what_10_TEXT == "N/A"), c('target_who_what', 'target_who_what_10_TEXT')] = ""

# coder already documents this, correctly , in the target_who_what, removing it from 'target_other'
qualtrics[which(qualtrics$target_other == "this travel ban regards towards government officials and State-Owned Enterprises"),'target_other'] = ''

# add in niklas' corrections
# RA Niklas had systematically made the mistake of entering the date he collected a source instead of the date the source was published
# since he already had 68 entries coded, Cindy decided it would be a better use of time to just allow him to fix
# the mistakes in a spreadsheet rather than having him make corrections on every single one, with the understanding that this was def an exception

# the following code loads his corrections and incorporates them

#niklas_sources = read_csv(paste0(path, '/data/CoronaNet/niklasSourcesCorrection_corrected.csv'))
#niklas_sources = niklas_sources[-1,] # remove first row where i had given him a description of the columns
#qualtrics[which(qualtrics$record_id %in% niklas_sources$record_id), c('sources_matrix_1_1', 'sources_matrix_2_1')]  = niklas_sources[, c('correct_date_1', 'correct_date_2')]


# coder used 'other' text entry for target_who_what for hopstials; change to 'health infrastructure' for now and we should think about whether to add an extra option for coders
qualtrics[which(qualtrics$target_who_what_10_TEXT == "Health-Infrastructure"),'target_who_what']  = "Health-Infrastructure"
qualtrics[which(qualtrics$target_who_what_10_TEXT == "Infirmiry"),'target_who_what']  = "Health-Infrastructure"

# Coder made new Entry but it should be update: Record 5733066 should be an update for 4262184 Record  6511463 should be an update for 3083058
qualtrics[which(qualtrics$record_id== 5733066),"entry_type"]  = "Update on Existing Entry (type in Record ID in text box)"
qualtrics[which(qualtrics$record_id== 5733066),"entry_type_3_TEXT"]  = 4262184

qualtrics[which(qualtrics$record_id== 6511463),"entry_type"]  = "Update on Existing Entry (type in Record ID in text box)"
qualtrics[which(qualtrics$record_id== 6511463),"entry_type_3_TEXT"]  = 3083058
 
qualtrics[which(qualtrics$record_id== 5501189),"type_ext_restrict"]  = c("Health Certificates")

# record id 645445 should be recoded as either declaration of emergency or quarantine but not curfew; asana is being super slow, putting this here for now
# record id 384499 should be recoded as restriciton of businesses; asana is being super slow, putting this here for now



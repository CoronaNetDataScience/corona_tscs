# clean RA information


contribution <- read_csv("data/CoronaNet/People/contribution.csv")


contribution_clean = contribution %>% mutate(
            Vita = gsub('righs', 'rights', Vita),
            Vita = gsub('Universtiy|Univesity|Universiity', 'University', Vita),
            Vita = gsub('Scienes', 'Sciences', Vita),
            Vita = gsub('Student an ', 'Student at ', Vita),
            Vita = gsub('Equatorinal', 'Equatorial', Vita),
            Vita = gsub('I am a 22 years old student at the technical university of munich where i study technology and management', 
                        'I am a 22 year old student at the Technical University of Munich where I study technology and management', Vita),
            Vita = gsub('(Ph. D)in ', '(Ph.D) in ', Vita),
            Vita = gsub('these exceptional time', 'this exceptional time', Vita),
            Vita = gsub('Palastine', 'Palestine', Vita),
            Vita = gsub('develepment', 'development', Vita),
            Vita = gsub('spanish', 'Spanish', Vita),
            Vita = gsub('poltical', 'political', Vita),
            Vita = gsub('revolutionalize', 'revolutionize', Vita),
            Vita = gsub('Sience', 'Science', Vita))

 
write_csv(contribution_clean, "data/CoronaNet/People/contribution_clean.csv")

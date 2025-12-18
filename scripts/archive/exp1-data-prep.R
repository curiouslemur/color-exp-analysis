source(file = './utils/exp1-utils.R')  # loads all necessary packages //NOTE: some packages

################################################################## MG demography data #####
mgdem <- read.csv("data/exp1/csv/mdg-dem.csv", encoding = "UTF-8")
mgdem <- mgdem %>%
  mutate(profession = replace(profession, 
                              profession %in% c("ETUDIANT", "ETUDIANTE", "ETUDIENT", "etudiant", "Etudiente", "Student", "Etudiant", 
                                                "étudiant", "etudiante", "etudiante ", "Student ", "etudient", "Etudiante", "étudiante", "Etudient"), "student")) %>% 
  mutate(gender = replace(gender, gender %in% c("homme", "Homme", "masculin", "Male", "FEMININ", "MASCULIN", "HOMME", "MALE", "Masculin"), "male")) %>%
  mutate(gender = replace(gender, gender %in% c("femme", "FEMME", "femelle", "Femme", "feminin", "Feminin", "femmme"), "female")) %>% 
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd1"), "mdg1")) %>% 
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd2"), "mdg2")) %>%
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd3"), "mdg3"))

# write_csv(filter(mgdem, expCountry == "mdg1"), "data/exp1/csv/clean/mg-dem-p1-.csv")
# write_csv(filter(mgdem, expCountry == "mdg2"), "data/exp1/csv/clean/mg-dem-p2-.csv")
# write_csv(filter(mgdem, expCountry == "mdg3"), "data/exp1/csv/clean/mg-dem-p3-.csv")

################################################################## MG response data #####
mgdf <- read.csv("data/exp1/csv/mdg.csv", encoding = "UTF-8")
mgdf <- mgdf %>% 
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd1"), "mdg1")) %>% 
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd2"), "mdg2")) %>%
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd3"), "mdg3")) %>% 
  mutate(concept = tolower(concept)) %>% 
  rename(conceptFr = concept)

# write_csv(filter(mgdf, expCountry == "mdg1"), "data/exp1/csv/clean/mg-p1-.csv")
# write_csv(filter(mgdf, expCountry == "mdg2"), "data/exp1/csv/clean/mg-p2-.csv")
# write_csv(filter(mgdf, expCountry == "mdg3"), "data/exp1/csv/clean/mg-p3-.csv")

# write_csv(mgdf, "data/exp1/csv/clean/mg.csv")

################################################################## US demography data #####
usdem <- read_csv("data/exp1/csv/usa-dem.csv")
usdem <- usdem %>% 
  mutate(profession = replace(profession, profession %in% c("ETUDIANT"), "student")) %>% 
  mutate(profession = replace(profession, profession %in% c("None", "n/a"), "Unemployed")) %>% 
  mutate(gender = replace(gender, gender %in% c("HOMME", "Male", "m"), "male")) %>% 
  mutate(gender = replace(gender, gender %in% c("Female", "F", "Woman"), "female"))
  
# write_csv(filter(usdem, expCountry == "usa1"), "data/exp1/csv/clean/us-dem-p1-.csv")
# write_csv(filter(usdem, expCountry == "usa2"), "data/exp1/csv/clean/us-dem-p2-.csv")
# write_csv(filter(usdem, expCountry == "usa3"), "data/exp1/csv/clean/us-dem-p3-.csv")

################################################################## US response data #####
usdf <- read_csv("data/exp1/csv/usa.csv")
usdf <- usdf %>% mutate(concept = tolower(concept))

# write_csv(filter(usdf, expCountry == "usa1"), "data/exp1/csv/clean/us-p1-.csv")
# write_csv(filter(usdf, expCountry == "usa2"), "data/exp1/csv/clean/us-p2-.csv")
# write_csv(filter(usdf, expCountry == "usa3"), "data/exp1/csv/clean/us-p3-.csv")

################################################################## US demography data - From Alt firestore #####
usdem_alt1 <- read_csv("data/exp1/csv/p3-usa-dem-alt-1.csv")
usdem_alt2 <- read_csv("data/exp1/csv/p3-usa-dem-alt-2.csv")

usdem_alt <- rbind(usdem_alt1, usdem_alt2)
# write_csv(usdem_alt, "data/exp1/csv/clean/us-p3-dem-alt.csv")

################################################################## US response data - From Alt firestore #####
## Note: The data from alt firestores are only for pilot #3 (done in alternating batches due to firestore writing limitations)
## Note: the demographic data from alt firebase have been merged manually
usdf_alt1 <- read_csv("data/exp1/csv/p3-usa-alt-1.csv")
usdf_alt2 <- read_csv("data/exp1/csv/p3-usa-alt-2.csv")

usdf_alt <- rbind(usdf_alt1, usdf_alt2)
# write_csv(usdf_alt, "data/exp1/csv/clean/us-p3-alt.csv")

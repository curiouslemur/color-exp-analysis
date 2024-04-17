source(file = './utils/exp1-utils.R')  # loads all necessary packages //NOTE: some packages


################################################################## MG demography data #####
mg_dem <- read.csv("data/exp1/csv/p3-mdg-dem.csv", encoding = "UTF-8")
mg_dem <- mg_dem %>%
  mutate(profession = replace(profession, 
                              profession %in% c("ETUDIANT", "ETUDIANTE", "ETUDIENT", "etudiant", "Etudiente", "Student", "Etudiant", 
                                                "étudiant", "etudiante", "etudiante ", "Student ", "etudient", "Etudiante", "étudiante", "Etudient"), "student")) %>% 
  mutate(gender = replace(gender, gender %in% c("homme", "Homme", "masculin", "Male", "FEMININ", "MASCULIN", "HOMME", "MALE", "Masculin"), "male")) %>%
  mutate(gender = replace(gender, gender %in% c("femme", "FEMME", "femelle", "Femme", "feminin", "Feminin", "femmme"), "female")) %>% 
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd1"), "mdg1")) %>% 
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd2"), "mdg2")) %>%
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd3"), "mdg3"))

write_csv(mg_dem, "data/exp1/csv/clean/mg-dem.csv")

#### MG response data #####
mgdf <- read.csv("data/exp1/csv/p3-mdg.csv", encoding = "UTF-8")
mgdf <- mgdf %>% 
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd1"), "mdg1")) %>% 
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd2"), "mdg2")) %>%
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd3"), "mdg3")) %>% 
  mutate(concept = tolower(concept)) %>% 
  rename(conceptFr = concept)


# TODO: ADD CONCEPT NAMES IN ENGLISH 

write_csv(mgdf, "data/exp1/csv/clean/mg.csv")

################################################################## US demography data #####
us_dem <- read_csv("data/exp1/csv/p3-usa-dem.csv")


write_csv(us_dem, "data/exp1/csv/clean/us-dem.csv")

#### US response data #####
usdf <- read_csv("data/exp1/csv/p3-usa.csv")
usdf <- usdf %>% 
  mutate(concept = tolower(concept))

write_csv(usdf, "data/exp1/csv/clean/us.csv")



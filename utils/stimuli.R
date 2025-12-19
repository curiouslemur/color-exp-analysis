# color_codes <- c("SR", "LR", "MR", "DR", "SO", "LO", "MO", "DO", "SY", "LY", "MY", "DY",
#                  "SH", "LH", "MH", "DH", "SG", "LG", "MG", "DG", "SC", "LC", "MC", "DC",
#                  "SB", "LB", "MB", "DB", "SP", "LP", "MP", "DP", "BK", "A1", "A2", "A3", "WH")

color_codes <- c("SR", "LR", "MR", "DR", "SP", "LP", "MP", "DP", "SB", "LB", "MB", "DB",
                 "SC", "LC", "MC", "DC", "SG", "LG", "MG", "DG", "SH", "LH", "MH", "DH",
                 "SY", "LY", "MY", "DY", "SO", "LO", "MO", "DO", "BK", "A1", "A2", "A3", "WH")

# the RGB values in the color_values_bcp37.csv file should be manually derived using http://www.brucelindbloom.com/index.html?ColorCalculator.html
# Ref. White D50
# Dom. Lambda: empty nm
# RGB model: sRGB
# Gamma: sRGB
# Adaptation: Bradford

bcp37 <- read_csv("./utils/color_values_bcp37.csv", show_col_types = FALSE)[1:37,] # color values for the bcp color library
# white RGB is replace with (0, 0, 255) temporarily for plotting

bcp37hex <- select(bcp37,color, hex)
### >>>>>>>> This is a temporary measure
# bcp37$R[bcp37$R > 255] <- 255; bcp37$R[bcp37$R < 0] <- 0;
# bcp37$G[bcp37$G > 255] <- 255; bcp37$B[bcp37$B > 255] <- 255
# ### <<<<<<<< 
# 
# bcp37$hex <- rgb(bcp37$R, bcp37$G, bcp37$B, maxColorValue = 255)
# bcp37 <- bcp37 %>% select(colorName, code, hex)
# 
# backgroundColor <- "#79797a"
##### List of concepts used for pilot study #1
# conceptListEn1 <- c(
#   "banana", "carrot", "mango", "peach",
#   "comfort", "justice", "peace", "safety",
#   "angry", "happy", "love", "sad",
#   "drought", "hurricane", "lightning", "sandstorm"
# )
# 
# conceptListFr1 <- c( 
#   'banane', 'carotte', 'mangue', 'pêche', 
#   'confort', 'justice', 'paix', 'sécurité',
#   'en colère', 'heureux', 'amour',  'triste',
#   'sécheresse', 'ouragan', 'foudre', 'tempête de sable')


##### List of concepts used for pilot study #2
# conceptListEn2 <- c(
#   "banana", "spices", "mango", "peach",
#   "life", "justice", "peace", "death",
#   "angry", "happy", "love", "sad",
#   "drought", "tree", "healthy", "sick"
# )
# 
# conceptListFr2 <- c( 
#   'banane', 'épices', 'mangue', 'pêche', 
#   'vie', 'justice', 'paix', 'mort',
#   'en colère', 'heureux', 'amour',  'triste',
#   'sécheresse', 'arbre', 'en bonne santé', 'malade')

##### List of concepts used for pilot study #3 (final set of 14 concepts)
conceptListEn3 = c('banana', 'mango', 'peach', 'death', 'justice', 'peace', 'safety',
                   'angry', 'happy', 'sad', 'sick', 'lightning', 'sandstorm', 'tree')
conceptListFr3 = c('banane', 'mangue', 'pêche', 'mort', 'justice', 'paix', 'sécurité',
                   'en colère', 'heureux', 'triste', 'malade', 'foudre', 'tempête de sable', 'arbre')

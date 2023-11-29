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

bcp37 <- read_csv("./utils/color_values_bcp37.csv")[1:37,] # color values for the bcp color library
### >>>>>>>> This is a temporary measure
bcp37$R[bcp37$R > 255] <- 255; bcp37$R[bcp37$R < 0] <- 0;
bcp37$G[bcp37$G > 255] <- 255; bcp37$B[bcp37$B > 255] <- 255
### <<<<<<<< 

bcp37$hex <- rgb(bcp37$R, bcp37$G, bcp37$B, maxColorValue = 255)
bcp37 <- bcp37 %>% select(colorName, code, hex)

library(tidyverse)
rm(list = ls())

# quick helper function to draw lines
ortho_v_phonemic <- function(d) {
  ggplot(data = d, aes(x = LENGTH, y = ORTHO_LENGTH)) + 
    geom_point() + geom_smooth() + 
    xlim(c(1, max(d$PHONEME_LENGTH, d$ORTHO_LENGTH))) +
    ylim(c(1, max(d$PHONEME_LENGTH, d$ORTHO_LENGTH))) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed")
}

setwd(*********)
# read in the data files
# files are cleaned up data from CELEX for Eng, German, Dutch
# have phonemic length, orthographic length, log Frequency, monomorpheme
eng <- read.csv('eng.txt')
ger <- read.csv('ger.txt')
dut <- read.csv('dut.txt')

# test to see effect of phonemic length on orthographic length
# dep variable : orthographic length
# indep variables : phonemeic length, log frequency, interaction between freq and phonemic length
eng.lm <- lm(data = eng, ORTHO_LENGTH ~ PHONEME_LENGTH * LOG_FREQ)
# all significant
summary(eng.lm)
# visualize the relationship
# dotted-line in the graph is the y=x line, what would happen if orthographic length and phoneme length were 
# identical
# the best fit line start ABOVE this line, showing more "letters" than phonemes
# however, once we get to long words, this switches back and the phoneme -> letter mapping gets more 1:1
ortho_v_phonemic(eng)
# because low freq words are longer, we see that frequence interacts with the "efficiency" (or whatever you want to call it)
# for phonemes to letters

# maybe better viz for this, but it's happy hour so.... I'm done
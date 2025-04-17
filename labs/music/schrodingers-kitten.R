# ==============================================================================
# packages
# ==============================================================================

library(gm)

# ==============================================================================
# every note on the piano
# ==============================================================================

pitches <- c("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
octaves <- 1:7
all_pitches <- c("A0", "A#0", "B0", 
                 paste(rep(pitches, length(octaves)), 
                       sort(rep(octaves, length(pitches))), 
                       sep = ""), 
                 "C8")


set.seed(8675309)

line1 <- Line(
  pitches = sample(all_pitches, 50, replace = TRUE),
  durations = .5
  #durations = sample(c(0.25, 0.5, 0.75, 1), 50, replace = TRUE)
)

line2 <- Line(
  pitches = sample(all_pitches, 50, replace = TRUE),
  durations = .5
  #durations = sample(c(0.25, 0.5, 0.75, 1), 50, replace = TRUE)
)

music <- Music() + 
  Meter(4, 4) + 
  Tempo(180) + 
  line1 + 
  line2 + 
  Instrument(1)

show(music)
# ==============================================================================
# packages
# ==============================================================================

library(gm)

# ==============================================================================
# helper functions
# ==============================================================================

random_walk <- function(vec, start_pos, steps) {
  # Check inputs
  n <- length(vec)
  if (start_pos < 1 || start_pos > n) stop("start_pos must be within the vector bounds.")
  
  # Initialize walk
  positions <- numeric(steps + 1)
  positions[1] <- start_pos
  
  for (i in 2:(steps + 1)) {
    current_pos <- positions[i - 1]
    
    # At the left boundary
    if (current_pos == 1) {
      new_pos <- 2
    }
    # At the right boundary
    else if (current_pos == n) {
      new_pos <- n - 1
    }
    # Otherwise move left or right with equal probability
    else {
      direction <- sample(c(-1, 1), size = 1)
      new_pos <- current_pos + direction
    }
    
    positions[i] <- new_pos
  }
  
  # Return the actual values from vec that were visited
  visited_values <- vec[positions]
  return(data.frame(step = 0:steps, position = positions, value = visited_values))
}

# ==============================================================================
# what pitches are fair game?
# ==============================================================================

pitches <- c("C", "D", "E-", "F", "G", "A-", "B-")
octaves <- 4:5
my_notes <- paste(rep(pitches, length(octaves)), 
                  sort(rep(octaves, length(pitches))), sep = "")

# ==============================================================================
# fixed accompaniment (stolen from Shostakovich)
# ==============================================================================

c_octave <- c("C2", "C3")
d_octave <- c("D2", "D3")
g_octave <- c("G1", "G2")
c_minor_triad <- c("C3", "E-3", "G3")

chunk_1 <- list(c_octave, c_minor_triad, c_minor_triad)
chunk_2 <- list(g_octave, c_minor_triad, c_minor_triad)
chunk_3 <- list(d_octave, c("F3", "A-3"), c("F3", "A-3"))
chunk_4 <- list(g_octave, c("F3", "A-3"), c("F3", "A-3"))

accompaniment <- Line(c(chunk_1, chunk_2, chunk_1, chunk_2,
                        chunk_1, chunk_2, chunk_1, chunk_2,
                        chunk_1, chunk_2, chunk_3, chunk_4))

timpani <- Line(c("C3", NA, NA, "G2", NA, NA, "C3", NA, NA, "G2", NA, NA,
                  "C3", NA, NA, "G2", NA, NA, "C3", NA, NA, "G2", NA, NA,
                  "C3", NA, NA, "G2", NA, NA, "D3", NA, NA, "G2", NA, NA))



# ==============================================================================
# simulate tune
# ==============================================================================

set.seed(1234)

tune_random_walk_1 <- Line(pitches = c(rep(NA, 12), 
                                     random_walk(my_notes, 1, 47)$value),
                         durations = c(rep(1, 12), 
                                       rep(1/2, 48)))

set.seed(2345678)

tune_random_walk_2 <- Line(pitches = c(rep(NA, 12), 
                                       random_walk(my_notes, 14, 47)$value),
                           durations = c(rep(1, 12), 
                                         rep(1/2, 48)))

set.seed(1234)

tune_iid <- Line(pitches = c(rep(NA, 12), 
                             sample(my_notes, 48, replace = TRUE)),
                 durations = c(rep(1, 12), 
                               rep(1/2, 48)))

# ==============================================================================
# compose!
# ==============================================================================

music <- Music() + 
  Meter(3, 4) + 
  Tempo(170) + 
  Key(-3) + 
  
  tune_iid + 
  Instrument(72) + # 72 - clarinet
  Dynamic("p", 13) + 
  
  accompaniment + 
  Clef("F") +
  Instrument(22) + # 72 - clarinet; 22 - accordion
  Dynamic("p", 1) +
  Dynamic("pp", 13) +
  #Hairpin(">", 7, 12) + # why doesn't this work?

  timpani + 
  Clef("F") + 
  Instrument(106) + # 106 - banjo
  Dynamic("p", 1) +
  Dynamic("pp", 13) +
  Hairpin(">", 7, 12) + 
  
  timpani + 
  Clef("F") + 
  Instrument(71) + # 71 - bassoon
  Dynamic("p", 1) +
  Dynamic("pp", 13) +
  Hairpin(">", 7, 12)

# add staccato mark to second and third beat of each bar of accompaniment

x <- 1:36
off_beats <- x[x %% 3 != 1]

for(i in off_beats){
  music <- music + Articulation(".", i, to = 2)
}

show(music)


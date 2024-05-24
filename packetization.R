# David Bass
# 24 May 2024

# Setup
set.seed(05242024)

phys <- c(
  "Classical mechanics", "Classical mechanics", "Classical mechanics", "Classical mechanics",
  "Electricity+magnetism", "Electricity+magnetism", "Electricity+magnetism", "Electricity+magnetism",
  "Quantum mechanics", "Quantum mechanics",
  "Thermo/stat mech", "Thermo/stat mech",
  "Particle/high-energy", "Particle/high-energy",
  "Relativity",
  "Optics/waves",
  "Fluids",
  "Condensed matter",
  "Nuclear/atomic",
  "Experiment/observation",
  "Other/any", "Other/any", "Other/any"
)

osci <- c(
  "Mathematics", "Mathematics", "Mathematics", "Mathematics", "Mathematics",
  "Mathematics", # "Statistics" in subdistro spreadsheet, but should avoid math
  "Astronomy", "Astronomy", "Astronomy", "Astronomy", "Astronomy",
  "Earth", # Earth tossup, but astronomy bonus
  "Earth", "Earth", "Earth", "Earth",
  "CS", "CS", "CS", "CS", "CS",
  "Engineering/misc", "Engineering/misc"
)

# Sample which questions will be tiebreakers
phys_tb_indices <- c(4, 23)
osci_tb_indices <- c(9, sample(c(1:6, 13:23), 1))

# Sample tossup and bonus categories under nice constraints
sample_tossup_and_bonus <- function(categories, tb_indices) {
  # Sample from non-tiebreakers
  tossups <- sample(categories[-tb_indices[1]])
  bonuses <- sample(categories[-tb_indices[2]])
  
  # Resample until (1) there are no packets with tossup and bonus from same topic,
  # (2) there are no more one instance of consecutive packets with the same topic
  # for both tossups and bonuses
  while (any(tossups == bonuses) | 
         sum(diff(as.numeric(factor(tossups))) == 0) > 1 |
         sum(diff(as.numeric(factor(bonuses))) == 0) > 1) {
    
    tossups <- sample(categories[-tb_indices[1]])
    bonuses <- sample(categories[-tb_indices[2]])
  }
  
  return(list(tossups, bonuses))
}

# Sample physics and osci tossup and bonus orders
phys_order <- sample_tossup_and_bonus(phys, phys_tb_indices)
osci_order <- sample_tossup_and_bonus(osci, osci_tb_indices)

# Replace one of the two "Earth" bonuses that are not adjacent to another "Astronomy"
# bonus with an "Astronomy" bonus
osci_order[[2]][sample(c(10, 16))] <- "Astronomy"

# Print it all out
phys_order
osci_order
phys_tb_indices
osci_tb_indices

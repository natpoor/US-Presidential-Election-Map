# US Electoral College Live Map
# "Live" well you have to enter the data as it updates, it doesn't pull data.
# This way you can enter states each party should win before they are called.
# Code by Nathaniel Poor, using various libraries and lots of Stack Overflow.
# Also internet sources for Maine and Nebraska's electoral college voting.
# August, 2024
# Code is provided "as is", no guarantees, your mileage may vary. 
# 
# NB: You have to add Maine and Nebraska manually.

library(usmap) 
library(ggplot2)
library(tidyverse)

# Maine and Nebraska are set to zero here, add them below to each party. Add the dem/gop to get the color.
# ME and NE allocate two electoral votes to the state popular vote winner, and then
# one electoral vote to the popular vote winner in each congressional district.
# Maine has two districts (four total EV), Nebraska has three districts (five total EV).

election_data <- tribble(
  ~state,                ~fips, ~ecvotes, ~values,
  "Alabama",              "01",     9,     "gop",
  "Alaska",               "02",     3,     "gop",
  "Arizona",              "04",    11,     "",
  "Arkansas",             "05",     6,     "gop",
  "California",           "06",    54,     "dem",
  "Colorado",             "08",    10,     "",
  "Connecticut",          "09",     7,     "dem",
  "Delaware",             "10",     3,     "",
  "District of Columbia", "11",     3,     "dem",
  "Florida",              "12",    30,     "gop",
  "Georgia",              "13",    16,     "",
  "Hawaii",               "15",     4,     "dem",
  "Idaho",                "16",     4,     "gop",
  "Illinois",             "17",    19,     "dem",
  "Indiana",              "18",    11,     "gop",
  "Iowa",                 "19",     6,     "gop",
  "Kansas",               "20",     6,     "gop",
  "Kentucky",             "21",     8,     "gop",
  "Louisiana",            "22",     8,     "gop",
  "Maine",                "23",     0,     "",
  "Maryland",             "24",    10,     "",
  "Massachusetts",        "25",    11,     "dem",
  "Michigan",             "26",    15,     "dem",
  "Minnesota",            "27",    10,     "dem",
  "Mississippi",          "28",     6,     "gop",
  "Missouri",             "29",    10,     "",
  "Montana",              "30",     4,     "gop",
  "Nebraska",             "31",     0,     "",
  "Nevada",               "32",     6,     "",
  "New Hampshire",        "33",     4,     "",
  "New Jersey",           "34",    14,     "dem",
  "New Mexico",           "35",     5,     "",
  "New York",             "36",    28,     "dem",
  "North Carolina",       "37",    16,     "",
  "North Dakota",         "38",     3,     "gop",
  "Ohio",                 "39",    17,     "",
  "Oklahoma",             "40",     7,     "gop",
  "Oregon",               "41",     8,     "dem",
  "Pennsylvania",         "42",    19,     "",
  "Rhode Island",         "44",     4,     "dem",
  "South Carolina",       "45",     9,     "gop",
  "South Dakota",         "46",     3,     "gop",
  "Tennessee",            "47",    11,     "gop",
  "Texas",                "48",    40,     "gop",
  "Utah",                 "49",     6,     "",
  "Vermont",              "50",     3,     "dem",
  "Virginia",             "51",    13,     "",
  "Washington",           "53",    12,     "dem",
  "West Virginia",        "54",     4,     "",
  "Wisconsin",            "55",    10,     "dem",
  "Wyoming",              "56",     3,     "gop",
  )

# colSums(election_data[, c(3)])   # 538 total, 529 + 4 for Maine + 5 for Nebraska, copied/entered them correctly, probably.

# Set up total EC votes for both parties and votes not won yet.
# Democrats.
dem_df    <- subset(election_data, election_data$values == "dem")
dem_total <- colSums(dem_df[, c(3)])
# For Maine EC Votes (four total).
dem_total <- dem_total + 0
# For Nebraska EC Votes (five total).
dem_total <- dem_total + 0

# Republicans.
gop_df    <- subset(election_data, election_data$values == "gop")
gop_total <- colSums(gop_df[, c(3)])
# For Maine EC Votes (four total).
gop_total <- gop_total + 0
# For Nebraska EC Votes (five total).
gop_total <- gop_total + 0

# Not called yet.
undeclared_total <- 538 - dem_total - gop_total

# Spot check:
# undeclared_total + dem_total + gop_total  # 538, spot check works out. 

# Set up the texts for the totals.
dem_text <- paste("Harris:", dem_total, sep=" ")  # 2024 candidate
gop_text <- paste("Trump:",  gop_total, sep=" ")  # 2024 candidate
und_text <- paste("Undeclared:", undeclared_total, sep=" ")

# Only fips and values for usmap call.
map_data <- election_data[c("fips", "values")]

# Set up the map object with usmap call. "Color" is the state borders. Some of the annotate adjustments are not as expected.
the_map <- plot_usmap(regions = "states", data=map_data, color="white", show.legend=FALSE, labels=TRUE, label_color='white') +
  scale_fill_manual(values = c("grey", "blue", "red")) + # This is the order the colors are on my machine, you may have to change them.
  annotate("text", -Inf, Inf, label = dem_text, hjust = -0.1, vjust = 2) +
  annotate("text", -Inf, Inf, label = gop_text, hjust = -0.1, vjust = 4.25) +
  annotate("text", -Inf, Inf, label = und_text, hjust = -0.07, vjust = 6.5) +
  annotate("text", -Inf, Inf, label = "(Need: 270)", hjust = -0.1, vjust = 9) +
  annotate("text", Inf, -Inf, label = "Remember! Land doesn't vote.", hjust = 1.2, vjust = -5) +
  annotate("text", Inf, -Inf, label = "Maine: 2 + 1 + 1", hjust = 2, vjust = -3) +
  annotate("text", Inf, -Inf, label = "Nebraska: 2 + 1 + 1 + 1", hjust = 1.38, vjust = -1)

# This adjusts the state label text size but not the annotation text sizes.
the_map$layers[[2]]$aes_params$size <- 3

# Print it!
the_map



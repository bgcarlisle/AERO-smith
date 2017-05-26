library(readr)

# Read trials from CSV
trials <- read_delim("~/Software/AERO-smith/trials.csv", ",", escape_double = FALSE, trim_ws = TRUE)

# Construct data frame for unique rows
rows <- data.frame(
  rowname = character(0),
  earliest_trial = numeric(0)
)
for ( row_name in unique(trials$row) ) {
  newrow <- data.frame(
    rowname = row_name,
    earliest_trial = min(as.Date(subset(trials, trials$row == row_name)$date))
  )
  rows <- rbind(rows, newrow)
}

# Order trials by earliest trial date
rows <- rows[order(rows$earliest_trial),]

# Number the rows
rows$y_coordinate <- 1 - 1:nrow(rows)

# Assign y coordinates based on row numbers
trials$y_coordinate <- rows$y_coordinate[match(trials$row, rows$rowname)]

# Generate ggplot
require(ggplot2)

aero <- ggplot (
  trials,
  aes(
    date,
    y_coordinate,
    size = scale,
    colour = factor(colour),
    shape = factor(shape)
  )
) +
  geom_point() +
  scale_colour_manual(values = c("orange", "red", "chartreuse4")) +
  labs (x = "Date", y = "Indication", size = "Trial N", colour = "Primary endpoint", shape = "Phase") +
  scale_y_discrete(limits=trials$y_coordinate, labels=trials$row) +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  # theme(legend.position = "none") +
  scale_shape_manual(values = c(15, 16, 17))

# Plot graph
aero
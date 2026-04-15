# Final Wildlife Crime Stats Code

# Setup
## Load Libraries
library(tidyverse)
library(ggplot2)
library(knitr)
library(dplyr)
library(sf)
library(rnaturalearth)
library(lubridate)
library(DT)
## Set Working Directory
setwd("/Users/alexley/Documents/STAT408/Semester Project")
## Read in and Clean Data
incidents = read_csv("Data/incident-data-3678.csv")
incidents = incidents[, 1:16]
incidents = incidents |>
  select(-"Primary Source", -"Source Type", -"Additional Sources", -"Method of Concealment", -"Name of Organisation Providing Information", -"Methods - Details")

names(incidents) = c("ID", "incident_type", "country", "date", "subject", "transport_mode", "where_found", "detection", "outcome", "arrests")
incidents = incidents |>
  mutate(incident_type = str_remove(incident_type, "^[0-9]+\\.\\s*"))

# Overview Page
# Value boxes (summary stats)
total_seizures <- format(nrow(incidents))
total_arrests = sum(incidents$arrests, na.rm = TRUE)

# Box 1
#| content: valuebox
#| title: "Total Seizures"

list(
  icon = "exclamation-triangle",
  color = "danger",
  value = total_seizures
)
# Box 2
#| content: valuebox
#| title: "Total Arrests"

list(
  icon = "person-badge",
  color = "warning",
  value = total_arrests
)

# Trends over time
incidents <- incidents |>
  mutate(date = as.Date(date))

trends_year <- incidents |>
  mutate(year = lubridate::year(date)) |>
  count(year, name = "n") |>
  arrange(year)

ggplot(trends_year, aes(x = year, y = n)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = unique(trends_year$year)) +
  labs(
    x = "Year",
    y = "Number of Seizures",
    title = "Wildlife Crime Seizure Trends Over Time"
  ) +
  theme_minimal()

# Pie Chart of most common seizure type
seizure_type = incidents |>
  count(incident_type, name = "n") |>
  arrange(desc(n))

ggplot(seizure_type, aes(x = "", y = n, fill = incident_type)) +
  geom_col(width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(
    title = "Distribution of Seizure Types",
    fill = "Incident Type"
  ) +
  scale_fill_brewer(palette = "Spectral")

# Global Patterns Page

# Map of Incidents by Country
# Count number of seizures per country
seizures_country = incidents |>
  count(country, name = "num_seizures")

# Load world shapefile
world = ne_countries(scale = "medium", returnclass = "sf")

# Join seizure data to world map
world_seizures = world |>
  left_join(seizures_country, by = c("name" = "country"))

# Replace NA values with 0 (countries with no seizures recorded)
world_seizures$num_seizures[is.na(world_seizures$num_seizures)] = 0

# Plot map
ggplot(world_seizures) +
  geom_sf(aes(fill = num_seizures), color = "gray70", size = 0.1) +
  scale_fill_gradient( 
    low = "lightyellow", 
    high = "darkred", 
    name = "Seizures"
  ) +
  labs(
    title = "Wildlife Crime Seizures by Country",
    subtitle = "Number of recorded incidents",
    caption = "Source: Wildlife Trade Portal"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

#Top 10 Countries
counts2 = incidents |> count(country)
top_counts = counts2 |>
  slice_max(n, n = 10)   # top 10 countries

ggplot(top_counts, aes(x = reorder(country, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Country",
    y = "Count",
    title = "Wildlife Crime Incidents (Top 10 Countries)"
  )

# Top 50 Countries by Incident Type
top50 = incidents |>
  count(country, sort = TRUE) |>
  slice_head(n = 50)

incidents_top50 = incidents |>
  filter(country %in% top50$country)

ggplot(incidents_top50, aes(x = country, fill = incident_type)) +
  geom_bar() +
  coord_flip() +
  labs(
    x = "Country",
    y = "Count",
    title = "Type of Incident by Country"
  ) +
  scale_fill_brewer(palette = "Spectral")


# Methods of Transport
transport_methods = incidents |>
  mutate(transport_mode = ifelse(is.na(transport_mode),
                                 "Unknown",
                                 transport_mode)) |>
  count(transport_mode, name = "num_transport")

ggplot(transport_methods, aes(x = reorder(transport_mode, num_transport), y = num_transport)) +
  geom_bar(stat = "identity") +
  labs(x = "Type of Transport", y = "Count",
       title = "Type of Transport Used for Smuggling")


# Arrests per Country
arrests_by_country = incidents |>
  group_by(country) |>
  summarise(total_arrests = sum(arrests, na.rm = TRUE)) |>
  filter(total_arrests > 10)

ggplot(arrests_by_country, 
       aes(x = reorder(country, total_arrests), y = total_arrests)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Country", y = "Number of Arrests",
       title = "Total Arrests by Country")

# Data Table
incidents_clean <- incidents |>
  rename(
    ID = ID,
    `Incident Type` = incident_type,
    Country = country,
    Date = date,
    Subject = subject,
    `Transport Mode` = transport_mode,
    `Where Found` = where_found,
    `Detection Method` = detection,
    Outcome = outcome,
    Arrests = arrests
  )
incidents_clean |>
  arrange(ID) |>
  head(500) |>
  datatable()

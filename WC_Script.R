# Final Wildlife Crime Stats Code

# Setup
library(tidyverse)
library(ggplot2)
library(knitr)
library(dplyr)

incidents <- read_csv("Data/incident-data-3653.csv")
incidents <- incidents[, 1:16]
incidents <- incidents |>
  select(-"Primary Source", -"Source Type", -"Additional Sources", -"Method of Concealment", -"Name of Organisation Providing Information", -"Methods - Details")

names(incidents)
names(incidents) = c("ID", "incident_type", "country", "date", "subject", "transport_mode", "where_found", "detection", "outcome", "arrests")


# Incidents and Countries
counts = incidents |>
  count(country, incident_type)

ggplot(incidents, aes(x = country, fill = incident_type)) +
  geom_bar() +
  coord_flip() +
  labs(x = "Country", y = "Count",
       title = "Wildlife Crime Incidents by Country and Type")

# All Countries
ggplot(counts, aes(x = reorder(country, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Country", y = "Number of Incidents",
       title = "Wildlife Crime Incidents by Country")


# Top 10 Countries
counts2 = incidents |> count(country)
top_counts = counts2 |>
  slice_max(n, n = 10)   # top 10 countries

ggplot(top_counts, aes(x = reorder(country, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip()


# Arrests per Country
arrests_by_country = incidents |>
  group_by(country) |>
  summarise(total_arrests = sum(arrests, na.rm = TRUE))

ggplot(arrests_by_country, 
       aes(x = reorder(country, total_arrests), y = total_arrests)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Country", y = "Number of Arrests",
       title = "Total Arrests by Country")
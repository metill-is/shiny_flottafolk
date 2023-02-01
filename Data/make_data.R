library(eurostat)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(arrow)

cache_dir <- "Data"


# Beneficiaries of temporary protection -----------------------------------

beneficiaries <- get_eurostat(
    "migr_asytpsm", 
    cache = T,
    # update_cache = T,
    cache_dir = cache_dir
) |>
    filter(
        sex == "T",
        age == "TOTAL",
        citizen == "TOTAL"
    ) |> 
    select(-unit, -sex, -age, -citizen) |> 
    label_eurostat()


# Population of countries -------------------------------------------------

pop <- get_eurostat(
    "demo_pjan",
    cache = T,
    cache_dir = cache_dir
) |> 
    filter(
        age == "TOTAL",
        sex == "T"
    ) |> 
    select(-unit, -age, -sex) |> 
    label_eurostat()

applicants <- get_eurostat(
    "migr_asypenctzm",
    cache = T,
    cache_dir = "data"
) |>
    janitor::remove_constant() |> 
    filter(
        age == "TOTAL",
        sex == "T",
        citizen == "TOTAL"
    ) |> 
    select(-citizen, -sex, -age) |> 
    label_eurostat()


# Number of decisions granting temporary protection -----------------------

grants <- get_eurostat(
    "migr_asytpfm",
    cache = T,
    cache_dir = cache_dir
) |> 
    filter(
        sex == "T",
        age == "TOTAL",
        citizen == "TOTAL"
    ) |> 
    select(-unit, -citizen, -sex, -age) |> 
    label_eurostat()


# Number of decisions, not specific to granting protection ----------------

decisions <- get_eurostat(
    "migr_asydcfstq",
    cache = TRUE,
    cache_dir = cache_dir
)  |> 
    filter(
        citizen == "TOTAL",
        sex == "T",
        age == "TOTAL",
        decision %in% c("TOTAL", "TOTAL_POS")
    ) |> 
    select(-citizen, -sex, -age, -unit) |> 
    label_eurostat() |> 
    pivot_wider(names_from = decision, values_from = values) |> 
    mutate(perc = `Total positive decisions` / Total) |> 
    select(geo, time, perc)


# Merge into one dataset --------------------------------------------------

d <- beneficiaries |> 
    mutate(
        year = year(time)
    ) |> 
    rename(beneficiaries = values) |> 
    inner_join(
        pop |> 
            group_by(geo) |> 
            filter(time == max(time)) |> 
            ungroup() |> 
            select(geo, pop = values),
        by = c("geo")
    ) |> 
    full_join(
        applicants |> 
            rename(applicants = values),
        by = c("geo", "time")
    ) |> 
    inner_join(
        grants |> 
            rename(grants = values),
        by = c("geo", "time")
    ) |> 
    rename(country = geo) |> 
    mutate(
        country = ifelse(str_detect(country, "Germany"), "Germany", country)
    ) |> 
    inner_join(
        metill::country_names(),
        by = "country"
    ) |> 
    select(
        -year, -country
    ) |> 
    pivot_longer(
        c(-land, -time, -pop)
    ) |> 
    group_by(land) |> 
    mutate(pop = max(pop, na.rm = T)) |> 
    ungroup() |> 
    mutate(per_pop = value / pop * 1e3) |> 
    select(-value, -pop) |> 
    pivot_wider(names_from = name, values_from = per_pop) |> 
    rename(
        "Fjöldi með vernd" = beneficiaries,
        "Umsækjendur" = applicants,
        "Veitt vernd í mánuði" = grants
    ) |> 
    pivot_longer(c(-time, -land), names_to = "variable") |> 
    rename(group = land)

d |> 
    write_parquet("Data/Data.parquet")

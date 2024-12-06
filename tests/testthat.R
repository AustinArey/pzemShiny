library(testthat)
library(DBI)
library(lubridate)

source("../group_analysis.R")

# Setup and Teardown
setup({
    # Mock database connection
    conn <<- dbConnect(RSQLite::SQLite(), ":memory:")

    # Create mock tables and data
    dbWriteTable(conn, "unitA", data.frame(
        energy = c(100, 200, 300, 0, 50),
        date_time = c("2023-01-01 00:00:00", "2023-01-15 00:00:00", "2023-02-01 00:00:00", "2023-02-15 00:00:00", "2023-02-28 00:00:00")
    ))

    tables <<- list(
        "Group 1" = c("unitA")
    )
})

teardown({
    dbDisconnect(conn)
})

# Print the data in the unitA table
print(dbGetQuery(conn, "SELECT * FROM unitA"))

# Unit test for days_diff_num
test_that("days_diff_num works with dates", {
    date_range <- c("2023-01-01", "2023-01-10")
    days_selected <- days_diff_num(date_range[1], date_range[2])
    expect_equal(days_selected, 10)
})

# Unit tests for calc_energy_used
test_that("calc_energy_used works with monotonic data", {
    date_range <- c("2022-12-31", "2023-02-01")

    final_query_str <- final_query("unitA", date_range[2])
    final <- dbGetQuery(conn, final_query_str)

    initial_query_str <- initial_query("unitA", date_range[1])
    initial <- dbGetQuery(conn, initial_query_str)

    max_query_str <- max_query("unitA", date_range[1], date_range[2])
    max <- dbGetQuery(conn, max_query_str)

    result <- calc_energy_used(initial$energy, max$`MAX(energy)`, final$energy)
    expect_equal(result, 200)
})

test_that("calc_energy_used works with data that overflowed bufffer", {
    date_range <- c("2023-01-01", "2023-03-01")
    days_selected <- days_diff_num(date_range[1], date_range[2])
    expect_equal(days_selected, 60)

    initial_query_str <- initial_query("unitA", date_range[1])
    final_query_str <- final_query("unitA", date_range[2])
    max_query_str <- max_query("unitA", date_range[1], date_range[2])

    initial <- dbGetQuery(conn, initial_query_str)
    final <- dbGetQuery(conn, final_query_str)
    max <- dbGetQuery(conn, max_query_str)

    result <- calc_energy_used(initial$energy, max$`MAX(energy)`, final$energy)
    expect_equal(result, 250)
})

# Unit tests for calc_energy_days_measured
test_that("calc_energy_days_measured works with monotonic data", {
    date_range <- c("2023-01-01", "2023-02-02")
    days_selected <- days_diff_num(date_range[1], date_range[2])
    expect_equal(days_selected, 33)

    measured_date_range <- c("2023-01-01", "2023-02-01")
    expected_days_measured <- days_diff_num(measured_date_range[1], measured_date_range[2])

    result <- calc_energy_days_measured(conn, "unitA", date_range)
    expect_equal(result$table, "unitA")
    expect_equal(result$energy_measured, 200)
    expect_equal(result$days_measured, 32)
})

test_that("calc_energy_days_measured works with data that overflowed bufffer", {
    date_range <- c("2023-01-01", "2023-03-01")
    days_selected <- days_diff_num(date_range[1], date_range[2])

    measured_date_range <- c("2023-01-01", "2023-02-28")
    expected_days_measured <- days_diff_num(measured_date_range[1], measured_date_range[2])

    result <- calc_energy_days_measured(conn, "unitA", date_range)
    expect_equal(result$table, "unitA")
    expect_equal(result$energy_measured, 250)
    expect_equal(result$days_measured, 59)
})

test_that("calc_energy_days_measured handles no data", {
    date_range <- c("1900-01-01", "1900-01-31")
    expect_error(calc_energy_days_measured(conn, "unitA", date_range), "argument is of length zero")
})

# Unit tests for create_meter_df
test_that("create_meter_df calculates kWh correctly", {
    date_range <- c("2023-01-01", "2023-03-15")
    days_selected <- days_diff_num(date_range[1], date_range[2]) # 74
    cost_per_kwh <- 0.5
    expected_kwh <- 0.250

    result <- create_meter_df(conn, tables, "Group 1", date_range, cost_per_kwh, days_selected)
    expect_equal(result$kWh, expected_kwh)
})

test_that("create_meter_df calculates DaysMeas correctly", {
    selected_date_range <- c("2023-01-01", "2023-03-15")
    days_selected <- days_diff_num(selected_date_range[1], selected_date_range[2]) # 74
    cost_per_kwh <- 0.5
    measured_date_range <- c("2023-01-01", "2023-02-28")
    expected_days_measured <- days_diff_num(measured_date_range[1], measured_date_range[2])

    result <- create_meter_df(conn, tables, "Group 1", selected_date_range, cost_per_kwh, days_selected)
    expect_equal(result$DaysMeas, expected_days_measured)
})

test_that("create_meter_df calculates CostEst correctly", {
    date_range <- c("2023-01-01", "2023-03-15")
    days_selected <- days_diff_num(date_range[1], date_range[2]) # 74
    cost_per_kwh <- 0.5
    expected_kwh <- 0.250
    expected_days_measured <- 58

    result <- create_meter_df(conn, tables, "Group 1", date_range, cost_per_kwh, days_selected)
    expected_cost_est <- round(expected_kwh * cost_per_kwh * days_selected / expected_days_measured, 2)
    expect_equal(result$CostEst, expected_cost_est)
})

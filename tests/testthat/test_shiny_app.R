library(shinytest2)

test_that("Shiny app works", {
    app <- AppDriver$new("../../app.R", timeout = 60000) # Increase timeout to 30000ms (30 seconds)

    # Interact with the app
    app$set_inputs(group = "Group 1")
    app$set_inputs(date_range = c("2023-01-01", "2023-01-31"))

    # Take a snapshot of the app's state
    app$expect_screenshot()

    # Verify output values
    app$expect_values(output = "total_energy", expected = "200kWh")
    app$expect_values(output = "cost_per_kwh", expected = "$0.5")
    app$expect_values(output = "est_bill_total", expected = "$100.00")
})

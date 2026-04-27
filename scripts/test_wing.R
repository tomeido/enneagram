library(testthat)

# Load the utility function. Note: assuming execution from the project root.
# If running from within the scripts directory, use source("wing_utils.R")
if (file.exists("scripts/wing_utils.R")) {
  source("scripts/wing_utils.R")
} else {
  source("wing_utils.R")
}

describe("calculate_wing", {
  # Mock praxis data for testing
  praxis <- data.frame(
    number = 1:9,
    count = c(10, 12, 14, 16, 18, 16, 14, 12, 10)
  )

  it("correctly identifies the wing for Type 9 (neighbors 8 and 1)", {
    # Type 9: wing1=8 (count=12), wing2=1 (count=10)
    expect_equal(calculate_wing(9, praxis), 8)

    # Swap counts to test wing2
    praxis_swapped <- praxis
    praxis_swapped$count[praxis_swapped$number == 1] <- 20
    expect_equal(calculate_wing(9, praxis_swapped), 1)
  })

  it("correctly identifies the wing for Type 1 (neighbors 9 and 2)", {
    # Type 1: wing1=9 (count=10), wing2=2 (count=12)
    expect_equal(calculate_wing(1, praxis), 2)

    # Swap counts to test wing1
    praxis_swapped <- praxis
    praxis_swapped$count[praxis_swapped$number == 9] <- 20
    expect_equal(calculate_wing(1, praxis_swapped), 9)
  })

  it("correctly identifies the wing for middle types (e.g., Type 5)", {
    # Type 5: wing1=4 (count=16), wing2=6 (count=16) -> Tie, returns wing2 (6)
    expect_equal(calculate_wing(5, praxis), 6)

    # Type 5 with wing1 higher: wing1=4 (count=20), wing2=6 (count=16)
    praxis_w1 <- praxis
    praxis_w1$count[praxis_w1$number == 4] <- 20
    expect_equal(calculate_wing(5, praxis_w1), 4)
  })

  it("handles ties by choosing the second wing (wing2)", {
    # Type 5: wing1=4 (count=16), wing2=6 (count=16)
    # The current logic: count1 > count2 is FALSE, so wing = wing2
    expect_equal(calculate_wing(5, praxis), 6)
  })
})

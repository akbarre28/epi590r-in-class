# 2.3 Functions - Square a number. You’re tired of writing x^2 when you want to square x, so you want a function to square a number. You can call it square(). I showed this in the slides, now try on your own!

# start out with a number to test
x <- 70

# you'll want your function to return this number
x^2
square <- function(x) {
	sq_val <- x*x
	return(sq_val)
}

# test it out
square(x)
square(70)
70^2 # does this match?



# 2.4 Functions -
# Write a function to raise to any power. You don’t just want to square numbers, you want to raise them to higher powers too. Make a function that uses two arguments, x for a number, and power for the power. Call it raise().
raise <- function(x, power) {
	power_val <- x ^ power
	return(power_val)
}

# test with
raise(x = 2, power = 4)
raise(2, 4)
# should give you
2^4
# IT WORKS!

# Change your raise() function to default to squaring x when the user doesn’t enter a value for power.
# Defaulted raise
raise_default <- function(x, power = 2) {
	power_val <- x ^ power # You can use functions that are already available or made functions in these
	return(power_val)
}

# test
raise_default(x = 5)
raise_default(5)
# should give you
5^2
# IT WORKS!



# 2.5 Functions -
# Fit the models from the slides.
logistic_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
											data = nlsy, family = binomial()
)

poisson_model <- glm(nsibs ~ eyesight_cat + sex_cat + income,
										 data = nlsy, family = poisson()
)

logbinomial_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
												 data = nlsy, family = binomial(link = "log")
)

# Using the function from the slides, create a table for each model. What types of situations would this be useful for, and when would it not?
new_table_function <- function(model) {
	tbl_regression(
		model,
		exponentiate = TRUE,
		label = list(
			sex_cat ~ "Sex",
			eyesight_cat ~ "Eyesight",
			income ~ "Income"
		)
	)
}

new_table_function(model = logistic_model)

new_table_function(model = poisson_model)

new_table_function(model = logbinomial_model) #???



# Add an argument to the function that allows someone to add robust standard errors (see the tidy_fun = argument from the regression exercise solutions yesterday).

new_table_function_tidy <- function(model, tidy_fun = broom.helpers::tidy_with_broom_or_parameters()) {
	tbl_regression(
		model,
		exponentiate = TRUE,
		label = list(
			sex_cat ~ "Sex",
			eyesight_cat ~ "Eyesight",
			income ~ "Income"
		)
	)
}

new_table_function_tidy(model = poisson_model, tidy_fun = partial(tidy_robust, vcov = "HC1"))
# Robust standard errors - variance and covariance use different calculations.And default SE just dont make sense for certain models so this is used instead.



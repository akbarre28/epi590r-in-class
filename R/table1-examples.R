library(tidyverse)
library(gtsummary)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Customization of `tbl_summary()`

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir))


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing")


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")




#Make a tbl_summary(). Include categorical region, race/ethnicity, income, and the sleep variables (use a helper function to select those) and make sure they are nicely labeled.
tbl_summary(
	nlsy,
	include = c(race_eth_cat, region_cat,
							income, starts_with("sleep")),
label = list(
	race_eth_cat ~ "Race/ethnicity",
	region_cat ~ "Region",
	income ~ "Income",
	sleep_wkdy ~ "Sleep during Weekday",
	sleep_wknd ~ "Sleep during Weekend"
))

#Stratify the table by sex. Add a p-value comparing the sexes and an overall column combining both sexes.
tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(race_eth_cat, region_cat,
							income, starts_with("sleep")),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		income ~ "Income",
		sleep_wkdy ~ "Sleep during Weekday",
		sleep_wknd ~ "Sleep during Weekend"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
		add_overall(col_label = "**Total**") |>
		bold_labels() |>


#For the income variable, show the 10th and 90th percentiles of income with 3 digits, and for the sleep variables, show the min and the max with 1 digit.
tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(race_eth_cat, region_cat,
							income, starts_with("sleep")),
label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		income ~ "Income",
		sleep_wkdy ~ "Sleep on Weekday",
		sleep_wknd ~ "Sleep on Weekend"),

statistic = list(
		income ~ "{p10}; {p90}",
		starts_with("sleep") ~ "{min}; {max}"),

digits = list(
	income ~ 3,
	starts_with("sleep") ~ 1),

missing_text = "Missing") |>


#Add a footnote to the race/ethnicity variable with a link to the page describing how NLSY classified participants: https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data
	tbl_summary(
		nlsy,
		by = sex_cat,
		include = c(race_eth_cat, region_cat,
								income, starts_with("sleep")),
		label = list(
			race_eth_cat ~ "Race/ethnicity",
			region_cat ~ "Region",
			income ~ "Income",
			sleep_wkdy ~ "Sleep on Weekday",
			sleep_wknd ~ "Sleep on Weekend"),

		statistic = list(
			income ~ "{p10} {p90}",
			starts_with("sleep") ~ "{min} {max}"),

		digits = list(
			income ~ 3,
			starts_with("sleep") ~ 1),

missing_text = "Missing") |>

	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |> #The two * are used to signify that this new label is to be bold
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**") |>
	modify_table_styling(columns = label, rows = label == "Race/ethnicity",
											 footnote = "https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data")




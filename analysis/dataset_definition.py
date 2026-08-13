from ehrql import create_dataset, days, get_parameter, years
from analysis.variable_functions import *

dataset = create_dataset()

vaccine_name = get_parameter("vaccine_name")
threshold_date = get_parameter("threshold_date")
min_dob = get_parameter("min_dob")
max_dob = get_parameter("max_dob")

dataset = demographics(threshold_date)
dataset = primary_care_outcomes(threshold_date)
dataset = primary_care_exclusions(threshold_date)
dataset = primary_care_controls_assumptions(threshold_date)
dataset = secondary_care_outcomes(threshold_date)
dataset = secondary_care_exclusions(threshold_date)
dataset = vaccinations(threshold_date)

# Note - we are extracting a broad population and will apply further exclusions downstream 
dataset.define_population(
    (dataset.reg_start_date.is_on_or_before(threshold_date - days(90)))
    & (dataset.reg_end_date.is_on_or_after(threshold_date))
    & (dataset.date_of_birth.is_on_or_between(min_dob, max_dob))
    & (dataset.date_of_death.is_after(threshold_date) | (dataset.date_of_death.is_null()))
)

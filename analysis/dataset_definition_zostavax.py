from ehrql import create_dataset, days, get_parameter, years
from analysis.variable_functions import *

dataset = create_dataset()

threshold_date = get_parameter("threshold_date")

# demographics, main outcomes, exclusions
dataset = demographics(threshold_date)
dataset = primary_care_main_outcomes(threshold_date)
dataset = secondary_care_main_outcomes(threshold_date)
dataset = primary_care_exclusions(threshold_date)
dataset = vaccinations()

# only extract negative controls / variables for testing assumptions for main analysis
if threshold_date == "2013-09-01":
    dataset = primary_care_controls_assumptions(threshold_date)

dataset.configure_dummy_data(population_size=100000, timeout=300,
                             additional_population_constraint=(
                                 dataset.zostavax_date_1.is_on_or_between("2013-09-01","2014-02-01")
                             ))

dataset.define_population(
    # registered for at least 90 days
    (dataset.reg_start_date.is_on_or_before(threshold_date - days(90)))
    # age 77-<82 years on index date
    & (dataset.date_of_birth.is_on_or_between(threshold_date - years(82), threshold_date - years(77)))
    # m/f sex only due to disclosure risk of non m/f sexes
    & ((dataset.sex == "male") | (dataset.sex == "female"))
    # alive on threshold_date
    & (dataset.date_of_death.is_after(threshold_date) | (dataset.date_of_death.is_null()))
)
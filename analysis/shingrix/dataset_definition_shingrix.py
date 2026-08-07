from ehrql import create_dataset, days, get_parameter
from analysis.variable_functions import *

dataset = create_dataset()

threshold_date = get_parameter("threshold_date")

dataset = demographics(threshold_date)
dataset = primary_care_main_outcomes(threshold_date)
dataset = secondary_care_main_outcomes(threshold_date)
dataset = primary_care_exclusions(threshold_date)
dataset = primary_care_controls_assumptions(threshold_date)
dataset = vaccinations()

dataset.configure_dummy_data(population_size=100000, timeout=300,
                             additional_population_constraint=(
                                 dataset.shingrix_date_1.is_on_or_between("2023-09-01","2024-02-01")
                                 & dataset.shingrix_date_2.is_on_or_after(dataset.shingrix_date_1))
                             )

dataset.define_population(
    # registered for at least 90 days
    (dataset.reg_start_date.is_on_or_before(threshold_date - days(90)))
    # note - DOB is recorded as 15th of month
    & (dataset.date_of_birth.is_on_or_between("1948-09-01", "1968-09-01"))
    # m/f sex only due to disclosure risk of non m/f sexes
    & ((dataset.sex == "male") | (dataset.sex == "female"))
    # non-missing IMD
    & dataset.imd_decile.is_not_in("unknown")
    # non-missing practice region
    & dataset.region.is_not_null()
    # alive on threshold_date
    & (dataset.date_of_death.is_after(threshold_date) | (dataset.date_of_death.is_null()))
)
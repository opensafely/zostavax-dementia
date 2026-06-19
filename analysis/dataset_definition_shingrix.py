from ehrql import create_dataset, days, get_parameter, years
from analysis.variable_functions import *
from analysis.helper_functions import *

dataset = create_dataset()
dataset.configure_dummy_data(population_size=100000)

index_date = get_parameter("index_date")

# demographics and main outcomes
dataset = demographics(index_date)
dataset = primary_care_main_outcomes(index_date)
dataset = primary_care_exclusions(index_date)

# only extract negative controls / variables for testing assumptions for main analysis
if index_date == "2023-02-01":
    dataset = primary_care_control_outcomes(index_date)
    dataset = primary_care_assumptions(index_date)

shingrix_products = ["Shingrix","Shingles (Herpes Zoster) adjuvanted rcmb vacc powder and suspension for suspension inj 0.5ml vials"]
dataset.shingrix_date_1  = first_vax_event_after(index_date, product_name=shingrix_products).date
dataset.shingrix_date_2  = first_vax_event_after(dataset.shingrix_date_1, product_name=shingrix_products).date


dataset.define_population(
    # registered for at least 90 days
    (dataset.reg_start_date.is_on_or_before(index_date - days(90)))
    & (dataset.dob.is_on_or_between("1956-09-01", "1961-09-01"))
    # m/f sex only due to disclosure risk of non m/f sexes
    & ((dataset.sex == "male") | (dataset.sex == "female"))
    # alive on index_date
    & (dataset.dod.is_after(index_date) | (dataset.dod.is_null()))
)

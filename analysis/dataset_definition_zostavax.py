from ehrql import create_dataset, days, get_parameter, years
from analysis.variable_functions import *
from analysis.helper_functions import *

dataset = create_dataset()
dataset.configure_dummy_data(population_size=100000)

index_date = get_parameter("index_date")

# demographics and main outcomes
dataset = demographics(index_date)
dataset = primary_care_main_outcomes(index_date)
dataset = secondary_care_main_outcomes(index_date)
dataset = primary_care_exclusions(index_date)

# only extract negative controls / variables for testing assumptions for main analysis
if index_date == "2014-02-01":
    dataset = primary_care_control_outcomes(index_date)
    dataset = primary_care_assumptions(index_date)

zostavax_products = ["Zostavax","Shingles (Herpes Zoster) vaccine (live) powder and solvent for suspension for injection 0.65ml pfs"]
dataset.zostavax_date_1  = first_vax_event_after(index_date, product_name=zostavax_products).date

dataset.define_population(
    # registered for at least 90 days
    (dataset.reg_start_date.is_on_or_before(index_date - days(90)))
    # age 77-<82 years on index date
    & (dataset.dob.is_on_or_between(index_date - years(82), index_date - years(77)))
    # m/f sex only due to disclosure risk of non m/f sexes
    & ((dataset.sex == "male") | (dataset.sex == "female"))
    # non-missing IMD
    & (dataset.imd_decile.is_not_null())
    # alive on index_date
    & (dataset.dod.is_after(index_date) | (dataset.dod.is_null()))
)
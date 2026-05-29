from ehrql import create_dataset, days, get_parameter, years
from ehrql.tables.tpp import vaccinations
from analysis.functions import *

dataset = create_dataset()
dataset.configure_dummy_data(population_size=100000)

index_date = get_parameter("index_date")

dataset = demographics(index_date)
dataset = primary_care_outcomes(index_date)
dataset = primary_care_controls(index_date)

## Vaccinations
# Note - exclude anyone whose first vaccination date is BEFORE 1 Sep 2023
dataset.shingrix_date_1 = (
    vaccinations
    .where(vaccinations.product_name.is_in(["Shingrix",
      "Shingles (Herpes Zoster) adjuvanted rcmb vacc powder and suspension for suspension inj 0.5ml vials"]))
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date
)
dataset.shingrix_date_2 = (
    vaccinations
    .where(vaccinations.product_name.is_in(["Shingrix",
        "Shingles (Herpes Zoster) adjuvanted rcmb vacc powder and suspension for suspension inj 0.5ml vials"])
        & vaccinations.date.is_after(dataset.shingrix_date_1))
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date
)

dataset.define_population(
    # registered for at least 90 days
    (dataset.reg_start_date.is_on_or_before(index_date - days(90)))
    # age 77-<82 years on index date
    & (dataset.dob.is_on_or_between(index_date - years(82), index_date - years(77)))
    # m/f sex only due to disclosure risk of non m/f sexes
    & ((dataset.sex == "male") | (dataset.sex == "female"))
)
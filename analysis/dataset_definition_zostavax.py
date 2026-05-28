from ehrql import create_dataset, days
from ehrql.tables.tpp import vaccinations
from variable_functions import *


### THIS DATASET DEF IS STILL A WORK IN PROGRESS ###

dataset = create_dataset()
dataset.configure_dummy_data(population_size=10000)

index_date = "2023-09-01"

dataset = demographics(index_date)
dataset = primary_care_events(index_date)
dataset = secondary_care_events(index_date)

## Vaccinations
# Note - will exclude anyone whose first vaccination date is BEFORE 1 Sep 2023
dataset.zostavax_date_1 = (
    vaccinations
    .where(vaccinations.product_name.is_in([
      "Shingles (Herpes Zoster) adjuvanted rcmb vacc powder and suspension for suspension inj 0.5ml vials",
      "Shingrix"
      ]))
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date
)

# NEED TO UDPATE
dataset.define_population(
    # registered for at least 90 days
    (dataset.reg_start_date.is_on_or_before(index_date - days(90)))
    & (dataset.dob.is_on_or_between("1931-09-02", "1936-09-01"))
    # m/f sex only due to disclosure risk of non m/f sexes
    & ((dataset.sex == "male") | (dataset.sex == "female"))
)
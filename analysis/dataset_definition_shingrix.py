from ehrql import create_dataset, days, case, when, minimum_of
from ehrql.tables.tpp import (
    patients, 
    practice_registrations, 
    clinical_events, 
    addresses, 
    ons_deaths, 
    vaccinations
)

import codelists

dataset = create_dataset()

index_date = "2013-09-01"

# Demographics
dataset.dob = patients.date_of_birth
dataset.age = patients.age_on(index_date)
dataset.sex = patients.sex
dataset.imd_decile = addresses.for_patient_on(index_date).imd_decile

ethnicity6 = clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.ethnicity_codes_6)
    ).where(
        clinical_events.date.is_on_or_before(index_date)
    ).sort_by(
        clinical_events.date
    ).last_for_patient().snomedct_code.to_category(codelists.ethnicity_codes_6)

dataset.ethnicity6 = case(
    when(ethnicity6 == "1").then("White"),
    when(ethnicity6 == "2").then("Mixed"),
    when(ethnicity6 == "3").then("South Asian"),
    when(ethnicity6 == "4").then("Black"),
    when(ethnicity6 == "5").then("Other"),
    when(ethnicity6 == "6").then("Not stated"),
    otherwise="Unknown"
)

dataset.dod = minimum_of(ons_deaths.date, patients.date_of_death)

# Registered for at least 90 days before index date
has_registration = practice_registrations.spanning(index_date - days(90), index_date)

current_registration = practice_registrations.for_patient_on(index_date)
dataset.reg_start_date = current_registration.start_date
dataset.reg_end_date = current_registration.end_date
dataset.region = current_registration.practice_nuts1_region_name

## Vaccinations
# Note - will exclude anyone whose vaccination date is BEFORE 1 Sep 2023
dataset.shingrix_vax_date_1 = (
    vaccinations
    .where(vaccinations.product_name.is_in([
      "Shingles (Herpes Zoster) adjuvanted rcmb vacc powder and suspension for suspension inj 0.5ml vials",
      "Shingrix"
      ]))
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date
)
dataset.shingrix_vax_date_2 = (
    vaccinations
    .where(vaccinations.product_name.is_in([
        "Shingles (Herpes Zoster) adjuvanted rcmb vacc powder and suspension for suspension inj 0.5ml vials",
        "Shingrix"
        ])
        & vaccinations.date.is_after(dataset.shingrix_vax_date_1))
    .sort_by(vaccinations.date)
    .first_for_patient()
    .date
)

## Exclusion criteria - note: will apply exclusion downstream
# Dementia diagnosis before index date
dataset.dementia_exclude_gp = clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists.dementia_snomed)
    & clinical_events.date.is_before(index_date)
)

# Unresolved shingles before index date
dataset.shingles_exclude_gp = clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists.shingles_snomed)
    & clinical_events.date.is_on_or_between(index_date - days(), index_date)
)

## Outcomes
# Dementia diagnosis
dataset.dementia_outcome_date = clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists.dementia_snomed)
    & clinical_events.date.is_after(index_date)
)

# Shingles 
dataset.shingles_outcome_date = clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists.shingles_snomed)
    & clinical_events.date.is_after(index_date)
)

# post-herpetic neuralgia
dataset.neuralgia_outcome_date = clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists.neuralgia_snomed)
    & clinical_events.date.is_after(index_date)
)

dataset.define_population(
    has_registration 
    & dataset.date_of_birth.is_on_or_between("1931-09-02","1936-09-01")
    & ((dataset.sex == "male") | (dataset.sex == "female"))
)
from ehrql import create_dataset, days, case, when, minimum_of
from ehrql.tables.tpp import (
    patients, 
    practice_registrations, 
    clinical_events, 
    addresses, 
    ons_deaths, 
    apcs,
    ons_deaths
)

import codelists

dataset = create_dataset()

def demographics(index_date):
    # Demographics
    dataset.dob = patients.date_of_birth
    dataset.age = patients.age_on(index_date)
    dataset.sex = patients.sex
    dataset.imd_decile = addresses.for_patient_on(index_date).imd_decile

    ethnicity6 = (
        clinical_events
        .where(clinical_events.snomedct_code.is_in(codelists.ethnicity_codes_6))
        .where(clinical_events.date.is_on_or_before(index_date))
        .sort_by(clinical_events.date)
        .last_for_patient()
        .snomedct_code.to_category(codelists.ethnicity_codes_6)
    )

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

    current_registration = practice_registrations.for_patient_on(index_date)
    dataset.reg_start_date = current_registration.start_date
    dataset.reg_end_date = current_registration.end_date
    dataset.region = current_registration.practice_nuts1_region_name

    return dataset

def primary_care_events(index_date):

    ## Exclusion criteria - note: will apply exclusion downstream
    # Dementia diagnosis before index date
    dataset.dementia_exclude_gp = (
        clinical_events
        .where(clinical_events.snomedct_code.is_in(codelists.dementia_snomed) 
               & clinical_events.date.is_before(index_date))
        .exists_for_patient()
    )

    # Unresolved shingles before index date
    dataset.shingles_exclude_gp = (
        clinical_events
        .where(clinical_events.snomedct_code.is_in(codelists.shingles_snomed)
               & clinical_events.date.is_on_or_between(index_date - days(90), index_date))
        .exists_for_patient()
    )

    # Dementia diagnosis
    dataset.dementia_outcome_gp_date = (
        clinical_events
        .where(clinical_events.snomedct_code.is_in(codelists.dementia_snomed)
               & clinical_events.date.is_after(index_date))
        .sort_by(clinical_events.date)
        .first_for_patient()
        .date
    )

    # Shingles 
    dataset.shingles_outcome_gp_date = (
        clinical_events
        .where(clinical_events.snomedct_code.is_in(codelists.shingles_snomed)
            & clinical_events.date.is_after(index_date))
        .sort_by(clinical_events.date)
        .first_for_patient()
        .date
    )

    # post-herpetic neuralgia
    dataset.neuralgia_outcome_gp_date = (
        clinical_events
            .where(clinical_events.snomedct_code.is_in(codelists.neuralgia_snomed) 
                & clinical_events.date.is_after(index_date))
        .sort_by(clinical_events.date)
        .first_for_patient()
        .date
    )

    return dataset

def secondary_care_events(index_date):
    
    ## Exclusion criteria
    # Dementia diagnosis before index date
    dataset.dementia_exclude_hosp = apcs.where(
        apcs.all_diagnoses.contains_any_of(codelists.dementia_icd10)
        & apcs.all_diagnoses.admission_date.is_before(index_date)
    ).exists_for_patient()

    # Unresolved shingles before index date
    dataset.shingles_exclude_hosp = apcs.where(
        apcs.all_diagnoses.contains_any_of(codelists.shingles_icd10)
        & apcs.all_diagnoses.admission_date.is_on_or_between(index_date - days(90), index_date)
    ).exists_for_patient()

    return dataset

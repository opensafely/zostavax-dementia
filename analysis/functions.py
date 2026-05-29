from ehrql import create_dataset, days, case, when, minimum_of
from ehrql.tables.tpp import (
    patients, 
    practice_registrations, 
    clinical_events, 
    addresses, 
    ons_deaths, 
    apcs,
    ons_deaths,
    medications,
    vaccinations
)

import codelists

dataset = create_dataset()

def demographics(index_date):

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

def first_gp_date_after(gp_events_after, codelist):
    
    first_gp_event_date = (
        gp_events_after
        .where(gp_events_after.snomedct_code.is_in(codelist))
        .sort_by(gp_events_after.date)
        .first_for_patient().date
    )

    return first_gp_event_date

def last_gp_date_before(gp_events_before, codelist):
    
    last_gp_event_date = (
        gp_events_before
        .where(gp_events_before.snomedct_code.is_in(codelist))
        .sort_by(gp_events_before.date)
        .last_for_patient().date
    )

    return last_gp_event_date
    
def last_rx_date_before(rx_events_before, codelist):
        
    last_rx_event_date = (
        rx_events_before
        .where(rx_events_before.dmd_code.is_in(codelist))
        .sort_by(rx_events_before.date)
        .last_for_patient().date
    )

    return last_rx_event_date
    
def first_hosp_date_after(hosp_events_after, codelist):
    
    first_hosp_event_date = (
        hosp_events_after
        .where(hosp_events_after.primary_diagnosis.is_in(codelist))
        .sort_by(hosp_events_after.admission_date)
        .first_for_patient()
        .admission_date
    )

    return first_hosp_event_date

def last_hosp_date_before(hosp_events_before, codelist):
    
    last_hosp_event_date = (
        hosp_events_before
        .where(hosp_events_before.primary_diagnosis.is_in(codelist))
        .sort_by(hosp_events_before.admission_date)
        .last_for_patient()
        .admission_date
    )

    return last_hosp_event_date
    
def primary_care_outcomes(index_date):

    gp_events_before = clinical_events.where(clinical_events.date.is_before(index_date))
    gp_events_after = clinical_events.where(clinical_events.date.is_after(index_date))

    ## Exclusion criteria
    dataset.dementia_exclude_gp_date = last_gp_date_before(gp_events_before, codelists.dementia_snomed)
    dataset.shingles_exclude_gp_date = last_gp_date_before(gp_events_before, codelists.shingles_snomed)
  
    ## Outcomes
    dataset.dementia_outcome_gp_date = first_gp_date_after(gp_events_after, codelists.dementia_snomed)
    dataset.shingles_outcome_gp_date = first_gp_date_after(gp_events_after, codelists.shingles_snomed)
    dataset.neuralgia_outcome_gp_date = first_gp_date_after(gp_events_after, codelists.neuralgia_snomed)

    ## Negative controls

    return dataset

def primary_care_controls(index_date):

    gp_events_before = clinical_events.where(clinical_events.date.is_before(index_date))
    rx_events_before = medications.where(medications.date.is_before(index_date))

    dataset.ihd_gp_date = last_gp_date_before(gp_events_before, codelists.ihd_snomed)
    dataset.stroke_gp_date = last_gp_date_before(gp_events_before, codelists.stroke_snomed)
    dataset.hypertension_gp_date = last_gp_date_before(gp_events_before, codelists.hypertension_snomed)
    dataset.t2dm_gp_date = last_gp_date_before(gp_events_before, codelists.t2dm_snomed)
    dataset.copd_gp_date = last_gp_date_before(gp_events_before, codelists.copd_snomed)

    dataset.antihypertensives_gp_date = last_rx_date_before(rx_events_before, codelists.antihypertensives_dmd)
    dataset.statins_gp_date = last_rx_date_before(rx_events_before, codelists.statins_dmd)

    dataset.flu_vax_date = (
        vaccinations
            .where(vaccinations.target_disease.is_in(["INFLUENZA"]))
            .sort_by(vaccinations.date)
            .last_for_patient()
            .date
        )
    dataset.pneumo_vax_date = (
        vaccinations
            .where(vaccinations.target_disease.is_in(["PNEUMOCOCCAL"]))
            .sort_by(vaccinations.date)
            .last_for_patient()
            .date
        )

    return dataset

def secondary_care_outcomes(index_date):

    hosp_events_before = apcs.where(apcs.admission_date.is_before(index_date))
    hosp_events_after = apcs.where(apcs.admission_date.is_after(index_date))

    ## Exclusion criteria
    dataset.dementia_exclude_hosp_date = last_hosp_date_before(hosp_events_before, codelists.dementia_icd10)
    dataset.shingles_exclude_hosp_date = last_hosp_date_before(hosp_events_before, codelists.shingles_icd10)
  
    ## Outcomes
    dataset.dementia_outcome_hosp_date = first_hosp_date_after(hosp_events_after, codelists.dementia_icd10)
    dataset.shingles_outcome_hosp_date = first_hosp_date_after(hosp_events_after, codelists.shingles_icd10)
    dataset.neuralgia_outcome_hosp_date = first_hosp_date_after(hosp_events_after, codelists.neuralgia_icd10)
    dataset.dementia_outcome_ons_date = (
        ons_deaths
        .where(ons_deaths.cause_of_death_is_in(codelists.dementia_icd10)
            & ons_deaths.date.is_after(index_date))
    )

    return dataset


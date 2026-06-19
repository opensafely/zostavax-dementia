from ehrql import create_dataset, case, when, minimum_of, weeks, months, years
from ehrql.tables.tpp import (
    patients, 
    practice_registrations, 
    clinical_events, 
    addresses, 
    ons_deaths, 
)
from analysis.helper_functions import *

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
    
def primary_care_exclusions(index_date):

    dataset.dementia_gp_pre_any = last_gp_event_before(index_date, codelists.dementia_snomed).exists_for_patient()
    dataset.neuralgia_gp_pre_any = last_gp_event_before(index_date, codelists.neuralgia_snomed).exists_for_patient()
    dataset.shingles_gp_pre_4wks = last_gp_event_between(index_date - weeks(4), index_date, codelists.shingles_snomed).exists_for_patient()

    zostavax_products = ["Zostavax","Shingles (Herpes Zoster) vaccine (live) powder and solvent for suspension for injection 0.65ml pfs"]
    dataset.zostavax_gp_pre_any = last_vax_event_before(index_date, product_name=zostavax_products).exists_for_patient()
    
    shingrix_products = ["Shingrix","Shingles (Herpes Zoster) adjuvanted rcmb vacc powder and suspension for suspension inj 0.5ml vials"]
    dataset.shingrix_gp_pre_any = last_vax_event_before(index_date, product_name=shingrix_products).exists_for_patient()

    immunosuppression_anytime = last_gp_event_before(index_date, codelists.immunosuppression_snomed_anytime).exists_for_patient()
    immunosuppression_2yrs = last_gp_event_between(index_date - years(2), index_date, codelists.immunosuppression_snomed_2yrs).exists_for_patient()
    immunosuppression_6mos = last_gp_event_between(index_date - months(6), index_date, codelists.immunosuppression_snomed_6mos).exists_for_patient()
    immunosuppression_rx = last_rx_event_between(index_date - months(6), index_date, codelists.imtrtatrisk_snomed).exists_for_patient()

    dataset.immunosuppression_gp_pre_any = (immunosuppression_anytime | immunosuppression_2yrs | immunosuppression_6mos | immunosuppression_rx)

    return dataset

def primary_care_main_outcomes(index_date):
    
    dataset.dementia_gp_first_date = first_gp_event_ever(codelists.dementia_snomed).date
    dataset.neuralgia_gp_first_date = first_gp_event_ever(codelists.neuralgia_snomed).date
    dataset.shingles_gp_first_date = first_gp_event_after(index_date, codelists.shingles_snomed).date

    return dataset

def primary_care_control_outcomes(index_date):
    
    conditions = {
        "ihd": codelists.ihd_snomed,
        "stroke": codelists.stroke_snomed,
        "hypertension": codelists.hypertension_snomed,
        "t2dm": codelists.t2dm_snomed,
        "copd": codelists.copd_snomed,
        "lrti": codelists.lrti_snomed,
        "asthma": codelists.asthma_snomed,
        "afib": codelists.afib_snomed,
        "ckd": codelists.ckd_snomed,
        "epilepsy": codelists.epilepsy_snomed,
        "hf": codelists.hf_snomed,
        "hypothyroid": codelists.hypothyroid_snomed,
        "smi": codelists.smi_snomed,
        "osteoporosis": codelists.osteoporosis_snomed,
        "obese": codelists.obese_snomed,
        "pad": codelists.pad_snomed,
        "ra": codelists.ra_snomed,
        "chd": codelists.chd_snomed,
        "depression": codelists.depression_snomed,
    }

    for name, codelist in conditions.items():
        setattr(
            dataset,
            f"{name}_gp_first_date",
            first_gp_event_ever(codelist).date(),
        )

    return dataset

def primary_care_assumptions(index_date):

    conditions = {
        "ihd": codelists.ihd_snomed,
        "stroke": codelists.stroke_snomed,
        "hypertension": codelists.hypertension_snomed,
        "t2dm": codelists.t2dm_snomed,
        "copd": codelists.copd_snomed,
        "lrti": codelists.lrti_snomed,
        "asthma": codelists.asthma_snomed,
        "afib": codelists.afib_snomed,
        "ckd": codelists.ckd_snomed,
        "epilepsy": codelists.epilepsy_snomed,
        "hf": codelists.hf_snomed,
        "hypothyroid": codelists.hypothyroid_snomed,
        "smi": codelists.smi_snomed,
        "osteoporosis": codelists.osteoporosis_snomed,
        "obese": codelists.obese_snomed,
        "pad": codelists.pad_snomed,
        "ra": codelists.ra_snomed,
        "chd": codelists.chd_snomed,
        "depression": codelists.depression_snomed,
    }

    for name, codelist in conditions.items():
        setattr(
            dataset,
            f"{name}_gp_pre_any",
            last_gp_event_before(index_date, codelist).exists_for_patient(),
        )

    dataset.antihypertensives_gp_pre_5yrs = last_rx_event_between(index_date - years(5), index_date, codelists.antihypertensives_dmd).exists_for_patient()
    dataset.statins_gp_pre_5yrs = last_rx_event_between(index_date - years(5), index_date, codelists.statins_dmd).exists_for_patient()

    dataset.flu_vax_pre_any = last_vax_event_before(index_date, target_disease=["INFLUENZA"]).exists_for_patient() 
    dataset.pneumo_vax_pre_any = last_vax_event_before(index_date, target_disease=["PNEUMOCOCCAL"]).exists_for_patient()

    dataset.current_smoker_pre_5yrs =  last_gp_event_between(index_date - years(5), index_date, codelists.current_smoker_snomed).exists_for_patient()
    dataset.past_smoker_pre_any = last_gp_event_before(index_date, codelists.past_smoker_snomed).exists_for_patient()

    return dataset

def secondary_care_exclusions(index_date):
    
    dataset.dementia_hosp_pre_any = last_hosp_event_before(index_date, codelists.dementia_icd10).exists_for_patient()
    dataset.shingles_hosp_pre_4wks = last_hosp_event_between(index_date - weeks(4), index_date, codelists.shingles_icd10).exists_for_patient()
    dataset.neuralgia_hosp_pre_any = last_hosp_event_before(index_date, codelists.neuralgia_icd10).exists_for_patient()

    return dataset

def secondary_care_main_outcomes(index_date):

    dataset.dementia_hosp_first_date = first_hosp_event_ever(codelists.dementia_icd10).admission_date
    dataset.shingles_hosp_first_date = first_hosp_event_ever(codelists.shingles_icd10).admission_date
    dataset.neuralgia_hosp_first_date = first_hosp_event_ever(codelists.neuralgia_icd10).admission_date
    dataset.dementia_ons_date = (
        ons_deaths
        .where(ons_deaths.cause_of_death_is_in(codelists.dementia_icd10)
            & ons_deaths.date.is_after(index_date))
        .date
    )

    return dataset


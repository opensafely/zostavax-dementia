from ehrql import create_dataset, case, when, minimum_of, maximum_of, months, years
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

def demographics(threshold_date):

    dataset.date_of_birth = patients.date_of_birth
    dataset.age = patients.age_on(threshold_date)
    dataset.sex = patients.sex
    dataset.imd_quintile = addresses.for_patient_on(threshold_date).imd_quintile

    ethnicity6 = (
        clinical_events
        .where(clinical_events.snomedct_code.is_in(codelists.ethnicity_codes_6))
        .where(clinical_events.date.is_on_or_before(threshold_date))
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

    dataset.date_of_death = minimum_of(ons_deaths.date, patients.date_of_death)

    current_registration = practice_registrations.for_patient_on(threshold_date)
    dataset.reg_start_date = current_registration.start_date
    dataset.reg_end_date = current_registration.end_date
    dataset.region = current_registration.practice_nuts1_region_name

    return dataset

def vaccinations(threshold_date):

    zostavax_products = ["Zostavax","Shingles (Herpes Zoster) vaccine (live) powder and solvent for suspension for injection 0.65ml pfs"]
    dataset.zostavax_date_1 = first_vax_event_ever(product_name=zostavax_products).date
    
    shingrix_products = ["Shingrix","Shingles (Herpes Zoster) adjuvanted rcmb vacc powder and suspension for suspension inj 0.5ml vials"]
    dataset.shingrix_date_1  = first_vax_event_ever(product_name=shingrix_products).date
    dataset.shingrix_date_2  = first_vax_event_after(dataset.shingrix_date_1, product_name=shingrix_products).date

    varicella_products = [
        "Chicken pox",
        "MMRV",
        "Priorix Tetra",
        "ProQuad",
        "Varicella vaccine",
        "Varicella vaccine (live) powder and solvent for solution for injection 0.5ml vials",
        "VARILRIX live vaccine",
        "VARIVAX live vaccine"
    ]
    dataset.varicella_first_date_after = first_vax_event_after(threshold_date, product_name=varicella_products).date

    return dataset

def primary_care_outcomes(threshold_date):
    
    dataset.dementia_gp_first_date_ever = first_gp_event_ever(codelists.dementia_snomed).date
    dataset.alzheimers_gp_first_date_ever = first_gp_event_ever(codelists.alzheimers_snomed).date
    dataset.vascular_gp_first_date_ever = first_gp_event_ever(codelists.vascular_snomed).date
    dataset.shingles_gp_first_date_after = first_gp_event_after(threshold_date, codelists.shingles_snomed).date
    dataset.neuralgia_gp_first_date_ever = first_gp_event_ever(codelists.neuralgia_snomed).date

    return dataset

def primary_care_exclusions(threshold_date):

    dataset.dementia_exclude_gp_first_date_ever = first_gp_event_ever(codelists.dementia_exclude_snomed).date
    dataset.shingles_gp_last_date_before = last_gp_event_before(threshold_date, codelists.shingles_snomed).date

    # immunosuppression def based on NHS business rules
    immunosupp_anytime = last_gp_event_before(threshold_date, codelists.immunosupp_snomed_anytime).exists_for_patient()
    immunosupp_2yrs = last_gp_event_between(threshold_date - years(2), threshold_date, codelists.immunosupp_snomed_2yrs).exists_for_patient()
    immunosupp_6mos = last_gp_event_between(threshold_date - months(6), threshold_date, codelists.immunosupp_snomed_6mos).exists_for_patient()
    immunosupp_rx_6mos = last_rx_event_between(threshold_date - months(6), threshold_date, codelists.imtrtatrisk1_snomed).exists_for_patient()

    dataset.immunosupp_gp_any_before = (immunosupp_anytime | immunosupp_2yrs | immunosupp_6mos | immunosupp_rx_6mos)

    return dataset

def primary_care_controls_assumptions(threshold_date):

    # note - these will also be used to assess balance at baseline
    negative_controls = {        
        "asthma": codelists.asthma_snomed,
        "afib": codelists.afib_snomed,
        "chd": codelists.chd_snomed,
        "ckd": codelists.ckd_snomed,
        "copd": codelists.copd_snomed,
        "depression": codelists.depression_snomed,
        "t2dm": codelists.t2dm_snomed,
        "epilepsy": codelists.epilepsy_snomed,
        "hf": codelists.hf_snomed,
        "hypothyroid": codelists.hypothyroid_snomed,
        "osteoporosis": codelists.osteoporosis_snomed,
        "pad": codelists.pad_snomed,
        "ra": codelists.ra_snomed,
        "stroke": codelists.stroke_snomed,
        "smi": codelists.smi_snomed
    }

    for name, codelist in negative_controls.items():
        setattr(
            dataset,
            f"{name}_gp_first_date_ever",
            first_gp_event_ever(codelist).date,
        )

    # obesity - set upper limit of 60 to account for incorrect entries (same as Samuel PLOS Medicine paper)
    obese_bmi_date = (
        clinical_events
        .where(clinical_events.snomedct_code.is_in(codelists.bmi_snomed))
        .where((clinical_events.numeric_value >= 30) & (clinical_events.numeric_value <= 60))
        .sort_by(clinical_events.date)
        .first_for_patient()
        .date
    )
    obese_coded_date = first_gp_event_ever(codelists.obese_snomed).date
    dataset.obese_gp_first_date_ever = minimum_of(obese_bmi_date, obese_coded_date)

    covariate_balance = {
        "lrti": codelists.lrti_snomed,
        "smoker": codelists.current_smoker_snomed,
        "past_smoker": codelists.past_smoker_snomed,
        "cognitive_impair": codelists.cognitive_impairment_snomed,
    }

    for name, codelist in covariate_balance.items():
        setattr(
            dataset,
            f"{name}_gp_last_date_before",
            last_gp_event_before(threshold_date, codelist).date,
        )

    dataset.antihypertensives_rx_last_date_before = last_rx_event_before(threshold_date, codelists.antihypertensives_dmd).date
    dataset.statins_rx_last_date_before = last_rx_event_before(threshold_date, codelists.statins_dmd).date
    
    dataset.fluvax_last_date_before = last_vax_event_before(threshold_date, target_disease=["INFLUENZA"]).date
    dataset.pneumovax_last_date_before = last_vax_event_before(threshold_date, target_disease=["PNEUMOCOCCAL"]).date
   
    return dataset

def secondary_care_outcomes(threshold_date):

    dataset.dementia_hosp_first_date_ever = first_hosp_event_ever(codelists.dementia_icd10).admission_date
    dataset.alzheimers_hosp_first_date_ever = first_hosp_event_ever(codelists.alzheimers_icd10).admission_date
    dataset.vascular_hosp_first_date_ever = first_hosp_event_ever(codelists.vascular_icd10).admission_date
    dataset.neuralgia_hosp_first_date_ever = first_hosp_event_ever(codelists.neuralgia_icd10).admission_date

    dataset.shingles_hosp_first_date_after = first_hosp_event_after(threshold_date, codelists.shingles_icd10).admission_date

    dataset.dementia_ons_date = ons_event_ever(codelists.dementia_icd10)
    dataset.alzheimers_ons_date = ons_event_ever(codelists.alzheimers_icd10)
    dataset.vascular_ons_date = ons_event_ever(codelists.vascular_icd10)
    
    return dataset

def secondary_care_exclusions(threshold_date):

    dataset.shingles_hosp_last_date_before = last_hosp_event_before(threshold_date, codelists.shingles_icd10).admission_date

    return dataset

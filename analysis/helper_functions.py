from ehrql.tables.tpp import (clinical_events, apcs, medications, vaccinations)


def first_vax_event_after(index_date, product_name=None, target_disease=None):
    
    if product_name is not None:
        return(
            vaccinations
            .where(vaccinations.product_name.is_in(product_name))
            .where(vaccinations.date.is_on_or_after(index_date))
            .sort_by(vaccinations.date)
            .first_for_patient()
        )
    
    if target_disease is not None:
        return(
            vaccinations
            .where(vaccinations.target_disease.is_in(target_disease))
            .where(vaccinations.date.is_on_or_after(index_date))
            .sort_by(vaccinations.date)
            .first_for_patient()
        )
    
    return None
    
def last_vax_event_before(index_date, product_name=None, target_disease=None):
        
    if product_name is not None:
        return(
            vaccinations
            .where(vaccinations.product_name.is_in(product_name))
            .where(vaccinations.date.is_before(index_date))
            .sort_by(vaccinations.date)
            .last_for_patient()
        )
    
    if target_disease is not None:
        return(
            vaccinations
            .where(vaccinations.target_disease.is_in(target_disease))
            .where(vaccinations.date.is_before(index_date))
            .sort_by(vaccinations.date)
            .last_for_patient()
        )
    
    return None

def first_gp_event_ever(codelist):
    
    return(
        clinical_events
        .where(clinical_events.snomedct_code.is_in(codelist))
        .sort_by(clinical_events.date)
        .first_for_patient()
    )

def first_gp_event_after(index_date, codelist):
    
    return(
        clinical_events
        .where(clinical_events.snomedct_code.is_in(codelist))
        .where(clinical_events.date.is_after(index_date))
        .sort_by(clinical_events.date)
        .first_for_patient()
    )

def last_gp_event_before(index_date, codelist):

    return(
        clinical_events
        .where(clinical_events.snomedct_code.is_in(codelist))
        .where(clinical_events.date.is_on_or_before(index_date))
        .sort_by(clinical_events.date)
        .last_for_patient()
    )

def last_gp_event_between(date1, date2, codelist):

    return(
        clinical_events
        .where(clinical_events.snomedct_code.is_in(codelist))
        .where(clinical_events.date.is_between_but_not_on(date1, date2))
        .sort_by(clinical_events.date)
        .last_for_patient()
    )

def last_rx_event_before(index_date, codelist):
        
    return(  
        medications
        .where(medications.dmd_code.is_in(codelist))
        .where(medications.date.is_before(index_date))
        .sort_by(medications.date)
        .last_for_patient()
    )

def last_rx_event_between(date1, date2, codelist):
    return(  
        medications
        .where(medications.dmd_code.is_in(codelist))
        .where(medications.date.is_between_but_not_on(date1, date2))
        .sort_by(medications.date)
        .last_for_patient()
    )

def first_hosp_event_ever(codelist):
    
    return(
        apcs
        .where(apcs.primary_diagnosis.is_in(codelist))
        .sort_by(apcs.admission_date)
        .first_for_patient()
    )

def last_hosp_event_before(index_date, codelist):
    
    return(
        apcs
        .where(apcs.primary_diagnosis.is_in(codelist))
        .where(apcs.admission_date.is_before(index_date))
        .sort_by(apcs.admission_date)
        .last_for_patient()
    )

def last_hosp_event_between(date1, date2, codelist):
    
    return(
        apcs
        .where(apcs.primary_diagnosis.is_in(codelist))
        .where(apcs.admission_date.is_between_but_not_on(date1, date2))
        .sort_by(apcs.admission_date)
        .last_for_patient()
    )

    
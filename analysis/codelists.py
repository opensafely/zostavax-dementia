from ehrql import codelist_from_csv

# Dementia - any type - SNOMED
dementia_snomed = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-dem_cod.csv",
  column = "code"
)
alzheimers_snomed = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-demalz_cod.csv",
  column = "code"
)
vascular_snomed = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-demvasc_cod.csv",
  column = "code"
)

# Dementia - for excluding history of dementia - SNOMED
dementia_exclude_snomed = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-dem_cod.csv",
  column = "code"
)

# Dementia - any type - ICD-10
dementia_icd10 = codelist_from_csv(
  "codelists/user-anschaf-dementia-icd-10.csv",
  column = "code"
)
dementia_icd10 = codelist_from_csv(
  "codelists/user-anschaf-dementia-icd-10.csv",
  column = "code"
)
alzheimers_icd10 = codelist_from_csv(
  "codelists/bristol-alzheimers-disease-icd10-v13.csv",
  column = "code"
)
vascular_icd10 = codelist_from_csv(
  "codelists/bristol-vascular-dementia-icd10-v13.csv",
  column = "code"
)

# Ethnicity - 6 categories
ethnicity_codes_6 = codelist_from_csv(
    "codelists/opensafely-ethnicity-snomed-0removed.csv",
    column = "code",
    category_column = "Grouping_6",
)

# smoking (any)
current_smoker_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-lsmok_cod.csv",
    column = "code"
)
past_smoker_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-exsmok_cod.csv",
    column = "code"
)
smoker_snomed = current_smoker_snomed + past_smoker_snomed

# immunosuppression
autograft_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-autotransp_cod.csv",
    column = "code"
)
allograft_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-allotransp_cod.csv",
    column = "code"
)
gvhd_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-gvhd_cod.csv",
    column = "code"
)
hiv_snomed   = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-hiv_cod.csv",
    column = "code"
)
aids_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-aids_cod.csv",
    column = "code"
)
imtrtatrisk1_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-imtrtatrisk1_cod.csv",
    column = "code"
)
imtemp_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-imtemp_cod.csv",
    column = "code"
)
imatrisk1_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-imatrisk1_cod.csv",
    column = "code"
)
radiotherap_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-radiotherap_cod.csv",
    column = "code"
)
lymphoproldis_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-lymphoproldis_cod.csv",
    column = "code"
)
epphaemcan_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-epphaemcan_cod.csv",
    column = "code"
)
dmards_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-dmards_cod.csv",
    column = "code"
)
imtrtatrisk_dmd = codelist_from_csv(
    "codelists/nhs-drug-refsets-imtrtatriskdrug_cod.csv",
    column = "code"
)
immunosupp_snomed_2yrs = (autograft_snomed + allograft_snomed)
immunosupp_snomed_anytime = (gvhd_snomed + hiv_snomed + aids_snomed + imatrisk1_snomed + lymphoproldis_snomed + epphaemcan_snomed)
immunosupp_snomed_6mos = (imtrtatrisk1_snomed + imtemp_snomed + radiotherap_snomed + dmards_snomed)

lrti_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-c19flurti_cod.csv",
    column = "code"
)
asthma_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-ast_cod.csv",
    column = "code"
)
afib_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-afib_cod.csv",
    column = "code"
)
ckd12_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-ckd1and2_cod.csv",
    column = "code"
)
ckd345_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-ckd_cod.csv",
    column = "code"
)
ckd_snomed = ckd12_snomed + ckd345_snomed
depression_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-depr_cod.csv",
    column = "code"
)
epilepsy_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-epil_cod.csv",
    column = "code"
)
hf_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-hf_cod.csv",
    column = "code"
)
hypothyroid_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-thy_cod.csv",
    column = "code"
)
smi_snomed = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-sev_mental.csv",
    column = "code"
)
osteoporosis_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-osteo_cod.csv",
    column = "code"
)
obese_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-bmiobese_cod.csv",
    column = "code"
)
pad_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-pad_cod.csv",
    column = "code"
)
ra_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-rarth_cod.csv",
    column = "code"
)
stroke_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-strk_cod.csv",
    column = "code"
)
chd_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-chd_cod.csv",
    column = "code"
)
ihd_snomed = codelist_from_csv(
    "codelists/opensafely-ischaemic-heart-disease.csv",
    column = "code"
)


##########################################################

shingles_snomed = codelist_from_csv(
    "codelists/opensafely-pharmacy-first-shingles-condition.csv",
    column = "code"
)
shingles_icd10 = codelist_from_csv(
    "codelists/user-anschaf-shingles-icd-10.csv",
    column = "code"
)

neuralgia_snomed = codelist_from_csv(
    "codelists/user-anschaf-post-herpetic-neuralgia-snomed.csv",
    column = "code"
)
neuralgia_icd10 = codelist_from_csv(
    "codelists/user-anschaf-postherpetic-neuralgia-icd-10.csv",
    column = "code"
)

# assumptions
ihd_snomed = codelist_from_csv(
    "codelists/opensafely-ischaemic-heart-disease.csv",
    column = "code"
)
stroke_snomed = codelist_from_csv(
    "codelists/opensafely-cerebrovascular-disease.csv",
    column = "code"
)
hypertension_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-hyp_cod.csv",
    column = "code"
)
t2dm_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-dmtype2_cod.csv",
    column = "code"
)
copd_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-copd_cod.csv",
    column = "code"
)
antihypertensives_dmd = codelist_from_csv(
    "codelists/nhs-drug-refsets-antihyp_cod.csv",
    column = "code"
)
statins_dmd = codelist_from_csv(
    "codelists/nhs-drug-refsets-stat_cod.csv",
    column = "code"
)
bmi_snomed = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-bmival_cod.csv",
    column = "code"
)
cognitive_impairment_snomed = codelist_from_csv(
    "codelists/opensafely-symptoms-cognitive-impairment.csv",
    column = "code"
)
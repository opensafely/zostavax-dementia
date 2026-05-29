from ehrql import codelist_from_csv

# Dementia - any type - SNOMED
dementia_snomed = codelist_from_csv(
  "codelists/nhsd-primary-care-domain-refsets-dem_cod.csv",
  column = "code"
)

# Dementia - any type - ICD-10
dementia_icd10 = codelist_from_csv(
  "codelists/user-anschaf-dementia-icd-10.csv",
  column = "code"
)

# Ethnicity - 6 categories
ethnicity_codes_6 = codelist_from_csv(
    "codelists/opensafely-ethnicity-snomed-0removed.csv",
    column = "code",
    category_column = "Grouping_6",
)

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

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
    column = "snomedcode",
    category_column = "Grouping_6",
)

shingles_snomed = codelist_from_csv(
    "/workspaces/shingles-vax/codelists/opensafely-pharmacy-first-shingles-condition.csv",
    column = "code"
)

from ehrql import create_dataset, days
from ehrql.tables.tpp import patients, practice_registrations

dataset = create_dataset()

index_date = "2013-09-01"

dataset.dob = patients.date_of_birth

dataset.registration = practice_registrations.spanning(index_date - days(90), index_date)



has_registration = practice_registrations.for_patient_on(
    index_date
).exists_for_patient()

dataset.define_population(has_registration)

dataset.sex = patients.sex

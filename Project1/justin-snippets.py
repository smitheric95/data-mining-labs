import numpy as np
import pandas as pd

claims = pd.read_csv('./data/hospital/Claims_Y1.csv')
members = pd.read_csv('./data/hospital/Members_Y1.csv')
dih = pd.read_csv('./data/hospital/DayInHospital_Y2.csv')
df = claims.merge(members, on='MemberID')
df = df.merge(dih, left_on='MemberID', right_on='memberid')
# create ohes
df = pd.concat([df, pd.get_dummies(df['specialty'], prefix='specialty')], axis=1)
df = pd.concat([df, pd.get_dummies(df['placesvc'], prefix='placesvc')], axis=1)
df = pd.concat([df, pd.get_dummies(df['PrimaryConditionGroup'], prefix='pcg')], axis=1)
df = pd.concat([df, pd.get_dummies(df['sex'], prefix='sex')], axis=1)
# convert ordinals to ints
df['CharlsonIndex'] = df['CharlsonIndex'].replace({'0': 0, '1-2': 1, '3-4': 2, '5+': 3})
df['AgeAtFirstClaim'] = df['AgeAtFirstClaim'].replace({
    '0-9': 0, '10-19': 1, '20-29': 2, '30-39': 3,
    '40-49': 4, '50-59': 5, '60-69': 6, '70-79': 7, '80+': 8})

specialty_cols = [
       'specialty_Anesthesiology',
       'specialty_Diagnostic Imaging', 'specialty_Emergency',
       'specialty_General Practice', 'specialty_Internal',
       'specialty_Laboratory', 'specialty_Obstetrics and Gynecology',
       'specialty_Other', 'specialty_Pathology', 'specialty_Pediatrics',
       'specialty_Rehabilitation', 'specialty_Surgery'
]
placesvc_cols = [
       'placesvc_Ambulance',
       'placesvc_Home', 'placesvc_Independent Lab',
       'placesvc_Inpatient Hospital', 'placesvc_Office', 'placesvc_Other',
       'placesvc_Outpatient Hospital', 'placesvc_Urgent Care'
]
pcg_cols = [
       'pcg_AMI',
       'pcg_APPCHOL', 'pcg_ARTHSPIN', 'pcg_CANCRA', 'pcg_CANCRB', 'pcg_CANCRM',
       'pcg_CATAST', 'pcg_CHF', 'pcg_COPD', 'pcg_FLaELEC', 'pcg_FXDISLC',
       'pcg_GIBLEED', 'pcg_GIOBSENT', 'pcg_GYNEC1', 'pcg_GYNECA', 'pcg_HEART2',
       'pcg_HEART4', 'pcg_HEMTOL', 'pcg_HIPFX', 'pcg_INFEC4', 'pcg_LIVERDZ',
       'pcg_METAB1', 'pcg_METAB3', 'pcg_MISCHRT', 'pcg_MISCL1', 'pcg_MISCL5',
       'pcg_MSC2a3', 'pcg_NEUMENT', 'pcg_ODaBNCA', 'pcg_PERINTL',
       'pcg_PERVALV', 'pcg_PNCRDZ', 'pcg_PNEUM', 'pcg_PRGNCY', 'pcg_RENAL1',
       'pcg_RENAL2', 'pcg_RENAL3', 'pcg_RESPR4', 'pcg_ROAMI', 'pcg_SEIZURE',
       'pcg_SEPSIS', 'pcg_SKNAUT', 'pcg_STROKE', 'pcg_TRAUMA', 'pcg_UTI'
]

member_ids = df['MemberID'].unique()
#member_ids = [25872, 27373]

rows = []
for member_id in member_ids:
    member_rows = df[df['MemberID'] == member_id]
    row = {'MemberID': member_id}

    for specialty in specialty_cols:
        row[specialty] = member_rows[specialty].sum()

    for placesvc in placesvc_cols:
        row[placesvc] = member_rows[placesvc].sum()

    for pcg in pcg_cols:
        row[pcg] = member_rows[pcg].sum()

    row['pay_delay_mean'] = member_rows['paydelay'].mean()
    row['pay_delay_max'] = member_rows['paydelay'].max()
    row['pay_delay_min'] = member_rows['paydelay'].min()
    row['charlson_index_mode'] = member_rows['CharlsonIndex'].mode()[0]
    row['charlson_index_max'] = member_rows['CharlsonIndex'].max()
    row['charlson_index_min'] = member_rows['CharlsonIndex'].min()
    row['sex_F'] = member_rows['sex_F'].mode()[0]
    row['sex_M'] = member_rows['sex_M'].mode()[0]
    row['age'] = member_rows['AgeAtFirstClaim'].mode()[0]

    rows.append(row)

flat_df = pd.DataFrame(rows)
flat_df.to_csv('./data/flattened_members.csv', index=False)

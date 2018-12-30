# Jake Son
# Child Mind Institute

import os
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

os.chdir('/Users/jakeson/Documents/CMI/bks_ace_analysis')

########################################################################################################################
# Testing on participant 1 session 1 for right-handed condition
# Comparing incorrect and correct response times

df = pd.read_csv('ACE_EXPORT_BRT_1.csv')

df['participant_id'] = df['participant_id'].str.replace('ADMIN-UCSF-BK', '')

# Probability density function
p1_r = df[(df.participant_id == '002') & (df.condition == 'Right')]

ax = sns.distplot(p1_r[p1_r.correct_button == 1].response_time.dropna(), rug=True, color='blue', label='Correct')
ax = sns.distplot(p1_r[p1_r.correct_button == 0].response_time.dropna(), rug=True, color='red', label='Incorrect')

plt.legend()

# Cumulative density function
ax = sns.distplot(p1_r[p1_r.correct_button == 1].response_time.dropna(),
             hist_kws=dict(cumulative=True),
             kde_kws=dict(cumulative=True),
             rug=True, color='blue', label='Correct')
ax = sns.distplot(p1_r[p1_r.correct_button == 0].response_time.dropna(),
             hist_kws=dict(cumulative=True),
             kde_kws=dict(cumulative=True),
             rug=True, color='red', label='Incorrect')

########################################################################################################################
# Testing on participant 1 session 1 & 2 for right-handed condition
# Comparing response times for each condition between session 1 & 2

df_1 = pd.read_csv('ACE_EXPORT_BRT_1.csv')
df_2 = pd.read_csv('ACE_EXPORT_BRT_2.csv')

df_1['participant_id'] = df_1['participant_id'].str.replace('ADMIN-UCSF-BK', '')
df_2['participant_id'] = df_2['participant_id'].str.replace('ADMIN-UCSF-BK', '')

# Probability density function
p1_r_1 = df_1[(df.participant_id == '002') & (df_1.condition == 'Right')]
p1_r_2 = df_2[(df.participant_id == '002') & (df_2.condition == 'Right')]

ax = sns.distplot(p1_r_1[p1_r_1.correct_button == 1][p1_r_1.condition=='Right'].response_time.dropna(),
                  rug=True, color='blue', hist=False, label='Session 1')
ax = sns.distplot(p1_r_2[p1_r_2.correct_button == 1][p1_r_1.condition=='Right'].response_time.dropna(),
                  rug=True, color='red', hist=False, label='Session 2')

plt.legend()

# Cumulative density function
ax = sns.distplot(p1_r_1[p1_r_1.correct_button == 1].response_time.dropna(),
             hist_kws=dict(cumulative=True),
             kde_kws=dict(cumulative=True),
             rug=True, color='blue', label='Session 1')
ax = sns.distplot(p1_r_2[p1_r_2.correct_button == 1].response_time.dropna(),
             hist_kws=dict(cumulative=True),
             kde_kws=dict(cumulative=True),
             rug=True, color='red', label='Session 2')

plt.legend()
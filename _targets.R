# RQ1.1) How strong is the total causal effect of midlife-PA
#     on cognitive variables in the sample?
# RQ1.2) How strong is the total causal effect of midlife-PA
#     on cognitive variables conditional on Education in the sample?
# RQ1.3) How strong is the total causal effect of Education
#     on cognitive variables in the sample?
# RQ1.4) How strong is the total causal effect of current-PA
#     on cognitive variables in the sample?
#
# RQ2.1) How strong is the total causal effect of midlife-PA
#     on mental health variables in the sample?
# RQ2.2) How strong is the total causal effect of midlife-PA
#     on mental health variables conditional on Education in the sample?
# RQ2.4) How strong is the total causal effect of current-PA
#     on mental health variables in the sample?
#
# RQ3.1) How strong is the total causal effect of midlife-PA
#     on current-PA in the sample?

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set()

# Load all in-house functions:
tar_source()

# List the targets:
list(
  targets_assumptions,
  targets_norms,
  targets_data,
  targets_regressions,
  targets_results
)

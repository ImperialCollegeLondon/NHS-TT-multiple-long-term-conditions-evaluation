# NHS-TT-multiple-long-term-conditions-evaluation


This repository contains the analytical code used to evaluate outcomes for
patients with multiple long-term conditions (MLTC) receiving NHS Talking Therapies 
(IAPT) interventions. NHS Talking Therapies services provide psychological 
interventions for adults in England who are struggling with anxiety 
disorders and/or depression. The services offer a range of NICE recommended 
talking therapies that are tailored to each individual’s needs. These therapies
can be delivered face-to-face, remotely, individually or in groups 
and are free for patients to access.


The analysis pipeline extracts data from secure storage, prepares intervention
and control cohorts, performs propensity score matching, and estimates primary,
secondary, and sensitivity outcomes.



## Repository Contents

- `scripts/main.R`  
  Orchestrates the full analysis pipeline from data access to output generation.

- `R/`  
  Modular functions for data cleaning, matching, statistical analysis, and
  output handling. All functions are documented using roxygen2.


## Data Availability

⚠️ **No patient-level data are included in this repository.**

Source data are stored in secure Azure Data Lake Storage environments and are
accessed only by users with appropriate governance approvals.

This repository contains **code only**.


## Security and Credentials

- No credentials, secrets, or tokens are stored in this repository.
- Access to Azure services is managed via environment variables.
- Users must supply their own credentials using approved organisational
  mechanisms.

Work was completed as part of NHS England's Multiple Long-Term conditions Workstream .
Contact: adrian.pratt@nhs.net , anju.paul7@nhs.net , dhope@ic.ac.uk
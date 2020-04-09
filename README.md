README
================
CoronaNet Project Team
April 9th, 2020

## About This Repository

This repository contains data from the [CoronaNet data collection
project](http://coronanet-project.org) and also data and code to fit the
model described in “A Retrospective Bayesian Model for Measuring
Covariate Effects on Observed COVID-19 Test and Case Counts”, [link
here](https://osf.io/preprints/socarxiv/jp4wk). Following is first a
list of data for the CoronaNet project, with data dictionary, and
subsequently a list of files relevant to “A Retrospective Bayesian Model
for Measuring Covariate Effects on Observed COVID-19 Test and Case
Counts”.

# CoronaNet Data

First, CoronaNet data releases:

**Please note that while we make every effort to validate this data, the
speed and scale with which it was collected means that we cannot
validate all of it. If you find an error in the data, please file an
issue on this Github page.**

1.  **`data/CoronaNet/coronanet_release.csv`** This file contains
    variables from the CoronaNet government response project,
    representing national and sub-national policy event data from more
    than 140 countries since January 1st, 2020. The data include source
    links, descriptions, targets (i.e. other countries), the type and
    level of enforcement, and a comprehensive set of policy types. For
    more detail on this data, you can see our [codebook
    here](https://docs.google.com/document/d/1zvNMpwj0onFvUZ_gLl4RRjqS-clbHv3TIX6EOHofsME).

2.  **`data/CoronaNet/coronanet_release.csv`** This file contains the
    government response information from `coronanet_release.csv` along
    with the following datasets:
    
    1.  Tests from the CoronaNet testing database (see
        <http://coronanet-project.org> for more info);
    2.  Cases/deaths/recovered from the JHU data repository
        (<https://github.com/CSSEGISandData/COVID-19>);
    3.  Country-level covariates including GDP, V-DEM democracy scores,
        human rights indices, power-sharing indices, and press freedom
        indices from the Niehaus World Economics and Politics Dataverse
        (<https://niehaus.princeton.edu/news/world-economics-and-politics-dataverse>)

## `coronanet_release.csv` Field Dictionary

1.  `record_id` Unique identifier for each policy record
2.  `entry_type` Whether the record is new, meaning no restriction had
    been in place before, or an update (restriction was in place but
    changed). Corrections are corrections to previous entries.
3.  `event_description` A short description of the policy change
4.  `type` The category of the policy
5.  `country` The country initiating the policy
6.  `init_country_level` Whether the policy came from the national level
    or a sub-national unit
7.  `index_prov` The ID of the sub-national unit
8.  `target_country` Which foreign country a policy is targeted at
    (i.e. travel policies)
9.  `target_geog_level` Whether the target of the policy is a country as
    a whole or a sub-national unit of that country
10. `target_who_what` Who the policy is targeted at
11. `recorded_date` When the record was entered into our data
12. `target_direction` Whether a travel-related policy affects people
    coming in (Inbound) or leaving (Outbound)
13. `travel_mechanism` If a travel policy, what kind of transportation
    it affects
14. `compliance` Whether the policy is voluntary or mandatory
15. `enforcer` What unit in the country is responsible for enforcement
16. `date_announced` When the policy goes into effect
17. `link` A link to at least one source for the policy
18. `ISO_A3` 3-digit ISO country codes
19. `ISO_A2` 2-digit ISO country codes
20. `severity_index_5perc` 5% posterior low estimate (i.e. lower bound
    of uncertainty interval) for severity index
21. `severity_index_median` posterior median estimate (point estimate)
    for severity index, which comes from a Bayesian latent variable
    model aggregating across policy types to measure country-level
    policy severity (see paper on our website)
22. `severity_index_5perc` 95% posterior high estimate (i.e. upper bound
    of uncertainty interval) for severity index

## `coronanet_release_allvars.csv` Field Dictionary

1.  All of the fields listed above, plus

2.  `tests_daily_or_total` Whether a country reports the daily count of
    tests a cumulative total

3.  `tests_raw` The number of reported tests collected from host country
    websites or media reports

4.  `deaths` The number of COVID-19 deaths, aggregated to the
    country-day level (JHU CSSE data)

5.  `confirmed_cases` The number of confirmed cases of COVID-19,
    aggregated to the country-day level (JHU CSSE data)

6.  `recovered` The number of recoveries from COVID-19, aggregated to
    the country-day level (JHU CSSE data)

7.  `ccode` The Correlates of War country code

8.  `ifs` IMF IFS country code

9.  `Rank_FP` (most recent year available from Niehaus dataset)
    Reporters without Borders Press Freedom Annual Ranking

10. `Score_FP` (most recent year available from Niehaus dataset)
    Reporters with Borders Press Freedom Score

11. `state_IDC` (most recent year available from Niehaus dataset)
    State/Provincial Governments Locally Elected

12. `muni_IDC` (most recent year available from Niehaus dataset)
    Municipal Governments Locally Elected

13. `dispersive_IDC` (most recent year available from Niehaus dataset)
    Dispersive Powersharing

14. `constraining_IDC` (most recent year available from Niehaus dataset)
    Constraining Powersharing

15. `inclusive_IDC` (most recent year available from Niehaus dataset)
    Inclusive powersharing

16. `sfi_SFI` (most recent year available from Niehaus dataset) State
    fragility index

17. `ti_cpi_TI` (most recent year available from Niehaus dataset)
    Corruption perceptions index

18. `pop_WDI_PW` (most recent year available from Niehaus dataset) World
    Bank population

19. `gdp_WDI_PW` (most recent year available from Niehaus dataset) World
    Bank GDP (total)

20. `gdppc_WDI_PW` (most recent year available from Niehaus dataset)
    World Bank GDP per capita

21. `growth_WDI_PW` (most recent year available from Niehaus dataset)
    World Bank GDP growth percent

22. `lnpop_WDI_PW` (most recent year available from Niehaus dataset) Log
    of World Bank population

23. `lngdp_WDI_PW` (most recent year available from Niehaus dataset) Log
    of World Bank GDP

24. `lngdppc_WDI_PW` (most recent year available from Niehaus dataset)
    Log of World Bank GDP per capita

25. `disap_FA` (most recent year available from Niehaus dataset) 3
    category, ordered variable for disappearances index

26. `polpris_FA` (most recent year available from Niehaus dataset) 3
    category, ordered variable for political imprisonment index

27. `latentmean_FA` (most recent year available from Niehaus dataset)
    the posterior mean of the latent variable index for human rights
    protection)

28. `transparencyindex_HR` (most recent year available from Niehaus
    dataset) Transparency Index

29. `EmigrantStock_EMS` (most recent year available from Niehaus
    dataset) Total emmigrant stock from

30. `v2x_polyarchy_VDEM` (most recent year available from Niehaus
    dataset) Electoral democracy index

31. `news_WB` (most recent year available from Niehaus dataset) Daily
    newspapers (per 1,000 people)

# A Retrospective Bayesian Model for Measuring Covariate Effects Data and Code

Files to reproduce the paper:

1.  **corona\_tscs\_betab.stan**: This Stan model contains a
    partially-identified model of COVID-19 that permits relative
    distinctions to be made between areas/countries/states’ infection
    rates. The parameter `num_infected_high` indexes the infection rate
    by time point and country. As the latent process is on the logit
    scale, it must be converted via the inverse-logit function to a
    proportion. However, the resulting estimate should not be
    interpreted as the total infected in a country, but rather a
    relative ranking of which countries/areas are the most infected up
    to the current time point.

2.  **corona\_tscs\_betab\_scale.stan**: This Stan model extends the
    partially-identified model with the 10% lower threshold for tests to
    infections ratio described in the paper. This model will produce an
    estimate for `num_infected_high` that when converted with the
    inverse-logit function will represent the proportion infected in a
    country conditional on the model’s prior concerning the tests to
    infections ratio.

3.  **kubinec\_model\_preprint.Rmd**: A copy of the paper draft with
    embedded R code. You can access fitted Stan model objects to compile
    the paper here:
    <https://drive.google.com/open?id=1cTCQTAjH8I-11jp3CEdIJZ0NaGRAn8dT>.
    Otherwise all Stan models must be re-fit to compile the paper. The
    process will take approximately 2 hours.

4.  **kubinec\_model\_SI.Rmd**: This file contains an Rmarkdown file
    with embedded R code showing how to simulate the model. It is the
    supplementary information for the paper. See the compiled .pdf
    version as well.

5.  **data**: The data folder contains CSVs of tests and cases for US
    states and other data that were used to fit the models in the paper.

6.  **BibTexDatabase.bib**: This file contains the Bibtex bibliography
    for the paper.

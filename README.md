README
================
CoronaNet Project Team
May 5th, 2021

## About This Repository

This repository contains the raw data from the [CoronaNet data
collection project](http://coronanet-project.org). This data is in a
policy record format, in which one row equals one specific policy with
end and beginning dates. While it is a very compact format, it is not
necessarily ideal for data analysis projects. We also have 6 indices
(social distancing, business restrictions, school restrictions, health
monitoring, health resources and masks), and a wide data format with 156
of our indicators for January 1st to January 15th, 2021. Both the
indices and the wide data format have one row per policy type per day
per country, which is much easier to merge with our data sources. For
more info, please see this [Github
repository](https://github.com/saudiwin/corona_index).

If you do want to see the data in policy record format, which includes
more information than what is released in our wide data, please see
below for more information about what is in this repository. More information about the fields are available in our [PDF Codebook](https://www.coronanet-project.org/assets/CoronaNet_Codebook.pdf?) and [Online Codebook](https://www.coronanet-project.org/taxonomy.html?). 

# CoronaNet Raw Data

When using our raw data, we recommend checking the [CoronaNet Update
Tracker](https://docs.google.com/spreadsheets/d/1h5dqVxLghvXr2wwl74ZUeSIWRQLySrxWCjD65_MevUM/edit#gid=0),
so you can track our policy updates by country and subnational unit. We
have hundreds of RAs working to keep the data up to date, but there will
inevitably be issues in the data in terms of being up to date.

First, CoronaNet data releases:

**Please note that while we make every effort to validate this data, the
speed and scale with which it was collected means that we cannot
validate all of it. If you find an error in the data, please file an
issue on this Github page.**

The format of the data is in country-day-`record_id` format. Some
`record_id` values have letters appended to indicate that the general
policy category `type` also has a value for `type_sub_cat`, which
contains more detail about the policy, such as whether health resources
refers to masks, ventilators, or hospitals. Some entries are marked as
`new_entry` in the `entry_type` field for when a policy of that type was
first implemented in the country. Later updates to those policies are
marked as updates in `entry_type`. To see how policies are connected,
look at the `policy_id` field for all policies from the first entry
through updates for a given country/province/city. If an entry was
corrected after initial data collection, it will read corrected in the
`entry_type` field (the original incorrect data has already been
replaced with the corrected data).

1.  **`data/CoronaNet/data_bulk/coronanet_release[.rds/csv.gz]`** These
    files contain variables from the CoronaNet government response
    project, representing national and sub-national policy event data
    from more than 140 countries since January 1st, 2020. The data
    include source links, descriptions, targets (i.e. other countries),
    the type and level of enforcement, and a comprehensive set of policy
    types. For more detail on this data, you can see our [codebook
    here](https://docs.google.com/document/d/1zvNMpwj0onFvUZ_gLl4RRjqS-clbHv3TIX6EOHofsME).

2.  **`data/CoronaNet/data_bulk/coronanet_release_allvars[.rds/csv.gz]`**
    These files contains the government response information from
    `coronanet_release.csv` along with the following datasets:

    1.  Tests from the CoronaNet testing database (see
        <http://coronanet-project.org> for more info);
    2.  Cases/deaths/recovered from the JHU data repository
        (<https://github.com/CSSEGISandData/COVID-19>);
    3.  Country-level covariates including GDP, V-DEM democracy scores,
        human rights indices, power-sharing indices, and press freedom
        indices from the Niehaus World Economics and Politics Dataverse
        (<https://niehaus.princeton.edu/news/world-economics-and-politics-dataverse>)

3.  **`data/CoronaNet/data_country/coronanet_release_[country].csv`**
    For each country in `coronanet_release`, we have generated a
    separate data file in a .csv format.

4.  **`data/CoronaNet/data_country/coronanet_release_allvars_[country].csv`**
    For each country in `coronanet_release_allvars`, we have generated a
    separate data file in a .csv format.

## `coronanet_release.csv` Field Dictionary

1.  `record_id` Unique identifier for each unique policy record
2.  `policy_id` Identifier linking new policies with subsequent updates
    to policies
3. `entry_type` Whether the record is new, meaning no restriction had
    been in place before, or an update (restriction was in place but
    changed). Corrections are corrections to previous entries.
4.  `update_type` Whether an update as recorded as ongoing ("Change of Policy") or ending ("End of Policy")
5.  `update_level_var`: What dimension of a policy is being updated (e.g. timing, compliance) and how (i.e., strengthening or relaxing)
6.  `description` A short textual description of the policy change
7.  `date_announced` When the policy is announced
8.  `date_start` When the policy goes into effect
9.  `date_end_spec` Qualtiative information on a policy's end date
10.  `date_end` When the policy ends (if it has an explicit end date)
11.  `country` The country where a policy was initiated
12. `ISO_A3` 3-digit ISO country codes for the country where a policy was initiated
13. `ISO_A2` 2-digit ISO country codes for the country where a policy was initiated
14. `init_country_level` The level of government that initiated a policy (e.g. national, provincial)
15. `domestic_policy` Indicates where policy targets an area within the
    initiating country (i.e. is domestic in nature)
16. `province` The province where a policy was initiated, if applicable
17. `ISO_L2` ISO province codes for the province where a policy was initiated, if applicable
18. `city` The city where a policy was initiated, if applicable    
19. `type` The category of the policy
20. `type_sub_cat` The sub-category of the policy (if one exists)
21. `type_new_admin_coop` The type of cooperation governments undertake, if applicable
22. `type_vac_cat` The particular vaccine (e.g. Pfizer) for which a policy is being made
23. `type_vac_mix` Whether mixing of different vaccines is allowed
24. `type_vac_reg` Regulatory status of a vaccine
25. `type_vac_purchase` Conditions under which a vaccine is purchased
26. `type_vac_group` Criteria for deciding how to administer vaccinations
27. `type_vac_group_rank` The number of groups given preferential treatment for vaccine access, if applicable
28. `type_vac_who_pays` The financial responisbility for paying for a vaccine
29. `type_vac_dist_admin` Entity in charge of distributing vaccines
30. `type_vac_loc` Where vaccination is taking place
31. `type_vac_cost_num` Monetary resources devoted for a given vaccine policy (number) 
32. `type_vac_cost_scale` Monetary resources devoted for a given vaccine policy (scale, e.g. millions, billions) 
33. `type_vac_cost_unit` Monetary resources devoted for a given vaccine policy (unit, e.g. currency type) 
34. `type_vac_cost_gov_perc` Monetary resources devoted for a given vaccine policy (gov perc: contribution covered by the government) 
35. `type_vac_amt_num` Material resources devoted for a given vaccine policy (number) 
36. `type_vac_amt_scale` Material resources devoted for a given vaccine policy (scale, e.g. milions, billions) 
37. `type_vac_amt_unit`  Material resources devoted for a given vaccine policy (unit, e.g. doses) 
38. `type_vac_amt_gov_perc`  Material resources devoted for a given vaccine policy (gov perc: contribution covered by the government) 
39. `type_text` Any additional information about the policy type (such
    as the number of ventilators/days of quarantine/etc.)
40. `institution_cat` Whether the business or government service is deemed as essential, non-essential or no information is provided
41. `institution_status` Whether a school, business or government service is open, open with conditions or closed
42. `institution_conditions` If a school, business or government service are open with conditions, records the conditions
43. `target_init_same` Whether the policy initiator is the same as the policy target
44. `target_country` Which foreign country a policy is targeted at
    (i.e. travel policies)
45. `target_geog_level` Whether the target of the policy is a country as
    a whole or a sub-national unit of that country
46. `target_region` The name of a regional grouping (like ASEAN) that is
    a target of the policy (if any)
47. `target_province` The name of a province targeted by the policy (if
    any)
48. `target_city` The name of a city targeted by the policy (if any)
49. `target_intl_org` The international org targeted by the policy (if any)
50. `target_other` Any geographical entity that does not fit into the
    targeted categories mentioned above
51. `target_who_what` The travel or residency status of who the policy is targeted at
52. `target_who_gen` Special populations the policy is targeted at (e.g. asylum seekers/refugees)
53. `target_direction` Whether a travel-related policy affects people
    coming in (Inbound) or leaving (Outbound)
54. `travel_mechanism` If a travel policy, what kind of transportation
    it affects
55. `compliance` Whether the policy is voluntary or mandatory
56. `enforcer` What unit in the country is responsible for enforcement
57. `dist_index_high_est` The high (95% posterior density) estimate of the
    country social distancing score (0-100)
58. `dist_index_med_est` The median (most likely) estimate of the country
    social distancing score (0-100)
59. `dist_index_low_est` The low (95% posterior density) estimate of the
    country social distancing score (0-100)
60. `dist_index_country_rank` The relative rank by each day for each country
    on the social distancing score
61. `pdf_link` Permanent pdf link for at least one source for the policy
62. `link` A link to at least one source for the policy
63. `date_updated` When we can confirm the country - policy type was
64. `recorded_date` When the record was entered into our data

    last checked/updated (we can only confirm policy type for a given country is up to date as of this date)
    <!-- 22. `severity_index_5perc` 5% posterior low estimate (i.e. lower bound of uncertainty interval) for severity index -->
    <!-- 23. `severity_index_median` posterior median estimate (point estimate) for severity index, which comes from a Bayesian latent variable model aggregating across policy types to measure country-level policy severity (see paper on our website) -->
    <!-- 24. `severity_index_5perc` 95% posterior high estimate (i.e. upper bound of uncertainty interval) for severity index -->

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

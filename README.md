### Site Level Profiles

Authors: A.Chafetz and T.Essam

This is a pilot we're using for Zambia to (1) generate facility level "profiles", (2) classify facilities into groupings (e.g. via index score, k-means cluster, etc.), and (3) limit review to "key" facilities (e.g. need, gap, relative size).

We have identified a number of metrics to use to evaluate a facility against other facilities in the same PSNU and give weighting based on the relative size (i.e. group site in quintiles for each metric) at the OU level. Below is a table of the metrics (and their relative size weighting indicator).

| area    | metric                           | calculation                     | relative size indicator |
|---------|----------------------------------|---------------------------------|-------------------------|
| HTS     | Test to Positive Ratio           | HTS_TST / HTS_TST_POS           | HTS_TST                 |
| Linkage | Proxy Linkage                    | TX_NEW / HTS_TST_POS            | HTS_TST_POS             |
| TX      | TX_NET_NEW  to TX_CURR Ratio     | TX_NET_NEW / TX_CURR            | TX_CURR                 |
| TX      | Interruptions in Treatment Share | TX_ML / (TX_CURR_Lag1 + TX_NEW) | TX_CURR                 |
| VL      | Viral Load Coverage              | TX_PVLS_D / TX_CURR_Lag2        | TX_CURR                 |
| VL      | Viral Load Suppression           | TX_PVLS / TX_PVLS_D             | TX_CURR                 |
| OVC     | OVC Reported (in last 2 periods) | OVC_SERV                        | NA                      |


MER data and organizational hierarchy data are pulled directly from DATIM (Scripts/01_data_pull.R), additional indicators and metrics are then calculated (Scripts/02_metric_calculation.R), before the profile 1-pager is created (Scripts/03_facility_plots). 

### TODO
- Map to identify relative location of facility
- Include lat/long data
- Develop (weighted) index score
- Incorporate OVC reported flag
- Develop filtering method to limit site review


## Sketches

![IMG-3211](https://user-images.githubusercontent.com/8933069/189140268-268aea08-15ac-473c-8935-dbbc584df86c.jpg)

![IMG-3212](https://user-images.githubusercontent.com/8933069/189140256-900bcc88-e05f-4442-98f6-9e53d59adeb8.jpg)
  


---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*
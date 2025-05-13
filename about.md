## Contributors
- Jenny Delasalle (Concept)
- Ursula Flitner (Concept)
- Elena Gandert (Technical implementation, Updates), since 2022
- Manuela Gregor (Data collection)
- Katja Schütze (Data collection), 2021
- Anja Siebert (Data collection)
- Jan Taubitz (Concept, Technical implementation), 2021

### Latest Update: May 13, 2025

## Sources:
- [Embase (via Ovid)](http://ovidsp.ovid.com/autologin.html)
- Statistics from the Charité Medical Library
- [Unpaywall](https://unpaywall.org)
- [Web of Science](https://www.webofscience.com/wos/woscc/basic-search)
- [OpenAPC](https://treemaps.openapc.net/)

Chosen data sources include information about corresponding authors and their affiliations. Corresponding authors are of special interest here because they are responsible for paying fees associated with gold and hybrid Open Access (OA) publishing. Such data is used in cost estimates for the Charité. However, any author can self-archive a paper (green open access) and thus the total number of papers with a Charité authorship is significant when measuring the percentage of open access reached by the institution as a whole. While the percentage of OA publishing is a key indicator for the visibility and discoverability of research findings, the quality and costs of open access must also be monitored, hence the selection of analyses presented in this dashboard.

## Dates of query:
- Bibliographic records 2016–2017: April 9, 2019
- Bibliographic records 2018–2020: April 28, 2021
- Bibliographic records 2021: April 2022
- Bibliographic records 2022: April 6, 2023
- Bibliographic records 2023: July 11, 2024
- Unpaywall lookup 2016–2017: August 24, 2021
- Unpaywall lookup 2018–2020: June 15, 2021
- Unpaywall lookup 2021: September 2022
- Unpaywall lookup 2022: June 23, 2023
- Unpaywall lookup 2023: September 25, 2024
- OpenAPC (a single query for all years): May 13, 2025

## Updates to the dashboard & data deduplication:
Data collection is time consuming and thus happens annually. Metadata describing articles from Web of Science, EMBASE and from the Charité Medical Library (including our Publication Fund) are combined and deduplicated largely by DOI-matching but also involving manual checks especially where DOIs are missing from the data. Articles without a DOI are deduplicated using the PMID, WOS Accession Number and Embase Accession Number, if applicable. Those deduplications are undertaken firstly within each raw data set (2016–2017, 2018–2020, 2021, 2022, 2023), secondly between these data sets, always preferring records from previous years over newer records.

## Choice of timeframe & introducing the analyses:

Dashboard data forms the basis of predictions for future open access costs for the Charité.

Since open access status changes and in particular green OA is often delayed, data on the OA-status for 2021 articles and beyond is captured as a snapshot at the time of the Unpaywall lookup. However, in the dashboard for the years 2017 and 2018 – 2020 we can see the snapshot across all years at the time of Unpaywall lookups in 2021.

The analysis "Publishers and journals among articles involving Charité authors, by open access status" shows the most recent publisher and journal choices of Charité authors from 2023 and looks at centrally financed OA-costs over the last six years: the blue chart is clickable to show expenditures by publisher or by year, showing the steep rise in central costs since the launch of the Publication Fund in 2018 and introduction of DEAL with Wiley in 2019 and Springer Nature in 2020. For reasons of consistency and standardization of publisher names, we use our own expenditure data after it has been enriched by OpenAPC. Actual and complete costs for the institution are likely to be much higher than reported here, since data is currently only available for centrally financed articles. Efforts are being made to complete the picture.

The last analysis looks at the use of open licenses in specific years, according to the open access status of an article.

## Open Access (OA) status or "color":

[Unpaywall](https://unpaywall.org/) is used to ascertain the open access status of articles via [DOI](https://www.doi.org/). For the 2021 articles, data was requested via the [Unpaywall REST API](https://unpaywall.org/products/api) with full metadata records. The OA-status in the dashboard differs slightly from the status as delivered by Unpaywall. Our goal is to find the most open versions of articles, while Unpaywall is focused on giving readers access to OA versions. Articles which Unpaywall identifies as “Bronze OA” but which have a copy in a repository documented in the Unpaywall metadata are given the status “Green OA” in this dashboard. Definitions for the OA-Status of articles are:

- **Gold:** Published in a journal which is 100% open access and indexed by the [DOAJ](https://doaj.org/).
- **Hybrid:** Published open access with an open license in a journal that hosts a mixture of open access and closed articles.
- **Green:** Published version remains closed or bronze, but a free to read copy is available in an OA repository.
- **Bronze:** Published version is free to read, but without an open license (more details below) and no copy is found in an OA repository.
- **Closed:** All other articles, including those in hybrid or closed journals.
- **No result:** Status not determined (often due to a missing or broken DOI).

## Bronze is not true open access:

This is included as a status for the sake of completeness but does not fit the definition of open access from the [Berlin Declaration](https://openaccess.mpg.de/Berlin-Declaration), which the Charité is a signatory of. There are two reasons why bronze is not true open access:

- without a clearly identifiable, license, re-use of the article is either not permitted or relies on individual enquiries.
- long term free access is not supported, since publishers can withdraw free access at any time and without a license, repository copies cannot be archived.

Similarly, as shown in the last analysis, most green open access publishing also happens without a clearly identifiable license but at least free access is granted for the long term.

## Open licenses:
These are determined using data from [Unpaywall](https://unpaywall.org/). Articles may be available under more than one license but a [hierarchy of licenses](https://creativecommons.org/about/cclicenses/) is used, so that only the least restrictive, and most open licenses are counted in the analysis in this dashboard. 



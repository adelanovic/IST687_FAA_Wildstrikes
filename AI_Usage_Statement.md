# Use of Artificial Intelligence

I used Claude Code and OpenAI's ChatGPT/Codex as coding and writing assistants
for this project, which analyzes over 350,000 wildlife-strike reports.

I developed the business questions and reviewed the data dictionary to select
relevant variables. I wrote the report myself; AI helped write, troubleshoot
and simplify R code, explain functions, and refine wording in text I had
already drafted. I selected the models, ran the analyses, reviewed the results
and decided which suggestions to keep. The main uses and limitations are
described below.

## Wildlife species grouping

The species field contains 974 distinct labels, including uncommon species and
unidentified wildlife. I chose to group these for analysis. AI helped propose
categories and write the assignment rules. The final cleanup uses 17 groups
and an exact-name lookup to make assignments visible and repeatable.

The first rules matched partial words and got some species wrong: `tern` matched
`Eastern`, and `fox` labeled `Fox sparrow` a mammal. AI helped switch to
whole-word matching and handle misleading names such as `Oriental turtle dove`
and `Pigeon guillemot`.

I reviewed the species-to-group CSV against taxonomy, starting with the 50 most
common labels, which cover 83.4% of records. This review identified seven
incorrect assignments:

| Species label | Assigned group | Correct group |
|---|---|---|
| Microbats | Other birds | Bats |
| Barn swallow | Other birds | Perching birds |
| Cliff swallow | Other birds | Perching birds |
| Swallows | Other birds | Perching birds |
| Tree swallow | Other birds | Perching birds |
| Bank swallow | Other birds | Perching birds |
| Perching birds (y) | Other birds | Perching birds |

I used these findings to revise the assignments with AI assistance.

Additional review addressed 177 previously unmatched labels. I did not
independently verify all 974 entries, so some grouping uncertainty remains.
Unidentified wildlife stays in separate groups rather than being assigned a
known identity.

## Filling in time of day

`TIME_OF_DAY` was missing from 153,368 of the 351,859 records (43.6%), but many
of those still had a raw `TIME` value. Rather than drop those records, I decided
to estimate time of day from that field. AI supplied approximate monthly
sunrise/sunset cutoffs and helped write the function that applies them. The code
turns `TIME`, which is formatted inconsistently, into
minutes past midnight. It then compares that against the month's sunrise and
sunset to label the record Day or Night, with a 30-minute window on either side
for Dawn and Dusk.

The sunrise and sunset values are a single set of monthly averages that AI
supplied, applied to every record regardless of airport, so the labels are
approximate for locations well north or south of the average and near the
boundaries between categories.

This filled in 38,189 records. Records with no usable time are left blank
instead of guessed, which is why 115,179 are still Unknown.

## Data cleaning and preparation

I made the data-preparation decisions for this project, including:

- Selecting only the columns required for each analysis
- Converting dates, years, months, height, and speed into appropriate types
- Creating month, season, and approximate migration-period variables
- Excluding the incomplete 2026 reporting year from the descriptive charts
- Replacing missing numeric values
- Separating earlier training records from later test records

The final models sample training records from 1990-2020 and use 2024-2025 for
testing. Records from 2021-2023 are not used in those final scripts. The airport
profiles and descriptive charts exclude the incomplete 2026 reporting year.

AI showed me `saveRDS` and `readRDS`. I use them to save the cleaned data so
later scripts can load it right away instead of cleaning it again.

## Code development and organization

AI also helped revise and simplify R code throughout the project, including:

- Explaining R functions such as `across()`, `reorder()`, `trimws()` and `case_when()`
- Organizing graphs and CSV output into dated or purpose-specific folders
- Checking scripts for syntax errors
- Suggesting a smaller training sample when the decision tree and support
  vector machine were slow to train

## Human review and responsibility

I reviewed code changes, examined tables and charts, and questioned results
that appeared inconsistent or were difficult to interpret. This led to
revisions in cleaning, grouping and model interpretation. My review was not
exhaustive; the remaining limitations are documented in the report.

The FAA data comes from voluntary strike reports, so it does not cover every
flight or every strike. The models therefore predict outcomes within the
reported strikes only. They do not predict whether a given flight will hit
wildlife.

AI supported this work, but the final decisions, the interpretation, and the
submitted project are my responsibility.

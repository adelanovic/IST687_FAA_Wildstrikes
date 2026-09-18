# Use of Artificial Intelligence

This was a 10-week course and the dataset is large: over 350,000 rows and more
than 100 variables. To finish in that time I used AI as a coding assistant,
through Anthropic's Claude Opus 5 (Claude Code) and OpenAI's ChatGPT 5.6
(Codex).

AI helped me explore the FAA Wildlife Strike Database and write R scripts,
mostly by checking my code for errors and suggesting changes. The business
questions were my own, and I read the data dictionary to choose which columns
mattered before AI was involved. I wrote the initial version of the scripts,
picked the models, ran them, read the output, and decided which suggestions to
keep.

## Wildlife species grouping

The species field is free text with 974 different entries, far too many to
predict one by one. Many appear in only a few records, and others are not a
single species at all, such as unidentified birds. I decided the species had to
be grouped, and AI helped write the rules that sort each name into a group. The
first set used six broad groups: birds of prey, gulls and shorebirds, water
birds, pigeons and doves, other birds, and non-bird wildlife. A later set in
`9_08_2026_data_cleanup.R` uses 17 narrower groups and feeds the damage models.

The first rules matched partial words and got some species wrong: `tern` matched
`Eastern`, and `fox` labeled `Fox sparrow` a mammal. AI helped switch to
whole-word matching and handle misleading names such as `Oriental turtle dove`
and `Pigeon guillemot`. I exported the full species-to-group list to CSV, read
through it, and fixed the rules wherever a species landed in the wrong group.
Unidentified entries stay in their own groups so they are never counted as a
known species.

## Filling in time of day

`TIME_OF_DAY` was missing from 153,368 of the 351,859 records (43.6%), but many
of those still had a raw `TIME` value. Rather than drop those records, I decided
to work out the time of day from that field, and AI helped build it. The code
turns `TIME`, which is formatted inconsistently, into minutes past midnight. It
then compares that against the month's sunrise and sunset to label the record
Day or Night, with a 30-minute window on either side for Dawn and Dusk.

This filled in 38,189 records. Records with no usable time are left blank
instead of guessed, which is why 115,179 are still Unknown.

## Data cleaning and preparation

I made the data-preparation decisions for this project, including:

- Selecting only the columns required for each analysis
- Converting dates, years, months, height, and speed into appropriate types
- Creating month, season, and approximate migration-period variables
- Excluding the incomplete 2026 reporting year
- Replacing missing numeric values
- Separating training, validation, and test records

I split the data by date: 1990-2020 to train, 2021-2023 to validate, and
2024-2025 to test. I did this so the models are judged on later records instead
of a random mix of old and new reports.

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

AI assistance did not replace my responsibility to understand and evaluate the
methods used in this project. I reviewed suggested code changes, examined the
resulting tables and charts, and questioned outputs that appeared inconsistent
or were difficult to interpret. This review led to revisions in data cleaning,
model interpretation, and the presentation of results. AI-generated code and
explanations were treated as material to evaluate, not as authoritative answers.

The FAA data comes from voluntary strike reports, so it does not cover every
flight or every strike. The models therefore predict outcomes within the
reported strikes only. They do not predict whether a given flight will hit
wildlife.

AI supported this work, but the final decisions, the interpretation, and the
submitted project are my responsibility.

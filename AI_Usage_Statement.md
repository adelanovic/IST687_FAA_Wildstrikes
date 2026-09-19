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
keep. Two parts of the project relied on AI more heavily than that, and I
describe both below.

## Wildlife species grouping

The species field is free text with 974 different entries, far too many to
predict one by one. Many appear in only a few records, and others are not a
single species at all, such as unidentified birds. Deciding that the species had
to be grouped was my call, but the grouping itself leaned on AI more than the
rest of the project did. I do not have a background in taxonomy, so AI proposed
the categories and wrote the rules that sort each species name into one. The
first set used six broad groups: birds of prey, gulls and shorebirds, water
birds, pigeons and doves, other birds, and non-bird wildlife. A later set in
`9_08_2026_data_cleanup.R` uses 17 narrower groups and feeds the damage models.

The first rules matched partial words and got some species wrong: `tern` matched
`Eastern`, and `fox` labeled `Fox sparrow` a mammal. AI helped switch to
whole-word matching and handle misleading names such as `Oriental turtle dove`
and `Pigeon guillemot`. I exported the species-to-group list to CSV and checked
roughly the first fifty entries by hand, fixing the rules where a species landed
in the wrong group. I did not verify all 974 entries. The less common species
therefore rest on the AI's classification and on the whole-word rules, not on my
own review, and any taxonomic error in the long tail would carry into the
species-group results. Unidentified entries stay in their own groups so they are
never counted as a known species.

## Filling in time of day

`TIME_OF_DAY` was missing from 153,368 of the 351,859 records (43.6%), but many
of those still had a raw `TIME` value. Rather than drop those records, I decided
to work out the time of day from that field. That decision and the choice to
leave unusable records blank were mine; the implementation was largely AI's. I
asked AI to define the sunrise and sunset cutoffs and to configure the function
that applies them. The code turns `TIME`, which is formatted inconsistently, into
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
Where my review was partial, as with the species groups, I have said so rather
than implying I checked everything.

The FAA data comes from voluntary strike reports, so it does not cover every
flight or every strike. The models therefore predict outcomes within the
reported strikes only. They do not predict whether a given flight will hit
wildlife.

AI supported this work, but the final decisions, the interpretation, and the
submitted project are my responsibility.

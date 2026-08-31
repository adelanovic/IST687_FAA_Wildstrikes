# FAA Wildlife Strike Analysis: IST 687 Final Project

Analysis of the FAA Wildlife Strike Database in R. All cleaning, modeling, and visualization is done in R.

## Dataset

Source: FAA Wildlife Strike Database (https://wildlife.faa.gov/search)

The full export contains **351,867 rows across 102 columns** (~35.9 million values), covering **1990 to present**. File size is roughly 152 MB. Reporting is voluntary, so the database is a sample of strikes that were reported, not a complete record of strikes that occurred.

Key field groups:

- **When/where:** incident date, month, year, time of day, airport, state, FAA region, latitude/longitude, runway
- **Aircraft and operator:** operator, aircraft type, class, mass, engine type and count
- **Flight conditions:** phase of flight, height, speed, distance, sky condition, precipitation
- **Wildlife:** species ID, species name, size, number seen vs. struck, whether the pilot was warned
- **Outcome:** indicated damage and damage level, struck/damaged component flags, effect on flight, aircraft out of service, repair and other costs (raw and inflation-adjusted), injuries, fatalities
- **Narrative:** remarks and comments

A data dictionary ships with the download and is the reference for coded fields.

## Business Questions

These are working questions and may evolve as the analysis develops.

1. **Seasonality.** Do strikes cluster seasonally, and does the pattern differ by species group or region?
   `INCIDENT_MONTH`, `INCIDENT_YEAR`, `SPECIES`, `STATE`, `FAAREGION`
2. **Airport risk profiles.** Can airports be segmented into risk profiles based on their strike patterns?
   `AIRPORT_ID`, strike total, % damaging strikes, % occurring at night, seasonality
3. **Damage risk drivers.** Which combinations of species, phase of flight, and time of day carry the highest damage risk?
   `SPECIES`, `SIZE`, `PHASE_OF_FLIGHT`, `TIME_OF_DAY`, `DAMAGE_LEVEL`
4. **Damage prediction.** Given conditions known before or at impact, can we predict whether a strike causes damage?
   `INDICATED_DAMAGE`, `SIZE`, `SPECIES`, `PHASE_OF_FLIGHT`, `HEIGHT`, `SPEED`, `TIME_OF_DAY`, `AC_MASS`, `NUM_ENGS`, `TYPE_ENG`, `NUM_STRUCK`

## Repository Structure

```
/Project Updates   Kanban boards and four-quadrant status summaries for updates 1 through 3
/RMD Files         R Markdown notebooks: cleaning, EDA, modeling, and the final write-up
/Charts            Exported plots and figures used in the report and presentation
```

## Additional Notes

Reports come from pilots, mechanics, controllers, and ground personnel with no standardized entry process, so completeness and consistency vary by record.

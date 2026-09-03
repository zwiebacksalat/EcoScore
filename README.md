# EcoScore online prototype

This is the online multi-user prototype based on the working v2 offline workflow.

- `app.R` — Shiny application
- `pressures.csv` — pressure master list
- `components.csv` — ecosystem component master list
- `subregions.csv` — subregion master list
- `supabase_schema.sql` — database table definition
- `.env.example` — example database environment variables
- `www/behaviour_flowchart.png` — copy this from the working v2 app before deploying

The application reads the master lists from the repository and stores shared scores in PostgreSQL/Supabase.

Never put the real database password in this repository.

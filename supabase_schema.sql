-- EcoScore online prototype database schema
-- Run this ONCE in Supabase -> SQL Editor before deploying the app.

create table if not exists assessment_scores (
    row_id bigint generated always as identity primary key,

    subregion text not null,
    pressure text not null,
    component text not null,

    extent text,
    extent_score double precision,

    dispersal text,
    dispersal_score double precision,

    frequency double precision,
    frequency_na boolean not null default false,

    hazard text,
    hazard_score double precision,

    magnitude text,
    magnitude_score double precision,

    behaviour text,
    behaviour_score double precision,

    resilience text,
    resilience_score double precision,

    comments text,

    updated_by text,
    updated_at timestamptz not null default now(),

    constraint assessment_scores_unique_combination
        unique (subregion, pressure, component)
);

create index if not exists assessment_scores_subregion_idx
    on assessment_scores (subregion);

create index if not exists assessment_scores_pressure_component_idx
    on assessment_scores (pressure, component);

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/843845244.svg)](https://doi.org/10.5281/zenodo.20339953)
<!-- badges: end -->

# midphysac

Research compendium for a retrospective study of **attitudes towards physical
activity (A2PA)** and cognitive **SuperAginf** in old-old women, pooling the
COSACTIW and KOKOSA samples.

## Sample

Both COSACTIW and KOKOSA are volunteer cohorts of women aged 80 and over.
COSACTIW was recruited for their high midlife physical activity; KOKOSA was
recruited as its low-activity counterpart and turned out more active than
intended. Pooled, they carry enough spread in A2PA to ask questions that neither
sample supports on its own.

## Research questions

1. How do old-old women differ in mental health (depressive and anxiety
   symptoms), cognition (screening, functional independence, SuperAging) and
   basic epidemiology (hypertension, diabetes, BMI, ...) conditional on their
   A2PA?
2. What predicts cognitive SuperAging in this cohort?

RQ2 is a prediction question rather than an effect question, so its estimand is
out-of-sample discrimination and calibration, not a contrast.

## Status

> [!WARNING]
> The compendium is being rebuilt for the A2PA study and **the pipeline does not
> run**. It stays that way until the estimands are fixed, pre-registered, and
> only then implemented.
>
> Nothing here is pre-registered yet, and no result in this repository should be
> read as findings. Work is tracked in
> [milestones M0-M5](https://github.com/josefmana/cosactiw-midphysac/milestones).

Whether the individual-level data can be deposited publicly is an open question
(milestone M2). Until it is settled, no participant data are in this repository.

## Installation

``` r
# install.packages("pak")
pak::pak("josefmana/cosactiw-midphysac")
```

The package is `midphysac`; the repository is `cosactiw-midphysac`. Installing
it gets you the code, not the data (yet).

## Previous work

### Georgi et al. ([2026](https://doi.org/10.1016/j.actpsy.2026.106478)), *Acta Psychologica*

Georgi, H., Mana, J., Schneiderova, M., Dadova, K., Ticha, Z., Vojtechova, I.,
... & Kopecek, M. (2026). Midlife physical activity as a link to better mental
health, functional status, and episodic memory in old-old women: Preliminary
study. *Acta Psychologica, 264*, 106478.
<https://doi.org/10.1016/j.actpsy.2026.106478>

Originally a brief report that compared high physical activity sustained into
later life against average levels, operationalised by the population each sample
was drawn from (COSACTIW active women vs. NANOK general population of women), on
measures of cognitive SuperAging, functional independence and affective health.

**Code as published.** Tag
[`v1.0.0`](https://github.com/josefmana/cosactiw-midphysac/releases/tag/v1.0.0),
archived at Zenodo under the version DOI
[10.5281/zenodo.20339954](https://doi.org/10.5281/zenodo.20339954). The badge at
the top of this file is the concept DOI
([10.5281/zenodo.20339953](https://doi.org/10.5281/zenodo.20339953)), which
always resolves to the most recent version, so cite the version DOI when you
mean the *Acta* code specifically.

> [!NOTE]
> The DAG shipped in `v1.0.0` is the *Acta* causal model and is not a statement
> about the data this repository now carries. Its exposure `mPA` is cohort
> membership (COSACTIW vs. NANOK) standing in for sustained midlife activity.

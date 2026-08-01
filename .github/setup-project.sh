#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# Creates labels, milestones and the issue backlog for the A2PA paper.
# Requires the GitHub CLI, authenticated:  gh auth login
# Safe to re-run: label/milestone creation failures are tolerated, but issues
# WILL be duplicated on a second run. Comment out the mk() calls you already made.
# ---------------------------------------------------------------------------
set -uo pipefail

REPO="josefmana/cosactiw-midphysac"

# ── Labels ────────────────────────────────────────────────────────────────
lbl () { gh label create "$1" --repo "$REPO" --color "$2" --description "$3" 2>/dev/null \
      || gh label edit   "$1" --repo "$REPO" --color "$2" --description "$3" ; }

lbl infrastructure  "0e8a16" "Repo, build, tooling"
lbl estimand        "1d76db" "RQs, DAG, estimands, analysis plan"
lbl data            "fbca04" "Data derivation, dictionary, freeze"
lbl prereg          "5319e7" "Pre-registration and transparency"
lbl analysis        "b60205" "Model fitting and results"
lbl manuscript      "c2e0c6" "Writing and preprint"
lbl long-lead       "d93f0b" "Start immediately; long turnaround"
lbl blocked-external "e99695" "Depends on someone other than me"

# ── Milestones ────────────────────────────────────────────────────────────
ms () { gh api "repos/$REPO/milestones" -f title="$1" -f due_on="$2" -f description="$3" >/dev/null 2>&1 \
      && echo "milestone created: $1" || echo "milestone exists/failed: $1" ; }

ms "M0: Infrastructure"             "2026-08-09T23:59:59Z" "Repo rebuilt on v2-rebuild. No science touched."
ms "M1: Estimands & analysis plan"  "2026-08-23T23:59:59Z" "RQs to estimands. Deliverable: Methods section (= prereg body)."
ms "M2: Data freeze"                "2026-08-30T23:59:59Z" "Consent verified, PA coding validated, v2.0.0-freeze tagged + Zenodo DOI."
ms "M3: Pre-registration"           "2026-09-06T23:59:59Z" "OSF Preregistration for Existing Data, publicly registered."
ms "M4: Analysis"                   "2026-09-20T23:59:59Z" "Pipeline executed, diagnostics, deviations logged."
ms "M5: Manuscript & preprint"      "2026-09-30T23:59:59Z" "Full draft, co-author round, PsyArXiv."

# ── Issues ────────────────────────────────────────────────────────────────
mk () { # mk <milestone> <labels> <title> <body>
  gh issue create --repo "$REPO" --milestone "$1" --label "$2" --title "$3" --body "$4" ;
}

# ---- M0 --------------------------------------------------------------------
M="M0: Infrastructure"
mk "$M" infrastructure \
  "Branch v2-rebuild from main" \
  "All M0 work lands on \`v2-rebuild\`; merge to \`main\` only once the skeleton is coherent so \`main\` is never broken."
mk "$M" infrastructure \
  "Fix DESCRIPTION: package name, title, LazyData" \
  "\`Package: midphys\` contradicts the README's \`pak::pak(\"josefmana/midphysac\")\`.Unify, rewrite Title/Description for the A2PA paper, set \`LazyData: true\`."
mk "$M" infrastructure \
  "Untrack _targets/meta/meta and extend .gitignore" \
  "Pipeline metadata is currently committed. \`git rm --cached _targets/meta/meta\`, then gitignore \`_targets/\` and \`renv/library/\`."
mk "$M" infrastructure \
  "Create .Rbuildignore" \
  "Must cover \`manuscript/\`, \`preregistration/\`, \`data-raw/\`, \`_targets*\`, \`*.Rproj\` so the compendium still passes R CMD check as a package."
mk "$M" infrastructure \
  "Apply keep/drop triage to R/" \
  "Drop Acta-specific output code: \`deprec-boxcox_transform.R\`, \`plot_results.R\`, \`plot_compare_results.R\`, \`plot_comparisons.R\`, \`table_results.R\`. Retained via tag v1.0.0 if ever needed."
mk "$M" infrastructure \
  "Stub the causal layer until estimands are fixed" \
  "\`draw_dag.R\`, \`adjustment_table.R\` and \`model_specs.R\` encode the *Acta* causal model; \`model_specs.R\` still hard-codes \`control = \"NANOK\"\`, which is dead under pooled COSACTIW+KOKOSA.

Replace with stubs that error loudly. Rationale: commit 7756455 redefined \`mPA\` as A2PA while the DAG underneath it stayed put. Rebuilding this layer before M1 invites the identical failure."
mk "$M" infrastructure \
  "Rewrite README" \
  "A2PA paper front and centre. Add an Acta section pointing at tag \`v1.0.0\` and Zenodo \`10.5281/zenodo.20339954\`, noting the archive predates the \`4781d1c\` moderator adjustment-set fix (which did not affect the published analyses)."
mk "$M" infrastructure \
  "Initialise renv and commit renv.lock" \
  "No lockfile exists today, so the computational environment is uncaptured. Closing this going forward."
mk "$M" "infrastructure,manuscript" \
  "Scaffold manuscript/ as a Quarto manuscript project" \
  "\`type: manuscript\`. Outputs: HTML site, docx for co-authors, PDF for the preprint. Must read results via \`tar_read()\` and never recompute, otherwise paper and pipeline drift."
mk "$M" "infrastructure,prereg" \
  "Scaffold preregistration/" \
  "Holds the OSF existing-data template, the prior-work archive (pilot poster), and the PA coding rule table."
mk "$M" infrastructure \
  "Update CITATION.cff" \
  "Currently carries the placeholder DOI \`10.5281/zenodo.1234\`."
mk "$M" "infrastructure,data" \
  "Add R/data.R roxygen skeleton and empty data/" \
  "\`R/data.R\` will document every column of \`analysis_data\` and thereby serve as the data dictionary."
mk "$M" infrastructure \
  "Merge v2-rebuild into main" \
  "Acta content leaves the working tree. It remains in git history, tag v1.0.0, Zenodo and Software Heritage."

# ---- M1 --------------------------------------------------------------------
M="M1: Estimands & analysis plan"
mk "$M" "estimand,blocked-external" \
  "Document the A2PA instrument" \
  "Need: exact item wording, all five response levels, when it was administered, and whether it is framed as a *current* or a *lifelong* attitude.

This single fact decides how much of the causal ladder is reachable. A2PA measured now, in women aged 80+, sits concurrently with cognition and depression."
mk "$M" "estimand,data" \
  "Report N per cohort, A2PA distribution, SA event count, missingness" \
  "Gates the RQ2 decision entirely."
mk "$M" "estimand,prereg" \
  "Archive the pilot poster and abstract as prior work" \
  "Into \`preregistration/prior_work/\`. Itemise which variables and which 3 of 5 A2PA levels were examined, and what was found. This is the raw material for the prior-exposure declaration."
mk "$M" estimand \
  "Decide and justify the A2PA operationalisation" \
  "Five independent levels, collapsed, or ordinal/ordinal-as-continuous. The current code does \`A2PA < 4\`, which is an undocumented dichotomisation, and analyses A2PA 1--3 as independent groups."
mk "$M" estimand \
  "Build the new DAG for the A2PA question" \
  "Must include a survival-and-participation selection node: both cohorts are volunteer samples of 80+ survivors, and A2PA plausibly causes participation as well."
mk "$M" estimand \
  "Derive adjustment sets and rewrite adjustment_table.R" \
  "From the new DAG, not the Acta one."
mk "$M" estimand \
  "Specify the estimand ladder E1/E2/E3 per outcome family" \
  "E1 stratum means; E2 covariate-standardised contrast (not causal); E3 (potentially) causal contrast.

The statistics are identical across E2 and E3, only the warrant changes, so the existing g-computation code already produces E2. State explicitly which outcomes are eligible for E3 and which are not and why.

Note: for GDS15/GAI/Depr/Anx the arrow may point the other way, since depression plausibly causes low current attitude to PA."
mk "$M" estimand \
  "Reframe RQ2" \
  "\"What predicts SuperAging best\" is a prediction question. Consequently, the estimand is out-of-sample discrimination and calibration, not an effect. With the likely event count it is probably underpowered as a variable-selection question.

Run the Riley et al. sample-size calculation, then decide: a set of adjusted associations reported together with no \"best\" claim, or a penalised model with bootstrap optimism correction or predictive projection via a full Bayesian way."
mk "$M" estimand \
  "Pre-specify the A2PA validity check with a pass/fail criterion" \
  "\"Proxy for lifelong attitudes\" is a measurement assumption, untestable in cross-section but partly probeable: does A2PA track cohort membership and retrospectively reported midlife PA? Criterion must be stated in advance."
mk "$M" estimand "Specify missing-data strategy" "Per variable, informed by the missingness audit."
mk "$M" estimand "Specify sensitivity and robustness analyses" "Including the A2PA operationalisation choice and the PA coding rules."
mk "$M" manuscript \
  "Write Introduction and Methods; circulate to co-authors" \
  "The Methods section *is* the prereg body. Write it once. Circulating in soon is what makes co-author round realistic within a given time-frame."

# ---- M2 --------------------------------------------------------------------
M="M2: Data freeze"
mk "$M" "data,blocked-external,long-lead" \
  "Verify consent and ethics cover PUBLIC deposit" \
  "Git is irrevocable: once individual-level health data on 80+ women is pushed to a public repo it lives in forks and caches permanently.

Decision: real data in \`data/\`, or synthetic public dataset plus restricted OSF component."
mk "$M" "data,blocked-external,long-lead" \
  "PA coding reliability check" \
  "\`data-raw/PA_pipeline.R\` states in its own header that the KOKOSA activity-coding rules were extracted from hand-coded COSACTIW data in dialogue with an LLM. That sits directly upstream of the exposure and is not yet validated.

Have a human independently recode a random KOKOSA subsample; report kappa or % agreement."
mk "$M" "data,prereg" \
  "Document the PA coding rules as a rule table" \
  "Currently they exist only as code comments. Needed as a prereg supplement."
mk "$M" data \
  "Test for differential misclassification in PA coding" \
  "The exclusion lists are asymmetric between cohorts. Check whether that induces misclassification that differs by study."
mk "$M" data \
  "Write data-raw/build_analysis_data.R" \
  "Reads the private source workbooks, applies \`import_data()\` and the PA coding, ends with \`usethis::use_data(analysis_data)\`. Source workbooks stay gitignored forever.

Freeze the derived table, not just raw: the SuperAging recoding, \`thres_type\`, and the A2PA dichotomisation are analytic decisions living inside what looks like a data step."
mk "$M" data \
  "Document every column in R/data.R" \
  "This is the data dictionary."
mk "$M" data \
  "Repoint the first targets target to midphysac::analysis_data" \
  "No file paths, one processed dataset, no drift."
mk "$M" data \
  "Tag v2.0.0-freeze and verify Zenodo archived it" \
  "The tag **is** the freeze: one release produces one Zenodo version DOI containing code and frozen data in a single citable artifact, with the git SHA as checksum. Verify the webhook actually fired and record the DOI."

# ---- M3 --------------------------------------------------------------------
M="M3: Pre-registration"
mk "$M" prereg "Create the OSF project and components" "Prereg component, and a restricted data component if that route is chosen."
mk "$M" prereg \
  "Complete the OSF Preregistration for Existing Data template" \
  "Not a standard prereg. The existing-data template (van den Akker et al., PRESD) has explicit fields for data provenance, access date and prior knowledge. Body comes from the M1 Methods section."
mk "$M" prereg \
  "Write the prior-exposure declaration" \
  "The pilot poster examined pooled COSACTIW+KOKOSA differences by A2PA across several variables at 3 of 5 levels. Declare it: which variables, which levels, what was found. Then state what is genuinely new: full 5-level A2PA, additional outcomes, estimand framing, pre-specified adjustment sets."
mk "$M" prereg "Cross-link prereg, Zenodo freeze DOI and repo tag" "All three directions."
mk "$M" "prereg,blocked-external" "Co-author sign-off and registration" ""

# ---- M4 --------------------------------------------------------------------
M="M4: Analysis"
mk "$M" analysis "Rebuild model_specs.R from the registered estimands" "Only now, not before."
mk "$M" analysis "Run the pipeline and model diagnostics" ""
mk "$M" analysis "Run registered sensitivity and robustness analyses" ""
mk "$M" analysis "Produce results tables and figures" ""
mk "$M" "analysis,prereg" \
  "Maintain a deviations log" \
  "Every departure from the prereg, with its reason, in a tracked file. Write it as we go, not retrospectively."

# ---- M5 --------------------------------------------------------------------
M="M5: Manuscript & preprint"
mk "$M" manuscript "Write Results" ""
mk "$M" manuscript "Write Discussion" ""
mk "$M" manuscript \
  "Write Limitations" \
  "Must explicitly cover: prior data exposure via the pilot poster; cross-sectional A2PA and possible reverse causation for affective outcomes; survival and participation selection in two volunteer 80+ samples; LLM-assisted PA coding and its measured reliability."
mk "$M" "manuscript,blocked-external" "Co-author round" ""
mk "$M" manuscript "Post PsyArXiv preprint" "At journal submission. Cross-link prereg, data DOI and repo tag."
mk "$M" "manuscript,infrastructure" "Tag v2.0.0; verify Zenodo; update CITATION.cff and README badges" ""

echo "Done."

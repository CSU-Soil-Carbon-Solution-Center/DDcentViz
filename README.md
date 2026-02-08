**DDcentViz** is an R package for exploring, interpreting, and visualizing DayCent model outputs across sites and management experiments. It provides standardized file discovery, robust output readers, and narrative plotting tools that work equally well in interactive Shiny applications and direct R analysis workflows.

DDcentViz is intentionally **visualization- and interpretation-focused**. It does not run the DayCent model or build input files; instead, it operates on existing DayCent project directories to help users understand, compare, and communicate model results.

---

## Design goals

DDcentViz is built around a few core principles:

- **UI-agnostic**  
  All functions return standard R objects (data frames and `ggplot2` objects). The package has no dependency on Shiny, but is designed to integrate cleanly with Shiny apps.

- **Project- and experiment-aware**  
  The package understands common DayCent directory conventions (sites, experiments, outputs) and uses them to drive discovery and visualization.

- **Narrative-first visualization**  
  Core plotting functions generate multi-panel, interpretable plots (e.g., carbon cycle narratives) rather than single-variable diagnostics.

- **Local-first and reproducible**  
  DDcentViz works directly on local project folders without assuming a working directory or global state.

---

## What this package does

DDcentViz provides tools to:

- Discover DayCent sites and experiments within a project
- Build lightweight file manifests for experiments and outputs
- Read common DayCent output formats (`.csv`, `.out`, `.txt`, `.dat`)
- Generate narrative plots (e.g., carbon cycle summaries)
- Support dynamic, user-defined plotting of any output table
- Serve as the visualization backbone for Shiny dashboards

---

## What this package does *not* do

DDcentViz intentionally does **not**:

- Build DayCent input files (`.100`, `.sch`, `.wth`)
- Execute the DayCent model
- Manage HPC job submission or execution
- Enforce a specific UI or workflow

For those tasks, DDcentViz is designed to complement execution-focused tools such as **DDcentutils**.

---

## Expected project structure
DDcentViz assumes a project organized roughly as:
project/
  sites/
    {site}/
      {site}_{exp}.sch
      {site}_site.100
  outputs/
    eq/
    base/
    {exp}/

- `{site}` is a site identifier (e.g., `WOOSTER`). A site defines a spin-up scenario with common equilibrium and baseline management.
- `{exp}` is an experiment or spin-up period (e.g., `base`, `cc_nt`)
- Multiple experiments per site are supported and
- Outputs are discovered primarily via subdirectories under `outputs/`

The package is flexible and will warn (not fail) if expected files are missing.
---

## Core functionality 
Typical workflows revolve around these functions:
### Discovery and indexing
- `dc_list_sites(project)`
- `dc_list_experiments(project, site)`
- `dc_manifest(project, site, exp = NULL)`

### Reading outputs
- `dc_read_output(path, role = NULL)`

### Visualization
- `dc_plot_carbon(manifest, exp, start_year, end_year, scenario)`
- `dc_plot_dynamic(data, x, y, color = NULL, facet = NULL, geom = "line")`

All plotting functions return `ggplot2` objects or named lists of plots.
---

## Example workflow
```r
library(DDcentViz)
project <- "/data/daycent_projects"

# Discover sites and experiments
sites <- dc_list_sites(project)
exps  <- dc_list_experiments(project, site = "wooster")

# Build a manifest for one experiment
m <- dc_manifest(project, site = "wooster", exp = "base")

# Read a specific output
sip <- dc_read_output(dplyr::filter(m, role == "dc_sip_csv")$path[1])

# Generate carbon narrative plots
plots <- dc_plot_carbon(
  manifest = m,
  exp = "cornSoybeans_NT",
  start_year = 2006,
  end_year   = 2020,
  scenario  = "cornSoybeans_NT"
)

plots[["Total system C"]]
```

## Relationship to other packages
DDcentViz is designed to complement, not replace, other DayCent tools: DDcentutils: input file setup, validation, and model execution

This separation keeps execution pipelines stable while allowing visualization and analysis tools to evolve rapidly.

### Status
DDcentViz is under active development. The initial focus is on:

- Carbon cycle narrative plots
- Robust file discovery and reading
- Integration with Shiny-based dashboards
- Additional narrative modules (soil water, nitrogen cycling) and experiment comparison features are planned.

## License
MIT 

## Acknowledgements
Developed by the Ecosystem Modeling and Data Consortium at Colorado State University as part of ongoing ecosystem modeling and data integration efforts supporting DayCent training, research, and applied decision support.
---

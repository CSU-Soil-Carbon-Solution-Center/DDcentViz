# R/00_constants.R

DDCENTVIZ_DEFAULT_SITES_DIR <- "sites"

DDCENTVIZ_RESERVED_EXPS <- c(".", "..")

# Roles (extend as you add more narratives)
DDCENTVIZ_ROLES <- c(
  "dc_sip_csv",
  "summary_out",
  "watrbal_out",
  "vswc_out",
  "wfps_out",
  "soils_in",
  "lis",
  "bin",
  "sch",
  "wth",
  "100",
  "unknown"
)

DDCENTVIZ_ROLE_PATTERNS <- list(
  dc_sip_csv = list(
    ext = c("csv"),
    regex = c("dc[_-]?sip")
  ),
  summary_out = list(
    ext = c("out", "txt"),
    regex = c("summary")
  ),
  watrbal_out = list(
    ext = c("out", "txt"),
    regex = c("watrbal")
  ),
  vswc_out = list(
    ext = c("out", "txt"),
    regex = c("vswc")
  ),
  wfps_out = list(
    ext = c("out", "txt"),
    regex = c("wfps")
  ),
  soils_in = list(
    ext = c("in", "txt"),
    regex = c("^soils\\.in$", "soils\\.in")
  ),
  lis = list(ext = c("lis"), regex = c(".*")),
  bin = list(ext = c("bin"), regex = c(".*")),
  sch = list(ext = c("sch"), regex = c(".*")),
  wth = list(ext = c("wth"), regex = c(".*")),
  `100` = list(ext = c("100"), regex = c(".*"))
)

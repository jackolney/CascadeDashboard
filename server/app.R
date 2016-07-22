# ------ #
# GLOBAL #
# ------ #

source("server/alert.R",           local = TRUE)
source("server/leaflet.R",         local = TRUE)
source("server/links.R",           local = TRUE)
source("server/misc-functions.R",  local = TRUE)
source("server/report-fig.R",      local = TRUE)
source("server/report.R",          local = TRUE)
source("server/valuebox.R",        local = TRUE)
source("ui/global-lists.R",        local = TRUE)

# ------- #
# COUNTRY #
# ------- #

source("server/country/observe.R", local = TRUE)
source("server/country/rhandsontable.R", local = TRUE)

# ----- #
# MODEL #
# ----- #

source("server/model/baseline-model.R", local = TRUE)
source("server/model/best-fit-model.R", local = TRUE)
source("server/model/beta.R",           local = TRUE)
source("server/model/initial.R",        local = TRUE)
source("server/model/model.R",          local = TRUE)
source("server/model/parameters.R",     local = TRUE)

# ---------- #
# CALBRATION #
# ---------- #

source("server/calibration/assumptions.R",      local = TRUE)
source("server/calibration/calibration-data.R", local = TRUE)
source("server/calibration/calibration.R",      local = TRUE)
source("server/calibration/check-data.R",       local = TRUE)
source("server/calibration/confirm-data.R",     local = TRUE)
source("server/calibration/data-table.R",       local = TRUE)
source("server/calibration/error.R",            local = TRUE)
source("server/calibration/initial.R",          local = TRUE)
source("server/calibration/marrakech-data.R",   local = TRUE)
source("server/calibration/master.R",           local = TRUE)
source("server/calibration/misc-functions.R",   local = TRUE)
source("server/calibration/mission-control.R",  local = TRUE)
source("server/calibration/model.R",            local = TRUE)
source("server/calibration/observe-input.R",    local = TRUE)
source("server/calibration/observe-output.R",   local = TRUE)
source("server/calibration/plot-functions.R",   local = TRUE)
source("server/calibration/plot.R",             local = TRUE)
source("server/calibration/result.R",           local = TRUE)
source("server/calibration/tx-guidelines.R",    local = TRUE)
source("server/calibration/update-input.R",     local = TRUE)

# ---------- #
# PROJECTION #
# ---------- #

source("server/projection/CD4-distribution.R", local = TRUE)
source("server/projection/data-table.R",       local = TRUE)
source("server/projection/extract-data.R",     local = TRUE)
source("server/projection/observe-event.R",    local = TRUE)
source("server/projection/plot-functions.R",   local = TRUE)
source("server/projection/plot.R",             local = TRUE)
source("server/projection/reactive.R",         local = TRUE)

# ------------ #
# OPTIMISATION #
# ------------ #

source("server/optimisation/functions.R",            local = TRUE)
source("server/optimisation/intervention-control.R", local = TRUE)
source("server/optimisation/observe-input.R",        local = TRUE)
source("server/optimisation/observe.R",              local = TRUE)
source("server/optimisation/optimise.R",             local = TRUE)
source("server/optimisation/parameters.R",           local = TRUE)
source("server/optimisation/plot-functions.R",       local = TRUE)
source("server/optimisation/plot.R",                 local = TRUE)
source("server/optimisation/prediction.R",           local = TRUE)
source("server/optimisation/reactive.R",             local = TRUE)
source("server/optimisation/sim.R",                  local = TRUE)
source("server/optimisation/table.R",                local = TRUE)
source("server/optimisation/unaids-goal.R",          local = TRUE)

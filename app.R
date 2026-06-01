# Launch the Themenatlas golem application.
# Development entry point: loads the package from source and runs the app.
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = FALSE)
run_app()

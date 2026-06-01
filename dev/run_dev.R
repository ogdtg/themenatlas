# Development launcher.
# Run from the project root so that the relative `data/` paths resolve.
options(golem.app.prod = FALSE)

# Document & reload the package, then launch the app.
golem::detach_all_attached()
golem::document_and_reload()

run_app()

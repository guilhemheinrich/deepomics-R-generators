source("generate_explorer.R")

entity_vector <- c("Project", "Experimental Serie", "measure", "compartment replicate", "monitored measure type")


template_file_path <- "template_GET_selectInput.mustache"
module_prefix <- "selectInput_GET"
module_suffix <- ""
dir.create("generated", showWarnings = FALSE)
sink(paste0("generated/golem_dev"))
for (entity in entity_vector) {
    cat(generate_explorer(
        entity,
        template_file_path = template_file_path,
        module_prefix = module_prefix,
        module_suffix = module_suffix))
    cat("\n")
}
sink()
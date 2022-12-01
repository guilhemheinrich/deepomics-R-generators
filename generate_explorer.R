# library(whisker)
# library(snakecase)


generate_explorer <- function(entity,
                              template_file_path = "template_GET_selectInput.mustache",
                              module_prefix = "",
                              module_suffix = "") {
  template <- readChar(template_file_path, file.info(template_file_path)$size)
  basic_config <- list(
                snake_entity = snakecase::to_snake_case(entity),
                entity = entity,
                module_prefix = module_prefix,
                module_suffix = module_suffix
                )
  module_name <- paste0(basic_config$snake_entity, "s")
  if (basic_config$module_prefix != "") {
    module_name <- paste0(c(basic_config$module_prefix, module_name),
      collapse = "_")
  }
  if (basic_config$module_suffix != "") {
    module_name <- paste0(c(module_name, basic_config$module_suffix),
      collapse = "_")
  }
  final_config <- c(
    basic_config,
    snake_entities = paste0(basic_config$snake_entity, "s"),
    entities = paste0(basic_config$entity, "s"),
    module_name = module_name
  )
  codegen <- whisker::whisker.render(template, final_config)
  output_file_name <- paste0("mod_", final_config$module_name, ".R")

  dir.create("generated", showWarnings = FALSE)
  sink(paste0("generated/", output_file_name))
  cat(codegen)
  sink()
  return(paste0(
    "golem::add_module(name = \"",
    final_config$module_name,
    "\")")
  )
}

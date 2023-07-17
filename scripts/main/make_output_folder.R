# make an output directory
output_folder = here(fhast_base_folder, paste0(run_name, "_outputs"))
output_shape_folder = here(output_folder, "shape_files")
dir.create(output_folder)
dir.create(output_shape_folder)

# Write the location of the input folder so the markdown files know where it is
write.table(here(fhast_base_folder, fhast_config_file),
            file = here("temporary", "input_file_path.txt"),
            row.names = F,
            col.names = F)
generate_pal_img <- function(palette, w = 170, h = 10) {
  # Generate color palette
  colors <- grDevices::hcl.colors(w, palette = palette)
  # SVG string
  svg_string <- paste0(collapse = "",
    '<svg width="', w,'" height="1" xmlns="http://www.w3.org/2000/svg">',
    paste0('<rect x="', seq_along(colors),'" y="0" width="1" height="1" fill="', colors,'" />', collapse = ""),
    '</svg>'
  )

  tmp_svg <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp_svg), add = TRUE)

  writeLines(svg_string, tmp_svg)
  
  # Convert SVG to PNG in memory
  png_data <- rsvg::rsvg(tmp_svg)
  
  # Convert PNG data to WebP
  webp_data <- webp::write_webp(png_data, quality = 100)
  
  # Encode WebP data to base64
  base64_data <- base64enc::base64encode(webp_data)
  
  # Construct the base64 image string
  base64_image <- paste0("<img height=", h," width=", w," class=\"palselect\" src=\"data:image/webp;base64,", base64_data, "\" alt=\"", palette,"\">")
}

pals <- list(
  select = setNames(grDevices::hcl.pals(), vapply(grDevices::hcl.pals(), generate_pal_img, character(1))),
  colors = setNames(lapply(grDevices::hcl.pals(), grDevices::hcl.colors, n = 256), grDevices::hcl.pals())
)

saveRDS(pals, "scripts/pals.rds")

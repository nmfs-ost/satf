
# create figure with alt text placeholder
fig1 <- satf::plot_spawning_biomass(
  Petrale_sole_std_res_2023
  ) +
  ggplot2::labs(
    caption = "my caption"
  )

# view figure
fig1

# add alt text
fig1_alttext <- "alt text here"
fig1 <- fig1 +
  ggplot2::labs(
    alt = fig1_alttext
  )

# view figure with alt text (shouldn't be any different)
fig1

# check alt text was added to figure
ggplot2::get_alt_text(fig1)

# export figure to jpg
ggplot2::ggsave(filename = "test.jpg",
                plot = fig1)

# make template csv
alt_text_df <- data.frame("Figure" = as.character(quote(fig1)),
                          "Alt_text" = as.character(fig1_alttext))
# export alt text to csv
write.csv(alt_text_df,
          "test.csv")

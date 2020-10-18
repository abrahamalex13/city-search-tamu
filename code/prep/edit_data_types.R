#edit_data_types.R

# str(data)
# colnames(data)

#assume special factor-type data structure initially,
#requires particular conversion to character.
varnames_to_char <- c("MET2013", "DEGFIELD")
primary1 <- primary1 %>%
  mutate(across(varnames_to_char, 
                .fns = list("to_char" = ~ as.character(haven::as_factor(.x))),
                .names = "{.col}_chr"))

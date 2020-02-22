library(here)
library(pdftools)
library(purrr)
library(stringr)

# download.file(
#   url = "http://bocyl.jcyl.es/boletines/1983/04/02/pdf/BOCYL-D-02041983-1.pdf",
#   destfile = here("pdfs", "BOCYL-D-02041983-1.pdf"),
#   mode = "wb"
# )

pdfs <- list.files(path = here("pdfs"), pattern = "pdf$")
text <- map(pdfs, ~ pdftools::pdf_text(here("pdfs", .)))

# be better to combine these into a single piped function (mappable)
text1_combined <- str_c(text[[1]], collapse = "")
text1_split <- str_split(text1_combined, "JUNTA")
text1_split <- text1_split[[1]] %>% tail(-1)
text1_list <- map(text1_split, ~ paste0("JUNTA", .))

# making the above into a function
split_text1 <- function(x) {
  x %>% 
    str_c(collapse = "") %>% 
    str_split(., "JUNTA") %>% 
    tail(-1) %>% 
    map(., ~ paste0("JUNTA", .))
}

text1_list <- text %>% map(., ~ split_text1(.))

text1_list <- text1_list %>% set_names(., nm = str_extract(., "^([:upper:]|\\s)+(?=\r)"))
text1_trimmed <- map(text1_list, ~ str_replace(., "^([:upper:]|\\s)+\r\n", ""))

text1_trim_tidy <- map(text1_trimmed, ~ str_replace_all(., "\r\n(?=[:digit:])", ","))
text1_trim_tidy <- map(text1_trim_tidy, ~ str_replace_all(., "\r\n", " "))
text1_trim_tidy <- map(text1_trim_tidy, ~ str_replace_all(., "\\s+$", ""))

text1_by_party <- map(text1_trim_tidy, ~ str_split(., ",(?=[:digit:]+\\.\\s[:upper:]{2,})"))


# clear up intermediate objects
# rm(text1_combined)
# rm(text1_split)
# rm(text1_list)
# rm(text1_trimmed)
# rm(text1_trim_tidy)
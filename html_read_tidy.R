library(rvest)
library(stringr)
library(purrr)
library(dplyr)
library(tibble)
library(conflicted)
conflict_prefer("pluck", "purrr")

# you should add any further URLs to this vector
urls <- c("http://bocyl.jcyl.es/html/1983/04/02/html/BOCYL-D-02041983-1.do")

# scrape text from the relevant part of the webpage
# (assume that any additional URLs have the same structure)
text <- urls %>%
  map(., ~ {xml2::read_html(.) %>%
      rvest::html_nodes("#presentDocumentos p:not([class])") %>% 
      html_text})

# extract a manageable name from the URL and use it to name each text
names(text) <- urls %>% 
  str_extract_all(., pattern = "(?<=/)BOCYL.*(?=\\.do$)")

# do any manual fixes for errors in source data
text1 <- text %>% 
  map(., ~
  str_replace_all(., "PARTIDO COMUNISTA DE ESPAÑA PARTIDO COMUNISTA DE CASTILLA- LEON", "2. PARTIDO COMUNISTA DE ESPAÑA PARTIDO COMUNISTA DE CASTILLA- LEON"))

text2 <- text1 %>%
  map(., ~ 
        str_replace_all(., "(\\.)*(\\s)*$", "") %>% 
        str_replace_all(., "(\\s)+", " ") %>% 
        str_replace_all(., "^Suplente.*", "") %>% 
        str_c(., collapse = ";") %>% 
        str_split(., pattern = "JUNTA ELECTORAL DE ") %>% 
        map(., ~ tail(., -1) %>% 
              str_split(.,
                        pattern = ";(?=\\d{1,2}\\.\\s([:upper:]|\\s){2,})") %>% 
              set_names(str_to_title(map(., 1))) %>% 
              map(., ~ tail(., -1))
        )
  )


text3 <- text2 %>% 
  map(., ~
        map(., ~ 
              map(., ~ str_split(., pattern = ";(?=\\d{1,2}\\.\\s)") %>% 
                    set_names(map(., 1) %>% 
                                str_extract(., pattern = "(?<=\\d{1,2}\\.\\s)[:upper:].*")) %>% 
                    map(., ~ tail(., -1) %>% 
                          enframe(., name = "list_position", value = "person_name") %>% 
                          mutate_at(
                            vars("person_name"),
                            ~ str_extract_all(.,
                                              pattern = "(?<=\\d{1,2}\\.\\s)[:alpha:]+.*"))))))

text4 <- text3 %>% 
  map(., ~
        map(., ~ 
              map(., ~
                    map_df(., c, .id = "political_group"))))

text5 <- text4 %>% 
  map(., ~
        map(., ~ 
              map_df(., c, .id = "territory")))

# EXAMPLE
# To look at just the data frame produced from the first web page supplied
# (with columns rearranged as desired):
data_frame1 <- text5 %>% 
  pluck(1, 1) %>% 
  select(person_name, everything())
data_frame1

## Load only a few needed libraries
library(readxl)
library(rvest)
library(stringr)

base_dir <- "projects/R_Code/help_a_friend/"
t_urls <- read_xlsx(paste0(base_dir, "links_list.xlsx"))

extract_imgs_to_output <- function(t_url, base_dir) {
    temp <- read_html(t_url)
    
    # Cleaning things up a bit - Weird HTML characters and encodings and what not:
    model_name <- temp %>% html_nodes("h2") %>% html_text()
    model_name <- str_replace_all(model_name, " ", "") %>% paste0("_folder")
    model_name <- URLencode(model_name)
    
    out_dir <- paste0(base_dir, "output/", model_name, "/")
    # Simplest of debugging, to keep track visually:
    print(out_dir)
    
    if(!dir.exists(out_dir)) {
        dir.create(paste0(out_dir))
        temp_nodesets <- temp %>% html_nodes("img")
        
        # This is the trick: Find a way to uniquely identify the URLs to extract.
        # Then extract from tag attributes (usually one would otherwise use html_text())
        img_urls <- temp_nodesets[grepl("formidable", temp_nodesets)] %>%
            html_attr('data-src')
        img_urls <- img_urls[!is.na(img_urls)] # required, and obvious...
        
        for(t_img in img_urls) {
            # Better to use lapply again here, but this is not optimal code at all.
            
            tryCatch( # This was needed, as some URLs were returning 404...
                {
                    # Keep the end of URL for filename...
                    t_filename <- sub(".*/", "", t_img, perl = T)
                    # first time using download.file ever. Quite straightforward
                    download.file(t_img, 
                                  paste0(out_dir, t_filename))
                },
                error=function(cond) {
                    message(paste("URL not found?", t_img))
                    message(cond)
                    return(NULL)
                },
                warning=function(cond) {
                    message(paste("Warning?", t_img))
                    message(cond)
                    return(NULL)
                },
                finally={
                })
        }
    } else { # Duplicate models
        message("****")
        message(t_url)
        message("Folder Name already exists!")
    }
}

lapply(t_urls$base_url, extract_imgs_to_output, base_dir)

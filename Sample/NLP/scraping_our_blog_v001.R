library(plyr) # for rbind.fill
library(dplyr)
library(curl)
library(xml2)
library(rvest) # for html_attr
library(microbenchmark)
library(future.apply) # launch in parallel

# Select base page. Could have been the Home too on this Blog:
plan(multisession)

get_article_in_page <- function(art_num = 1, page_articles) {
  if(is.list(page_articles) & length(page_articles) >= art_num) {
    tryCatch({
      # Date is tricky here:
      possible_date_locations <- c(".//time[@class='entry-date published']", ".//time[@class='entry-date published updated']")
      art_date <- xml_text(xml_find_all(page_articles[art_num], possible_date_locations[1]))
      if(length(art_date) == 0) {
        art_date <- xml_text(xml_find_all(page_articles[art_num], possible_date_locations[2]))
      }
      
      art_title <- xml_text(xml_find_all(page_articles[art_num], ".//h1[@class='entry-title']"))
      art_content <- xml_text(xml_find_all(page_articles[art_num], ".//div[@class='entry-content']"))
      print(paste("Article:", art_num, "Date Found: ", art_date)); flush.console();
      
      return(data.frame(article_date = art_date,
                        article_title = art_title,
                        article_content = art_content))
    }, warning = function(w) {
      message(paste(w, length(art_date)))
      return(NULL)
    }, error = function(e) {
      message(paste(e, length(art_date)))
      return(NULL)
    }, finally = {
      #cleanup-code
    })
  }
  
  # Should never be reached.
  print("Function get_article_in_page: Uncontrolled error?"); flush.console()
  return(NULL)
} 

get_page_kaizen_blog <- function(pagenum = 1) {
  base_page <- "https://www.kaizen-r.com/category/blog/page/"
  
  tryCatch({
    page_con <- curl(paste0(base_page, pagenum, "/"))
    print(paste("get_page: ", pagenum)); flush.console()

    page_text <- tryCatch(xml2::read_html(page_con), 
                          error = function(e) { 
                            close(page_con)
                            return(NULL)
                            })
    
    if(!is.null(page_text)) {
      print(paste("Getting Articles for Page:", pagenum)); flush.console()
      page_articles <- xml_find_all(page_text, ".//article")
      print(paste("Number of articles found in page:", length(page_articles))); flush.console()
      
      t_articles_df <- rbind.fill(lapply(1:length(page_articles), 
                                         get_article_in_page,
                                         page_articles))
      
      return(cbind(data.frame(pagenum = pagenum), t_articles_df))
    } else {
      return(NULL)
    }
  }, warning = function(w) {
    message(w)
    print("Function get_page: Controlled warning"); flush.console()
    return(NULL)
  }, error = function(e) {
    message(e)
    print("Function get_page: Controlled error"); flush.console()
    return(NULL)
  }, finally = {
    #cleanup-code
  })
  
  # Should never be reached.
  print("Function get_page: Uncontrolled error?"); flush.console()
  return(NULL)
}

# Suppose we don't know how many pages we have...
# We need to loop through it until we find a page with no articles:
get_all_articles_kaizen_blog_v1 <- function() {
  articles_df <- NULL
  pagenum <- 1
  
  while(TRUE) {
    t_articles <- get_page_kaizen_blog(pagenum)
    if(is.null(t_articles)) {
      break;
    } else {
      articles_df <- rbind.fill(t_articles,
                           articles_df)
    }
    pagenum <- pagenum + 1
  }
  articles_df
}

# Similar, but using lapply on groups of pages (wait for it...)
get_all_articles_kaizen_blog_v2 <- function() {
  t_pager <- 4 # How many parallel scrapes, depending on available CPU Cores.
  t_pager_start <- 1
  t_list <- NULL
  
  while(TRUE) {
    t_pages_set <- t_pager_start:(t_pager_start+t_pager-1)
    
    print(paste("Entering pageset:", paste(t_pages_set, collapse=","))); flush.console()

    t_new <- lapply(t_pages_set, get_page_kaizen_blog)
    
    if(is.null(t_new[[1]])) { # No articles:
      break
    }
    
    t_new_empty <- which(sapply(t_new, is.null))
    print(t_new_empty)
    print((is.integer(t_new_empty) && length(t_new_empty) == 0L) || # No empties
            (length(t_new_empty) < t_pager))
    
    if((is.integer(t_new_empty) && length(t_new_empty) == 0L) || # No empties
       (length(t_new_empty) < t_pager)) { # Some articles
      t_list <- append(t_list, t_new)
      t_pager_start <- t_pager_start + t_pager
      #print(dim(articles_df)); flush.console()
      
      if(is.integer(t_new_empty) && length(t_new_empty) != 0L) {
        break
      }
      next
    }
  }
  rbind.fill(t_list)
}

# If many pages, use future_lapply to parallelize some of it...
get_all_articles_kaizen_blog_vfuture <- function() {
  t_pager <- 4 # How many parallel scrapes, depending on available CPU Cores.
  t_pager_start <- 1
  t_list <- NULL
  
  while(TRUE) {
    t_pages_set <- t_pager_start:(t_pager_start+t_pager-1)
    
    print(paste("Entering pageset:", paste(t_pages_set, collapse=","))); flush.console()
    
    t_new <- future_lapply(t_pages_set, get_page_kaizen_blog)
    
    if(is.null(t_new[[1]])) { # No articles:
      print("No more articles found."); flush.console()
      break
    }
    
    t_new_empty <- which(sapply(t_new, is.null))
    print(t_new_empty); flush.console()
    print((is.integer(t_new_empty) && length(t_new_empty) == 0L) || # No empties
            (length(t_new_empty) < t_pager)); flush.console()
    
    if((is.integer(t_new_empty) && length(t_new_empty) == 0L) || # No empties
       (length(t_new_empty) < t_pager)) { # Some articles
      t_list <- append(t_list, t_new)
      t_pager_start <- t_pager_start + t_pager
      #print(dim(articles_df)); flush.console()
      
      if(is.integer(t_new_empty) && length(t_new_empty) != 0L) {
        break
      }
      next
    }
  }
  rbind.fill(t_list)
}

# all_articles_df_v1 <- get_all_articles_kaizen_blog_v1()
# all_articles_df_v2 <- get_all_articles_kaizen_blog_v2()
# all_articles_df_vfuture <- get_all_articles_kaizen_blog_vfuture()

microbenchmark(
  get_all_articles_kaizen_blog_v1(),
  get_all_articles_kaizen_blog_v2(),
  get_all_articles_kaizen_blog_vfuture(),
  times = 5L
)

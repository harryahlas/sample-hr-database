setwd("C:\\Development\\github\\blog")

blog_files <- list.files(getwd(), 
                         #pattern=(".html"),
                         full.names=F,
                         recursive = TRUE)

# do not change this file or image files
blog_files <- blog_files[blog_files != "scripts/updateURLs.R"]
blog_files <- blog_files[grepl(blog_files,pattern = "/images/") != TRUE]


# Make files for server ---------------------------------------------------

local_url <- "C:\\\\Development\\\\github\\\\blog\\\\public"
new_url <- "http://earlydemise.com/blog/public"

for(blog_file in blog_files) {
  x <- readLines(blog_file)
  y <- gsub(local_url, new_url, x)
  cat(y, file=blog_file, sep="\n")
}

# Make files local --------------------------------------------------------

server_url <- "http://earlydemise.com/blog/public"
new_url <- "C:\\\\Development\\\\github\\\\blog\\\\public"

for(blog_file in blog_files) {
  x <- readLines(blog_file)
  y <- gsub(server_url, new_url, x)
  cat(y, file=blog_file, sep="\n")
}

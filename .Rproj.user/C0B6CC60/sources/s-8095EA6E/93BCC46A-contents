library(RCurl)
setwd("C:\\Development\\github\\blog")

blog_files_all <- list.files(getwd(), 
                         #pattern=(".html"),
                         full.names=F,
                         recursive = TRUE)

# do not change this file or image files
blog_files <- blog_files_all[blog_files != "scripts/updateURLs.R"]
blog_files <- blog_files[grepl(blog_files,pattern = "/images/") != TRUE]
blog_files <- blog_files[grepl(blog_files,pattern = "jpg|png") != TRUE]

# Make files for server ---------------------------------------------------

local_url <- "C:\\\\Development\\\\github\\\\blog\\\\public"
new_url <- "http://earlydemise.com/blog/public"

for(blog_file in blog_files) {
  x <- readLines(blog_file)
  y <- gsub(local_url, new_url, x)
  cat(y, file=blog_file, sep="\n")
}


# Reap --------------------------------------------------------------------
# (enter pw first)
# Note: prior to reap, make sure new folders are created

for (blog_file in blog_files_all) {
  print(blog_file)
  ftpUpload(blog_file, 
            paste0("sftp://edemise:",pw,"@boron.he.net/home/edemise/public_html/blog/",
                   blog_file),
            .opts = list(ftp.create.missing.dirs=TRUE)
  )
}

# Make files local --------------------------------------------------------

server_url <- "http://earlydemise.com/blog/public"
new_url <- "C:\\\\Development\\\\github\\\\blog\\\\public"

for(blog_file in blog_files) {
  x <- readLines(blog_file)
  y <- gsub(server_url, new_url, x)
  cat(y, file=blog_file, sep="\n")
}


library(yaml)
library(bibtex)
library(rjson)

path_pubs <- "https://raw.githubusercontent.com/guillaumelobet/guillaumelobet.github.io/master/_data/publications.yml"
path_pres <- "https://raw.githubusercontent.com/guillaumelobet/guillaumelobet.github.io/master/_data/presentations.yml"
path_bib <-"~/Dropbox/science/admin/cv/default/bibliography_auto.bib"

pubs <- read_yaml(path_pubs)
pres <- read_yaml(path_pres)
art_count <- 1

bib <- "%%% ----------------- PREPRINTS -------------------- %%%\n\n"

for(p in pubs){
  if(p$type == "journal"){
    if(length(p$preprint) > 0){   
      
      print(p$title)
      
      auths <- p$authors
      auths <- gsub(", ", "--", auths)
      auths <- gsub(" ", ", ", auths)
      auths <- gsub("--", " and ", auths)
      
      bib <- paste0(bib, "@article{art-",art_count,",\n")
      bib <- paste0(bib, "\t title = {",p$title,"},\n")
      bib <- paste0(bib, "\t author = {",auths,"},\n")
      bib <- paste0(bib, "\t year = {",p$year,"},\n")
      
      dims <- NULL
      altm <- NULL
      if(length(p$doi) > 0){
        
        altm <- tryCatch({
          fromJSON(readLines(paste0("https://api.altmetric.com/v1/doi/",p$doi)))
        }, error = function(e) {
        })
        
        dims <- tryCatch({
          fromJSON(readLines(paste0("https://metrics-api.dimensions.ai/doi/",p$doi)))
        }, error = function(e) {
        })
        
        bib <- paste0(bib, "\t doi = {",p$doi,"},\n")
        
        if(!is.null(altm)){
          bib <- paste0(bib, "\t altmetric = {",round(altm$score),"},\n")
        }else{
          bib <- paste0(bib, "\t altmetric = {-},\n")
        }
        if(!is.null(dims)){
          bib <- paste0(bib, "\t citations = {",dims$times_cited,"},\n")
          bib <- paste0(bib, "\t fcr = {",dims$field_citation_ratio,"},\n")
        }else{
          bib <- paste0(bib, "\t citations = {-},\n")
          bib <- paste0(bib, "\t fcr = {-},\n")
        }
      }
      if(length(p$pmid) > 0){
        bib <-paste0(bib, "\t url = {http://www.ncbi.nlm.nih.gov/pubmed/",p$pmid,"},\n")
      }else{
        bib <-paste0(bib, "\t url = {http://dx.doi.org/",p$doi,"},\n")
      }
      if(length(p$quote) > 0){
        bib <-paste0(bib, "\t quote = {",p$quote,"},\n")
      }
      bib <- paste0(bib, "\t keywords = {preprint}\n")      
      bib <- paste0(bib, "}\n\n")      
      art_count <- art_count + 1
    }
  }
}

bib <- paste0(bib, "\n\n %%% ----------------- PUBLISHED ARTICLES -------------------- %%%\n\n")

cites <-c()
published <- 0
journals <- c()
years <- c()

for(p in pubs){
  if(p$type == "journal"){
    if(length(p$preprint) == 0){   
      if(length(p$doi) > 0){
        print(p$title)
        
        auths <- p$authors
        auths <- gsub(", ", "--", auths)
        auths <- gsub(" ", ", ", auths)
        auths <- gsub("--", " and ", auths)
        
        bib <- paste0(bib, "@article{art-",art_count,",\n")
        bib <- paste0(bib, "\t title = {",p$title,"},\n")
        bib <- paste0(bib, "\t author = {",auths,"},\n")
        bib <- paste0(bib, "\t year = {",p$year,"},\n")
        
        dims <- NULL
        altm <- NULL

      
        altm <- tryCatch({
          fromJSON(readLines(paste0("https://api.altmetric.com/v1/doi/",p$doi)))
        }, error = function(e) {
        })
        
        dims <- tryCatch({
          fromJSON(readLines(paste0("https://metrics-api.dimensions.ai/doi/",p$doi)))
        }, error = function(e) {
        })

        bib <- paste0(bib, "\t doi = {",p$doi,"},\n")
        
        if(!is.null(altm)){
          bib <- paste0(bib, "\t altmetric = {",round(altm$score),"},\n")
        }else{
          bib <- paste0(bib, "\t altmetric = {-},\n")
        }
        if(!is.null(dims)){
          bib <- paste0(bib, "\t citations = {",dims$times_cited,"},\n")
          cites <-c(cites, dims$times_cited)
          journals <-c(journals, p$journal)
          years <-c(years, p$year)
          bib <- paste0(bib, "\t fcr = {",dims$field_citation_ratio,"},\n")
        }else{
          bib <- paste0(bib, "\t citations = {-},\n")
          bib <- paste0(bib, "\t fcr = {-},\n")
        }
        
        if(length(p$pmid) > 0){
          bib <-paste0(bib, "\t url = {http://www.ncbi.nlm.nih.gov/pubmed/",p$pmid,"},\n")
        }else{
          bib <-paste0(bib, "\t url = {http://dx.doi.org/",p$doi,"},\n")
        }
        if(length(p$quote) > 0){
          bib <-paste0(bib, "\t quote = {",p$quote,"},\n")
        }
        bib <- paste0(bib, "\t keywords = {accepted}\n")      
        bib <- paste0(bib, "}\n\n")      
        published <- published +1
      }
      art_count <- art_count + 1
      
    }
  }
}


bib <- paste0(bib, "\n\n %%% ----------------- INVITED CONFERENCES -------------------- %%%\n\n")
invited <- 0
for(p in pres){
  if(length(p$status) > 0){
      print(p$title)
      invited <- invited + 1
      year <- strsplit(p$date, " ")[[1]][2]
      loc <- paste0(strsplit(p$location, ",")[[1]][2],", ",strsplit(p$location, ",")[[1]][3])
      conf <- strsplit(p$location, ",")[[1]][1]
      
      bib <- paste0(bib, "@inproceedings{pres-",art_count,",\n")
      bib <- paste0(bib, "\t title = {",p$title,"},\n")
      bib <- paste0(bib, "\t author = {Lobet, G},\n")
      bib <- paste0(bib, "\t year = {",year,"},\n")
      bib <- paste0(bib, "\t booktitle = {",conf,"},\n")
      bib <- paste0(bib, "\t address = {",loc,"},\n")
      if(length(p$slides_url) > 0){
        bib <-paste0(bib, "\t url = {",p$slides_url,"},\n")
      }
      bib <- paste0(bib, "\t keywords = {invited}\n")      
      bib <- paste0(bib, "}\n\n")      
      art_count <- art_count + 1
  }
}

bib <- paste0(bib, "\n\n %%% ----------------- CONFERENCES -------------------- %%%\n\n")

for(p in pres){
  if(length(p$status) == 0){
    print(p$title)
    
    year <- strsplit(p$date, " ")[[1]][2]
    loc <- paste0(strsplit(p$location, ",")[[1]][2],", ",strsplit(p$location, ",")[[1]][3])
    conf <- strsplit(p$location, ",")[[1]][1]
    
    bib <- paste0(bib, "@inproceedings{pres-",art_count,",\n")
    bib <- paste0(bib, "\t title = {",p$title,"},\n")
    bib <- paste0(bib, "\t author = {Lobet, G},\n")
    bib <- paste0(bib, "\t year = {",year,"},\n")
    bib <- paste0(bib, "\t booktitle = {",conf,"},\n")
    bib <- paste0(bib, "\t address = {",loc,"},\n")
    if(length(p$slides_url) > 0){
      bib <-paste0(bib, "\t url = {",p$slides_url,"},\n")
    }
    bib <- paste0(bib, "\t keywords = {spont}\n")      
    bib <- paste0(bib, "}\n\n")      
    art_count <- art_count + 1
  }
}

cat(bib, file = path_bib)

cites <- sort(cites, decreasing = T)
hindex <- 0
for(i in 1:length(cites)){
  if(i <= cites[i]) hindex <- hindex+1
} 
mess <- "------------------------------------\n"
mess <- paste0(mess, "published articles = ",published,"\n")
mess <- paste0(mess, "h-index = ",hindex,"\n")
mess <- paste0(mess, "total citations = ",sum(cites),"\n")
mess <- paste0(mess, "invitation to conferences = ",invited,"\n")
message(mess)








# Recompile the PDF
unlink("/Users/g.lobet/Dropbox/science/admin/cv/auto/tmp/", force = T, recursive = T)
unlink("/Users/g.lobet/Dropbox/science/admin/cv/auto/cv_g_lobet.pdf", force = T, recursive = T)
dir.create("/Users/g.lobet/Dropbox/science/admin/cv/auto/tmp")
file.copy(from="/Users/g.lobet/Dropbox/science/admin/cv/auto/cv_g_lobet.tex", to="/Users/g.lobet/Dropbox/science/admin/cv/auto/tmp/cv_g_lobet.tex")
file.copy(from="/Users/g.lobet/Dropbox/science/admin/cv/auto/bibliography_auto.bib", to="/Users/g.lobet/Dropbox/science/admin/cv/auto/tmp/bibliography_auto.bib")
file.copy(from="/Users/g.lobet/Dropbox/science/admin/cv/auto/fontawesome.sty", to="/Users/g.lobet/Dropbox/science/admin/cv/auto/tmp/fontawesome.sty")
file.copy(from="/Users/g.lobet/Dropbox/science/admin/cv/auto/friggeri-cv.cls", to="/Users/g.lobet/Dropbox/science/admin/cv/auto/tmp/friggeri-cv.cls")
file.copy(from="/Users/g.lobet/Dropbox/science/admin/cv/auto/workoverview.pdf", to="/Users/g.lobet/Dropbox/science/admin/cv/auto/tmp/workoverview.pdf")
system("/usr/local/texlive/2016/bin/x86_64-darwin/xelatex -file-line-error -interaction=nonstopmode -synctex=1 -output-directory=/Users/g.lobet/Dropbox/science/admin/cv/auto/tmp cv_g_lobet.tex")
system("/usr/local/texlive/2016/bin/x86_64-darwin/biber /Users/g.lobet/Dropbox/science/admin/cv/auto/tmp/cv_g_lobet.bcf")
system("/usr/local/texlive/2016/bin/x86_64-darwin/xelatex -file-line-error -interaction=nonstopmode -synctex=1 -output-directory=/Users/g.lobet/Dropbox/science/admin/cv/auto/tmp cv_g_lobet.tex")
system("/usr/local/texlive/2016/bin/x86_64-darwin/xelatex -file-line-error -interaction=nonstopmode -synctex=1 -output-directory=/Users/g.lobet/Dropbox/science/admin/cv/auto/tmp cv_g_lobet.tex")
file.rename(from="/Users/g.lobet/Dropbox/science/admin/cv/auto/tmp/cv_g_lobet.pdf", to="/Users/g.lobet/Dropbox/science/admin/cv/auto/cv_g_lobet.pdf")

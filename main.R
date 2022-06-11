# Introduce parameters somehow

main <- function(mainfile = "main.Rnw", texcompiler = "xelatex", l_code = "./scripts/",  l_text = "/tex", rerun = FALSE, reload = FALSE, clean = TRUE, overleaf = FALSE, l_overleaf = "", open = FALSE) {
    library(knitr)
    library(here)
    
    if (rerun) {
        files = list.files( l_code, full.names = T)
        files = files [ grepl("\\.[rR]$" , files) ]
        for (f in files) {
            print(paste0("Sourcing the script file", " ", f))
            {try(source(f, local = F, echo = F))}
            print(paste0("Done", " ", f))
        }
        save.image(paste0(here::here(), "/resources/thesis.RData"))
    }
    if (reload) {
        load(paste0(here::here(), "/resources/thesis.RData"))
    }

    # if (load) {
    #     load_files = list.files( l_load, full.names = T)
    #     load_files_rds = load_files [ grepl("\\.rds$" , load_files) ]
    #     load_files_rdata = load_files [ grepl("\\.RData$" , load_files) ]
    #     for (f in load_files_rds) {
    #         print(paste0("Loading the RDS file", " ", f))
    #         {try(readRDS(f))}
    #         print(paste0("Done", " ", f))
    #     }   
    #     for (f in load_files_rdata) {
    #         print(paste0("Loading the RData file", " ", f))
    #         {try(load(f))}
    #         print(paste0("Done", " ", f))
    #     }
    # }
    setwd(paste0(here::here(), l_text))

    # options(tinytex.clean = FALSE)
    # system("makeglossaries 'main-knitr'")
    
    knitr::Sweave2knitr( mainfile )
    knitr::knit2pdf( compiler = texcompiler, input = "main-knitr.Rnw" , min_times = 3)
    
    if (clean) {
        system(
            paste0(
                "cd '",
                getwd(),
                "';rm *.lof;rm *.lot;rm *.bbl;rm *.gz;rm *.toc;rm *.log;rm *.glo;rm *.aux;rm *.ist;rm *-knitr.Rnw;rm *-wordcount.tex;rm *.aux.copy;rm *.auxlock;rm *.aux.for;rm *.for.tmp;rm *.for"
            )
        )
    }
    
    if (overleaf) {
        command = sprintf("cp -R %s %s", paste0(l_text,"/"), l_overleaf)
        system(command, intern = T)
    }
    
    if (open) {
        system2("open", args = "main-knitr.pdf", wait = F)
    }
    setwd(here::here())
}
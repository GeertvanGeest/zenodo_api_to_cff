library(yaml)

source(".env.R")

resp <- httr::GET(httr::modify_url("https://zenodo.org",
    path = "/api/records/5703106"
),
query = list(
    access_token = acc_token
)
)
parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
    simplifyVector = FALSE
)

cff_list <- list(
    "cff-version" = "1.2.0",
    "keywords" = parsed$metadata$keywords,
    "license" = parsed$metadata$license,
    "abstract" = parsed$metadata$description,
    "title" = parsed$metadata$title,
    "date-released" = parsed$metadata$publication_date
)

identifiers <- list(
    list(
        description = "all versions",
        type = "doi",
        value = parsed$conceptdoi
    ),
    list(
        description = "latest version",
        type = "doi",
        value = parsed$doi
    )
)

cff_list$identifiers <- identifiers

get_repo_link <- function(related_identifiers, cff_list) {
    is_supp <- sapply(related_identifiers, function(x) {
        x$relation == "isSupplementTo"
    })
    if (sum(is_supp) != 1) {
        message("No supplement found")
        return(cff_list)
    }
    gh_url <- related_identifiers[is_supp][[1]]$identifier
    if (startsWith(gh_url, "https://github.com")) {
        gh_url <- strsplit(gh_url, "\\/")[[1]][1:5] |> paste(collapse = "/")
        cff_list$`repository-code` <- gh_url
        return(cff_list)
    } else {
        message("Supplement is not github url")
        return(cff_list)
    }
}

cff_list <- get_repo_link(parsed$metadata$related_identifiers, cff_list)

convert_author_info <- function(person_list, cff_list) {
    cff_list$authors <- lapply(person_list, function(x) {
        name <- x$name
        name_spl <- strsplit(name, ",")[[1]] |> trimws()
        if (length(name_spl) == 2) {
            out_list <- list(
                "family-names" = name_spl[1],
                "given-names" = name_spl[2]
            )
        } else {
            out_list <- list(
                "name" = name
            )
        }
        if ("orcid" %in% names(x)) {
            out_list <- c(out_list, orcid = paste0(
                "https://orcid.org/",
                x$orcid
            ))
        }
        return(out_list)
    })

    return(cff_list)
}

cff_list <- convert_author_info(parsed$metadata$creators, cff_list)

cff_list$message <- "If you use this training material, please cite it using these metadata."


write_yaml(cff_list, file = "test_yaml.yml")

#' Dataset that matches Istanbul towns with the continent part
#' @export
town_to_istanbul_part <- tibble::frame_data(
  ~town,          ~city,
  "Adalar",		  	"İstanbul (Anadolu)",
  "Ataşehir",			"İstanbul (Anadolu)",
  "Beykoz",	  		"İstanbul (Anadolu)",
  "Çekmeköy",			"İstanbul (Anadolu)",
  "Kadıköy",			"İstanbul (Anadolu)",
  "Kartal",	  		"İstanbul (Anadolu)",
  "Maltepe",			"İstanbul (Anadolu)",
  "Pendik",		  	"İstanbul (Anadolu)",
  "Sancaktepe",		"İstanbul (Anadolu)",
  "Şile",		    	"İstanbul (Anadolu)",
  "Sultanbeyli",	"İstanbul (Anadolu)",
  "Tuzla",		  	"İstanbul (Anadolu)",
  "Ümraniye",			"İstanbul (Anadolu)",
  "Üsküdar",			"İstanbul (Anadolu)",
  "Arnavutköy",		"İstanbul (Avrupa)",
  "Avcılar",			"İstanbul (Avrupa)",
  "Bağcılar",			"İstanbul (Avrupa)",
  "Bahçelievler",	"İstanbul (Avrupa)",
  "Bakırköy",			"İstanbul (Avrupa)",
  "Başakşehir",		"İstanbul (Avrupa)",
  "Bayrampaşa",		"İstanbul (Avrupa)",
  "Beşiktaş",			"İstanbul (Avrupa)",
  "Beylikdüzü",		"İstanbul (Avrupa)",
  "Beyoğlu",			"İstanbul (Avrupa)",
  "Büyükçekmece",	"İstanbul (Avrupa)",
  "Çatalca",			"İstanbul (Avrupa)",
  "Esenler",			"İstanbul (Avrupa)",
  "Esenyurt",			"İstanbul (Avrupa)",
  "Eyüp",			    "İstanbul (Avrupa)",
  "Fatih",	  		"İstanbul (Avrupa)",
  "Gaziosmanpaşa","İstanbul (Avrupa)",
  "Güngören",			"İstanbul (Avrupa)",
  "Kağıthane",		"İstanbul (Avrupa)",
  "Küçükçekmece",	"İstanbul (Avrupa)",
  "Sarıyer",			"İstanbul (Avrupa)",
  "Silivri",			"İstanbul (Avrupa)",
  "Şişli",		  	"İstanbul (Avrupa)",
  "Sultangazi",		"İstanbul (Avrupa)",
  "Zeytinburnu",	"İstanbul (Avrupa)")

#' Add Istanbul continent part to city
#'
#' Given a frame with \code{city} and \code{town} variables, if there are
#' towns for Istanbul, it add the continent information to the \code{city}
#' variable of that town.
#'
#' @param frame A frame with \code{city} and \code{town} variables.
#' @export
add_istanbul_part <- function(frame){
  frame %>%
    # Taking data with town İstanbul
    dplyr::filter(city != "İstanbul") %>%

    # Adding the new data with İstanbul parts
    dplyr::bind_rows(
      # Preparing the new İstanbul data
      frame %>%
        dplyr::filter(city == "İstanbul") %>%
        dplyr::select(-city) %>% # This will come from `town_to_istanbul_part`
        dplyr::left_join(town_to_istanbul_part, by = "town")
    )
}

#' @importFrom dplyr %>%
NULL


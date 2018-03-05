.onAttach <- function(libname, pkgname) {
    pkgVersion <- utils::packageDescription(pkgname, fields="Version")
    msg <- paste0(pkgname, " v", pkgVersion)

    citation <- paste0(
      "\n\nIf you use '", pkgname, "' in published research, please cite:\n",
      "  the github website for now https://github.com/thackl/ggworldmap",
      "\n\nIf you use the 'Volcanic Eruptions' sample data, please cite:\n",
      "  National Geophysical Data Center / World Data Service (NGDC/WDS):\n",
      "  Significant Volcanic Eruptions Database. National Geophysical Data\n",
      "  Center, NOAA. doi:10.7289/V5JW8BSH\n"
    )

    packageStartupMessage(paste0(msg, citation))
}

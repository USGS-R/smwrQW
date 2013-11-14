#'Column Names
#'
#'Create valid, unique column names for water-quality constituents
#'
#'The arguments \code{name} and \code{short} are expected to be columns
#'retreived by those arguments in the function \code{pcodeNWISqw}.
#'
#'@param params the parameter codes.
#'@param name the long-name of the parameter code.
#'@param short the short name of teh parameter code.
#'@return A data frame containing the columns \code{parm_cd} and
#'\code{col_name}.
#'@note This function is a support function designed to be called within other
#'functions.
#'@seealso \code{\link{pcodeNWISqw}}, \code{\link{pcodeColNames}}
#'@keywords utilities
#'@export
makeColNames <- function(params, name, short) {
  ## Coding history:
  ##    2012Sep11 DLLorenz original Coding
  ##    2012Dec28 DLLorenz Roxygenized
  ##    2012Dec28          This version
  ##
  ## Get the predefined names 
  if(exists("pcode.ColNames", mode="list")) {# copy user's version
    pcode.ColNames <- get("pcode.ColNames")
    ## Force names to be valid and unique
    pcode.ColNames$col_name <- make.names(col_name, unique=TRUE)
  }
  else
    pcode.ColNames <- pcodeColNames()
  ## Are we done?
  done <- params %in% pcode.ColNames$parm_cd
  retval <- pcode.ColNames[pcode.ColNames$parm_cd %in% params, ]
  if(all(done))
    return(retval)
  ## Generate new names
  todo <- !done
  name <- name[todo]
  short <- short[todo]
  params <- params[todo]
  col_name <- character(sum(todo))
  ## If short is blank, then generate name from the first 2 entries in name
  toshort <- short == ""
  if(any(toshort)) {
    ## make name from an abbreviation of the first 2 parts of name
    col_name[toshort] <- sapply(as.list(name[toshort]), function(x)
                                abbreviate(paste(strsplit(x, split=",")[[1]][c(1,2)], collapse=','), 20))
    col_name[toshort] <- paste(col_name[toshort], params[toshort], sep='.')
  }
  todo <- !toshort
  col_name[todo] <- abbreviate(short[todo], 20)
  ## append special tags
  bed <- name[todo] %cn% "bed sediment"
  col_name[todo][bed] <- paste(col_name[todo][bed], "_bed", sep='')
  whowat <-  name[todo] %cn% "water, unfiltered"
  col_name[todo][whowat] <- paste(col_name[todo][whowat], "_WW", sep='')
  susp <- name[todo] %cn% "suspended sediment"
  col_name[todo][susp] <- paste(col_name[todo][susp], "_susp", sep='')
  soil <- name[todo] %cn% "soil"
  col_name[todo][soil] <- paste(col_name[todo][soil], "_soil", sep='')
  tiss <- name[todo] %cn% "tissue"
  col_name[todo][tiss] <- paste(col_name[todo][tiss], "_tiss", sep='')
  ## Any duplicated?
  dups <- duplicated(col_name) | duplicated(col_name, fromLast=TRUE)
  col_name[dups] <- paste(col_name[dups], params[dups], sep='.')
  retval <- rbind(retval, data.frame(parm_cd=params, col_name=col_name))
  retval$col_name <- make.names(retval$col_name)
  return(retval)
}

pcodeColNames <- function() ## Provide column name data frame
  data.frame(parm_cd=c("00010", "00011", "00070", "00075", "00076", "00095",
                    "00096", "00300", "00301", "00400", "00402", "00403", "00410", "00411",
                    "00413", "00415", "00416", "00417", "00418", "00419", "00420", "00421",
                    "00425", "00430", "00431", "00440", "00445", "00446", "00447", "00448",
                    "00449", "00450", "00451", "00452", "00453", "00500", "00505", "00510",
                    "00515", "00520", "00525", "00545", "00600", "00602", "00605", "00607",
                    "00608", "00610", "00613", "00615", "00618", "00620", "00623", "00625",
                    "00630", "00631", "00635", "00636", "00650", "00653", "00660", "00665",
                    "00666", "00670", "00671", "00673", "00680", "00681", "00682", "00685",
                    "00690", "00691", "00720", "00723", "00740", "00745", "00746", "00900",
                    "00901", "00902", "00903", "00904", "00905", "00906", "00907", "00908",
                    "00909", "00910", "00915", "00916", "00920", "00921", "00923", "00925",
                    "00927", "00929", "00930", "00932", "00933", "00935", "00937", "00939",
                    "00940", "00945", "00946", "00950", "00951", "00955", "00956", "01045",
                    "01046", "01047", "01048", "01049", "01051", "01055", "01056", "01065",
                    "01067", "01074", "01075", "01077", "01079", "01080", "01082", "01084",
                    "01090", "01092", "01094", "01100", "01102", "01114", "01123", "01140",
                    "01142", "01150", "01152", "01154", "01155", "29801", "29802", "29803",
                    "29804", "29805", "29806", "29807", "29808", "29809", "29810", "29811",
                    "29812", "29813", "39036", "39086", "39087", "46005", "50286", "50287",
                    "61028", "62854", "62855", "63675", "63676", "63679", "63680", "63681",
                    "63682", "63683", "63684", "63689", "63786", "63787", "63788", "63789",
                    "64832", "64903", "70299", "70300", "70301", "70348", "70349", "70507",
                    "71830", "71831", "71832", "71833", "71834", "71845", "71846", "71850",
                    "71851", "71855", "71856", "71865", "71870", "71886", "71887", "71888",
                    "71890", "71900", "71901", "72002", "72003", "72019", "80154", "82079",
                    "90095", "90410", "90430", "90440", "90445", "90859", "90860", "91003",
                    "91004", "91005", "91053", "91054", "95410", "95430", "95440", "95445",
                    "95830", "95902", "99032", "99113", "99114", "99116", "99118", "99119",
                    "99120", "99121", "99123", "99124", "99125", "99126", "99127", "99128",
                    "99220", "99402", "99403", "99430", "99440", "99445", "99830", "99889",
                    "99890", "99891", "99892", "99893", "99894", "99895"),
             col_name=c("WaterTempDegC", "WaterTempDegF", "Turbidity.JTU", "Turbidity.SiO2",
                   "Turbidity.NTU", "SpecCond_WW.Fld", "Salinity.mg.L", "OxygenDissolved",
                   "OxygenDissolved.Pct", "pH_WW.Fld", "SpecCond_WW.uncorr", "pH_WW.Lab",
                   "ANC_FET.Fld", "ANC_Methylorange", "ANC_Gran", "ANC_Phenolphthalein",
                   "ANC_Inc.Lab", "ANC_FET.Lab.00410", "ALK_FET.Fld.00418", "ANC_Inc.Fld",
                   "Hydroxide_WW", "ALK_FET.Lab.00421", "Bicarbonate_WW.CaCO3",
                   "Carbonate_WW.CaCO3", "ANC", "Bicarbonate_WW_FET.Fld", "Carbonate_WW_FET.Fld",
                   "Carbonate_WW_Inc.Lab.00446", "Carbonate_WW_Inc.Fld.00447",
                   "Carbonate_WW_FET.Lab.00448", "Bicarbonate_WW_Inc.Lab.00449",
                   "Bicarbonate_WW_Inc.Fld.00450", "Bicarbonate_WW_FET.Lab.00451",
                   "Carbonate_Inc.Fld", "Bicarbonate_Inc.Fld", "ResidueEvap105_WW",
                   "ResidueLossIgnition_WW", "ResidueEvap_WW", "ResidueEvap105",
                   "ResidueLossIgnition", "ResidueEvap", "ResidueSettleable", "NitrogenTotal_WW",
                   "NitrogenTotal.sum", "NitrogenOrg_WW", "NitrogenOrg", "Ammonia.N",
                   "Ammonia_WW.N", "Nitrite.N", "Nitrite_WW.N", "Nitrate.N", "Nitrate_WN.N",
                   "Kjeldahl.N.00623", "Kjeldahl_WW.N.00625", "NO2PlusNO3_WW.N", "NO2PlusNO3.N",
                   "Kjeldahl_WW.N.00635", "Kjeldahl.N.00636", "Phosphate_WW", "Phosphate",
                   "OrthoPhosphate.PO4", "Phosphorus_WW.P", "Phosphorus.P", "PhosphorusOrg_WW",
                   "OrthoPhosphate.P", "PhosphorusOrg", "CarbonOrg_WW", "CarbonOrg", "CarbonTot",
                   "CarbonInorg_WW", "CarbonTot_WW", "CarbonInorg", "Cyanide_WW", "Cyanide",
                   "Sulfite", "Sulfide_WW", "Sulfide", "Hardness.CaCO3", "CO3Hardness_WW",
                   "HardnessNonCO3_WW.Fld", "HardnessNonCO3_WW.Lab", "HardnessNonCO3.Fld",
                   "HardnessNonCO3.Lab", "Hardness.calc", "Hardness_WW.calc", "Hardness.meas",
                   "Hardness_WW.meas", "Calcium_WW.CaCO3", "Calcium", "Calcium_WW",
                   "Magnesium_WW.CaCO3", "Magnesium_WW.00921", "Sodium_WW.00923", "Magnesium",
                   "Magnesium_WW.00927", "Sodium_WW.00929", "Sodium", "SodiumPctMajor",
                   "SodiumPlusPotassium.Na", "Potassium", "Potassium_WW.00937",
                   "Potassium_WW.00939", "Chloride", "Sulfate", "Sulfate_WW", "Fluoride",
                   "Fluoride_WW", "Silica", "Silica_WW", "Iron_WW", "Iron", "IronFerrous",
                   "IronFerricPlusFerrous", "Lead", "Lead_WW.01051", "Manganese_WW.01055",
                   "Manganese", "Nickel", "Nickel_WW.01067", "Nickel_WW.01074", "Silver",
                   "Silver_WW.01077", "Silver_WW.01079", "Strontium", "Strontium_WW.01082",
                   "Strontium_WW.01084", "Zinc", "Zinc_WW", "ZincTotRecWat", "Tin", "Tin_WW",
                   "Lead_WW.01114", "Manganese_WW.01123", "Silicon", "Silicon_WW", "Titanium",
                   "Titanium_WW", "Tungsten_WW", "Tungsten", "ALK_FET.Lab.29801", "ALK_Gran.Fld",
                   "ALK_Gran.Lab", "Bicarbonate_FET.Fld", "Bicarbonate_FET.Lab",
                   "Bicarbonate_Inc.lab", "Carbonate_FET.Fld", "Carbonate_FET.Lab",
                   "Carbonate_Inc.Lab", "Hydroxide_FET.Fld", "Hydroxide_FET.Lab",
                   "Hydroxide_Inc.Lab", "ANC_Gran.Fld", "ALK_FET.Fld.39036", "ALK_Inc.Fld",
                   "ALK_Inc.Lab", "ANC_BromthymolBlue", "Mercury_WW.ng.L", "Mercury.ng.L",
                   "Turbidity.Fld.NTU", "NitrogenTotal", "NitrogenTotal_WW.sum",
                   "Turbidity.63675", "Turbidity.63676", "Turbidity.63679", "Turbidity.63680",
                   "Turbidity.63681", "Turbidity.63682", "Turbidity.63683", "Turbidity.63684",
                   "Bromide_WW", "Bicarbonate_Gran.Fld", "Bicarbonate_Gran.Lab",
                   "Carbonate_Gran.Fld", "Carbonate_Gran.lab", "Nitrate.N.ug.L", "Iodide_WW",
                   "SuspSolids", "ResidueEvap180", "ResidueSumConstituents",
                   "ResidueSettleable_WW", "ResidueUnsettleable", "OrthoPhosphate_WW.P",
                   "Hydroxide_WW_FET.Fld", "Hydroxide_WW_Inc.Lab", "Hydroxide_WW_Inc.Fld.71832",
                   "Hydroxide_WW_FET.Lab.71833", "Hydroxide_Inc.Fld", "Ammonia_WW.NH4",
                   "Ammonia.NH4", "Nitrate_WW.NO3", "Nitrate.NO3", "Nitrite_WW.NO2",
                   "Nitrite.NO2", "Iodide", "Bromide", "Phosphorus_WW.PO4",
                   "NitrogenTotal_WW.NO3", "Phosphorus.PO4", "Mercury.ug.L", "Mercury_WW.71900",
                   "Mercury_WW.71901", "DepthToTopOfWatBearingZone", "DepthToBotOfWatBearingZone",
                   "DepthBlwLandSurface", "SuspSed", "Turbidity.Lab.NTU", "SpecCond_WW.Lab",
                   "ANC_FET.Lab.00417", "Carbonate_WW_Inc.Lab.CaCO3",
                   "Bicarbonate_WW_Inc.lab.90440", "Carbonate_WW_Inc.Lab.90445",
                   "NO2PlusNO3.N.calc", "Salinity.practical", "Nitrate.NO3.ug.L",
                   "OrthoPhosphate.P.uq.L", "Sulfate.uq.L", "Sodium.uq.L", "Potassium.ug.L",
                   "ANC_FET.Lab.90410", "Carbonate_WW_FET.Lab.CaCO3",
                   "Bicarbonate_WW_FET.Lab.95440", "Carbonate_WW_FET.Lab.95445",
                   "Hydroxide_WW_FET.Lab.95830", "HardnessNonCO3", "IronFerrous_WW",
                   "Sulfate.Fld", "IronFerrous.Fld", "Nitrite.N.Fld", "Sulfide.Fld",
                   "Sulfide_WW.Fld", "Ammonia.N.Fld", "Nitrate.N.Fld", "Ammonia_WW.N.Fld",
                   "Nitrate_WW.Fld", "Nitrite_WW.Fld", "OrthoPhosphate_WW.PO4", "Sulfate_WW.Fld",
                   "IronFerrous_WW.Fld", "Chloride_WW", "ResidueSuspended", "ResidueTotal",
                   "Carbonate_WW_Inc.Fld.CaCO3", "Bicarbonate_WW_Inc.Fld.99440",
                   "Carbonate_WW_Inc.Fld.99445", "Hydroxide_WW_Inc.Fld.99830", "NO2PlusNO3.N.Fld",
                   "Sulfate.uncorr", "Phosphorus_WW.jirka", "Kjeldahl_WW.N.jirka",
                   "Phosphorus.jirka", "Kjeldahl.N.jirka", "Silver_WW.99895"),
             stringsAsFactors=FALSE)

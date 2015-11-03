# ======================================================================
# Eworx S.A.- 2012
# Author kp@eworx.gr
# ======================================================================
# Contains the configurable settings of the process
# ======================================================================

valid.column.types <- c("CountryCode", "CountryName", "Subset", "Question", "QuestionMean", "Weight", "Delete")
substitutable.column.types <- c("Subset", "Question", "QuestionMean")
user.substitutable.column.types <- c("Subset", "Question")
mean.substitutable.column.types <- "QuestionMean" 
value.delete <- "DELETE"

valid.values.localization.columns <- c("EntryType", "Column", "Value", "TranslatedValue")
valid.questions.localization.columns <- c("QuestionCode", "Category", "WebTranslation", "FullTranslation", "Note")

# ======================================================================

step0.input.file.name  <- "spss.sav"
step0.output.file.name <- "analyse.columns.xls"
step1.input.file.name  <- "analysed.columns.xls"
step1.output.file.name <- "substitutable.columns.values.xls"
step1.means.output.file.name <- "substitutable.means.columns.values.xls"
step2.input.file.name  <- "substitute.columns.values.xls"
survey.refined.data.file.name <- "survey.refined.data.RData"

countries.code.name.status.file.name <- "defaultData/countries.code.name.xls"
DVT.key.localized.values.file.name <- "defaultData/DVT.translate.values.XX.xls"
DVS.key.localized.values.file.name <- "defaultData/DVS.translate.values.XX.xls"

step2.values.output.file.name     <- "translate.values.XX.xls"
step2.questions.output.file.name  <- "translate.questions.XX.xls"
step3.values.input.file.name.pattern <- "translated.values.*.xls"
step3.questions.input.file.name.pattern <- "translated.questions.*.xls"

# ======================================================================

valid.parameters.php <- "valid.parameters.php"
valid.survey.locales.php <- "valid.locales.php"

# ======================================================================

# ======================================================================
# Eworx S.A. - 2012 / 2013
# Author kp@eworx.gr
# ======================================================================
# Contains the configurable settings of the process
# ======================================================================

column.type.Weight <- "Weight"
column.type.Delete <- "Delete"
column.type.Question <- "Question"
column.type.QuestionMean <- "QuestionMean"
column.type.Subset <- "Subset"
column.type.CountryName <- "CountryName"
column.type.CountryCode <- "CountryCode"
column.type.QuestionMR <- "QuestionMR"

# ======================================================================

valid.column.types <- c(column.type.CountryCode, column.type.CountryName, column.type.Subset, column.type.Question, column.type.QuestionMean, column.type.QuestionMR , column.type.Weight, column.type.Delete)

substitutable.column.types <- c(column.type.Subset, column.type.Question, column.type.QuestionMean)
user.substitutable.column.types <- c("Subset", "Question")
mean.substitutable.column.types <- "QuestionMean"

value.delete <- column.type.Delete

multiple.response.true <- "Mention"
multiple.response.false <- "Did not mention"

valid.values.localization.columns <- c("EntryType", "Column", "Value", "TranslatedValue")
valid.questions.localization.columns <- c("QuestionCode", "Category", "WebTranslation", "FullTranslation", "Note")

# ======================================================================

step0.input.file.name  <- "spss.sav"
step0.output.file.name <- "analyse.columns.xls"

step1.input.file.name  <- "analysed.columns.xls"
step1.output.file.name <- "substitutable.columns.values.xls"
step1.means.output.file.name <- "substitutable.means.columns.values.xls"


step1.multiple.output.file.name <- "substitutable.multiple.response.columns.values.xls" # variable subject to be dropped 

step2.input.file.name  <- "substitute.columns.values.xls"
step2.means.input.file.name <- "substitute.means.columns.values.xls"

step2.values.output.file.name     <- "translate.values.XX.xls"
step2.questions.output.file.name  <- "translate.questions.XX.xls"

survey.refined.data.file.name <- "survey.refined.data.RData"
survey.refined.mrq.data.file.name<- "survey.refined.mrq.data.RData"

countries.code.name.status.file.name <- "defaultData/countries.code.name.xls"

DVT.key.localized.values.file.name <- "defaultData/DVT.translate.values.XX.xls"
DVS.key.localized.values.file.name <- "defaultData/DVS.translate.values.XX.xls"

step3.values.input.file.name.pattern <- "translated.values.*.xls"
step3.questions.input.file.name.pattern <- "translated.questions.*.xls"

# ======================================================================

valid.parameters.php <- "valid.parameters.php"
valid.survey.locales.php <- "valid.locales.php"

# ======================================================================

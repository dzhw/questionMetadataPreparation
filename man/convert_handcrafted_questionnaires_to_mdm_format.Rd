% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/convert_handcrafted_questionnaire_to_mdm_format.R
\name{convert_handcrafted_questionnaires_to_mdm_format}
\alias{convert_handcrafted_questionnaires_to_mdm_format}
\title{Convert Handcrafted Questionnaires to MDM Format}
\usage{
convert_handcrafted_questionnaires_to_mdm_format(
  input_directory = file.path(".", "pages"),
  output_directory = file.path(".", "mdm", "questions"),
  images_subdirectory = file.path("Bilder", "png")
)
}
\arguments{
\item{input_directory}{Input path, e.g. "./questions"}

\item{output_directory}{Output directory, e.g. "./mdm/questions", will
be created if it does not exist or will be overwritten otherwise}

\item{images_subdirectory}{Path relative to input_directory containing the
images, e.g. "Bilder/png"}
}
\description{
This script converts the handcrafted questionnaires which contains an excel
and many images of several questionnaires into the format which can be
uploaded to the \href{https://metadata.fdz.dzhw.eu}{MDM}. The handcrafted
questionnaire must have the following format:
\preformatted{
|--questions
  |--questions*.xlsx (two sheets, questions and images)
  |--Bilder
    |--png
      |--ins1
        |--5_1.png (must match the filename in the images excel sheet)
        |--5_2.png (must match the filename in the images excel sheet)
      |--ins2
        |--5_1.png (must match the filename in the images excel sheet)
        |--5_2.png (must match the filename in the images excel sheet)
}
The format of the excel sheets is defined \href{https://metadatamanagement.readthedocs.io/de/latest/questions.html#questions-manuell-bzw-handcrafted}{here}.
}
\examples{
\dontrun{
# All examples do exactly the same. They convert everything under "./pages"
# into the MDM format and write the output in "./mdm/questions". Images will be
# searched in "./pages/Bilder/png".
convert_handcrafted_questionnaires_to_mdm_format(input_directory = "./pages")
convert_handcrafted_questionnaires_to_mdm_format(input_directory = "./pages", output_directory = "./output/questions")
convert_handcrafted_questionnaires_to_mdm_format(input_directory = "./pages", output_directory = "./output/questions", images_subdirectory = "Bilder/png")
}
}

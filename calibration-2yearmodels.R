
source(knitr::purl("C:\\Users\\hajar.hasannejadasl\\Documents\\prospect2022\\FinalModels+calibration/LR-UI-2year.rmd"))

source(knitr::purl("C:\\Users\\hajar.hasannejadasl\\Documents\\prospect2022\\FinalModels+calibration/UIRF-2-year.rmd"))

source(knitr::purl("C:\\Users\\hajar.hasannejadasl\\Documents\\prospect2022\\FinalModels+calibration/SVM-UI2year.rmd"))


Regression <- data.frame(Class = binaryOutcomeTest,
                      Prob = 1-test_prob)

Forest <- data.frame(Class = binaryOutcomeTest,
                 Prob = test_probRF[,1])

Support <- data.frame(Class = binaryOutcomeTest,
                 Prob = test_probSVM[,1])



CalTest <- dplyr::bind_rows(list(df1=Regression, df2=Forest, df3 =Support), .id = 'Source')
CalTest$Class = as.factor(CalTest$Class)
#CalVal <- dplyr::bind_rows(list(df1=ClinVal, df2=PromVal), .id = 'Source')

# Renaming factor levels dplyr
CalTest$Source <- recode_factor(CalTest$Source, df1 = "LR", df2 = "RF", df3 = "SVM")
#CalVal$Source <- recode_factor(CalVal$Source, df1 = "Clinical", df2 = "PROMs")

#remotes::install_github("tidymodels/probably")

library(tidymodels)
library(probably)



CalTest %>%
  group_by(Source) %>%
  cal_plot_breaks(Class, Prob,num_breaks = 5) +
  theme_classic() +
  theme(legend.position = "top",legend.direction = "horizontal") +
  theme(text = element_text(size=15, face = "bold")) + 
  labs(y = "Predicted UI",x = "Observed UI") +
  geom_abline(size = 1.5,color = "gray", linetype = "dashed" )


Test <- data.frame(Class = binaryOutcomeTest,
                         Reg = test_prob,
                         RF = test_probRF[,2],
                         SVM = test_probSVM[,2])

library(dcurves)
dca(Class ~ Reg + RF + SVM,
    data = Test,
    thresholds = seq(0, 0.90, by = 0.0001),
    label = list(Prob = "Decision tree")) %>%
  plot(smooth = TRUE,cex.lab=3, cex.axis=3)








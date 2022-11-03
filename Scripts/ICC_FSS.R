library(readxl)
library(psych)

FSS <- read_excel("Processed Data/R data/retest.xlsx")




Observe =~ Item1 + Item6 + Item11 + Item15 + Item20 + Item26 + Item31 + Item36 
Describe =~ Item2 + Item7 + Item27 + Item32 + Item37
Actaware =~ Item5 + Item8 + Item13 + Item18 + Item23 + Item28 + Item34 + Item22
Nonjudge =~  Item14 + Item25 + Item30 
Nonreact =~ Item9 + Item19 + Item21 + Item24 


FSS_total <- FSS %>% 
  rowwise() %>% 
  mutate(Observe_total = sum(Item1, Item6, Item11, Item15, Item20, 
                               Item26, Item31, Item36))

FSS_total.2 <- FSS_total %>% 
  rowwise() %>% 
  mutate(Describe_total = sum(Item2, Item7,  Item27, Item32, Item37))


FSS_total.2 <- FSS_total.2 %>% 
  rowwise() %>% 
  mutate(Actaware_total = sum(Item5, Item8, Item13, Item18,
                              Item23, Item28,  Item34, Item22))


FSS_total.2 <- FSS_total.2 %>% 
  rowwise() %>% 
  mutate(Nonjudge_total = sum(Item14, Item25, Item30))


FSS_total.2 <- FSS_total.2 %>% 
  rowwise() %>% 
  mutate(Nonreact_total = sum(Item9, Item19, Item21, Item24))

retest_Total <- FSS_total.2


write_csv(retest_Total, "Processed Data/retest_Total.csv")



FSS_ICC <- read_excel("Processed Data/R data/ICC_Test_retest.xlsx")


observe <- data.frame(FSS_ICC$Observe_one,FSS_ICC$Observe_two )
describe <- data.frame(FSS_ICC$Describe_one, FSS_ICC$Describe_two)
actaware <- data.frame(FSS_ICC$Actaware_one, FSS_ICC$Actaware_two)
nonjudge <- data.frame(FSS_ICC$Nonjudge_one, FSS_ICC$Nonjudge_two)
nonreact <- data.frame(FSS_ICC$Nonreact_one, FSS_ICC$Nonreact_two)
ICC(observe)
ICC(describe)
ICC(actaware)
ICC(nonjudge)
ICC(nonreact)

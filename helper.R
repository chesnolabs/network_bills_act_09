# Файл для оновлення проекту вручну
# Допоміжні дані для дашборду shiny

# Download the factions and groups of Rada-9  ####

get_factions_open <- function(){
  posts <- read_csv("https://data.rada.gov.ua/ogd/mps/skl9/mp-posts_ids.csv")
  posts_ids <- read_tsv("https://data.rada.gov.ua/ogd/mps/skl9/mp-posts_unit.txt", 
                        locale(encoding = "windows-1251"), col_names = F, col_types = NULL) %>% 
    rename(unit_id = X1, unit = X2)
  mps <- read_csv("https://data.rada.gov.ua/ogd/mps/skl9/mps09-data.csv")
  
  factions_full <- posts %>% 
    left_join(mps[, c("rada_id" ,"id", "full_name", "date_end", "region_name")], by = c("mp_id" = "id")) %>% 
    left_join(posts_ids) %>% 
    filter(unit_type %in% c("grp", "fra")) %>% 
    select(mp_id, full_name, unit)
  
  factions_df <-  mps %>% 
    #filter(is.na(resignation_text)) %>% 
    select(rada_id, id, full_name, region_name, date_end) %>% 
    left_join(factions_full, by = c("id" = "mp_id", "full_name")) %>% 
    mutate(unit = ifelse(is.na(unit), "Позафракційні", unit)) %>% 
    rename(factions = unit, fullname = full_name) %>% 
    mutate(factions = recode(factions,
                             `Фракція ПОЛІТИЧНОЇ ПАРТІЇ "СЛУГА НАРОДУ"` = "Слуга Народу",
                             `Фракція Політичної Партії "ГОЛОС" у Верховній Раді України дев'ятого скликання` = "ГОЛОС",
                             `Фракція ПОЛІТИЧНОЇ ПАРТІЇ "ЄВРОПЕЙСЬКА СОЛІДАРНІСТЬ"` = "ЄС",
                             `Фракція Політичної партії "ОПОЗИЦІЙНА ПЛАТФОРМА - ЗА ЖИТТЯ" у Верховній Раді України` = "ОПЗЖ",
                             `Фракція політичної партії Всеукраїнське об'єднання "Батьківщина" у Верховній Раді України дев'ятого скликання` = "Батьківщина",
                             `Група "Партія "За майбутнє"` = "За майбутнє",
                             `Група "ДОВІРА"`= "ДОВІРА"))%>%
    mutate(rada_id=as.character(rada_id)) %>% 
    mutate(rada_id=recode(rada_id, 
                          "208|438"="438"))%>% # Можливо тимчасово, бо не встигли змінити айдішник Радіної
    mutate(date_end = ifelse(is.na(date_end), "", date_end))%>% # Replace NA with a blank
    mutate(region_name = ifelse(is.na(region_name), "", region_name)) # Replace NA with a blank
  
  return(factions_df)
}

factions_09 <- get_factions_open()

# Членство в комітетах і фракціях ####
# Об'єднати з фракціями і групами Ради-9

factions_09_k <- factions_09 %>% 
  left_join(membership_k_short, by=c("fullname"="full_name_k"))%>% # membership_k_short у файлі helper_komitety.R 
  filter(date_end=="")%>% 
  mutate(department_k = ifelse(is.na(department_k), "Не є членами комітету", department_k))


# Скорочення назв комітетів ####

komit_members <- factions_09_k %>%  # 
  mutate(department_k = recode(department_k, 
                          "Комітет з питань прав людини, деокупації та реінтеграції тимчасово окупованих територій у Донецькій, Луганській областях та Автономної Республіки Крим, міста Севастополя, національних меншин і міжнаціональних відносин" = "З прав людини і деокупації",
                          "Комітет з питань організації державної влади, місцевого самоврядування, регіонального розвитку та містобудування" = "Держвлади і самоврядування",
                          "Комітет з питань Регламенту, депутатської етики та організації роботи Верховної Ради України" = "Регламентний комітет",
                          "Комітет з питань соціальної політики та захисту прав ветеранів" = "Соцполітика і захист ветеранів",
                          "Комітет з питань інтеграції України до Європейського Союзу" = "Інтеграція з ЄС",
                          "Комітет з питань зовнішньої політики та міжпарламентського співробітництва"="Зовнішня політика",
                          "Комітет з питань енергетики та житлово-комунальних послуг"="Енергетика і ЖКГ",
                          "Комітет з питань здоров'я нації, медичної допомоги та медичного страхування"="Медичний комітет",
                          "Комітет з питань національної безпеки, оборони та розвідки"="Нацбезпеки і оборони",
                          "Комітет з питань екологічної політики та природокористування"="Екологія і природа",
                          "Комітет з питань антикорупційної політики"="Антикорупційний комітет",
                          "Комітет з питань гуманітарної та інформаційної політики"="Гуманітарна і інформполітика",
                          "Комітет з питань транспорту та інфраструктури"="Транспорт і інфраструктура",
                          "Комітет з питань освіти, науки та інновацій"="Освіта і наука",
                          "Комітет з питань аграрної та земельної політики"="Аграрний комітет",
                          "Комітет з питань фінансів, податкової та митної політики"="Фінанси і податки",
                          "Комітет з питань правоохоронної діяльності"="Правоохоронний",
                          "Комітет з питань економічного розвитку"="Економрозвиток",
                          "Комітет з питань бюджету"="Бюджетний комітет",
                          "Комітет з питань правової політики"="Правова політика",
                          "Комітет з питань цифрової трансформації"="Цифрова трансформація",
                          "Комітет з питань молоді і спорту"="Молоді та спорту")
  )

# Вантажимо дані по ЗП ВРУ ####

# Головні виконавці ####
bills_executives <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_executives-skl9.csv", 
                             fileEncoding = "UTF-8" ) %>%
  filter(type=="mainExecutive") %>%
  select(bill_id, department)

# Законопроекти Ради-9, усі ####
# https://data.rada.gov.ua/open/data/bills_main-skl9
bills_main_skl9 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_main-skl9.csv", 
                            fileEncoding = "UTF-8") %>% 
  left_join(bills_executives)


# Ініціатори законопроектів ####
bills_initiators <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_initiators-skl9.csv",
                             fileEncoding = "UTF-8")%>%
  left_join(komit_members, by=c("person"="fullname"))     # Приєднуємо чинні фракції нардепів


# Чинні закони  ####
bills_acts09 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_acts-skl9.csv", 
                         fileEncoding = "UTF-8" )%>%
  left_join(bills_main_skl9, by=c("bill_id"="bill_id"))%>%
  left_join(bills_executives, by=c("bill_id"="bill_id")) %>% 
  left_join(bills_initiators, by=c("bill_id"="bill_id"))


# 1  -------- Ініціатори актів ####

list_initiators_acts <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_initiators-skl9.csv",
                                 fileEncoding = "UTF-8")%>%
  filter(convocation=="IX скл.")%>%
  filter(type=="official")%>%
  select(person, bill_id)%>%
  left_join(bills_main_skl9, by=c("bill_id"="bill_id")) %>% 
  filter(currentPhase_title=="Закон підписано") %>% 
  left_join(komit_members, by=c("person"="fullname"))%>%
  filter(date_end =="")%>% 
  group_by(person, factions, department_k, position_k) %>%   # Or group_by(person, bill_id)%>%
  summarise(weight_name=n()) %>%
  ungroup() # Скільки разів був ініціатором законопроектів



#  Hidden groupes to the list of initiators ####
# https://www.pravda.com.ua/articles/2021/02/18/7283826/
list_initiators_acts <- mutate(list_initiators_acts, 
                               hidden_groups = ifelse(person %in% c("Павлюк Максим Васильович", 
                                                                    "Марусяк Олег Романович",
                                                                    "Богданець Андрій Володимирович",
                                                                    "Божик Валерій Іванович",
                                                                    "Бунін Сергій Валерійович",
                                                                    "Воронько Олег Євгенійович",
                                                                    "Гайду Олександр Васильович",
                                                                    "Герасименко Ігор Леонідович",
                                                                    "Горобець Олександр Сергійович",
                                                                    "Гринчук Оксана Анатоліївна",
                                                                    "Грищенко Тетяна Миколаївна",
                                                                    "Дануца Олександр Анатолійович",
                                                                    "Діденко Юлія Олександрівна",
                                                                    "Заремський Максим Валентинович",
                                                                    "Кицак Богдан Вікторович",
                                                                    "Ковальчук Олександр Володимирович",
                                                                    "Кузбит Юрій Михайлович",
                                                                    "Лис Олена Георгіївна",
                                                                    "Мазурашу Георгій Георгійович",
                                                                    "Марченко Людмила Іванівна",
                                                                    "Мурдій Ігор Юрійович",
                                                                    "Нагаєвський Артем Сергійович",
                                                                    "Павліш Павло Васильович",
                                                                    "Соломчук Дмитро Вікторович",
                                                                    "Торохтій Богдан Григорович",
                                                                    "Чорноморов Артем Олегович",
                                                                    "Шол Маргарита Віталіївна"), 
                                                      "Група Павлюка",
                                                      # Source
                                                      # https://www.pravda.com.ua/articles/2021/03/4/7285446/
                                                      ifelse(person %in% c("Дубінський Олександр Анатолійович", 
                                                                           "Палиця Ігор Петрович",
                                                                           "Шаповалов Юрій Анатолійович",
                                                                           "Березін Максим Юрійович",
                                                                           "Боблях Андрій Ростиславович",
                                                                           "Бородін Владислав Валерійович",
                                                                           "Бужанський Максим Аркадійович",
                                                                           "Василевська-Смаглюк Ольга Михайлівна",
                                                                           "Демченко Сергій Олексійович",
                                                                           "Дунда Олег Андрійович",
                                                                           "Зуєв Максим Сергійович",
                                                                           "Касай Костянтин Іванович",
                                                                           "Куницький Олександр Олегович",
                                                                           "Ляшенко Анастасія Олексіївна",
                                                                           "Матусевич Олександр Борисович",
                                                                           "Мовчан Олексій Васильович",
                                                                           "Нестеренко Кирилл Олександрович",
                                                                           "Прощук Едуард Петрович",
                                                                           "Рубльов Вячеслав Володимирович",
                                                                           "Северин Сергій Сергійович",
                                                                           "Трухін Олександр Миколайович",
                                                                           "Фріс Ігор Павлович",
                                                                           "Чорний Дмитро Сергійович",
                                                                           "Швець Сергій Федорович",
                                                                           "Яременко Богдан Васильович"), 
                                                             "Група Коломойського", 
                                                             "Інше")))

head(list_initiators_acts,2)

# Save nodes ####
dir.create("data_network")

save(list_initiators_acts, file = paste0("list_initiators_acts_", Sys.Date(), ".Rda"))

write.csv(list_initiators_acts, file=paste0("/list_initiators_nodes_acts_", Sys.Date(), ".csv"))


# Завантажуємо ініціаторів проектів законів/актів ####

x_initiators_acts <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_initiators-skl9.csv",
                              fileEncoding = "UTF-8")%>%
  filter(convocation=="IX скл.")%>%
  filter(type=="official")%>%
  left_join(bills_main_skl9, by=c("bill_id"="bill_id"))%>%
  filter(currentPhase_title=="Закон підписано") %>% 
  left_join(komit_members, by=c("person"="fullname"))%>%
  filter(date_end=="") %>% 
  select(person, bill_id)%>%
  group_by(person, bill_id)%>%
  rename(person_x=person, 
         bill_id_x=bill_id)

head(x_initiators_acts,2)

#  Завантажуємо ініціаторів проектів законів/актів

y_initiators_acts <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_initiators-skl9.csv",
                              fileEncoding = "UTF-8")%>%
  filter(convocation=="IX скл.")%>%
  filter(type=="official")%>%
  left_join(bills_main_skl9, by=c("bill_id"="bill_id"))%>%
  filter(currentPhase_title=="Закон підписано") %>%
  left_join(komit_members, by=c("person"="fullname"))%>%
  filter(date_end=="") %>% 
  select(person, bill_id)%>%
  group_by(person, bill_id)%>%
  rename(person_y=person,
         bill_id_y=bill_id)

head(y_initiators_acts,2)

# Type 1 #### 

initiators_acts_x_y <- x_initiators_acts%>%
  inner_join(y_initiators_acts, by=c("bill_id_x"="bill_id_y"))%>% # Retain only rows in both sets
  filter(person_x != person_y)%>%                                 # Відкидаємо повторювання
  group_by(person_x, person_y) %>%                                # Групуємо без законопроектів
  count() %>% 
  mutate(pair = (
    sort(c(person_x, person_y)) %>% paste0(collapse = "|")
  )) %>% 
  group_by(pair) %>% 
  mutate(order = seq_along(pair))%>%
  filter(order == 1) %>% # # Є якась проблема у декількох ЗП з подвоєнням ініціаторів, тому треба відсіяти все, що більше 1
  left_join(list_initiators_acts, 
            by=c("person_x"="person"))#%>%                       # Додаємо ініціаторів для створення колонок від/фром

head(initiators_acts_x_y,2)


# Крок ####
initiators_acts_x_y3 <- initiators_acts_x_y%>%
  # Знову додаємо ініціаторів до попереднього датасету
  left_join(list_initiators_acts, by=c("person_y"="person")) %>% 
  ungroup()

head(initiators_acts_x_y3,1)


# 2 -------- Edges основний датасет ####
IA_x_y3 <- initiators_acts_x_y3 %>% 
  select(1:3)

# Зберігаємо наш один із основних файлів у форматі .Rda
save(IA_x_y3, file = paste0("IA_x_y3_", Sys.Date(), ".Rda"))

#load(paste0("IA_x_y3_", Sys.Date(), ".Rda"))
#head(IA_x_y3,2)


# Скоротити ПІБи у nodes and edges ####

short_nodes <- list_initiators_acts %>% 
  separate(person, c("name", "surname"), sep=" ")%>%
  unite("full_name", name, surname, sep=" ")


short_edges <- IA_x_y3 %>% 
  separate(person_x, c("name_x", "surname_x"), sep=" ")%>%
  unite("full_name_x", name_x, surname_x, sep=" ") %>% 
  
  separate(person_y, c("name_y", "surname_y"), sep=" ")%>%
  unite("full_name_y", name_y, surname_y, sep=" ") %>% 
  arrange(desc(n))


# Data For Gephi ####

library(igraph)
g <- graph_from_data_frame(short_edges, directed=FALSE, vertices=short_nodes)

network <- igraph_to_networkD3(g, group = vertex_attr(g, "factions", index = V(g)))

network$nodes$department <- vertex_attr(g, "department_k")
network$nodes$hidden_groups <- vertex_attr(g, "hidden_groups")
network$nodes$weight_name <- short_nodes$weight_name

write.csv(network$links, file = paste0("data_network/gephi_edges_act_bills_", Sys.Date(), ".csv"))
write.csv(network$nodes, file = paste0("data_network/gephi_nodes_act_bills_", Sys.Date(), ".csv"))


# Static network ####
library(igraph)
library(ggraph)


# *** Degree ####
degree(g)

max(degree(g))

# *** Closeness ####
closeness(g)
max(closeness(g))

1/(closeness(g) * 15)

mm <- 1/(closeness(g) * (vcount(g)-1))

mm <- as.data.frame(mm)

V(g)

vcount(g) # кількість вузлів

E(g)

ecount(g)

rr <- betweenness(g)

g$betw <- betweenness(g)


max(betweenness(g))

# Нормальний графік
plot(g, vertex.size = closeness(g) * 1000,
     main = "Closeness")



# Download factions of Rada-9  ####
# Файл для оновлення проекту вручну


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
                          "208"="438"))%>% # Можливо тимчасово, бо не встигли змінити айдішник Радіної
    mutate(date_end = ifelse(is.na(date_end), "", date_end))%>% # Replace NA with a blank
    mutate(region_name = ifelse(is.na(region_name), "", region_name)) # Replace NA with a blank
  
  return(factions_df)
}

factions_09 <- get_factions_open()


# Членство в комітетах ####

komit_members <- read.csv2("komit_members_04_02_2021.csv") %>% 
  mutate(full_name=str_squish(full_name),
         komitet=str_squish(komitet)) %>% 
  left_join(factions_09, by=c("full_name"="fullname")) %>% 
  select(-position, -rada_id, -id)

# Скорочення назв комітетів ####

komit_members <- komit_members %>%  # 
  mutate(komitet = recode(komitet, 
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

#head(komit_members,2)

# Комітет з питань інтеграції України до Європейського Союзу

# 1. Вантажимо дані для обробки ####

# Законопроекти Ради-9, усі ####
# https://data.rada.gov.ua/open/data/bills_main-skl9
bills_main_skl9 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_main-skl9.csv", 
                            fileEncoding = "UTF-8")

# Головні виконавці ####
bills_executives <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_executives-skl9.csv", 
                             fileEncoding = "UTF-8" ) %>%
  filter(type=="mainExecutive") %>%
  select(bill_id, department)

# Ініціатори законопроектів ####
bills_initiators <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_initiators-skl9.csv",
                             fileEncoding = "UTF-8")%>%
  left_join(factions_09, by=c("person"="fullname"))     # Приєднуємо чинні фракції нардепів


# Чинні закони  ####
bills_acts09 <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_acts-skl9.csv", 
                         fileEncoding = "UTF-8" )%>%
  left_join(bills_main_skl9, by=c("bill_id"="bill_id"))%>%
  left_join(bills_executives, by=c("bill_id"="bill_id")) %>% 
  left_join(bills_initiators, by=c("bill_id"="bill_id"))


# 2. Ініціатори актів ####

list_initiators_acts <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_initiators-skl9.csv",
                                 fileEncoding = "UTF-8")%>%
  filter(convocation=="IX скл.")%>%
  filter(type=="official")%>%
  select(person, bill_id)%>%
  left_join(bills_main_skl9, by=c("bill_id"="bill_id")) %>% 
  filter(currentPhase_title=="Закон підписано") %>% 
  left_join(komit_members, by=c("person"="full_name"))%>%
  filter(date_end =="")%>% 
  group_by(person, factions, komitet) %>%   # group_by(person, bill_id)%>%
  summarise(weight_name=n()) %>%
  #tibble::rowid_to_column("Id") %>% 
  ungroup() # Скільки разів був ініціатором законопроектів


#rename(Label=person) %>% 
#tibble::rowid_to_column("Id") 
#class(list_initiators_acts)
#head(list_initiators_acts,2)

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

# 

save(list_initiators_acts, file =  "list_initiators_acts.Rda")


dir.create("data_network/acts")

write.csv(list_initiators_acts, file=paste0("data_network/acts/list_initiators_nodes_acts_", Sys.Date(), ".csv"))



# Завантажуємо ініціаторів проектів законів/актів ####

x_initiators_acts <- read.csv("https://data.rada.gov.ua/ogd/zpr/skl9/bills_initiators-skl9.csv",
                              fileEncoding = "UTF-8")%>%
  filter(convocation=="IX скл.")%>%
  filter(type=="official")%>%
  left_join(bills_main_skl9, by=c("bill_id"="bill_id"))%>%
  filter(currentPhase_title=="Закон підписано") %>% 
  left_join(factions_09, by=c("person"="fullname"))%>%
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
  left_join(factions_09, by=c("person"="fullname"))%>%
  filter(date_end=="") %>% 
  select(person, bill_id)%>%
  group_by(person, bill_id)%>%
  rename(person_y=person,
         bill_id_y=bill_id)

head(y_initiators_acts,2)
# Джойними ініціаторів двох датасетів
# Тут утворюються пари, які найбільше одне з одним пов"язані по кількості законопроектів

# Type 1 #### 
# Треба вибрати щось одне 1 чи 2
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
  filter(order == 1) %>% # 
  #ungroup()%>%           # Є якась проблема у декількох ЗП з подвоєнням ініціаторів, тому треба відсіяти все, що більше 1
  left_join(list_initiators_acts, 
            by=c("person_x"="person"))#%>%                       # Додаємо ініціаторів для створення колонок від/фром
#select(-faction, -n)%>%
#dplyr::rename(from=Id,
#             weight_name_x=weight_name) # Створюємо частину еджес

head(initiators_acts_x_y,2)
# Type 2 #### 
# Треба вибрати щось одне 1 чи 2
# 
# initiators_bills_x_y <- x_initiators_bills%>%
#   inner_join(y_initiators_bills, by=c("bill_id_x"="bill_id_y"))%>%
#   filter(person_x != person_y)%>% 
#   group_by(person_x, person_y, bill_id_x) %>%  # Тут додаткове групування по біллс, окрім імен
#   count()%>%
#   filter(n==1)%>% # Є якась проблема у декількох законопроектах з подвоєнням ініціаторів, тому треба відсіяти все, що більше 1
#   left_join(list_initiators, 
#             by=c("person_x"="person"))%>% # Додаємо ініціаторів для створення колонок від/фром
#   #select(-faction, -n)%>%
#   rename(from=id)%>% # Створюємо частину еджес
#   rename(weight_name_x=weight_name)


# Крок ####
initiators_acts_x_y3 <- initiators_acts_x_y%>%
  #select(-faction, -komitet, hidden_groups)%>%
  left_join(list_initiators_acts, by=c("person_y"="person")) %>% # Знову додаємо ініціаторів до попереднього датасету
  #select(-faction)%>%
  ungroup()#%>%
#rename(to=Id)%>%  # Створюємо частинку еджес до/ту
#rename(weight_name_y=weight_name)

head(initiators_acts_x_y3,1)


# Для побудови мережі

IA_x_y3 <- initiators_acts_x_y3 %>% 
  select(1:3)

save(IA_x_y3, file =  "IA_x_y3.Rda")
load("IA_x_y3.Rda")
#head(IA_x_y3,2)

# Фінальний датасет із даними для Edges
initiators_acts_x_y4 <- initiators_acts_x_y3%>%
  select(from, to, n)%>% # Вибираємо це, якщо треба надалі записати файл edges_for_gephy
  #select(person_x, person_y, n)%>% #
  rename(Weight=n) # Для Гефі має бути "вага", а "н"


# Для гефі ####
edges_for_gephy_acts <- initiators_acts_x_y4%>%
  rename(Source=from, Target=to, Value=Weight)

dir.create("data_network")
dir.create("data_network/acts")

# Save network data ####

# Зберегти дані для подального використання в програмі ####
shortened_list_initiators <- list_initiators_acts %>% 
  separate(person, c("name", "surname"), sep=" ")%>%
  unite("full_name", name, surname, sep=" ")


short_LI <- shortened_list_initiators %>%
  select(1:3)


# Or here you can find short names of MPs 
write.csv(shortened_list_initiators, file = paste0("data_network/acts/nodes_bills_short_", Sys.Date(), ".csv"))


write.csv(edges_for_gephy_acts, file = paste0("data_network/acts/edges_bills_acts_", Sys.Date(), ".csv"))
write.csv(list_initiators_acts, file = paste0("data_network/acts/nodes_bills_acts_", Sys.Date(), ".csv"))


# Interactive network ####
library(igraph)
library(ggraph)

# Create an igraph object with attributes directly from dataframes




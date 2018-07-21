# Часть 2. Векторные данные
# Автор: Александр Шелудков, ИГРАН
# Дата: 24.07.2018


# Главные packeges для работы с пространственными данными в R: `sp` и `raster`. 
# Они определяют классы объектов, с которыми работают все остальные пакеты.

library(sp)
library(raster)

# ============================
# 2.1. Классы векторных данных
# ============================

# Точки (points) - простейший вид векторных данных. Каждая точка имееи одну координатную пару (x, y).  
# Линия (line) - набор точек (узлов), соединенный в определенном порядке.  
# Полигон (polygon) - замкнутая линия: первая координатная пара является и последней.  
# В пакете sp им соответствуют различные объекты класса Spatial.  

# Spatial* object можно представить в виде списка, 
# слоты которого хранят свойства объекта (параметры, методы)  

# Давайте создадим объект класса SpatialPoints из 10 случайных точек. 
# Для этого достаточно дать на вход соответствующей функции табличку с координатами

set.seed(12)
# Широта
latitude <- sample(-90:90, 10, replace = T)
# Долгота
longitude <- sample(-180:180, 10, replace = T)
lonlat <- cbind(longitude, latitude)
pts <- SpatialPoints(coords = lonlat)
# Посмотрим на структуру объекта
str(pts)


# Ничего не забыли? Нужно указать систему координат  

# Set the coordinate reference system
WGS84 <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
pts <- SpatialPoints(coords = lonlat, proj4string = WGS84)
pts


# Чтобы создать SpatialLines и SpatialPlygons 
# воспользуемся функциями spLines() and spPlolygons() из пакета raster
lns <- spLines(lonlat, crs=WGS84)               # линии
pols <- spPolygons(lonlat, crs=WGS84)           # полигоны
lns
pols


# Посмотрим, как это выглядит на карте
plot(pols, col = "#e5f5f9", axes = T)           # plot polygon
plot(lns, col = "#99d8c9", add = T)             # add lines
plot(pts, col = "#2ca25f", pch = 20, add = T)   # add points

# ========================
# 2.2. Атрибутивные данные
# ========================

# Spatial object = geometry + attributes
  
# Данные, привязанные к пространственным объектам, называются атрибутами.
# Отдельные features мы рассматриваем как наблюдения, число переменных не ограничено.  

# Давайте добавим нашим точкам атрибуты
# Создадим data.frame с данными
df <- data.frame(ID=1:nrow(lonlat), var=longitude^2/max(longitude)*10)
# Получилась вот такая табличечка
df
# Соединяем геометрию и атрибуты
ptsdf <- SpatialPointsDataFrame(pts, df)

# Визуализируем
plot(pols, col = "#e5f5f9", axes = T)           # plot polygon
plot(lns, col = "#99d8c9", add = T)             # add lines
plot(ptsdf, col = "#2ca25f", pch = 20, 
     cex = log(ptsdf$var),                      # устанавливаем размер точек
     add = T)

# ======================
# 2.3. Форматы геоданных
# ======================

# Третий must-have пакет для работы с пространственными данными в R: rgdal. 
# Это обертка для GDAL/OGR (Geospatial Data Abstraction Library) - открытой библиотеки, 
# написанной на C и используемой во всех ключевых ГИС-программах (ArcGIS, QGIS, SAGA GIS и др.).

library(rgdal)

# Самые распространенные форматы геоданных - ESRI Shapefile и GeoJSON.
# Загрузим данные об административно-территориальных границах проекта
# [Natural Earth](https://www.naturalearthdata.com/downloads/10m-cultural-vectors/) 

# Предобработка файла
# countries@data %>% select(SOVEREIGNT, TYPE, ADMIN, POP_EST, GDP_MD_EST, POP_YEAR, GDP_YEAR, ECONOMY, INCOME_GRP, ISO_A2, REGION_WB) -> countries@data
# countries@data[countries@data$ADMIN == "France",10] <- "FR"
# countries@data[countries@data$ADMIN == "Norway",10] <- "NO"
# countries@data$POP_EST <- as.numeric(countries@data$POP_EST)/1000
# colnames(countries@data) <- c("SOVEREIGNT", "TYPE", "ADMIN", "POP_THS", "GDP_MD_EST", "POP_YEAR", "GDP_YEAR", "ECONOMY", "INCOME_GRP", "ISO_A2", "REGION_WB")
# writeOGR(countries, "data/world/world.shp", "world.shp", driver = "ESRI Shapefile", overwrite_layer = T)

# из файла в формате shapefile. Для этого используем функцию readOGR() из пакета rgdal.
countries <- readOGR(dsn = "data/world/world.shp", stringsAsFactors = F)
plot(countries)

# В случае с shapefiles мы также можем использовать обертку для rgdal из пакета raster
countries <- shapefile("data/world/world.shp", stringsAsFactors = F)

# Что из себя представляет объект countries?
countries
# Взглянем на таблицу атрибутов
View(countries@data)

# ================
# 2.4. Картограммы
# ================

# Какие данные хранит countries?
# POP_THS: численность наслеения, тыс. чел.
# GDP_MD_EST: ВВП, млн $
head(countries@data)

# ==================================================================================
# На этом занятии наша задача - построить картограмму плотности населения по странам

# Для работы с атрибутивными данными и визуализации понадобится еще несколько пакетов  
library(dplyr)
library(ggplot2)
library(classInt)
library(RColorBrewer)
library(cartography)

# 2.4.1 Изменение атрибутивных данных

# Рассчитаем площадь стран с помощью функции `area()` из пакета `raster`
countries@data$AREA <- area(countries)/1000000  # км2
# Плотность населения по странам
countries@data %>% 
  mutate(POP_DENS = round(x = POP_THS*1000/AREA, digits = 2)) -> 
  countries@data

# Отлично! Однако прежде чем строить саму карту, надо понять, 
# на какие интервалы мы разобьем наблюдения, и выбрать цветовую палитру  

# 2.4.2 Как разделить вектор на интервалы 

# Посмотрим на распределение данных  
countries@data %>% 
  ggplot(aes(POP_DENS))+
  geom_histogram(binwidth = 200)
# Есть выбросы, поэтому равные интервалы не подходят.    

# Функция classIntervals() из пакета classInt принимает на вход числовой вектор и 
# возвращает границы интервалов в зависимости от выбранного метода: "fixed", "sd", 
# "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher" или "jenks"  
# Я выбрал квантили
pop_den_ints <- classIntervals(var = countries@data$POP_DENS, n = 7, style = "quantile")
pop_den_ints

# 2.4.3 Цветовые палитры 

# В R есть встроенный набор цветовых схем, однако у картографов есть свои любимчики.
# Во-первых, это библиотека [ColorBrewer](http://colorbrewer2.org/) и 
# созданный на ее основе пакет `RColorBrewer`. 
# Во-вторых, `viridis` - стандартный набор цветовых схем для карт в библиотеке matplotlib.

# 2.4.4 Строим картограмму (choropleth map) с помощью Cartography

# Plot в `cartography` создается послойно. Сначала рисуем layout, затем основной слой, 
# а сверху добавляем легенду. Для разных типов карт (Label Map, Сhoropleth Ьap, Proportional Symbols, Gridded Map и др.) 
# в пакете есть отдельные функции. В случае с картограммами это `choroLayer()`  

# Сократим поля (по умолчанию со всех сторон 2)
par(mar = c(1,0,1,0))

# Layout layer
layoutLayer(title = "Population Density", author = "Al.Shel. for AnDan",
            sources = "Natural Earth, 2018", col = NA, frame = FALSE,
            bg = "#A6CAE0",
            scale = NULL, coltitle = "black", extent = countries)
# Основной слой
choroLayer(spdf = countries,                       # объект SpatialPolygonsDataFrame
           var = "POP_DENS",                       # картографируемая переменная (столбец) 
           border = "grey1", lwd = 0.5,            # цвет и толщина границ полигонов
           breaks = pop_den_ints$brks,             # границы интервалов
           col = rev(brewer.pal(7, "Spectral")),   # color palette 
           legend.pos = "n",                       # none - легенду отрисуем позже
           add = T)
# Легенда
legendChoro(pos = "left", title.txt = "per/sq km", 
            values.cex = 0.7, breaks = pop_den_ints$brks, values.rnd = 0,
            col = rev(brewer.pal(7, "Spectral")), frame = T, symbol = "box", nodata = T)

# Посмотреть/почитать: 
# https://cran.r-project.org/web/packages/cartography/vignettes/cartography.html#base-map-and-proportional-symbols


# ===============================
# 2.4.5. Самостоятельная работа 1
# ===============================

# Задание: Рассчитайте для стран ВВП на душу населения, разбейте на собственное усмотрение 
# полученные наблюдения на группы (страны с высоким GDP per capita, средним и пр.) и 
# отобразите эти группы на карте.


# =============================
# 2.5. Самостоятельная работа 2
# =============================

# Задание: с 2012 года CRAN ежедневно публикует [анонимизированные данные](http://cran-logs.rstudio.com) 
# о скачивании пакетов для R. Возьмите любой день (вчера? ваш день рождения? и т.д.) и проанализируйте, 
# какие пакеты чаще всего качают пользователи и из каких они стран. *Создайте карту, которая показывает 
# территориальное распределение пользователей*. 
# Описание структуры данных вы найдете на странице cran-logs.  
                           
# Подсказка: чтобы найти координаты центроида полигона, используйте `coordinates()` из пакета `sp`   

# ====================
# 2.6. Быстрый leaflet
# ====================
                          
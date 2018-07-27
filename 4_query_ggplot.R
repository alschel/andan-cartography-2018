# Часть 4. Пространственные выборки. ggplot
# Автор: Александр Шелудков, ИГРАН
# Дата: 25.07.2018

library(rgdal)
library(rgeos)

# ============================
# 4.3 Пространственные выборки
# ============================

rivers <- readOGR(dsn = "data/rivers.geojson")
rivers <- raster::shapefile("data/rivers.geojson")
bars <- readOGR("data/bars.geojson")
rivers
bars@data %>% View()

# Предположим, мы хотим найти бары в 300 м от реки
# Для этого построим буфер и выделим точки внутри этого буфера
# Однако в ситуации с нашими данными есть одно большое НО. Какое?

# 4.3.1 Перепроецирование

# The Universal Transverse Mercator (UTM) делит Землю на 60 зон по 6 градусов. Москва находится 
# в зоне 37N (http://spatialreference.org/ref/epsg/32637/)
UTM <- CRS("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# Перепроецируем наши данные
library(raster)
moscow_utm <- aggregate(moscow) %>% spTransform(UTM)
moscow_utm
rivers_utm <- spTransform(rivers, UTM)
bars_utm <- spTransform(bars, UTM)

# 4.3.2 Intersection

# Обрежем реки по границе Москвы
# Для этого используем intersect() из пакета raster
raster::intersect(rivers_utm, moscow_utm) -> rivers_utm
raster::intersect(bars_utm, moscow_utm) -> bars_utm

# Строим буфер вокруг рек
gBuffer(rivers_utm, width = 300) -> river_buffer
plot(river_buffer)

# Выделим бары внутри буфера 
bars_utm %>% raster::intersect(river_buffer) -> my_bars
plot(river_buffer)
plot(my_bars, add = T)
my_bars@data %>% View()
# ==================
# 4.4 Карты в ggplot
# ==================


# 4.4.1 Полигоны

# Подготовка данных
# Так как ggplot привык работать с data.frame, первое, что мы должны сделать -
# преобразовать Spatial* object в df так, чтобы у нас появились колонки long и lat
# Делается это с помощью встроенной в ggplot2 функции fortify() 
fortify(moscow) %>% dplyr::mutate(id = as.numeric(id)) -> moscow_fort 
str(moscow_fort)

# Единственная проблема: во время преобразования fortify() отбрасывает атрибутивные данные
# Придется их снова приклеить
moscow@data %>% 
  mutate(id = 0:(nrow(moscow@data)-1)) %>% 
  right_join(moscow_fort, by = c("id" = "id")) -> moscow_fort

# А теперь 
ggplot()+
  geom_polygon(data = moscow_fort,
               aes(x = long, y = lat, group = group,
                   fill = factor(NAME)))

# Посмотреть/почитать: https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/

# 4.4.2 Точки

# С точками немного проще: сохраним координаты в отдельные столбцы
bars@data %>% 
  mutate(x = coordinates(bars)[,1],
         y = coordinates(bars)[,2]) -> bars@data

# Визуализируем
ggplot()+
  geom_polygon(data = aggregate(moscow), 
               aes(x = long, y = lat, group = group), alpha = 0.5, col = "grey3", fill = "steelblue" )+
  geom_point(data = bars@data, aes(x = x, y = y, col = factor(craft_beer)), 
             size = 1.3,  alpha = 0.5)

# 4.4.3 Density

ggplot(bars@data, aes(x = x, y = y))+
  geom_polygon(data = aggregate(moscow), 
               aes(x = long, y = lat, group = group), alpha = 0.5, col = "grey3", fill = "steelblue" )
               
             
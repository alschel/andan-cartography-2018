# Часть 4. Пространственные выборки. ggplot
# Автор: Александр Шелудков, ИГРАН
# Дата: 25.07.2018

library(rgdal)
library(rgeos)

# ============================
# 4.3 Пространственные выборки
# ============================

rivers <- readOGR("data/rivers.geojson")
bars <- readOGR("data/bars.geojson")
rivers
bars

# Предположим, мы хотим найти бары в 300 м от реки
# Для этого построим буфер и выделим точки внутри этого буфера
# Однако в ситуации с нашими данными есть одно большое НО. Какое?

# 4.3.1 Перепроецирование

# The Universal Transverse Mercator (UTM) делит Землю на 60 зон по 6 градусов. Москва находится 
# в зоне 37N (http://spatialreference.org/ref/epsg/32637/)
UTM <- CRS("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# Перепроецируем наши данные
moscow_utm <- aggregate(moscow) %>% spTransform(UTM)
moscow_utm
rivers_utm <- spTransform(rivers, UTM)
bars_utm <- spTransform(bars, UTM)

# 4.3.2 Intersection

# Обрежем реки по границе Москвы
# Для этого используем intersect() из пакета rgeos
raster::intersect(rivers_utm, moscow_utm) -> rivers_utm

# Строим буфер вокруг рек
gBuffer(rivers_utm, width = 300) -> river_buffer

# Выделим бары внутри буфера 
bars_utm %>% raster::intersect(river_buffer) -> my_bars
plot(river_buffer)
plot(my_bars, add = T)

# ==================
# 4.4 Карты в ggplot
# ==================

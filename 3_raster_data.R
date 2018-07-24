# Часть 3. Растровые данные
# Автор: Александр Шелудков, ИГРАН
# Дата: 25.07.2018

#=================================
# 3.2 Классы растровых изображений
#=================================

# Пакет raster позволяет создавать объекты трех классов: RasterLayer, RasterStack и RasterBrick

# RasterLayer состоит из 1 слоя (1 переменная)
r <- raster(ncol=10, nrow=10, xmx=-80, xmn=-150, ymn=20, ymx=60) # creates an "empty" RasterLayer object
r
# В данном примере мы создали RasterLayer, состоящий из 100 ячеек (10*10). 
# Обратите внимание на систему координат объекта.
# В каких единицах указано разрешение растра (размеры одной ячейки)?

values(r) # Сейчас ячейки пустые, наполним их значениями
values(r) <- 1:ncell(r)
plot(r)

# RasterStack и RasterBrick это классы растровых объектов с множеством слоев. 
# Разница в том, как они хранят данные: RasterStack - это список (list) из нескольких файлов 
# или слоев ("bands") одного файла, имеющих одинаковый экстент и разрешение. 
# RasterBrick - это всегда один многослойный объект, поэтому расчеты с ним могут быть быстрее. 

# RasterStack
s <- stack(r, r*r)
s
plot(s)
# RasterBrick
b <- brick(s)
b
plot(b)

#=============================
# 3.3. Чтение растровых данных
#=============================

# Для примера посмотрим на данные о высотах из проекта ASTER GDEM 2011
# Read raster data
elev <- raster("data/ASTGTM2_N55E037_dem.tif")
elev

# Что это за территория? Попробуем взглянуть на картинку    
plot(elev)
  
#==============================================================
# 3.4. Визуализация растровых изображений с помощью `rasterVis`
#==============================================================
  
# Построим простую карту высот  

# 3.4.1 Подготовка данных

# Для начала удалим все лишнее, обрезав растр в границах города
# Чтение данных о границах
moscow <- shapefile("data/mos_ao/ao.shp")
plot(moscow)
moscow@data

# Чтение данных о границах
elev %>% 
  crop(moscow) %>%                      # returns raster clipped by Spatial* object’s extent
  mask(moscow) -> elev_cropped          # returns raster, clipped by Spatial* object’s contour
elev_cropped
plot(elev_cropped)

# 3.4.2 Интервалы

# Какой перевад высот мы получили? Взглянем на гистограмму
ggplot()+
  geom_histogram(aes(values(elev_cropped)))
summary(values(elev_cropped))
# Есть небольшое число выбросов, проигнорируем их   

# 3.4.3 Levelplot

library(rasterVis)
levelplot(elev_cropped, 
          at=seq(75, 300, 25),
          col.regions=colorRampPalette(rev(brewer.pal(10, 'RdYlGn'))),
          margin=FALSE, colorkey=list(space='bottom',
                                      labels=list(at=seq(75, 300, 25), font=4),
                                      axis.line=list(col='black'),
                                      width=0.75),
          par.settings=list(strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='transparent')),
          xlab = NULL,
          ylab = NULL,
          scales=list(draw=FALSE),
          alpha.regions = 1)

# ================
# 3.5. А что если?
# ================

# 3.5.1 А что если я хочу вытащить статистику из растра по отдельным полигонам и не обрезать растр каждый раз  

extract(elev_cropped,   # RasterLayer
        moscow,         # SpatialPolygon
        fun = mean,     # функция
        # sp = T,       # сохрани результат расчета в таблицу атрибутов Spatial* объекта
        df = T)         # верни табличку 

# а что если растр такой тяжелый, а у меня не видеокарта для майнинга?  
# Тогда используй пакет velox. Он использует другие алгоритмы вычислений. 
# Минус в том, что velox работает только с объектами собственных классов  

library(velox)
# создаем объект velox из нашего растра
my_velox <- velox(elev_cropped)
# доступные функции хранятся в свойствах самого объекта.
# Вызываем их через $
my_velox$extract(sp = moscow, 
                 fun = mean, 
                 df = T)

# 3.5.2 А что если я хочу добавить в levelplot векторные объекты?

levelplot(elev_cropped, 
          at=seq(75, 300, 25),
          col.regions=colorRampPalette(rev(brewer.pal(10, 'RdYlGn'))),
          margin=FALSE, colorkey=list(space='bottom',
                                      labels=list(at=seq(75, 300, 25), font=4),
                                      axis.line=list(col='black'),
                                      width=0.75),
          par.settings=list(strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='transparent')),
          xlab = NULL,
          ylab = NULL,
          scales=list(draw=FALSE),
          alpha.regions = 1)+
  layer(sp.lines(moscow, col = "grey32", lwd = 1.5))


# 3.5.3 А что если я хочу хочу сохранить мой растр?

writeRaster(elev_cropped, filename = "data/Moscow_elevation.tif")

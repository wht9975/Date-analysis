library(rio)
library(tidyverse)
library(MetBrewer)
library(showtext); showtext_auto()
library(ggmap)
library(mapproj)
library(sf)
library(ggrepel)
library(extrafont)
library(RColorBrewer)
library(cowplot)
font_add_google("Calligraffitti", "call")
font_add_google("Kalam", "kalam")
font_add_google("ZCOOL KuaiLe", "cool")

china_map = st_read(dsn = "http://xzqh.mca.gov.cn/data/quanguo.json", stringsAsFactors = FALSE)
st_crs(china_map) <- 4326
data1 <- import('GDP/Chinas GDP in Province En.csv')
line <- st_read('GDP/geojson/九段线GS（2019）1719号.geojson')

GDP <- data1 %>% 
  rename(Guangxi = `Guangxi,`,
         year = V1) %>% 
  pivot_longer(cols = 2:ncol(data1),names_to = 'Province', values_to = 'gdp') %>% 
  pivot_wider(names_from = year,values_from = gdp, names_prefix = 'year',names_sort = T) %>% 
  mutate(quhao = case_when(
         Province == 'Beijing' ~ '110000',
         Province == 'Tianjin' ~ '120000',
         Province == 'Hebei' ~ '130000',
         Province == 'Shanxi' ~ '140000',
         Province == 'Inner Mongolia'~ '150000',
         Province == 'Liaoning'~ '210000',
         Province == 'Jilin'~ '220000',
         Province == 'Heilongjiang'~ '230000',
         Province == 'Shanghai'~ '310000',
         Province == 'Jiangsu'~ '320000',
         Province == 'Zhejiang'~ '330000',
         Province == 'Anhui'~ '340000',
         Province == 'Fujian'~ '350000',
         Province == 'Jiangxi'~ '360000',
         Province == 'Shandong'~ '370000',
         Province == 'Henan'~ '410000',
         Province == 'Hubei'~ '420000',
         Province == 'Hunan'~ '430000',
         Province == 'Guangdong'~ '440000',
         Province == 'Guangxi'~ '450000',
         Province == 'Hainan'~ '460000',
         Province == 'Chongqing'~ '500000',
         Province == 'Sichuan'~ '510000',
         Province == 'Guizhou'~ '520000',
         Province == 'Yunnan'~ '530000',
         Province == 'Tibet'~ '540000',
         Province == 'Shaanxi'~ '610000',
         Province == 'Gansu'~ '620000',
         Province == 'Qinghai'~ '630000',
         Province == 'Ningxia'~ '640000',
         Province == 'Xinjiang'~ '650000')) %>% 
    select(Province, quhao, everything()) %>% 
    mutate(ratio = (year2020 - year1992)/(year1992))

china <-china_map %>% 
  rename(quhao = QUHUADAIMA) %>% 
  mutate(quhao = case_when(quhao== 'daodian' ~ NAME,
                           TRUE ~ quhao)) %>% 
  select(quhao,geometry)

GDP_map <- left_join(china, GDP, by = 'quhao') 

 

GDP %>% pivot_longer(year1992:year2020, names_to = 'year',values_to = 'gdp',names_prefix = 'year') %>%
  mutate(area = case_when(substring(quhao,1,1) == '1' ~ '华北',
                          substring(quhao,1,1) == '2' ~ '东北',
                          substring(quhao,1,1) == '3' ~ '华东',
                          quhao %in% c('410000','420000','430000') ~ '华中',
                          quhao %in% c('440000','450000','460000') ~ '华南',
                          substring(quhao,1,1) == '5' ~ '西南',
                          substring(quhao,1,1) == '6' ~ '西北')) -> GDP_area 
GDP_area %>%  
  ggplot(aes(x=year, y = gdp))+
  facet_wrap(~area,ncol = 2,strip.position = "top")+
  stat_summary(aes(x = year, y = gdp, group = area), fun.y = 'mean', geom = 'line',
               size = 0.6, lty = 'dotted',color = 'black')+
  geom_line(aes(group = Province,color = quhao),size = 0.6, alpha = 0.7,show.legend = F)+
  geom_text_repel(data = GDP_area[GDP_area$year == '2020',],aes(label = Province),
                  nudge_y = 1500,max.overlaps = 13, nudge_x = 1, size = 6, alpha = 1, family = 'kalam')+
  theme_classic()+
  scale_color_manual(values = met.brewer('Signac',length(unique(GDP_area$quhao))),guide='none')+
  labs(x = 'Year', y = 'GDP')+
  theme(panel.grid.major = element_line(),
        strip.background = element_blank(),
        strip.text = element_text(size = 18, family = 'wqy-microhei'),
        strip.placement = 'outside',
        axis.title = element_text(size = 30, family = 'kalam'),
        axis.text.y = element_text(size = 18,family = 'kalam'),
        axis.text.x = element_text(size = 14, angle = 10, family = 'kalam'),
        ) -> GDP_line

GDP_area %>%  
  select(Province, quhao, ratio, area) %>% 
  dplyr::distinct() %>% #distinct(df, x, .keep_all = TRUE)  
  arrange(ratio) %>% 
  mutate(Province = fct_reorder(Province, ratio),
         angle = 90 - 360*((as.numeric(Province) - 0.5)/(max(as.numeric(Province))+4)), #+4 流出空白区域
         hjust = if_else(angle < -90,1,0),
         angle = if_else(angle < -90,angle + 180,angle)) -> GDP_col
GDP_col %>% 
  ggplot(aes(x= as.numeric(Province), y = ratio)) + 
  geom_col(aes(fill = ratio),color = '#8F999F',show.legend = F, size = 0.3)+
  coord_polar() +
  geom_text(aes(y = ratio+0.2,label = Province, angle = angle, hjust = hjust), vjust = 0.5, size = 7, family = 'kalam') +
  scale_y_continuous(expand = c(0,0), limits = c(-10, max(GDP_col$ratio)+10), labels = scales::percent_format()) +
  xlim(c(0,nrow(GDP_col)+4))+
  geom_segment(data = data.frame(x = rep(nrow(GDP_col)+1,3),
                                 xend = rep(nrow(GDP_col)+4,3),
                                 y = c(15,30,60),
                                 yend = c(15,30,60)),
               aes(x = x, xend = xend , y =y, yend =yend), color = '#8F999F',size = 0.6) +
  geom_segment(aes(x = 0, xend = nrow(GDP_col)+4, y = 0 , yend = 0),color = '#8F999F',size = 0.3)+ #内部圆环
  geom_text(data = data.frame(label = c('1,500%',"3,000%","6,000%"),
                              y = c(15,30,60)),
            aes(x = nrow(GDP_col)+2.5, y = y + 2, label =label),vjust = 0, 
            angle = 360 - 360*(nrow(GDP_col)+2.5)/(max(as.numeric(GDP_col$Province))+4),
            size = 8, family = 'kalam',color = '#8F999F')+
  theme_void()+
  scale_fill_gradientn(colors=met.brewer("Tam", direction = -1)) -> GDP_cycle



GDP_col %>% 
  select(Province,ratio) %>% 
  mutate(relative = ratio - mean(ratio),
         hjust = if_else(relative >= 0, 0,1),
         type = case_when(relative >= 15 ~ 1,
                         10 <= relative & relative < 15  ~ 2,
                         5 <= relative & relative <  10  ~ 3,
                         0 <= relative & relative <  5  ~ 4,
                        -5 <= relative & relative < 0  ~ 5,
                        -10 <= relative & relative < -5  ~ 6,
                        -15 <= relative & relative < -10  ~ 7,
                         relative < -15 ~ 8),
         type = factor(as.character(type)))%>% 
  right_join(GDP_map) %>% 
  arrange(desc(relative)) %>% 
  mutate(fill = case_when(type == "1" ~ "#FC4E2A",
                          type == "2" ~ "#FD8D3C",
                          type == "3" ~ "#FEB24C",
                          type == "4" ~ "#FED976",
                          type == "5" ~ "#C6DBEF",
                          type == "6" ~ "#9ECAE1",
                          type == "7" ~ "#6BAED6",
                          type == "8" ~ "#2470a0",
                          TRUE ~ "lightgrey")) -> map
  


map %>% 
  ggplot()+
  geom_sf(aes(fill = fill, geometry = geometry),show.legend = F,color = 'lightgrey', size = 0.5) +
  scale_fill_identity()+
  theme_void()+
  theme(aspect.ratio = 0.93) -> MGDP

map %>% 
  ggplot()+
  geom_sf(aes(fill = fill, geometry = geometry),show.legend = F,color = 'lightgrey', size = 0.5) +
  scale_fill_identity()+
  geom_sf(data = line) +
  coord_sf(xlim = c(108,124), ylim = c(2,24))+
  theme(aspect.ratio = 1.5, #调节长宽比
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(), #panel - 分面或者单图的画布信息
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, color="grey10",linetype=1,size=0.5),
        plot.margin=unit(c(0,0,0,0),"mm")) -> nine_line

map %>%
  mutate(Province = fct_reorder(Province, relative),
         label = paste0(Province,"(",scales::comma(round(relative*100),1),"%",")")) %>% 
  distinct(Province, .keep_all = T) %>% 
  filter(!is.na(Province)) %>% 
  ggplot()+
  geom_col(aes(x=Province,y = relative, fill = fill, color = fill)) +
  scale_fill_identity()+
  scale_color_identity()+
  coord_flip()+
  geom_text(aes(x = Province, y = relative+0.2 - hjust * 0.4 ,label = label, hjust = hjust), vjust = 0.5, size = 7, family = 'kalam') +
  scale_y_continuous(labels = scales::percent_format(), limits = c(-27,20)) +
  theme_classic()+
  theme(axis.ticks= element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 18)) -> relative_plot
ggdraw()+
  draw_plot(relative_plot,x = 0.45, y = 0.5,hjust = 0.5, vjust = 0.5, width = 0.9,height = 0.9)+
  draw_plot(MGDP, x = 0.75, y = 0.20,hjust = 0.5, vjust = 0.5,width = 0.75,height = 0.75)+
  draw_plot(nine_line, x = 0.95, y = 0.12,hjust = 0.5, vjust = 0.5, width = 0.2,height = 0.2)+
  draw_text('1992~2020年中国各省份GDP相对增长速率',x = 0.25, y = 0.9,hjust = 0.5, vjust = 0.5, size = 56, family = 'cool')+
  draw_text('自1992年以来各省的GDP都呈现快速的增长模式，但从此部分的可视化结果可以看出，与全国GDP平均增长率相比，各省间的',x = 0.25, y = 0.85,hjust = 0.5, vjust = 0.5, size = 20, family = 'cool')

